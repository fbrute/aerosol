input = list()
input$daterange1[1] <- "2008-01-01"
input$daterange[2] <- "2012-12-31"

input$lowerpressure <- 700
input$higherpressure <- 850


main_power <- function () {

        library(RMySQL)

        dbg <- 0

        querystring <- paste(
                "select date, time , pressure, height, mixr, temp",
                "from sounding1",
                "where date between", "'", input$daterange[1], "'" ,
                "and" , "'", input$daterange[2], "'" ,
                "and time = '12:00:00'",
                "and pressure between", input$lowerpressure ,"and", input$higherpressure,
                "order by date, pressure desc")

        con <- dbConnect(dbDriver("MySQL"), user="dbmeteodb",
                         password="dbmeteodb",
                         dbname="dbmeteodb",
                         host="localhost")

        # send the query

        #browser()
        queryResultsData <-  dbSendQuery(con, querystring)


        #get the data
        soundingdata <-  fetch(queryResultsData, n=-1)
        # freeing resources
        dbClearResult(queryResultsData)
        dbDisconnect(con)
        #View(soundingdata)


        #browser()

        #soundingdata <- getMysqlData(QueryString)

        matrixadiab <- getAdiabTemps(soundingdata)
        #                 temps <- getAdiabTemps(soundingdata)
        #                 surfs <- getAdiabTemps(soundingdata)



        if(dbg) browser()

        temps <- matrixadiab[,1]
        surfs <- matrixadiab[,2]

        soundingdata['tempadiab'] <- temps
        soundingdata['surfadiab'] <- surfs

        #data2 <- sqldf("select * from ")
        datelevels <- factor(soundingdata$date)
        data2 <- subset(soundingdata,select = c(date,surfadiab))
        dfs <- split(data2, datelevels)
        lapply(dfs, function(elt) sum(elt$surfadiab))
        unlist(lapply(dfs, function(elt) sum(elt$surfadiab)))

        powers <- as.numeric(unlist(lapply(dfs, function(elt) sum(elt$surfadiab))))

        df.powers <- data.frame(levels(datelevels), powers)

        colnames(df.powers) <- c("date", "power")

        #save(soundingdata, file="soundingdata.RData")
        #save(df.powers, file="df.powers.RData")

        #View(df.powers)


        fname <- paste(as.character(input$daterange[1]),
                       "_",
                       as.character(input$daterange[2]),
                       ".pdf",
                       sep="")

        df.powers

}

getAdiabTempsforOneDate <- function (data) {
        # go through all columns for same date
        temps <- numeric()
        for (i in 1:length(data)) {

                if (i == 1)
                        temps[i] = data$temp[i]
                else
                        temps[i] = calcAdiabTemp(temps[i-1],
                                                 data$pressure[i-1],
                                                 data$pressure[i])
        }
        temps
}

getAdiabTemps <- function (data1) {
        # go through all columns for same date
        temps <- numeric()
        surfs <- numeric()
        currentdate <- Sys.Date()
        triangle <- F
        for (i in 1:nrow(data1)) {
                #if (dbg) browser()
                if (data1$date[i] != currentdate){
                        temps[i] <- data1$temp[i]
                        surfs[i] <- 0
                        triangle <- T
                        currentdate <- data1$date[i]
                }

                else {
                        temps[i] = calcAdiabTemp(temps[i-1],
                                                 data1$pressure[i-1],
                                                 data1$pressure[i])
                        if (triangle) {
                                # side a of triangle, parallel to x axis
                                a <- abs(temps[i] - data1$temp[i])

                                # side b of triangle along the sounding curve
                                b <- calcDistance(temps[i-1], data1$pressure[i-1],
                                                  data1$temp[i], data1$pressure[i])
                                # side c of triangle along the adiabatic curve
                                c <- calcDistance(temps[i-1], data1$pressure[i-1],
                                                  temps[i], data1$pressure[i])

                                #if (dbg) browser()

                                surfs[i] <- round(calcTriangleArea(a,b,c),0)

                                triangle <- F}

                        else {
                                # side a,  at top of trapeze
                                a <- abs(temps[i] - data1$temp[i])
                                # size b, along the sounding curve
                                b <- calcDistance(temps[i-1], data1$pressure[i-1],
                                                  data1$temp[i], data1$pressure[i])
                                # side c,  at bottom of trapeze
                                c <- abs(data1$temp[i-1] - temps[i-1])
                                # side d, along the adiabatic curve
                                d <- calcDistance(temps[i-1], data1$pressure[i-1],
                                                  temps[i], data1$pressure[i])
                                h <- abs(data1$pressure[i] - data1$pressure[i-1])
                                #surfs[i] <- calcTrapezeArea(max(a,c),b,min(a,c),d)
                                surfs[i] <- round(calcTrapezeArea(a,c,h),0)
                                #                                         surf <- calcTrapezeArea(a,b,c,d)
                                #
                                #                                         if (is.nan(surf))
                                #                                                 surfs[i] <- a*b
                                #                                         else
                                #                                                 surfs[i] <- surf
                        }
                }
        }
        #temps
        matrixadiab <- c(temps,surfs)
        dim(matrixadiab) <- c(nrow(data1),2)
        matrixadiab
}

calcTriangleArea <- function (a=1,b=1,c=sqrt(2)) {
        s <- 1/2 * (a + b + c)
        sqrt(s * (s-a) * (s-b) * (s-c))}

calcTrapezeAreaBof <- function (a=4,b=sqrt(5),c=6,d=sqrt(5)) {
        (a + c) / (4 * abs(a-c)) * sqrt(( a + b - c + d) * (a - b - c + d) *
                                                (a + b - c - d) * (-a + b + c + d))
}
calcTrapezeArea <- function (b1=4, b2=5, h=2) {
        (b1+b2)/2*h

}
calcDistance <- function (x1=0, y1=0, x2=1,y2=1) {
        sqrt((x2-x1)^2 + (y2-y1)^2)
}
calcAdiabTemp <- function (T1, P1, P2) {
        # Compute Adiabatic Temperature
        k = 0.2857
        T2 <- (P2 / P1)^k * (T1 + 273.15)
        #                T2 <- (P2^k / P1) * (T1 + 273.15)

        T2 - 273.15
}

getData <- function(datatype=""){
        # Init string to know if it is valid at the end of the function
        QueryString <- ""
        #                 if (dbg) browser()

        if (datatype == "sounding")
                QueryString <- paste(
                        "select date, time , pressure, height, mixr, temp",
                        "from sounding1",
                        "where date between", "'", input$daterange[1], "'" ,
                        "and" , "'", input$daterange[2], "'" ,
                        "and time = '12:00:00'",
                        "and pressure between", input$lowerpressure ,"and", input$higherpressure,
                        "order by date, pressure desc;"
                )

        browser()
        if (QueryString != "")
                return(getMysqlData(QueryString))

}

getMysqlData <- function(queryString){
        #   View(queryString)
        library(RMySQL)

        con <- dbConnect(dbDriver("MySQL"), user="dbmeteodb",
                         password="dbmeteodb",
                         dbname="dbmeteodb",
                         host="localhost")

        # send the query
        browser()
        queryResultsData <-  dbSendQuery(con, queryString)


        #get the data
        data <-  fetch(queryResultsData, n=-1)
        # freeing resources
        dbClearResult(queryResultsData)
        dbDisconnect(con)
        View(data)
        data
}
# TODO
# décalage d'une heure entre pm10_16 et pm10_24, ok entre 1 et 23 pour l'instant
# comment traiter les jours où il y a peu de données aot ? moins de 3 ?
