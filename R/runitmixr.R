main <- function () {}

.setUp <- function () {
        library(RMySQL)
        
        con = dbConnect(dbDriver("MySQL"), user="dbmeteodb", 
                        password="dbmeteodb",
                        dbname="dbmeteodbtest",
                        host="localhost")
        

        
}
test.getData <-function (){
        #con = getCon()
        data <- getData(con, 2012, 1)
        expect_that(data, is_a("data.frame"))
        expect_that(format(data$date[1],"%m"), equals(1))
        #dbDisconnect(con)
}

test.meanForAMonth <- function() {
        mean = meanForAMonth(13)
        expect_that(mean, is_a("numeric"))
        expect_that(mean > 0, is_true())
        print()
        
}

# .tearDown <- function () {
#         dbDisconnect(con)       
# }
