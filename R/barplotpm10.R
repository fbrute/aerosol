library(RMySQL)
library(ggplot2)
#sqlstr <- "select power.date, year(power.date) as year, month(power.date) as month, day(power.date) as day, power, meanmixr from power, (select date, avg(mixr) as meanmixr from sounding1 where time = '12:00:00' and pressure between 700 and 850 group by date) as mean where power.date = mean.date;"
sqlstr <- "select  year(datetime) as year, month(datetime) as month,  avg(pmptp) as pm10 from pm10 where pmptp > 0 and year(datetime) between 2008 and 2012 group by month order by month"
con <- dbConnect(dbDriver("MySQL"), user="dbmeteodb",
                 password="dbmeteodb",
dbname="dbmeteodb",
host="localhost")

queryResultsData <-  dbSendQuery(con, sqlstr)
df <- df.pm10.bymonth <-  fetch(queryResultsData, n=-1)
dbClearResult(queryResultsData)
dbDisconnect(con)

#mutate(df.power.bymonth2oct, annee = year)

df.pm10.bymonth <- transform(df.pm10.bymonth,
          month = factor(month,
                         labels = c("Janvier","Février","Mars","Avril","Mai",
                                    "Juin","Juillet","Aout","Septembre","Octobre","Novembre","Décembre")))
df.pm10.bymonth <- transform(df.pm10.bymonth, mois = month)
df.pm10.bymonth$color <- c("blue","blue","blue","blue","red","red","red","red","blue","blue","blue","blue")
g <- ggplot(data = df.pm10.bymonth)
#g + geom_bar(aes(x = mois, y = pm10), stat="identity", position = "dodge")
# bleu et rouge
#rhg_cols1<- c("#00b0e6", "#db4437")

# orange et rouge
rhg_cols1<- c("#ffcc33", "#db4437")
gpm10 <- g + geom_bar(aes(x = mois, y = pm10,fill = color), stat="identity") + scale_fill_manual(values = rhg_cols1) + theme(legend.position = "none")
gpm1O
ggsave(gpm10, file="barplotpm10_2008_2012_orange_rouge.png",scale=1.9)

