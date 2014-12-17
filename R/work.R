library(RMySQL)
library(ggplot2)
#sqlstr <- "select power.date, year(power.date) as year, month(power.date) as month, day(power.date) as day, power, meanmixr from power, (select date, avg(mixr) as meanmixr from sounding1 where time = '12:00:00' and pressure between 700 and 850 group by date) as mean where power.date = mean.date;"
sqlstr <- "select power.date, year(power.date) as year, month(power.date) as month, day(power.date) as day, meanmixr, power ,pm10 from power, (select date, avg(mixr) as meanmixr from sounding1  where pressure between 700 and 850 group by date) as mean, (select year(datetime) as year, month(datetime) as month, day(datetime) as day, cast(date_format(date(datetime),'%j') as decimal(5,0)) as julian_day, avg(pmptp) as pm10 ,date(datetime) as date from pm10 where pmptp > 0 group by date(datetime) order by date(datetime)) as pm10 where power.date = mean.date and power.date = pm10.date;"
con <- dbConnect(dbDriver("MySQL"), user="dbmeteodb",
                 password="dbmeteodb",
dbname="dbmeteodb",
host="localhost")

queryResultsData <-  dbSendQuery(con, sqlstr)
df <- df.power.byday <-  fetch(queryResultsData, n=-1)
dbClearResult(queryResultsData)
dbDisconnect(con)

# qplot(meanmixr, power, data = df,  facets = . ~ year, geom = c("point", "smooth"),method = "lm")
# qplot(meanmixr, power, data = df,  facets = year ~ month, geom = c("point", "smooth"),method = "lm")
#
# qplot(power, data = df,  facets = . ~ year, geom = "histogram")
gday <- ggplot(df.power.byday, aes(power,meanmixr))
gday <- gday + geom_point() + facet_grid(year ~ month) + geom_smooth(method="lm")
ggsave(gday, file="meanmixrdays.pdf",scale=1.9)

sqlstr <- "select  year(power.date) as year, month(power.date) as month,  avg(meanmixr) as meanmixr, avg(power) as power ,avg(pm10) as pm10 from power, (select date, avg(mixr) as meanmixr from sounding1  where pressure between 700 and 850 group by date) as mean, (select year(datetime) as year, month(datetime) as month, day(datetime) as day, cast(date_format(date(datetime),'%j') as decimal(5,0)) as julian_day, avg(pmptp) as pm10 ,date(datetime) as date from pm10 where pmptp > 0 group by date(datetime) order by date(datetime)) as pm10 where power.date = mean.date and power.date = pm10.date group by year, month"
con <- dbConnect(dbDriver("MySQL"), user="dbmeteodb",
                 password="dbmeteodb",
                 dbname="dbmeteodb",
                 host="localhost")

queryResultsData <-  dbSendQuery(con, sqlstr)
df <- df.power.bymonth <-  fetch(queryResultsData, n=-1)
dbClearResult(queryResultsData)
dbDisconnect(con)

# qplot(meanmixr, power, data = df,  facets = . ~ year, geom = c("point", "smooth"),method = "lm")
# qplot(meanmixr, power, data = df,  facets = year ~ month, geom = c("point", "smooth"),method = "lm")
#
# qplot(power, data = df,  facets = . ~ year, geom = "histogram")

gmonth <- ggplot(df.power.bymonth, aes(power,meanmixr))
gmonth <- gmonth + geom_point() + facet_wrap(year ~ month, ncol =5, scales = "free") + geom_smooth(method="lm")
ggsave(gmonth, file="meanmixrmonths.pdf",scale=1.9)

# exlude november and december for year 2012
#library(dplyr)
df.power.bymonth2oct <- subset(df.power.bymonth, year %in% 2008:2011 | (year == 2012 & month < 11))
#mutate(df.power.bymonth2oct, annee = year)

df.power.bymonth2oct <- transform(df.power.bymonth2oct,
          month = factor(month,
                         labels = c("Janvier","Février","Mars","Avril","Mai",
                                    "Juin","Juillet","Aout","Septembre","Octobre","Décembre")))
df.power.bymonth2oct <- transform(df.power.bymonth2oct, année = year, mois = month)

g <- ggplot(data = df.power.bymonth2oct)
g + geom_bar(aes(x = année, y = pm10, fill = factor(month)), stat="identity", position = "dodge") + guides(fill = guide_legend(title="Mois"))

g + geom_bar(aes(x = mois, y = pm10, fill = factor(year)), stat="identity", position = "dodge") + guides(fill = guide_legend(title="Année"))

g + geom_bar(aes(x = mois, y = pm10), stat="identity", position = "dodge") + guides(fill = guide_legend(title="Année"))
