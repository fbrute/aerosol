# getCon <- function () {
#
#         RMySQL::dbConnect(dbDriver("MySQL"), user="dbmeteodb",
#                         password="dbmeteodb",
#                         dbname="dbmeteodb",
#                         host="localhost")
# }
# sqlstr = paste("select avg(mixr) as meanmixr from sounding1 where year(date)=",
#               year,  "and month(date) =", month ,"group by date")


