library(RSQLite)
#set file
url <- "D:/YURU/Documents/YURU/CS/R/DataMining/"
setwd(url)
#connect DB
drv <- dbDriver("SQLite")
db <- dbConnect(drv, "data.db")
dbListTables(db)
#read DB
mlb_record <- dbReadTable(db, "mlb_record")
mlb_player <- dbReadTable(db, "mlb_player")

dbGetQuery(db, "SELECT pitch_type,zone,type from mlb_record")
