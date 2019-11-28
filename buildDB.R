library(RSQLite)

url <- "D:/YURU/Documents/YURU/CS/R/DataMining/"
setwd(url)

mlb_record <- read.csv(file="MLB_Game_Record.csv", header=TRUE, sep=",")
mlb_player <- read.csv(file="MLB_Player_Id.csv", header=TRUE, sep=",")

drv <- dbDriver("SQLite")
db <- dbConnect(drv, "data.db")
#write DB
dbWriteTable(db, "mlb_record", mlb_record)
dbWriteTable(db, "mlb_player", mlb_player)
#read DB
dbReadTable(db, "mlb_record")
dbReadTable(db, "mlb_player")
dbListTables(db)
