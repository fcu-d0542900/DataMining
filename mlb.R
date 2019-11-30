library(RSQLite)
library(dplyr)
#set file
url <- "D:/YURU/Documents/YURU/CS/R/DataMining/"
setwd(url)
#connect DB
drv <- dbDriver("SQLite")
db <- dbConnect(drv, "data.db")
dbListTables(db)
#read DB
#mlb_record <- dbReadTable(db, "mlb_record")
#mlb_player <- dbReadTable(db, "mlb_player")
#dbGetQuery(db, "SELECT pitch_type,zone,type from mlb_record")

release_speed_rate <- dbGetQuery(db, "SELECT release_speed,release_spin_rate,description,launch_angle from mlb_record")
unique(release_speed_rate$description)

release_speed_rate[["release_speed"]] <- ordered(cut(release_speed_rate[[ "release_speed"]], c(0,70,75,80,85,90,95,100,110)),
                                              labels = c("<70", "70-75", "75-80", "80-85", "85-90", "90-95", "95-100", ">100"))

release_speed_rate[["release_spin_rate"]] <- ordered(cut(release_speed_rate[[ "release_spin_rate"]], c(0,1700,1900,2100,2300,2500,4000)),
                                                  labels = c("<1700", "1700-1900", "1900-2100", "2100-2300", "2300-2500", ">2500"))

#swinging_strike
hit <- c("hit_into_play_score","hit_into_play","foul","swinging_strike","swinging_strike_blocked","hit_into_play_no_out","foul_tip")
swinging_strike <- release_speed_rate[-4]
swinging_strike$swinging_strike <- FALSE

swinging_strike[swinging_strike$description=='swinging_strike','swinging_strike'] <- TRUE
swinging_strike[swinging_strike$description=='swinging_strike_blocked','swinging_strike'] <- TRUE

swinging_strike <- na.omit(swinging_strike)
swinging_strike <- filter(swinging_strike,description %in% hit)
swinging_strike <- swinging_strike[-3]

swinging_strike_mean <- swinging_strike %>%
  group_by(release_speed,release_spin_rate) %>%
  summarise(swinging_strike.sum = sum(swinging_strike), swinging_strike.mean = mean(swinging_strike))

#launch_angle
launch_angle <- release_speed_rate[-3]
launch_angle <- na.omit(launch_angle)
#?刪掉短打的部分
launch_angle_mean <- launch_angle %>%
  group_by(release_speed,release_spin_rate) %>%
  summarise(launch_angle.mean = mean(launch_angle))

data.frame(full_join(swinging_strike_mean,launch_angle_mean,by=c("release_speed","release_spin_rate")))
