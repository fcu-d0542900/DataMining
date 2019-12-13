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
swinging_strike_mean <- data.frame(swinging_strike_mean)
swinging_strike_mean[48,] <- swinging_strike_mean[47,]
swinging_strike_mean[48,"release_spin_rate"] <- "<1700"
swinging_strike_mean[48,"swinging_strike.sum"] <- NA
swinging_strike_mean[48,"swinging_strike.mean"] <- NA

#launch_angle
launch_angle <- release_speed_rate[-3]
launch_angle <- na.omit(launch_angle)

launch_angle_mean <- launch_angle %>%
  group_by(release_speed,release_spin_rate) %>%
  summarise(launch_angle.mean = mean(launch_angle))

##data.frame(full_join(swinging_strike_mean,launch_angle_mean,by=c("release_speed","release_spin_rate")))

##這邊開始畫圖 by 鈴

#swinging_strike_mean
##拿出release_spin_rate的所有範圍
release_spin_rate<-swinging_strike_mean$release_spin_rate%>%unique()
release_speed_rate<-swinging_strike_mean$release_speed%>%unique()
##畫表格
table_swinging_strike_mean<-lapply(release_spin_rate,function(r){
  #print(r)
  data.frame(
    release_spin_rate = r,
    '<70' = swinging_strike_mean%>%filter(release_speed == "<70",release_spin_rate == r)%>%.$swinging_strike.mean*100,
    '70-75' = swinging_strike_mean%>%filter(release_speed == "70-75",release_spin_rate == r)%>%.$swinging_strike.mean*100,
    '75-80' = swinging_strike_mean%>%filter(release_speed == "75-80",release_spin_rate == r)%>%.$swinging_strike.mean*100,
    '80-85' = swinging_strike_mean%>%filter(release_speed == "80-85",release_spin_rate == r)%>%.$swinging_strike.mean*100,
    '85-90' = swinging_strike_mean%>%filter(release_speed == "85-90",release_spin_rate == r)%>%.$swinging_strike.mean*100,
    '90-95' = swinging_strike_mean%>%filter(release_speed == "90-95",release_spin_rate == r)%>%.$swinging_strike.mean*100,
    '95-100' = swinging_strike_mean%>%filter(release_speed == "95-100",release_spin_rate == r)%>%.$swinging_strike.mean*100,
    '>100' = swinging_strike_mean%>%filter(release_speed == ">100",release_spin_rate == r)%>%.$swinging_strike.mean*100
  )
})%>%bind_rows()%>%t()%>%{
  colnames(.) <- release_spin_rate
  .[-1,]
  }%>%apply(.,2,function(ta){
    ta%>%as.numeric%>%round(digits = 2)
  })

table_swinging_strike_mean <- data.frame(table_swinging_strike_mean)
colnames(table_swinging_strike_mean) <- release_spin_rate
rownames(table_swinging_strike_mean) <- release_speed_rate

##畫圖 一般的熱力圖
heatmap(table_swinging_strike_mean%>%as.matrix(),Colv = NA, Rowv = NA)
##熱力圖+數字
install.packages("gplots", dependencies = TRUE)
library(gplots)
#change color
heatmap.2(table_swinging_strike_mean%>%as.matrix(),
          cellnote = table_swinging_strike_mean%>%as.matrix(),
          notecol="black", 
          density.info="none",
          trace="none",
          col=colorpanel(100,low="white",mid="yellow",high="red"),
          Colv = NA, Rowv = NA)

##球路 揮空率
pitch_type_DOC<-read.csv(file="pitch_type_DOC.csv", header=TRUE, sep=",",stringsAsFactors = F,fileEncoding = "utf-8")
all_pitch<-mlb_record%>%filter(pitch_type!="")%>%data.table()%>%split(by = "pitch_type")
pitch<-lapply(names(all_pitch), function(pitch){
  sw<-mlb_record%>%filter(description == "swinging_strike" & pitch_type == pitch)%>%nrow
  all<-all_pitch[[pitch]]$strikes%>%sum
  pro<-sw/all
  rbind(pro%>%round(digits = 3),pitch_type = pitch)
})%>%bind_cols()%>%t()%>%{
  colnames(.)<-c("swing","pitch_type")
  .
  }%>%as.data.frame()%>%left_join(pitch_type_DOC)


barplot(pitch$swing%>%as.character()%>%as.numeric(),names.arg=pitch$ZH,
        width = 700,high = 500,
        main = "pitch_name")
abline(h = c(0.03,0.05,0.08,0.12,0.14),col = "red")


##球速+轉速
a<-dbGetQuery(db, "SELECT release_speed,release_spin_rate,description,launch_angle from mlb_record")
a[["release_speed"]] <- ordered(cut(a[[ "release_speed"]], c(0,70,75,80,85,90,95,100,110)),
                                labels = c("<70", "70-75", "75-80", "80-85", "85-90", "90-95", "95-100", ">100"))
a%>%filter(!is.na(release_speed))
boxplot(formula = release_spin_rate ~ release_speed,
        data = a,
        xlab = "release_speed",
        ylab = "release_spin_rate")
