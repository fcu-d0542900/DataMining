library(shiny)
library(RSQLite)
library(dplyr)
library(gplots)

#set file
url <- "D:/YURU/Documents/YURU/CS/R/DataMining/"
setwd(url)
#connect DB
drv <- dbDriver("SQLite")
db <- dbConnect(drv, "data.db")
dbListTables(db)

release_speed_rate <- dbGetQuery(db, "SELECT release_speed,release_spin_rate,description,launch_angle,pitch_name,launch_speed_angle from mlb_record")

release_speed_rate[["release_speed"]] <- ordered(cut(release_speed_rate[[ "release_speed"]], c(0,70,75,80,85,90,95,100,110)),labels = c("<70", "70-75", "75-80", "80-85", "85-90", "90-95", "95-100", ">100"))
release_speed_rate[["release_spin_rate"]] <- ordered(cut(release_speed_rate[[ "release_spin_rate"]], c(0,1700,1900,2100,2300,2500,4000)),labels = c("<1700", "1700-1900", "1900-2100", "2100-2300", "2300-2500", ">2500"))

#選出有揮擊之動作
hit <- c("hit_into_play_score","hit_into_play","foul","swinging_strike","swinging_strike_blocked","hit_into_play_no_out","foul_tip")
out <- c('swinging_strike','swinging_strike_blocked')
swinging_strike <- release_speed_rate[c(-4,-6)]
swinging_strike$swinging_strike <- FALSE
#判斷是否揮空
swinging_strike[swinging_strike$description=='swinging_strike','swinging_strike'] <- TRUE
swinging_strike[swinging_strike$description=='swinging_strike_blocked','swinging_strike'] <- TRUE
#刪除空值
swinging_strike <- na.omit(swinging_strike)
swinging_strike <- filter(swinging_strike,description %in% hit)
swinging_strike <- swinging_strike[-3]

swinging_strike_mean <- swinging_strike %>%
  group_by(release_speed,release_spin_rate) %>%
  summarise(count = n(),mean = mean(swinging_strike))

spin <- swinging_strike_mean$release_spin_rate%>%unique()
speed <- swinging_strike_mean$release_speed%>%unique()

get <- function(data,speed,spin){
  d <- data%>%filter(release_speed == speed,release_spin_rate == spin)
  if(nrow(d) == 0)
    return(NA)
  else if(d$count<3)
    return(NA)
  return(d$mean)
}

table_swinging_strike_mean<-lapply(spin,function(r){
  data.frame(
    spin = r,
    '<70' = get(swinging_strike_mean,"<70",r)*100,
    '70-75' = get(swinging_strike_mean,"70-75",r)*100,
    '75-80' = get(swinging_strike_mean,"75-80",r)*100,
    '80-85' = get(swinging_strike_mean,"80-85",r)*100,
    '85-90' = get(swinging_strike_mean,"85-90",r)*100,
    '90-95' = get(swinging_strike_mean,"90-95",r)*100,
    '95-100' = get(swinging_strike_mean,"95-100",r)*100,
    '>100' = get(swinging_strike_mean,">100",r)*100
  )
})%>%bind_rows()%>%t()%>%{
  colnames(.) <- spin
  .[-1,]
}%>%apply(.,2,function(ta){
  ta%>%as.numeric%>%round(digits = 2)
})

table_swinging_strike_mean <- data.frame(table_swinging_strike_mean)
colnames(table_swinging_strike_mean) <- spin
rownames(table_swinging_strike_mean) <- speed


ui<-fluidPage(
  sliderInput(inputId = "num",
              label = "choose a number",
              value = 25,
              min = 1,
              max = 100),
  
  plotOutput("hist")
)
server<-function(input,output){
  output$hist <- renderPlot({
    heatmap.2(table_swinging_strike_mean%>%as.matrix(),
              cellnote = table_swinging_strike_mean%>%as.matrix(),
              notecol="black", 
              density.info="none",
              trace="none",
              col=colorpanel(100,low="white",mid="yellow",high="red"),
              Colv = NA, Rowv = NA)
  })
}
shinyApp(ui=ui,server=server)