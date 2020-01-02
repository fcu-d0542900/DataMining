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

swinging_strike <- dbGetQuery(db, "SELECT * from swinging_strike")

pitch_type <- unique(filter(swinging_strike,pitch_type!="")$pitch_type)
spin <- levels(swinging_strike$release_spin_rate)<- c("<1700","1700-1900", "1900-2100", "2100-2300", "2300-2500", ">2500")
speed <- levels(swinging_strike$release_speed)<- c("<70", "70-75", "75-80", "80-85", "85-90", "90-95", "95-100", ">100")

get <- function(data,speed,spin,ch=0){
  d <- data%>%filter(release_speed == speed,release_spin_rate == spin)
  if(ch==1) {
    if(nrow(d) == 0)
      return(NA)
    return(d$count)
  }
  if(nrow(d) == 0)
    return(NA)
  else if(d$count<3)
    return(NA)
  return(d$mean)
}

get_swinging_strike <- function(balltype="",ch=0) {
  if(balltype!="") {
    swinging_strike_ball <- filter(swinging_strike,pitch_type == balltype)
  }
  
  swinging_strike_mean <- swinging_strike_ball %>%
    group_by(release_speed,release_spin_rate) %>%
    summarise(count = n(),mean = mean(swinging_strike))
  
  if(ch!=1) {
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
    
    return(table_swinging_strike_mean)
  }
  else {
    table_swinging_strike_sum<-lapply(spin,function(r){
      data.frame(
        spin = r,
        '<70' = get(swinging_strike_mean,"<70",r,1),
        '70-75' = get(swinging_strike_mean,"70-75",r,1),
        '75-80' = get(swinging_strike_mean,"75-80",r,1),
        '80-85' = get(swinging_strike_mean,"80-85",r,1),
        '85-90' = get(swinging_strike_mean,"85-90",r,1),
        '90-95' = get(swinging_strike_mean,"90-95",r,1),
        '95-100' = get(swinging_strike_mean,"95-100",r,1),
        '>100' = get(swinging_strike_mean,">100",r,1)
      )
    })%>%bind_rows()%>%t()%>%{
      colnames(.) <- spin
      .[-1,]
    }%>%apply(.,2,function(ta){
      ta%>%as.numeric%>%round(digits = 2)
    })
    table_swinging_strike_sum <- data.frame(table_swinging_strike_sum)
    colnames(table_swinging_strike_sum) <- spin
    rownames(table_swinging_strike_sum) <- speed
    
    return(table_swinging_strike_sum)
  }
  
}


ui<-fluidPage(
  titlePanel("球速、轉速對於揮空率"),
  selectInput(inputId = "select_ball_type",
              label = "選擇球種",
              choices = pitch_type),
  plotOutput("heatmap"),
  h3("球速、轉速之總球數"),
  tableOutput("table")
)
server<-function(input,output){
  output$heatmap <- renderPlot({
    table <- get_swinging_strike(input$select_ball_type)
    if(na.omit(table) %>% nrow() != 0) {
      heatmap.2(table%>%as.matrix(),
                main = "球速、轉速對於揮空率",
                cellnote = table%>%as.matrix(),
                notecol="black", 
                density.info="none",
                trace="none",
                col=colorpanel(100,low="white",mid="yellow",high="red"),
                Colv = NA, Rowv = NA)
    }
  })
  output$table <- renderTable({
    table <- get_swinging_strike(input$select_ball_type,ch=1)
  },
  rownames=TRUE,digits=0)
}
shinyApp(ui=ui,server=server)