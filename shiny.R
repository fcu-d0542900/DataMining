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

speed_spin <- dbGetQuery(db, "SELECT * from speed_spin")
swinging_strike <- dbGetQuery(db, "SELECT * from swinging_strike")

pitch_name <- unique(filter(swinging_strike,pitch_name!="")$pitch_name)
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

get_swinging_strike <- function(balltype="",ch=0,check=0) {
  swinging_strike_ball <- swinging_strike
  if(check==1)
    swinging_strike_ball <- filter(swinging_strike,pitch_name%in%balltype)
  else if(balltype!="All") {
    swinging_strike_ball <- filter(swinging_strike,pitch_type%in%balltype)
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

get_speed_spin <- function(speed,spin) {
  table <- filter(speed_spin,
         findInterval(release_speed, speed)==1L,
         findInterval(release_spin_rate,spin)==1L)
  pitch <- lapply(pitch_name, function(pitch){
    hit <- filter(table,pitch_name == pitch) %>% nrow
    out <- filter(table,pitch_name == pitch)$swinging_strike %>% sum()
    pro <- out/hit
    rbind(pro%>%round(digits = 3),pitch_type = pitch,hit=hit)
  })%>%bind_cols()%>%t()%>%{
    colnames(.)<-c("swing","pitch_name","hit")
    .
  }%>%as.data.frame()
  row.names(pitch) <- pitch$pitch_name
  pitch$swing <- pitch$swing%>%as.character()%>%as.numeric()
  pitch$hit <- pitch$hit%>%as.character()%>%as.numeric()
  pitch <- pitch[order(-pitch$swing),]
  
  return(pitch)
}

ui<-fluidPage(
  titlePanel("球種、球速、轉速對於揮空率"),
  sliderInput(inputId = "speed",
              label = "球速範圍:",
              min = min(speed_spin$release_speed) %>% floor(),
              max = max(speed_spin$release_speed) %>% ceiling(),
              step = 1,
              value = c(80, 90)),
  sliderInput(inputId = "spin",
              label = "轉速範圍:",
              min = (min(speed_spin$release_spin_rate)-50) %>% round(digits=-2),
              max = (max(speed_spin$release_spin_rate)+50) %>% round(digits=-2),
              step = 100,
              value = c(1900, 2100)),
  plotOutput("hist"),
  #titlePanel("球速、轉速對於揮空率"),
  selectInput(inputId = "select_ball_type",
              label = "選擇球種",
              choices = c("All",pitch_type)),
  checkboxGroupInput(inputId = "check_ball_type",
                     label = NULL,
                     choices = pitch_name,
                     inline = TRUE),
  plotOutput("heatmap_mean"),
  #h3("球速、轉速之總球數"),
  plotOutput("heatmap_sum"),
  #tableOutput("table")
)
server<-function(input,output){
  output$hist <- renderPlot({
    table <- get_speed_spin(input$speed,input$spin)
    bp <- barplot(table$hit,yaxt="n",ylim=c(0,max(table$hit)),
            col="gray",border=NA,
            main = "球種對於揮空率")
    text(bp,max(table$hit)*0.98,labels=table$hit,col="black")
    axis(side=4)
    par(new=T)
    bp <- barplot(table$swing,ylim=c(0,max(table$swing+0.2,na.rm=TRUE)),names.arg=table$pitch_name,
                  col=rainbow(12,alpha=0.3),border=NA,las=2,# horiz=TRUE,legend.text = table$pitch_name,cex.names=0.53,las=1,
                  main = "球種對於揮空率")
    text(bp,abs(table$swing+0.02),labels=round(table$swing,digits=2),col="blue")
    
  })
  output$heatmap_mean <- renderPlot({
    if(input$select_ball_type == "All" & !is.null(input$check_ball_type))
      table <- get_swinging_strike(input$check_ball_type,check=1)
    else
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
  output$heatmap_sum <- renderPlot({
    if(input$select_ball_type == "All" & !is.null(input$check_ball_type))
      table <- get_swinging_strike(input$check_ball_type,check=1,ch=1)
    else
      table <- get_swinging_strike(input$select_ball_type,ch=1)
    if(na.omit(table) %>% nrow() != 0) {
      heatmap.2(table%>%as.matrix(),
                main = "球速、轉速之總球數",
                cellnote = table%>%as.matrix(),
                notecol="black", 
                density.info="none",
                trace="none",
                col=colorpanel(100,low="white",high="steelblue"),
                Colv = NA, Rowv = NA)
    }
  })
  output$table <- renderTable({
    if(input$select_ball_type == "All" & !is.null(input$check_ball_type))
      table <- get_swinging_strike(input$check_ball_type,check=1,ch=1)
    else
      table <- get_swinging_strike(input$select_ball_type,ch=1)
  },
  rownames=TRUE,digits=0)
}
shinyApp(ui=ui,server=server)