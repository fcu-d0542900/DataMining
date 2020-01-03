mlb_record <- dbGetQuery(db, "SELECT * from mlb_record")
release_speed_rate <- dbGetQuery(db, "SELECT release_speed,release_spin_rate,description,launch_angle from mlb_record")
unique(release_speed_rate$description)

release_speed_rate[["release_speed"]] <- ordered(cut(release_speed_rate[[ "release_speed"]], c(0,70,75,80,85,90,95,100,110)),
                                                 labels = c("<70", "70-75", "75-80", "80-85", "85-90", "90-95", "95-1300", ">100"))

release_speed_rate[["release_spin_rate"]] <- ordered(cut(release_speed_rate[[ "release_spin_rate"]], c(0,1700,1900,2100,2300,2500,4000)),
                                                     labels = c("<1700", "1700-1900", "1900-2100", "2100-2300", "2300-2500", ">2500"))


release_speed_rate%<>%filter(launch_angle!="NA")
sapply_des<-function(speed,spin,description){
  sapply(description, function(d) {
    release_speed_rate%>%filter(description==d & 
                                  release_speed == speed &
                                release_spin_rate == spin)%>%
      .$launch_angle%>%mean()%>%round(digits = 2)
  })
}

description<-release_speed_rate$description%>%unique()
get_mean_angle(description[1])
get_mean_angle<-function(description){
    table_mean_angle<-lapply(spin,function(spin){
        data.frame(
          spin = spin,
          `<70` = sapply_des("<70",spin,description),
          `70-75` = sapply_des("70-75",spin,description),
          `75-80` = sapply_des("75-80",spin,description),
          `80-85` = sapply_des("80-85",spin,description),
          `85-90` = sapply_des("85-90",spin,description),
          `90-95` = sapply_des("90-95",spin,description),
          `95-100` = sapply_des("95-100",spin,description),
          `>100` = sapply_des(">100",spin,description)
        )
      })%>%bind_rows()%>%t()%>%{
      colnames(.) <- spin
      .[-1,]
    }%>%apply(.,2,function(ta){
      ta%>%as.numeric%>%round(digits = 2)
    })
    
    table_mean_angle <- data.frame(table_mean_angle)
    colnames(table_mean_angle) <- spin
    rownames(table_mean_angle) <- speed
    
    return(table_mean_angle)
  
}



ui1 <- fluidPage(
  titlePanel("球速、轉速 - description-angle"),
  selectInput(inputId = "description",
              label = "選擇描述",
              choices = description),
  tableOutput("table")
)

server1<-function(input,output){
  output$table <- renderTable( {
    table <- get_mean_angle(input$description)
  },
  rownames=TRUE,digits=0)
}
shinyApp(ui=ui1,server=server1)
