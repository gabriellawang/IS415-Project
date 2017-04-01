library(maptools)
library(plyr)
library(dplyr)
library(rgdal)
library(flows)
library(data.table)
library(stats)
library(shiny)
library(shinydashboard)

wd <- setwd(".")
setwd(wd)

project_path <- wd
shp_path <- "SHPFiles"
attribute_path <- "attributeTables"


#----------------------------------------------------- Perform Once ----------------------------------------------------

plan_area_sdf <- readShapePoly(paste(project_path,sep = "/",paste(shp_path,sep = "/","MP14_PLNG_AREA_WEB_PL.shp")))
busstop_data <- read.csv(paste(project_path,sep = "/",paste(attribute_path,sep = "/","busstop.csv")))
busstop_data$BUS_STOP_N <- c(as.character(busstop_data$BUS_STOP_N))

data_file_name <<- "2016-02-16.csv"

ride_data <-  fread(paste(project_path,sep = "/",paste(attribute_path,sep = "/",data_file_name)))
ride_data$RIDE_START_HOUR <- as.POSIXlt(ride_data$RIDE_START_TIME, format = "%H:%M:%S")$hour
ride_data$BOARDING_STOP_STN <- c(as.character(ride_data$BOARDING_STOP_STN))
ride_data$ALIGHTING_STOP_STN <- c(as.character(ride_data$ALIGHTING_STOP_STN))

processed_data <- inner_join(ride_data,busstop_data,by=c("BOARDING_STOP_STN"="BUS_STOP_N"),suffix=c(".BOARDING",".ALIGHTING"))
processed_data <- inner_join(processed_data,busstop_data,by=c("ALIGHTING_STOP_STN"="BUS_STOP_N"),suffix=c(".BOARDING",".ALIGHTING"))

#set boarding x y as point
coordinates(processed_data)<-~X.BOARDING+Y.BOARDING
processed_data$BOARDING_AREA <- over(processed_data,plan_area_sdf)$PLN_AREA_N


#convert sub_data as df again.
processed_data <- as.data.frame(processed_data)

#set alighting x y as point
coordinates(processed_data)<-~X.ALIGHTING+Y.ALIGHTING
processed_data$ALIGHTING_AREA <- over(processed_data,plan_area_sdf)$PLN_AREA_N

original_data <<- processed_data

first_load <<- TRUE
#----------------------------------------------------- Perform Once End ----------------------------------------------------

options(shiny.maxRequestSize=600*1024^2)
shinyServer(function(input, output, session){
  
  get_data <- reactive(function(inFile){
    if (!is.null(inFile)){
      if (inFile$name != data_file_name){
        data <- fread(inFile$datapath, header = T, sep = ",")
        cat("file name =",inFile$name)
        data_file_name <<- inFile$name
        original_data <<- process_ride_data()(data)
      }
    }
    return (original_data)
  })
  
  process_ride_data <- reactive(function(data_original){
    cat("process_ride_data() executed ")
    ride_data <- data_original
    ride_data$RIDE_START_HOUR <- as.POSIXlt(ride_data$RIDE_START_TIME, format = "%H:%M:%S")$hour
    ride_data$BOARDING_STOP_STN <- c(as.character(ride_data$BOARDING_STOP_STN))
    ride_data$ALIGHTING_STOP_STN <- c(as.character(ride_data$ALIGHTING_STOP_STN))
    
    processed_data <- inner_join(ride_data,busstop_data,by=c("BOARDING_STOP_STN"="BUS_STOP_N"),suffix=c(".BOARDING",".ALIGHTING"))
    processed_data <- inner_join(processed_data,busstop_data,by=c("ALIGHTING_STOP_STN"="BUS_STOP_N"),suffix=c(".BOARDING",".ALIGHTING"))
    #set boarding x y as point
    coordinates(processed_data)<-~X.BOARDING+Y.BOARDING
    processed_data$BOARDING_AREA <- over(processed_data,plan_area_sdf)$PLN_AREA_N
    
    
    #convert sub_data as df again.
    processed_data <- as.data.frame(processed_data)
    
    #set alighting x y as point
    coordinates(processed_data)<-~X.ALIGHTING+Y.ALIGHTING
    processed_data$ALIGHTING_AREA <- over(processed_data,plan_area_sdf)$PLN_AREA_N
    
    dates <-  unique(processed_data$RIDE_START_DATE)
    updateSelectInput(session,"date_1",choices = dates,selected = dates[1])
    updateSelectInput(session,"date_2",choices = dates,selected = dates[1])
    original_data <<- processed_data
    original_data 
  })
  
  create_matrix <- reactive(function(process_ride_data){
    
    mat <- data.frame ( table ( process_ride_data@data$BOARDING_AREA, process_ride_data@data$ALIGHTING_AREA,dnn = c("PLN_AREA_N","jname")))
    mat <- merge(x = mat, y = plan_area_sdf@data[c("PLN_AREA_N", "OBJECTID")], by = "PLN_AREA_N", all.x=TRUE)
    mat <- plyr::rename(mat, c("PLN_AREA_N"="iname", "jname" = "PLN_AREA_N", "Freq" = "fij", "OBJECTID" = "i"))
    mat <- merge(x = mat, y = plan_area_sdf@data[c("PLN_AREA_N", "OBJECTID")], by = "PLN_AREA_N", all.x=TRUE)
    mat <-  plyr::rename(mat, c("PLN_AREA_N"="jname","OBJECTID"= "j" ))
    mat
  })
  
  create_plot <- reactive(function(myflows, date, hour, k){
    
    diag(myflows) <- 0
    
    # Select flows that represent at least 20% of the sum of outgoing flows for 
    # each urban area.
    flowSel1 <- firstflows(mat = myflows/rowSums(myflows)*100, method = "xfirst", k = k)
    
    # Select the dominant flows (incoming flows criterion)
    flowSel2 <- domflows(mat = myflows, w = colSums(myflows), k = 1)
    
    # Combine selections
    flowSel <- myflows * flowSel1 * flowSel2
    
    # Node weights
    inflows <- data.frame(id = colnames(myflows), w = colSums(myflows))
    
    # Plot dominant flows map
    opar <- par(mar = c(0,0,2,0), cex=2)
    sp::plot(plan_area_sdf, col = "#cceae7")
    plotMapDomFlows(mat = flowSel, spdf = plan_area_sdf, spdfid = "OBJECTID", w = inflows, wid = "id",
                    wvar = "w", wcex = 0.05, add = TRUE,
                    legend.flows.pos = "topright",
                    legend.flows.title = "Nb. of commuters")
    title(paste("Dominant Flows of Commuters",date,hour, sep = " - "))
    mtext(text = "TEAM JSR, 2017", side = 4, line = -1, adj = 0.01, cex = 0.8)
    par(opar)
  })
  
  
  
  #Do once
  
  observe({
    cat("observed triggered \n")
    inFile <- input$file11
    if (!is.null(inFile)){
      if  (inFile$name != data_file_name){
        
        processed_ride_data <- get_data()(input$file1)
      }
    }else{
      processed_ride_data <- get_data()(NULL)
      
      if (first_load){
        dates <-  unique(processed_data$RIDE_START_DATE)
        updateSelectInput(session,"date_1",choices = dates,selected = dates[1])
        updateSelectInput(session,"date_2",choices = dates,selected = dates[1])
        first_load <<- FALSE
      }
    }
    
    if (!is.null(processed_ride_data)){
      
      date_1 <- input$date_1
      date_2 <- input$date_2
      
      if (!is.null(date_1)){
        
        data <- subset(processed_ride_data,RIDE_START_DATE == date_1)
        hours <-  unique(data$RIDE_START_HOUR)
        hours <- sort(hours, decreasing = FALSE)
        
        updateSelectInput(session,"hour_1",choices = hours,selected = hours[1])
      }
      
      if (!is.null(date_2)){
        data <- subset(processed_ride_data,RIDE_START_DATE == date_2)
        hours <-  unique(data$RIDE_START_HOUR)
        hours <- sort(hours, decreasing = FALSE)
        updateSelectInput(session,"hour_2",choices = hours,selected = hours[1])
      }
    }
    
  })
  
  output$stat1 <- renderPlot({
    req(input$date_1,input$hour_1, cancelOutput = TRUE)
    date_1 <- input$date_1
    hour_1 <- input$hour_1
    
    processed_ride_data <- get_data()(input$file1)
    
    subset_data_1 <- subset(processed_ride_data,RIDE_START_HOUR == hour_1 & RIDE_START_DATE == date_1)
    
    mat1 <- create_matrix()(subset_data_1)
    myflows1 <- prepflows(mat = mat1, i = "i", j = "j", fij = "fij")
    
    
    statmat(mat = myflows1, output = "all", verbose = TRUE)
    
  })
  
  output$dominance_plot1 <- renderPlot({
    req(input$date_1,input$hour_1, cancelOutput = TRUE)
    date_1 <- input$date_1
    hour_1 <- input$hour_1
    K_1 <- input$K_1
    
    processed_ride_data <- get_data()(input$file1)
    
    subset_data_1 <- subset(processed_ride_data,RIDE_START_HOUR == hour_1 & RIDE_START_DATE == date_1)
    
    mat1 <- create_matrix()(subset_data_1)
    myflows1 <- prepflows(mat = mat1, i = "i", j = "j", fij = "fij")
    
    output$ride_table_1 <- renderDataTable(subset_data_1@data)
    output$flow_matrix_table_1 <- renderDataTable(mat1)
    create_plot()(myflows1,date_1, hour_1,K_1)
  })
  
  output$stat2 <- renderPlot({
    req(input$date_2,input$hour_2, cancelOutput = TRUE)
    date_2 <- input$date_2
    hour_2 <- input$hour_2
    
    processed_ride_data <- get_data()(input$file1)
    
    subset_data_2 <- subset(processed_ride_data,RIDE_START_HOUR == hour_2 & RIDE_START_DATE == date_2)
    
    mat2 <- create_matrix()(subset_data_2)
    myflows2 <- prepflows(mat = mat2, i = "i", j = "j", fij = "fij")
    
    
    statmat(mat = myflows2, output = "all", verbose = TRUE)
    
  })
  
  output$dominance_plot2 <- renderPlot({
    req(input$date_2,input$hour_2, cancelOutput = TRUE)
    date_2 <- input$date_2
    hour_2 <- input$hour_2
    K_2 <- input$K_2
    processed_ride_data <- get_data()(input$file1)
    subset_data_2 <- subset(processed_ride_data,RIDE_START_HOUR == hour_2 & RIDE_START_DATE == date_2)
    
    mat2 <- create_matrix()(subset_data_2)
    myflows2 <- prepflows(mat = mat2, i = "i", j = "j", fij = "fij")
    
    output$ride_table_2 <- renderDataTable(subset_data_2@data)
    output$flow_matrix_table_2 <- renderDataTable(mat2)
    
    create_plot()(myflows2,date_2,hour_2,K_2)
  })
  
})