library(shiny)
library(tmap)
library(leaflet)
library(maptools)
library(plyr)
library(dplyr)
library(rgdal)
library(flows)
library(data.table)

wd <- setwd(".")
setwd(wd)

project_path <- wd
shp_path <- "SHPFiles"
attribute_path <- "attributeTables"

plan_area_sdf <- readShapePoly(paste(project_path,sep = "/",paste(shp_path,sep = "/","MP14_PLNG_AREA_WEB_PL.shp")))

busstop_data <- read.csv(paste(project_path,sep = "/",paste(attribute_path,sep = "/","busstop.csv")))
#proj4string(boundary_sdf) <- "+proj=tmerc +lat_0=1.366666666666667 +lon_0=103.8333333333333 +k=1 +x_0=28001.642 +y_0=38744.572 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

busstop_data$BUS_STOP_N <- c(as.character(busstop_data$BUS_STOP_N))


assign("data_file_name", NULL, envir = .GlobalEnv) 

options(shiny.maxRequestSize=600*1024^2)
shinyServer(function(input, output, session) {
  
  get_data <- reactive(function(inFile){
    
    
    data_file_name <- get('data_file_name', envir=.GlobalEnv) 
    
    if(is.null(inFile)){
      if (is.null(data_file_name)){
        data_original <<-  fread(paste(project_path,sep = "/",paste(attribute_path,sep = "/","2016-02-16.csv")))
        data_original <<- process_ride_data()(data_original)
        assign("data_file_name", "2016-02-16", envir = .GlobalEnv) 
      }
      processed_ride_data <- data_original
      
    }else{
      if (data_file_name != inFile$name){
        #processed_ride_data <- data_original
        data_original  <<- fread(inFile$datapath, header = T, sep = ",")
        data_original <<- process_ride_data()(data_original)
        assign("data_file_name", inFile$name, envir = .GlobalEnv) 
      }
      processed_ride_data <- data_original
    }  
    processed_ride_data
  })
  
  process_ride_data <- reactive(function(data_original){
    cat("process_ride_data() executed: ")
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
    hours <- unique(processed_data$RIDE_START_HOUR)
    updateSelectInput(session,"date_1",choices = dates,selected = dates[1])
    updateSelectInput(session,"date_2",choices = dates,selected = dates[1])
    updateSelectInput(session,"hour_1",choices = hours,selected = hours[1])
    updateSelectInput(session,"hour_2",choices = hours,selected = hours[2])
    
    processed_data 
  })
  
  create_matrix <- reactive(function(process_ride_data){
    
    mat <- data.frame ( table ( process_ride_data@data$BOARDING_AREA, process_ride_data@data$ALIGHTING_AREA,dnn = c("PLN_AREA_N","jname")))
    mat <- merge(x = mat, y = plan_area_sdf@data[c("PLN_AREA_N", "OBJECTID")], by = "PLN_AREA_N", all.x=TRUE)
    mat <- plyr::rename(mat, c("PLN_AREA_N"="iname", "jname" = "PLN_AREA_N", "Freq" = "fij", "OBJECTID" = "i"))
    mat <- merge(x = mat, y = plan_area_sdf@data[c("PLN_AREA_N", "OBJECTID")], by = "PLN_AREA_N", all.x=TRUE)
    mat <-  plyr::rename(mat, c("PLN_AREA_N"="jname","OBJECTID"= "j" ))
    mat
  })
  
  create_plot <- reactive(function(myflows,filename){
    
    diag(myflows) <- 0
    
    # Select flows that represent at least 20% of the sum of outgoing flows for 
    # each urban area.
    flowSel1 <- firstflows(mat = myflows/rowSums(myflows)*100, method = "xfirst", k = 20)
    
    # Select the dominant flows (incoming flows criterion)
    flowSel2 <- domflows(mat = myflows, w = colSums(myflows), k = 1)
    
    # Combine selections
    flowSel <- myflows * flowSel1 * flowSel2
    
    # Node weights
    inflows <- data.frame(id = colnames(myflows), w = colSums(myflows))
    
    # Plot dominant flows map
    opar <- par(mar = c(0,0,2,0))
    sp::plot(plan_area_sdf, col = "#cceae7")
    plotMapDomFlows(mat = flowSel, spdf = plan_area_sdf, spdfid = "OBJECTID", w = inflows, wid = "id",
                    wvar = "w", wcex = 0.05, add = TRUE,
                    legend.flows.pos = "topright",
                    legend.flows.title = "Nb. of commuters")
    title(paste("Dominant Flows of Commuters",filename, sep = " - "))
    mtext(text = "TEAM JSR, 2017", side = 4, line = -1, adj = 0.01, cex = 0.8)
    par(opar)
  })
  
  #output$statst <- renderLeaflet({
  #tmap_leaflet(qtm(sub_data))
  #leaflet() %>%
  #addProviderTiles(providers$OpenStreetMap) %>%
  #addMarkers(ride_data, lat = ride_data$X.BOARDING, lng=ride_data$Y.BOARDING, clusterOptions = markerClusterOptions())
  #})
  
  output$ride_table <- renderDataTable({
    get_data()(input$file1)
  })
  
  output$stat1 <- renderPlot({
    cat("stat1 executed")
    
    processed_ride_data <- get_data()(input$file1)
    
    hour_1 <- input$hour_1
    date_1 <- input$date_1
    subset_data_1 <- subset(processed_ride_data,c(RIDE_START_HOUR == hour_1 & RIDE_START_DATE == date_1))
    
    mat1 <- create_matrix()(subset_data_1)
    myflows1 <- prepflows(mat = mat1, i = "i", j = "j", fij = "fij")
    
    
    output$ride_table_1 <- renderDataTable(subset_data_1)
    output$flow_matrix_table_1 <- renderDataTable(mat1)
    
    output$dominance_plot1 <- renderPlot({create_plot()(myflows1,hour_1)})
    statmat(mat = myflows1, output = "all", verbose = TRUE)
    
  })
  
  
  output$stat2 <- renderPlot({
    cat("stat2 executed")
    
    processed_ride_data <- get_data()(input$file1)
    
    hour_2 <- input$hour_2
    date_2 <- input$date_2
    subset_data_2 <- subset(processed_ride_data,c(RIDE_START_HOUR == hour_2 & RIDE_START_DATE == date_2))
    
    mat2 <- create_matrix()(subset_data_2)
    myflows2 <- prepflows(mat = mat2, i = "i", j = "j", fij = "fij")
    
    output$ride_table_2 <- renderDataTable(subset_data_2)
    output$flow_matrix_table_2 <- renderDataTable(mat2)
    
    output$dominance_plot2 <- renderPlot({create_plot()(myflows2,hour_2)})
    statmat(mat = myflows2, output = "all", verbose = TRUE)
    
  })
})#end shinyserver


