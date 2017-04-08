library(maptools)
library(plyr)
library(dplyr)
library(rgdal)
library(sp)
library(flows)
library(data.table)
library(stats)
library(shiny)
library(plotly)
library(leaflet)
library(shinydashboard)
library(DT)
library(spatstat)
library(classInt)

wd <- setwd(".")
setwd(wd)

project_path <- wd
shp_path <- "SHPFiles"
attribute_path <- "attributeTables"


#----------------------------------------------------- Perform Once ----------------------------------------------------
plan_area_sdf2 <- readOGR(paste(project_path,sep = "/",paste(shp_path,sep = "/")),"MP14_PLNG_AREA_WEB_PL")
plan_area_sdf <- readShapePoly(paste(project_path,sep = "/",paste(shp_path,sep = "/","MP14_PLNG_AREA_WEB_PL.shp")))
sub_zone_sdf2 <- readOGR(paste(project_path,sep = "/",paste(shp_path,sep = "/")),"MP14_SUBZONE_NO_SEA_PL")
sub_zone_sdf <- readShapePoly(paste(project_path,sep = "/",paste(shp_path,sep = "/","MP14_SUBZONE_NO_SEA_PL.shp")))
busstop_data <- read.csv(paste(project_path,sep = "/",paste(attribute_path,sep = "/","busstop.csv")),stringsAsFactors = FALSE)
busstop_data$BUS_STOP_N <- c(as.character(busstop_data$BUS_STOP_N))

#data_file_name <<- "2016-02-16.csv"
#data_file_name <<- "two_days_data.csv"
data_file_name <<- "CITY_NATION_RIDE_DATA_FULL.csv"
global_date_1 <<- NULL
global_date_2 <<- NULL


ride_data <-  fread(paste(project_path,sep = "/",paste(attribute_path,sep = "/",data_file_name)))
ride_data$RIDE_START_HOUR <- as.POSIXlt(ride_data$RIDE_START_TIME, format = "%H:%M:%S")$hour
ride_data$BOARDING_STOP_STN <- c(as.character(ride_data$BOARDING_STOP_STN))
ride_data$ALIGHTING_STOP_STN <- c(as.character(ride_data$ALIGHTING_STOP_STN))

processed_data <- inner_join(ride_data,busstop_data,by=c("BOARDING_STOP_STN"="BUS_STOP_N"),suffix=c(".BOARDING",".ALIGHTING"))
processed_data <- inner_join(processed_data,busstop_data,by=c("ALIGHTING_STOP_STN"="BUS_STOP_N"),suffix=c(".BOARDING",".ALIGHTING"))
original_data <<- processed_data
#set boarding x y as point
coordinates(processed_data)<-~X.BOARDING+Y.BOARDING
processed_data$BOARDING_AREA <- over(processed_data,plan_area_sdf)$PLN_AREA_N


#convert sub_data as df again.
processed_data <- as.data.frame(processed_data)

#set alighting x y as point
coordinates(processed_data)<-~X.ALIGHTING+Y.ALIGHTING
processed_data$ALIGHTING_AREA <- over(processed_data,plan_area_sdf)$PLN_AREA_N

#original_data <<- processed_data
selected_type <<- "P_AREA"
first_load <<- TRUE


addLegendCustom <- function(map, colors, labels, sizes, opacity = 0.5,title){
  colorAdditions <- paste0(colors, "; width:", 30, "px; height:", sizes, "px")
  labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>", labels, "</div>")
  
  return(addLegend(map, title=title,colors = colorAdditions, labels = labelAdditions, opacity = opacity,position="bottomright"))
}


#replace the plotMapDomFlows function
plotMapDomFlows2 <- function(mat,original, spdf, spdfid, w, wid, wvar, wcex = 0.05){
  # points management
  pts <- data.frame(sp::coordinates(spdf), id  = spdf@data[,spdfid])
  names(pts)[1:2] <- c("long", "lat")
  w <- w[,c(wid, wvar)]
  names(w) <- c("id", "var")
  pts <- merge(pts, w, by.x = "id", by.y = "id", all.x = T)
  
  # points size
  bbbox <- sp::bbox(spdf)
  x1 <- bbbox[1]
  y1 <- bbbox[2]
  x2 <- bbbox[3]
  y2 <- bbbox[4]
  sfdc <- (x2-x1)*(y2-y1)
  sc <- sum(pts$var, na.rm=TRUE)
  pts$cex <- sqrt((pts$var * wcex * sfdc / sc) / pi)
  pts <- pts[order(pts$cex,decreasing=TRUE),]
  pts <- pts[pts$cex > 0, ]
  
  # Segment management
  colnames(mat) <- paste("X", colnames(mat), sep="")
  row.names(mat) <- paste("X", row.names(mat), sep="")
  fdom <- reshape2::melt(mat)
  names(fdom) <- c("i", "j", "fij")
  fdom <- fdom[fdom$fij > 0,]
  fdom$i <- substr(x = fdom$i, 2 , nchar(as.character(fdom$i)))
  fdom$j <- substr(x = fdom$j, 2 , nchar(as.character(fdom$j)))
  fdom <- merge(fdom, pts, by.x = "i", by.y = "id", all.x = T,
                suffixes = c("i","j"))
  fdom <- merge(fdom, pts, by.x = "j", by.y = "id", all.x = T,
                suffixes = c("i","j"))
  fdom$width <- (fdom$fij * 8 / (max(fdom$fij) - min(fdom$fij))) + 2

  # points color
  pts$col <- "green"
  pts[pts$id %in% fdom$j & !pts$id %in% fdom$i, "col"] <- "red"
  pts[pts$id %in% fdom$j & pts$id %in% fdom$i, "col"] <- "orange"
  pts[!pts$id %in% fdom$j & pts$id %in% fdom$i, "col"] <- "yellow"
  pts <- pts[pts$col != "green",]
  # Affichage points and segments
  
  internal <- diag(original)
  internal_flow <- data.frame(internal)
  name <- row.names(internal_flow)
  internal_flow <- data.frame(name, internal)
  
  pts <- merge(pts, internal_flow, by.x = "id", by.y = "name", all.x = T)
  map <<- addCircles(map, lng = pts$long, lat = pts$lat,radius = pts$cex*80000,
                     fill = TRUE, fillColor = pts$col, color = "grey50", weight = 0.5,
                     fillOpacity = 0.8, opacity = 1, group = "Points",
                     label = paste("Total in-flow: ",pts$var,"| Internal Flow: ",pts$internal, "| Total: ",as.numeric(pts$var)+as.numeric(pts$internal)),
                     highlightOptions = highlightOptions(color = "white", weight = 1.5,
                                                         bringToFront = FALSE))
  
  begin.coord <- data.frame(lon=fdom$longi, lat=fdom$lati)
  end.coord <- data.frame(lon=fdom$longj, lat=fdom$latj)
  width <<- data.frame(x=fdom$width)
  
  p <- psp(begin.coord[,1], begin.coord[,2], end.coord[,1], end.coord[,2],     owin(range(c(begin.coord[,1], end.coord[,1])), range(c(begin.coord[,2], end.coord[,2]))))
  
  p<-as(p, "SpatialLines")   
  inv <- classIntervals(fdom$fij,n=3,style="jenks")
  
  map <<- addPolylines(map, data=p,weight = fdom$width,col='black', opacity = 1, group = "Segments",
                       label = paste("Flow to Dominant:", fdom$fij),
                       highlightOptions = highlightOptions(color = "white",bringToFront = FALSE))%>%
    addLegendCustom(colors = c("black", "black", "black","black"), labels = inv$brks, sizes = c(4,6,8,10),title="Size proportional to flows", opacity = 1)
  
  
  map <<- addLegend(map, position = "bottomleft", 
                    title = "Size proportional\nto sum of inflows",
                    colors = c("red","orange", "yellow"), opacity = 1,
                    labels = c("Dominant", "Intermediary", 
                               "Dominated"))
  map <<- addLayersControl(map, overlayGroups = c("Points", "Segments", "Basemap"))
  
}


#----------------------------------------------------- Perform Once End ----------------------------------------------------

options(shiny.maxRequestSize=600*1024^2)
shinyServer(function(input, output, session){
  
  get_data <- reactive(function(inFile,type){
    if (!is.null(inFile)){
      #has file
      if (inFile$name != data_file_name){
        #different file name
        cat("New upload ")
        
        data <- fread(inFile$datapath, header = T, sep = ",")
        ride_data <- data
        ride_data$RIDE_START_HOUR <- as.POSIXlt(ride_data$RIDE_START_TIME, format = "%H:%M:%S")$hour
        ride_data$BOARDING_STOP_STN <- c(as.character(ride_data$BOARDING_STOP_STN))
        ride_data$ALIGHTING_STOP_STN <- c(as.character(ride_data$ALIGHTING_STOP_STN))
        
        processed_data <- inner_join(ride_data,busstop_data,by=c("BOARDING_STOP_STN"="BUS_STOP_N"),suffix=c(".BOARDING",".ALIGHTING"))
        processed_data <- inner_join(processed_data,busstop_data,by=c("ALIGHTING_STOP_STN"="BUS_STOP_N"),suffix=c(".BOARDING",".ALIGHTING"))
        original_data <<- processed_data
        
        cat("file name =",inFile$name)
        data_file_name <<- inFile$name
        processed_data <<- process_ride_data()(processed_data,type)
        selected_type <<- type
      }else if  (selected_type != type){
        cat("Previous file upload, different type")
        
        ride_data <- original_data
        ride_data$RIDE_START_HOUR <- as.POSIXlt(ride_data$RIDE_START_TIME, format = "%H:%M:%S")$hour
        ride_data$BOARDING_STOP_STN <- c(as.character(ride_data$BOARDING_STOP_STN))
        ride_data$ALIGHTING_STOP_STN <- c(as.character(ride_data$ALIGHTING_STOP_STN))
        
        processed_data <- inner_join(ride_data,busstop_data,by=c("BOARDING_STOP_STN"="BUS_STOP_N"),suffix=c(".BOARDING",".ALIGHTING"))
        processed_data <- inner_join(processed_data,busstop_data,by=c("ALIGHTING_STOP_STN"="BUS_STOP_N"),suffix=c(".BOARDING",".ALIGHTING"))
        
        processed_data <<- process_ride_data()(processed_data,type)
        selected_type <<- type
        
      }
      
    }else{
      #no input file
      if(selected_type != type){
        cat("No upload, different type  executed ")
        
        
        processed_data <<- process_ride_data()(original_data,type)
        selected_type <<- type
        
      }
    }
    return (processed_data)
  })
  
  process_ride_data <- reactive(function(data_original, type){
    cat("process_ride_data() executed ")
    ride_data <- data_original
    
    #set boarding x y as point
    if (type == "P_AREA"){
      
      coordinates(ride_data)<-~X.BOARDING+Y.BOARDING
      ride_data$BOARDING_AREA <- over(ride_data,plan_area_sdf)$PLN_AREA_N
      
      
      #convert sub_data as df again.
      ride_data <- as.data.frame(ride_data)
      
      #set alighting x y as point
      coordinates(ride_data)<-~X.ALIGHTING+Y.ALIGHTING
      ride_data$ALIGHTING_AREA <- over(ride_data,plan_area_sdf)$PLN_AREA_N
      
    }else{
      
      coordinates(ride_data)<-~X.BOARDING+Y.BOARDING
      ride_data$BOARDING_AREA <- over(ride_data,sub_zone_sdf)$SUBZONE_N
      
      
      #convert sub_data as df again.
      ride_data <- as.data.frame(ride_data)
      
      #set alighting x y as point
      coordinates(ride_data)<-~X.ALIGHTING+Y.ALIGHTING
      ride_data$ALIGHTING_AREA <- over(ride_data,sub_zone_sdf)$SUBZONE_N 
    }
    dates <-  unique(ride_data$RIDE_START_DATE)
    updateSelectInput(session,"date_1",choices = dates,selected = dates[1])
    updateSelectInput(session,"date_2",choices = dates,selected = dates[1])
    ride_data
  })
  
  create_matrix <- reactive(function(process_ride_data,type,dates){
    if (type == "P_AREA"){
      
      mat <- data.frame ( table ( process_ride_data@data$BOARDING_AREA, process_ride_data@data$ALIGHTING_AREA,dnn = c("PLN_AREA_N","jname")))
      mat <- merge(x = mat, y = plan_area_sdf@data[c("PLN_AREA_N", "OBJECTID")], by = "PLN_AREA_N", all.x=TRUE)
      mat <- plyr::rename(mat, c("PLN_AREA_N"="iname", "jname" = "PLN_AREA_N", "Freq" = "fij", "OBJECTID" = "i"))
      mat <- merge(x = mat, y = plan_area_sdf@data[c("PLN_AREA_N", "OBJECTID")], by = "PLN_AREA_N", all.x=TRUE)
      mat <-  plyr::rename(mat, c("PLN_AREA_N"="jname","OBJECTID"= "j" ))
      
    }else{
      mat <- data.frame ( table ( process_ride_data@data$BOARDING_AREA, process_ride_data@data$ALIGHTING_AREA,dnn = c("SUBZONE_N","jname")))
      mat <- merge(x = mat, y = sub_zone_sdf@data[c("SUBZONE_N", "OBJECTID")], by = "SUBZONE_N", all.x=TRUE)
      mat <- plyr::rename(mat, c("SUBZONE_N"="iname", "jname" = "SUBZONE_N", "Freq" = "fij", "OBJECTID" = "i"))
      mat <- merge(x = mat, y = sub_zone_sdf@data[c("SUBZONE_N", "OBJECTID")], by = "SUBZONE_N", all.x=TRUE)
      mat <-  plyr::rename(mat, c("SUBZONE_N"="jname","OBJECTID"= "j" ))
    }
    mat <- transform(mat, fij = round(fij/ length(dates), digits=2))
    
    mat
  })
  
  create_leaflet <- reactive(function(myflows, date, hour, k, type){
    original <- myflows
    diag(myflows) <- 0
    # Select flows that represent at least 20% of the sum of outgoing flows for 
    # each urban area.
    flowSel1 <- firstflows(mat = myflows/rowSums(myflows)*100, method = "xfirst", k = k)
    
    # Select the dominant flows (incoming flows criterion)
    flowSel2 <- domflows(mat = myflows, w = colSums(myflows), k = 1)
    
    # Combine selections
    flowSel <<- myflows * flowSel1 * flowSel2
    
    # Node weights
    inflows <<- data.frame(id = colnames(myflows), w = colSums(myflows))
    
    # Plot dominant flows map
    map <<- leaflet() %>% setView(lng = 103.8517, lat = 1.2908, zoom = 11) %>% addProviderTiles(providers$OpenStreetMap, options = providerTileOptions(minZoom=11, maxZoom=15))
    
    if (type == "P_AREA"){
      #proj4string(plan_area_sdf2) <- CRS("+proj=utm +ellps=WGS84 +datum=WGS84")
      plan_area_sdf2 <- spTransform(plan_area_sdf2, CRS("+proj=longlat"))
      map <<- addPolygons(map,data=plan_area_sdf2,weight=1,col = 'blue',opacity = 1, 
                          fill = TRUE, fillColor = 'blue', fillOpacity = 0.5,
                          label = plan_area_sdf2$PLN_AREA_N,
                          group = "Basemap",
                          highlightOptions = highlightOptions(color = "white", weight = 1.5,
                                                              bringToFront = FALSE))
      
      plotMapDomFlows2(mat = flowSel,original, spdf = plan_area_sdf2, spdfid = "OBJECTID", w = inflows, wid = "id",
                       wvar = "w", wcex = 0.05)
      
    }else{
      #proj4string(sub_zone_sdf2) <- CRS("+proj=utm +ellps=WGS84 +datum=WGS84")
      sub_zone_sdf2 <- spTransform(sub_zone_sdf2, CRS("+proj=longlat"))
      
      map <<- addPolygons(map,data=sub_zone_sdf2,weight=1,col = 'blue',opacity = 1, 
                          fill = TRUE, fillColor = 'blue', fillOpacity = 0.5,
                          label = sub_zone_sdf2$SUBZONE_N,
                          group = "Basemap",
                          highlightOptions = highlightOptions(color = "white", weight = 1.5,
                                                              bringToFront = FALSE))
      
      plotMapDomFlows2(mat = flowSel, original,spdf = sub_zone_sdf2, spdfid = "OBJECTID", w = inflows, wid = "id",
                       wvar = "w", wcex = 0.05)
      
    }
  })
  
  #Do once
  
  observe({
    cat("observed triggered \n")
    inFile <- input$file11
    if (!is.null(inFile)){
      if  (inFile$name != data_file_name){
        
        processed_ride_data <- get_data()(input$file1,input$type)
      }
    }else{
      processed_ride_data <- get_data()(NULL,input$type)
      
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
        if (is.null(global_date_1) | length(setdiff(global_date_1,date_1)) > 0){
          global_date_1 <<- input$date_1
          
          data <- subset(processed_ride_data,RIDE_START_DATE %in% date_1)
          hours <-  unique(data$RIDE_START_HOUR)
          hours <- sort(hours, decreasing = FALSE)
          
          updateSelectInput(session,"hour_1",choices = hours,selected = hours[1])
        }
      }
      
      if (!is.null(date_2)){
        if (is.null(global_date_2) | length(setdiff(global_date_2,date_2)) > 0){
          
          global_date_2 <<- input$date_2
          
          data <- subset(processed_ride_data,RIDE_START_DATE %in% date_2)
          hours <-  unique(data$RIDE_START_HOUR)
          hours <- sort(hours, decreasing = FALSE)
          updateSelectInput(session,"hour_2",choices = hours,selected = hours[1])
        }
      }
    }
    
  })
  
  output$stat1 <- renderPlot({
    req(input$date_1,input$hour_1, cancelOutput = TRUE)
    date_1 <- input$date_1
    hour_1 <- input$hour_1
    
    processed_ride_data <- get_data()(input$file1,input$type)
    
    subset_data_1 <- subset(processed_ride_data,RIDE_START_HOUR == hour_1 & RIDE_START_DATE %in% date_1)
    
    mat1 <- create_matrix()(subset_data_1,input$type,date_1)
    myflows1 <- prepflows(mat = mat1, i = "i", j = "j", fij = "fij")
    
    
    statmat(mat = myflows1, output = "all", verbose = TRUE)
    
  })
  
  output$dominance_leaflet1 <- renderLeaflet({
    req(input$date_1,input$hour_1, cancelOutput = TRUE)
    date_1 <- input$date_1
    hour_1 <- input$hour_1
    K_1 <- input$K_1
    
    processed_ride_data <- get_data()(input$file1,input$type)
    
    subset_data_1 <- subset(processed_ride_data,RIDE_START_HOUR == hour_1 & RIDE_START_DATE %in% date_1)
    
    mat1 <- create_matrix()(subset_data_1,input$type,date_1)
    myflows1 <- prepflows(mat = mat1, i = "i", j = "j", fij = "fij")
    
    output$ride_table_1 <- renderDataTable(subset_data_1@data)
    output$flow_matrix_table_1 <- renderDataTable(mat1)
    create_leaflet()(myflows1,date_1, hour_1,K_1,input$type)
  })
  
  output$plotCount1 <- renderPlotly({
    req(input$date_1,input$hour_1, cancelOutput = TRUE)
    processed_ride_data <- get_data()(input$file1,input$type)
    date_1 <- input$date_1
    hour_1 <- input$hour_1
    
    subset_data_1 <- subset(processed_ride_data,RIDE_START_HOUR == hour_1 & RIDE_START_DATE %in% date_1)
    
    length<-length(date_1)
    cat("length = ",length)
    count_table <- table(subset_data_1$BOARDING_STOP_STN)
    count_table <- as.data.frame(count_table,stringsAsFactors = FALSE)
    
    count_table$Freq <- round(count_table$Freq/length,2)
    count_table <- head(count_table[order(-count_table$Freq),],5)
    count_table <- left_join(count_table,busstop_data,by=c("Var1"="BUS_STOP_N"),copy=TRUE,stringsAsFactors = FALSE)
    count_table$LOC_DESC <- factor(count_table$LOC_DESC, levels = unique(count_table$LOC_DESC)[order(count_table$Freq, decreasing = TRUE)])
    
    plot_ly(data= count_table,x =~LOC_DESC,y=~Freq, type = 'bar') %>%
      layout(title = "Top 5 Bus stop", yaxis = list(title = 'Number Of Rides'),xaxis = list(title = ''))
  })  
  
  output$stat2 <- renderPlot({
    req(input$date_2,input$hour_2, cancelOutput = TRUE)
    date_2 <- input$date_2
    hour_2 <- input$hour_2
    
    processed_ride_data <- get_data()(input$file1,input$type)
    
    subset_data_2 <- subset(processed_ride_data,RIDE_START_HOUR == hour_2 & RIDE_START_DATE %in% date_2)
    
    mat2 <- create_matrix()(subset_data_2,input$type,date_2)
    myflows2 <- prepflows(mat = mat2, i = "i", j = "j", fij = "fij")
    
    
    statmat(mat = myflows2, output = "all", verbose = TRUE)
    
  })
  
  output$dominance_leaflet2 <- renderLeaflet({
    req(input$date_2,input$hour_2, cancelOutput = TRUE)
    date_2 <- input$date_2
    hour_2 <- input$hour_2
    K_2 <- input$K_2
    processed_ride_data <- get_data()(input$file1,input$type)
    subset_data_2 <- subset(processed_ride_data,RIDE_START_HOUR == hour_2 & RIDE_START_DATE %in% date_2)
    
    mat2 <- create_matrix()(subset_data_2,input$type,date_2)
    myflows2 <- prepflows(mat = mat2, i = "i", j = "j", fij = "fij")
    
    output$ride_table_2 <- renderDataTable(subset_data_2@data)
    output$flow_matrix_table_2 <- renderDataTable(mat2)
    
    create_leaflet()(myflows2,date_2,hour_2,K_2,input$type)
  })
  
  output$plotCount2 <- renderPlotly({
    req(input$date_2,input$hour_2, cancelOutput = TRUE)
    processed_ride_data <- get_data()(input$file1,input$type)
    date_2 <- input$date_2
    hour_2 <- input$hour_2
    subset_data_2 <- subset(processed_ride_data,RIDE_START_HOUR == hour_2 & RIDE_START_DATE %in% date_2)
    
    length<-length(date_2)
    count_table <- table(subset_data_2$BOARDING_STOP_STN)
    count_table <- as.data.frame(count_table,stringsAsFactors = FALSE)
    
    count_table$Freq <- round(count_table$Freq/length, digits=2)
    count_table <- head(count_table[order(-count_table$Freq),],5)
    count_table <- left_join(count_table,busstop_data,by=c("Var1"="BUS_STOP_N"),copy=TRUE,stringsAsFactors = FALSE)
    count_table$LOC_DESC <- factor(count_table$LOC_DESC, levels = unique(count_table$LOC_DESC)[order(count_table$Freq, decreasing = TRUE)])
    
    plot_ly(data= count_table,x =~LOC_DESC,y=~Freq, type = 'bar') %>%
      layout(title = "Top 5 Bus stop", yaxis = list(title = 'Number Of Rides'),xaxis = list(title = ''))
  })
  
})