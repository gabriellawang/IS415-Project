#------------------------------Check Installation------------------------------------------
packages <- c("maptools", "plyr", "dplyr", "rgdal", "sp", "flows", "data.table", "stats", 
              "shiny", "plotly", "leaflet", "shinydashboard", "DT", "spatstat", "classInt")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

#------------------------------Import packages---------------------------------------------
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

#------------------Set path----------------
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
busstop_data <- read.csv(paste(project_path,sep = "/",paste(attribute_path,sep = "/","bus_stop.csv")),stringsAsFactors = FALSE)
busstop_data$BUS_STOP_N <- c(as.character(busstop_data$BUS_STOP_N))

global_date_1 <<- NULL
global_date_2 <<- NULL
global_date <<- NULL

#-----------uncomment the following code if you wish to use csv file and generate the rds file---------------
#-----------put the CITY_NATION_RIDE_DATA_FULL.csv under the attributeTables folder inside the project file---------
#data_file_name <<- "CITY_NATION_RIDE_DATA_FULL.csv"
#ride_data <-  fread(paste(project_path,sep = "/",paste(attribute_path,sep = "/",data_file_name)))
#ride_data$RIDE_START_HOUR <- as.POSIXlt(ride_data$RIDE_START_TIME, format = "%H:%M:%S")$hour
#ride_data$BOARDING_STOP_STN <- c(as.character(ride_data$BOARDING_STOP_STN))
#ride_data$ALIGHTING_STOP_STN <- c(as.character(ride_data$ALIGHTING_STOP_STN))

#processed_data <- inner_join(ride_data,busstop_data,by=c("BOARDING_STOP_STN"="BUS_STOP_N"),suffix=c(".BOARDING",".ALIGHTING"))
#processed_data <- inner_join(processed_data,busstop_data,by=c("ALIGHTING_STOP_STN"="BUS_STOP_N"),suffix=c(".BOARDING",".ALIGHTING"))
#original_data <<- processed_data
#saveRDS(original_data, file = "original_data.rds", ascii = FALSE, version = NULL,compress = TRUE, refhook = NULL)
#---------------------------finished----------------------------------------------------------------------------

#------------comment the following code if you wish to use csv file-----------
processed_data <<- readRDS("original_data.rds")
original_data <<- processed_data
#------------------------------------------------------------------------------

selected_type <<- "P_AREA"
first_load <<- TRUE

addLegendCustom <- function(map, colors, labels, sizes, opacity = 0.5,title){
  colorAdditions <- paste0(colors, "; width:", 30, "px; height:", sizes, "px")
  labelAdditions <- paste0("<div style='display: inline-block;height: ", 
                           sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>", labels, "</div>")
  
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
  pts$cex <- sqrt((pts$var * wcex * sfdc / sc) / pi) * 100000
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
  fdom2 <<- fdom
  
  # points color
  pts$col <- "green"
  pts[pts$id %in% fdom$j & !pts$id %in% fdom$i, "col"] <- "red"
  pts[pts$id %in% fdom$j & pts$id %in% fdom$i, "col"] <- "orange"
  pts[!pts$id %in% fdom$j & pts$id %in% fdom$i, "col"] <- "yellow"
  pts <- pts[pts$col != "green",]
  # Affichage points and segments
  
  internal <- diag(original)
  internal_flow <- data.frame(internal)
  id <- row.names(internal_flow)
  internal_flow <- data.frame(id, internal)
  
  # add internal flows data
  pts <- merge(pts, internal_flow, by.x = "id", by.y = "id", all.x = T)
  pts$cex_internal <- sqrt((pts$internal * wcex * sfdc / sc) / pi) * 100000
  # add total flows data
  total <- pts$var+pts$internal
  id <- pts$id
  total_flow <- data.frame(id, total)
  pts <- merge(pts, total_flow, by.x ="id", by.y = "id", all.x = T)
  pts$cex_total <- sqrt((pts$total * wcex * sfdc / sc) / pi) * 100000
  
  # add area names data
  namelist <- names(spdf@data)
  result <- contains("PLN_AREA_N", TRUE, namelist)
  if(length(result) == 1){
    #plan_area_sdf
    area_name <- data.frame(spdf$OBJECTID, spdf$PLN_AREA_N)
    area_name <- plyr::rename(area_name,c("spdf.OBJECTID"="OBJECTID","spdf.PLN_AREA_N"="AREA_N"))
  }else{
    #sub_zone_sdf
    area_name <- data.frame(spdf$OBJECTID, spdf$SUBZONE_N)
    area_name <- plyr::rename(area_name,c("spdfp.OBJECTID"="OBJECTID","spdf.SUBZONE_N"="AREA_N"))
  }
  area_name <- as.data.frame(area_name, stringsAsFactors = FALSE)
  pts <<- merge(pts, area_name,by.x = "id", by.y = "OBJECTID", all.x = T)
  
  # add circles onto leaflet (represent the flow amount) with legend
  map <<- addCircles(map, lng = pts$long, lat = pts$lat,radius = pts$cex,
                     fill = TRUE, fillColor = pts$col, color = "grey50", weight = 0.5,
                     fillOpacity = 0.8, opacity = 1, group = "In-flow",
                     label = paste("In-flow: ",pts$var),
                     highlightOptions = highlightOptions(color = "white", weight = 1.5,
                                                         bringToFront = FALSE))
  
  map <<- addCircles(map, lng = pts$long, lat = pts$lat,radius = pts$cex_internal,
                     fill = TRUE, fillColor = "green", color = "grey50", weight = 0.5,
                     fillOpacity = 0.5, opacity = 1, group = "Internal Flow",
                     label = paste("Internal Flow(within area): ", pts$internal),
                     highlightOptions = highlightOptions(color = "white", weight = 1.5,
                                                         bringToFront = FALSE)) %>% hideGroup("Internal Flow")
  
  map <<- addCircles(map, lng = pts$long, lat = pts$lat,radius = pts$cex_total,
                     fill = TRUE, fillColor = "pink", color = "grey50", weight = 0.5,
                     fillOpacity = 0.5, opacity = 1, group = "Total Flow",
                     label = paste("Total: ",pts$total),
                     highlightOptions = highlightOptions(color = "white", weight = 1.5,
                                                         bringToFront = FALSE)) %>% hideGroup("Total Flow")
  
  map <<- addLegend(map, position = "bottomleft", 
                    title = "Size proportional\nto sum of inflows",
                    colors = c("red","orange", "yellow"), opacity = 1,
                    labels = c("Dominant", "Intermediary", 
                               "Dominated"))
  
  # generate the lines to represent the flows
  begin.coord <- data.frame(lon=fdom$longi, lat=fdom$lati)
  end.coord <- data.frame(lon=fdom$longj, lat=fdom$latj)
  width <<- data.frame(x=fdom$width)
  
  p <- psp(begin.coord[,1], begin.coord[,2], end.coord[,1], end.coord[,2],
           owin(range(c(begin.coord[,1], end.coord[,1])), range(c(begin.coord[,2], end.coord[,2]))))
  
  p<-as(p, "SpatialLines")
  # get intervals for legend displaying
  inv <- classIntervals(fdom$fij,n=3,style="jenks")
  
  # add the ploylines onto the leaflet with corresponding weight with proper legend
  map <<- addPolylines(map, data=p,weight = fdom$width,col='black', opacity = 1, group = "Segments",
                       label = paste("Flow to Dominant:", fdom$fij),
                       highlightOptions = highlightOptions(color = "white",bringToFront = FALSE))%>%
    addLegendCustom(colors = c("black", "black", "black","black"), labels = inv$brks, sizes = c(4,6,8,10),
                    title="Size proportional to flows", opacity = 1)
  
  # layer controls
  map <<- addLayersControl(map, baseGroups = c("OSM (default)", "WorldImagery"),
                           overlayGroups = c("In-flow", "Internal Flow", "Total Flow","Segments", "Basemap"))
}


#----------------------------------------------------- Perform Once End ----------------------------------------------------

options(shiny.maxRequestSize=5*1024*1024^2)
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
        
        processed_data <- inner_join(ride_data,busstop_data,
                                     by=c("BOARDING_STOP_STN"="BUS_STOP_N"),suffix=c(".BOARDING",".ALIGHTING"))
        processed_data <<- inner_join(processed_data,busstop_data,
                                      by=c("ALIGHTING_STOP_STN"="BUS_STOP_N"),suffix=c(".BOARDING",".ALIGHTING"))
        original_data <<- processed_data
        
        cat("file name =",inFile$name)
        data_file_name <<- inFile$name
        selected_type <<- type
        dates <- unique(processed_data$RIDE_START_DATE)
        if(selected_type == "P_AREA"){
          areas <- unique(processed_data$PLN_AREA_N.BOARDIN)
        }else{
          areas <- unique(processed_data$SUBZONE_N.BOARDIN)
        }

        updateSelectInput(session, "date", choices = dates,selected = dates[1])
        updateSelectInput(session, "area", choices = areas,selected = areas[1])
        
        updateSelectInput(session,"date_1",choices = dates,selected = dates[1])
        updateSelectInput(session,"date_2",choices = dates,selected = dates[1])
  
		    data <- subset(processed_data,RIDE_START_DATE %in% dates[1])
        hours <-  unique(data$RIDE_START_HOUR)
        hours <- sort(hours, decreasing = FALSE)

        updateSelectInput(session, "hour", choices = hours, selected = hours[1])
        updateSelectInput(session,"hour_1",choices = hours,selected = hours[1])
        updateSelectInput(session,"hour_2",choices = hours,selected = hours[1])
        
      }else if  (selected_type != type){
        cat("Previous file upload, different type")
        
        ride_data <- original_data
        ride_data$RIDE_START_HOUR <- as.POSIXlt(ride_data$RIDE_START_TIME, format = "%H:%M:%S")$hour
        ride_data$BOARDING_STOP_STN <- c(as.character(ride_data$BOARDING_STOP_STN))
        ride_data$ALIGHTING_STOP_STN <- c(as.character(ride_data$ALIGHTING_STOP_STN))
        
        processed_data <- inner_join(ride_data,busstop_data,
                                     by=c("BOARDING_STOP_STN"="BUS_STOP_N"),suffix=c(".BOARDING",".ALIGHTING"))
        processed_data <<- inner_join(processed_data,busstop_data,
                                      by=c("ALIGHTING_STOP_STN"="BUS_STOP_N"),suffix=c(".BOARDING",".ALIGHTING"))
        selected_type <<- type
        if(selected_type == "P_AREA"){
          areas <- unique(processed_data$PLN_AREA_N.BOARDIN)
        }else{
          areas <- unique(processed_data$SUBZONE_N.BOARDIN)
        }
        updateSelectInput(session, "area", choices = areas,selected = areas[1])
      }
    }else{
      #no input file
      if(selected_type != type){
        cat("No upload, different type  executed ")
        selected_type <<- type
        if(selected_type == "P_AREA"){
          areas <- unique(processed_data$PLN_AREA_N.BOARDIN)
        }else{
          areas <- unique(processed_data$SUBZONE_N.BOARDIN)
        }
        updateSelectInput(session, "area", choices = areas,selected = areas[1])
      }
    }
    return (processed_data)
  })
  
  create_matrix <- reactive(function(process_ride_data,type,dates){
    if (type == "P_AREA"){
      
      mat <- data.frame ( table ( process_ride_data$PLN_AREA_N.BOARDING, process_ride_data$PLN_AREA_N.ALIGHTING,dnn = c("PLN_AREA_N","jname")))
      orange <<- process_ride_data

      mat <- merge(x = mat, y = plan_area_sdf@data[c("PLN_AREA_N", "OBJECTID")], by = "PLN_AREA_N", all.x=TRUE)
      mat <- plyr::rename(mat, c("PLN_AREA_N"="iname", "jname" = "PLN_AREA_N", "Freq" = "fij", "OBJECTID" = "i"))
      mat <- merge(x = mat, y = plan_area_sdf@data[c("PLN_AREA_N", "OBJECTID")], by = "PLN_AREA_N", all.x=TRUE)
      mat <-  plyr::rename(mat, c("PLN_AREA_N"="jname","OBJECTID"= "j" ))
      
    }else{
      mat <- data.frame ( table ( process_ride_data$SUBZONE_N.BOARDING, 
                                  process_ride_data$SUBZONE_N.ALIGHTING,dnn = c("SUBZONE_N","jname")))
      mat <- merge(x = mat, y = sub_zone_sdf@data[c("SUBZONE_N", "OBJECTID")], by = "SUBZONE_N", all.x=TRUE)
      mat <- plyr::rename(mat, c("SUBZONE_N"="iname", "jname" = "SUBZONE_N", "Freq" = "fij", "OBJECTID" = "i"))
      mat <- merge(x = mat, y = sub_zone_sdf@data[c("SUBZONE_N", "OBJECTID")], by = "SUBZONE_N", all.x=TRUE)
      mat <-  plyr::rename(mat, c("SUBZONE_N"="jname","OBJECTID"= "j" ))
    }
    mat <- transform(mat, fij = round(fij/ length(dates), digits=2))
    
    mat
  })
  
  create_leaflet <- reactive(function(myflows, date, hour, k, type){
    original <<- myflows
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
    
    # Plot basemap using OSM
    map <<- leaflet() %>% setView(lng = 103.8517, lat = 1.2908, zoom = 11) %>% 
      addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap (Default)", 
                       options = providerTileOptions(minZoom=11, maxZoom=15)) %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "WorldImagery", 
                       options = providerTileOptions(minZoom=11, maxZoom=15))
    
    if (type == "P_AREA"){
      plan_area_sdf2 <- spTransform(plan_area_sdf2, CRS("+proj=longlat"))
      map <<- addPolygons(map,data=plan_area_sdf2,weight=1,col = 'blue',opacity = 1, 
                          fill = TRUE, fillColor = 'blue', fillOpacity = 0.5,
                          label = plan_area_sdf2$PLN_AREA_N,
                          group = "Basemap",
                          highlightOptions = highlightOptions(color = "white", fillColor = "white",
                                                              fillOpacity = 0.5, weight = 1.5,
                                                              bringToFront = FALSE))
      
      plotMapDomFlows2(mat = flowSel,original, spdf = plan_area_sdf2, spdfid = "OBJECTID", w = inflows, wid = "id",
                       wvar = "w", wcex = 0.05)
      
    }else{
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
        dates <- unique(processed_data$RIDE_START_DATE)
        areas <- unique(processed_data$PLN_AREA_N.BOARDIN)

        updateSelectInput(session, "date", choices = dates,selected = dates[1])
        updateSelectInput(session, "area", choices = areas,selected = areas[1])
        updateSelectInput(session,"date_1",choices = dates,selected = dates[1])
        updateSelectInput(session,"date_2",choices = dates,selected = dates[1])
        first_load <<- FALSE
      }
    }
    
    if (!is.null(processed_ride_data)){
      
      date_1 <- input$date_1
      
      date_2 <- input$date_2
      
      date <- input$date
      
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
      
      if(!is.null(date)){
        if(is.null(global_date) | length(setdiff(global_date,date)) > 0){
          global_date <<- input$date
          
          data <- subset(processed_ride_data,RIDE_START_DATE %in% date)
          hours <-  unique(data$RIDE_START_HOUR)
          hours <- sort(hours, decreasing = FALSE)
          updateSelectInput(session,"hour",choices = hours,selected = hours[1])
        }
      }
    }
  })
  
  output$daily_flow <- renderPlotly({
    selected_area <- input$area
    selected_date <- input$date
    if(input$type == "P_AREA"){
      area_data <- subset(processed_data, processed_data$PLN_AREA_N.BOARDING==selected_area & 
                            processed_data$RIDE_START_DATE==selected_date)
    }else{
      area_data <- subset(processed_data, processed_data$SUBZONE_N.BOARDING==selected_area & 
                            processed_data$RIDE_START_DATE==selected_date)
    }
    
    flow_by_hour <- count(area_data, vars=RIDE_START_HOUR)
    flow_by_hour <- plyr::rename(flow_by_hour, c("vars"="hours", "n"="flows"))
    plot_ly(flow_by_hour, x=~hours, y=~flows,type = 'bar') %>%
      layout(title=paste("Bus flows at",selected_area, "on", selected_date, sep=" "))
  })
  
  output$area_leaflet <- renderLeaflet({
    req(input$date,input$date, cancelOutput = TRUE)
    selected_area <- input$area
    selected_date <- input$date
    selected_hour <- input$hour
    
    if(input$type == "P_AREA"){
      area_data <- subset(processed_data, processed_data$PLN_AREA_N.BOARDING==selected_area & 
                            processed_data$RIDE_START_DATE==selected_date & 
                            processed_data$RIDE_START_HOUR==selected_hour)
      plan_area_sdf2 <- spTransform(plan_area_sdf2, CRS("+proj=longlat"))
      area_location <- subset(plan_area_sdf2, plan_area_sdf2$PLN_AREA_N==selected_area)
      area_location <- sp::coordinates(area_location)
    }else{
      area_data <- subset(processed_data, processed_data$SUBZONE_N.BOARDING==selected_area & 
                            processed_data$RIDE_START_DATE==selected_date & 
                            processed_data$RIDE_START_HOUR==selected_hour)
      sub_zone_sdf2 <- spTransform(sub_zone_sdf2, CRS("+proj=longlat"))
      area_location <- subset(sub_zone_sdf2, sub_zone_sdf2$SUBZONE_N==selected_area)
      area_location <- sp::coordinates(area_location)
    }
    
    flow_by_stops <- count(area_data, vars = BOARDING_STOP_STN)
    flow_by_stops <- plyr::rename(flow_by_stops, c("vars"="station", "n"="flows"))
    busstop <- readOGR(paste(project_path,sep = "/",paste(shp_path,sep = "/")),"BusStop")
    busstop <- spTransform(busstop, CRS("+proj=longlat"))
    
    busstop <- data.frame(busstop)
    busstop$BUS_STOP_N <- as.vector(busstop$BUS_STOP_N)
    flow_by_stops <- left_join(flow_by_stops, busstop, by=c("station"="BUS_STOP_N"), copy=T)
    area_map <- leaflet() %>%addProviderTiles(providers$OpenStreetMap, 
                                              options = providerTileOptions(minZoom=13, maxZoom=16))%>% 
      setView(lng=area_location[1],lat=area_location[2],zoom=13)
    area_map <- addMarkers(area_map, lng=flow_by_stops$coords.x1,lat=flow_by_stops$coords.x2,
                           label = as.character(flow_by_stops$flows))
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
    
    output$ride_table_1 <- renderDataTable(subset_data_1)
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
    m <- list(
      l=50, r=50, t=100,b=100, pad=4
    )
    plot_ly(data= count_table,x =~LOC_DESC,y=~Freq, type = 'bar') %>%
      layout(title = "Top 5 Bus stop",margin = m,yaxis = list(title = 'Number Of Rides'),xaxis = list(title = ''))
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
    
    output$ride_table_2 <- renderDataTable(subset_data_2)
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
    m <- list(
      l=50, r=50, t=100,b=100, pad=4
    )
    plot_ly(data= count_table,x =~LOC_DESC,y=~Freq, type = 'bar') %>%
      layout(title = "Top 5 Bus stop",margin = m, yaxis = list(title = 'Number Of Rides'),xaxis = list(title = ''))
  })
  
})