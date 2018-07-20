library(shiny)
library(windtools)
library(RSQLite)
library(DT)
library(plyr)
library(circular)
library(grid)
library(xts)
library(leaflet)
library(parallel)
#library(shinyTime)



#############################################################################################################
#### Database Information
#############################################################################################################

#set some initial data
db_src<-'/home/michael/Documents/shiny-applauncher/hires-wind/WindData/src.sqlite'
#db_src <- '/home/ubuntu/hd2/src/hires-wind/src.sqlite'
db_bsb<-'/home/michael/Documents/shiny-applauncher/hires-wind/WindData/bsb.sqlite'
#db_bsb <- '/home/ubuntu/hd2/src/hires-wind/bsb.sqlite'
db_birch <- '/home/michael/Documents/shiny-applauncher/hires-wind/WindData/birch.sqlite'
#db_birch <- '/home/ubuntu/hd2/src/hires-wind/birch.sqlite'


src_mindate<-'2011-07-13 23:00:00'
src_maxdate<-'2011-09-14 03:13:30'
src_plot_ids<-c("K1", "K2", "NE1", "NE2", "NE3", "NE4",                             
                "NM1", "NM2", "NM3", "NM4", "NW1", "NW2",
                "NW3", "NW4", "Natalie1", "Natalie2", "Natalie3", "Natalie4",
                "SE1", "SE2", "SE3", "SE4", "SE5", "SM1",
                "SM4", "SW2", "SW3", "SW4")

bsb_mindate<-'2010-06-13 00:00:00'
bsb_maxdate<-'2010-09-10 13:01:32'
bsb_plot_ids<-c("R1", "R2", "R3", "R4", "R5", "R6", "R7", "R8", "R9",
                "R10", "R11", "R12", "R13", "R14", "R15", "R16", "R17", "R18", "R19", 
                "R20", "R21", "R22", "R23", "R24", "R25", "R26", "R27", "R28", "R29", 
                "R30", "R31", "R32", "R33", "R34", "R35", 
                "TSW1", "TSW2", "TSW3", "TSW4", "TSW5", "TSW6", "TSW7", "TSW8", "TSW9", 
                "TSW10", "TSW11", "TSW12", "TSW13",
                "TWSW1", "TWSW3",  "TWSW4",  "TWSW5",  "TWSW6",  "TWSW8", "TWSW9", 
                "TWSW10", "TWSW11")

birch_mindate<-'2013-06-11 18:16:00'
birch_maxdate<-'2013-10-03 22:08:30'
birch_plot_ids<-c("BC-1", "BC-2", "BC-3", "BC-4", "BC-6", "BC-8", "BC-9",
                  "BC-10", "BC-11", "BC-12", "BC-13", "BC-15", "BC-17", 
                  "BC-20", "BC-21", "BC-22", "BC-23", "BC-25", "BC-26", "BC-27", "BC-28", 
                  "BC-30", "BC-31", "BC-33", "BC-34", "BC-35", "BC-37", "BC-38", "BC-39",
                  "BC-40", "BC-42", "BC-44", "BC-45", "BC-46", "BC-48", "BC-49", 
                  "BC-50", "BC-54",
                  "BC-60", "BC-61", "BC-62", "BC-64", "BC-67", "BC-68", "BC-69", 
                  "BC-70", "BC-71", "BC-72", "BC-73", "BC-75", "BC-76", "BC-77", 
                  "BC-81", "BC-82", "BC-83", "BC-84", "BC-85", "BC-86", "BC-87", "BC-88", "BC-89", 
                  "BC-90", "BC-91", "BC-92", "BC-93", "BC-95", "BC-96", "BC-97", "BC-98", 
                  "BC-100", "BC-101", "BC-102", "BC-103", "BC-106"  )

#########################################################################################################################
#####  Functions
#########################################################################################################################

###################################################################################################################
####   dbFetchSensor2 Function
###################################################################################################################

# dbFetchSensor2 <- function(db, sensor, start_time, end_time){
#   stopifnot(require("RSQLite"))
#   con <- dbConnect(SQLite(), dbname = db)
#   
#   sql <- paste0("SELECT * FROM mean_flow_obs ", 
#                 "WHERE Date_time BETWEEN '", start_time, "' ", "AND '", end_time, "' ",
#                 "AND plot_id ='", sensor, "' ", "AND Quality='OK'", collapse="")
#   
#   res <- dbSendQuery(con, statement = sql)
#   d <- fetch(res, n = -1) #fetch all data
#   
#   if (grepl("src.sqlite", db) == TRUE || grepl("birch.sqlite", db) == TRUE) {
#     d[,"date_time"] <- as.POSIXct(strptime(d[,"date_time"], '%Y-%m-%d %H:%M:%S'))
#   }
#   else if (grepl("bsb.sqlite", db) == TRUE){
#     d[,"Date_time"] <- as.POSIXct(strptime(d[,"Date_time"], '%Y-%m-%d %H:%M:%S'))
#     
#   }
#   
#   dbClearResult(res)
#   dbDisconnect(con)
#   
#   return(d)
# }
# 
# ###################################################################################################################
# ####   dbFetchSensorLocations2 Function
# ###################################################################################################################
# 
# dbFetchSensorLocation2 <- function(db, sensors){
#   stopifnot(require("RSQLite"))
#   con <- dbConnect(SQLite(), dbname = db)
#   
#   
#   for(s in 1:length(sensors)){
#     if (grepl("src.sqlite", db) == TRUE || grepl("birch.sqlite", db) == TRUE) {
#       sql <- paste0("SELECT plot_id, geometry, geometry FROM plot_location ", 
#                     "WHERE plot_id ='", sensors[s], "'", collapse="")
#       res <- dbSendQuery(con, statement = sql)
#       d <- fetch(res, n = -1) 
#     }
#     else if (grepl("bsb.sqlite", db) == TRUE) {
#       sql <- paste0("SELECT plot_id, latitude, longitude FROM plot_location ", 
#                     "WHERE plot_id ='", sensors[s], "'", collapse="")
#       res <- dbSendQuery(con, statement = sql)
#       d <- fetch(res, n = -1) 
#     }
#     
#     
#     dbClearResult(res)
#     if(s == 1){
#       master <- d
#     }
#     else{
#       master <- rbind(master, d)
#     }
#   }
#   dbDisconnect(con)
#   
#   if (grepl("src.sqlite", db) == TRUE) {
#     colnames(master) <- c("Plot_id", "Latitude", "Longitude")
#     master$Latitude <- sapply(strsplit(master$Latitude, ","), "[[",1)
#     master$Latitude <- gsub("POINT", "", master$Latitude)
#     master$Latitude <- as.numeric(substring(master$Latitude, 2))
#     
#     master$Longitude <- sapply(strsplit(master$Longitude, ","), "[[", 2)
#     master$Longitude <- as.numeric(gsub(")", "", master$Longitude))
#   }
#   else if (grepl("birch.sqlite", db) == TRUE) {
#     colnames(master) <- c("Plot_id", "Latitude", "Longitude")
#     master$Latitude <- as.numeric(sapply(strsplit(master$Latitude, " "), "[[",1))
#     master$Longitude <- as.numeric(sapply(strsplit(master$Longitude, " "), "[[", 2))
#   }
#   
#   return(master)
# }
# 
# 
# ###################################################################################################################
# ####   dbFetchHourlyAvg2 Function
# ###################################################################################################################
# 
# dbFetchHourlyAvg2 <- function(db, sensor, start_time, end_time, avg_time, align='center'){
#   stopifnot(require("RSQLite"))
#   stopifnot(require("plyr"))
#   stopifnot(require("xts"))
#   con <- dbConnect(SQLite(), dbname = db)
#   
#   sql <- paste0("SELECT * FROM mean_flow_obs ", 
#                 "WHERE Date_time BETWEEN '", start_time, "' ", "AND '", end_time, "' ",
#                 "AND plot_id ='", sensor, "' ", "AND Quality='OK'", collapse="")
#   
#   res <- dbSendQuery(con, statement = sql)
#   d <- fetch(res, n = -1) #fetch all data
#   
#   if (grepl("src.sqlite", db) == TRUE || grepl("birch.sqlite", db) == TRUE) {
#     d[,"date_time"] <- as.POSIXct(strptime(d[,"date_time"],
#                                            '%Y-%m-%d %H:%M:%S'), tz='America/Denver')
#     
#     #compute avgerage at top of hour
#     #convert to xts format
#     ts<-xts(d$wind_speed, d$date_time)
#   }
#   else if (grepl("bsb.sqlite", db) == TRUE) {
#     d[,"Date_time"] <- as.POSIXct(strptime(d[,"Date_time"],
#                                            '%Y-%m-%d %H:%M:%S'), tz='America/Denver')
#     
#     #compute avgerage at top of hour
#     #convert to xts format
#     ts<-xts(d$Wind_speed, d$Date_time)
#   }
#   
#   #compute a rolling average
#   rAvg<-rollmean(ts, avg_time, align=align)
#   #extract the value from rAvg every hour
#   e<-endpoints(rAvg,on="hours")
#   ee<-rAvg[e]
#   
#   dbClearResult(res)
#   dbDisconnect(con)
#   
#   return(ee)
# }
# 
# 
# ###################################################################################################################
# ####   dbFetchWind Function  
# ###################################################################################################################
# 
# dbFetchWind <- function(db, sensors, start_time, end_time, progressBar=FALSE) {
#   # Pulls database data for sensor(s) and puts it in format usable by windtools packages    #Averaging=FALSE
#   stopifnot(require("RSQLite"))
#   con <- dbConnect(SQLite(), dbname = db)
# 
#   if (progressBar == TRUE) {
#     progress <- shiny::Progress$new()
#     on.exit(progress$close())
#     progress$set(message = "Accessing Data", value = 0)
#   }
# 
#   for(s in 1:length(sensors)){
#     # Get wind data for sensor
#     sql <- paste0("SELECT * FROM mean_flow_obs ",
#                   "WHERE Date_time BETWEEN '", start_time, "' ", "AND '", end_time, "' ",
#                   "AND plot_id ='", sensors[s], "' ", "AND Quality='OK'", collapse="")
#     res <- dbSendQuery(con, statement = sql)
#     d <- fetch(res, n = -1) #fetch all data
#     dbClearResult(res)
# 
# 
#     # Get sensor lat/lon
#     if (grepl("src.sqlite", db) == TRUE) {
#       sql <- paste0("SELECT plot_id, geometry, geometry FROM plot_location ",
#                     "WHERE plot_id ='", sensors[s], "'", collapse="")
#       res <- dbSendQuery(con, statement = sql)
#       latlon <- fetch(res, n=-1)
#       dbClearResult(res)
# 
#       # Pull coordinates from text strings {POINT(###,###)}
#       colnames(latlon) <- c("Plot_id", "Latitude", "Longitude")
#       latlon$Latitude <- sapply(strsplit(latlon$Latitude, ","), "[[",1)
#       latlon$Latitude <- gsub("POINT", "", latlon$Latitude)
#       latlon$Latitude <- as.numeric(substring(latlon$Latitude, 2))
#       latlon$Longitude <- sapply(strsplit(latlon$Longitude, ","), "[[", 2)
#       latlon$Longitude <- as.numeric(gsub(")", "", latlon$Longitude))
# 
#       # Format lat/lon matrix
#       latlon <- matrix(c(rep(latlon[2], NROW(d)), rep(latlon[3], NROW(d))), nrow = NROW(d), ncol=2)
#     }
#     else if (grepl("birch.sqlite", db) == TRUE) {
#       sql <- paste0("SELECT plot_id, geometry, geometry FROM plot_location ",
#                     "WHERE plot_id ='", sensors[s], "'", collapse="")
#       res <- dbSendQuery(con, statement = sql)
#       latlon <- fetch(res, n=-1)
#       dbClearResult(res)
# 
#       # Pull coordinates from text strings {POINT(###,###)}
#       colnames(latlon) <- c("Plot_id", "Latitude", "Longitude")
#       latlon$Latitude <- as.numeric(sapply(strsplit(latlon$Latitude, " "), "[[",1))
#       latlon$Longitude <- as.numeric(sapply(strsplit(latlon$Longitude, " "), "[[", 2))
# 
#       # Format lat/lon matrix
#       latlon <- matrix(c(rep(latlon[2], NROW(d)), rep(latlon[3], NROW(d))), nrow = NROW(d), ncol=2)
#     }
#     else if (grepl("bsb.sqlite", db) == TRUE) {
#       sql <- paste0("SELECT latitude, longitude FROM plot_location ",
#                     "WHERE plot_id ='", sensors[s], "'", collapse="")
#       res <- dbSendQuery(con, statement = sql)
#       latlon <- fetch(res, n = -1)
#       dbClearResult(res)
# 
#       # Format lat/lon matrix
#       latlon <- matrix(c(rep(latlon[1], NROW(d)), rep(latlon[2], NROW(d))), nrow = NROW(d), ncol=2)
#     }
# 
# 
#     # Combine data
#     d <- cbind(d, latlon)
#     if(s == 1){
#       master <- d
#     }
#     else{
#       master <- rbind(master, d)
#     }
# 
#     if (progressBar == TRUE) {
#       progress$inc(1/length(sensors), detail = paste("Sensor", s))
#     }
# 
#   }
# 
#   dbDisconnect(con)
# 
# 
#   # Rename columnes to windtools standards
#   colnames(master) <- c("plot","datetime","obs_speed","wind_gust", "obs_dir", "quality", "sensor_quality", "lat", "lon")
# 
#   # Conversions
#   master$obs_speed <- master$obs_speed*1609.34/3600   # convert to m/s
#   master$wind_gust <- master$wind_gust*1609.34/3600   # convert to m/s
#   master$datetime <- as.POSIXct(strptime(master$datetime, '%Y-%m-%d %H:%M:%S'))
#   master = transform(master, lat = as.numeric(lat), lon = as.numeric(lon))
# 
#   return(master)
# }
# 
# ###################################################################################################################
# ####   buildAverages2 Function
# ###################################################################################################################
# 
# buildAverages2 <- function(df){
#   stopifnot(require("circular"))
#   
#   obs_dir_radians <- df$obs_dir * pi/180 #convert to radians
#   df <- cbind(df, obs_dir_radians)
#   
#   avgs<-data.frame(rbind(rep(NA,7)))
#   names(avgs)<-c("obs_speed", "obs_dir", "lat", "lon", "plot", "u", "v")
#   
#   #make df with avgs for each plot
#   spdAvg<-tapply(df$obs_speed, df$plot, mean)
#   dirAvgRad<-tapply(df$obs_dir_radians, df$plot, mean.circular)
#   latAvg<-tapply(df$lat, df$plot, mean)
#   lonAvg<-tapply(df$lon, df$plot, mean)
#   dirAvg<-dirAvgRad * 180/pi
#   
#   for (m in 1:length(dirAvg)){
#     if(!is.na(dirAvg[m]) && dirAvg[m] < 0.0){
#       dirAvg[m]<-dirAvg[m] + 360.0
#     }
#   }
#   
#   avgData<-as.data.frame(cbind(spdAvg, dirAvg, latAvg, lonAvg))
#   plot<-rownames(avgData)
#   avgData<-as.data.frame(cbind(avgData, plot))
#   row.names(avgData) <- NULL
#   
#   # calc u, v for avg speeds and add to speed df
#   u<-mapply(speed2u, avgData$spdAvg, avgData$dirAvg)
#   v<-mapply(speed2v, avgData$spdAvg, avgData$dirAvg)
#   avgData<-as.data.frame(cbind(avgData,u,v))
#   colnames(avgData)<-c('obs_speed', 'obs_dir', 'lat', 'lon', 'plot', 'u', 'v')
#   avgs<-rbind(avgs, avgData)
#   
#   
#   avgs<-na.omit(avgs)
#   
#   return(avgs)
# }
# 
# ###################################################################################################################
# ####   buildHourlyAverages2 Function
# ###################################################################################################################
# 
# buildHourlyAverages2 <- function(df, min_time = 0, max_time = 23){
#   stopifnot(require("circular"))
#   
#   obs_dir_radians <- df$obs_dir * pi/180 #convert to radians
#   df <- cbind(df, obs_dir_radians)
#   
#   hrSpeed<-data.frame(rbind(rep(NA,8)))
#   names(hrSpeed)<-c("obs_speed", "obs_dir", "lat", "lon", "plot", "u", "v", "hour")
#   
#   for (i in min_time:max_time){
#     if(!(i %in% unique(as.POSIXlt(df$datetime)$hour))){
#       next
#     } 
#     hour<-subset(df, subset=(as.POSIXlt(datetime)$hour == i))
#     
#     #make df with avgs for each plot
#     spdAvg<-tapply(hour$obs_speed, hour$plot, mean)
#     dirAvgRad<-tapply(hour$obs_dir_radians, hour$plot, mean.circular)
#     latAvg<-tapply(hour$lat, hour$plot, mean)
#     lonAvg<-tapply(hour$lon, hour$plot, mean)
#     dirAvg<-dirAvgRad * 180/pi
#     
#     for (m in 1:length(dirAvg)){
#       if(!is.na(dirAvg[m]) && dirAvg[m] < 0.0){
#         dirAvg[m]<-dirAvg[m] + 360.0
#       }
#     }
#     
#     hourlyAvg<-as.data.frame(cbind(spdAvg, dirAvg, latAvg, lonAvg))
#     plot<-rownames(hourlyAvg)
#     hourlyAvg<-as.data.frame(cbind(hourlyAvg, plot))
#     row.names(hourlyAvg) <- NULL
#     
#     # calc u, v for avg speeds and add to speed df
#     u<-mapply(speed2u, hourlyAvg$spdAvg, hourlyAvg$dirAvg)
#     v<-mapply(speed2v, hourlyAvg$spdAvg, hourlyAvg$dirAvg)
#     hourlyAvg<-as.data.frame(cbind(hourlyAvg,u,v,i))
#     colnames(hourlyAvg)<-c('obs_speed', 'obs_dir', 'lat', 'lon', 'plot', 'u', 'v', 'hour')
#     hrSpeed<-rbind(hrSpeed, hourlyAvg)
#   }
#   
#   hrSpeed<-na.omit(hrSpeed)
#   hrSpeed[,"hour"] <- as.factor(hrSpeed[,"hour"])
#   
#   return(hrSpeed)
# }
# 
# ###################################################################################################################
# ####   makeVectorMap2 Function 
# ###################################################################################################################
# 
# makeVectorMap2 <- function(df, lat, lon, zoom, maptype, min_time = 0, max_time = 23, colorscale='discrete',
#                            axis_labels=TRUE, scaling_factor=800.0, hourly_averaging=TRUE){
#   stopifnot(require("ggmap"))
#   stopifnot(require("grid"))
#   
#   if(hourly_averaging == TRUE){
#     df<-buildHourlyAverages2(df, min_time, max_time)
#   }
#   else{
#     df<-buildAverages(df)
#   }
#   
#   df<-cbind(df, scaling_factor)
#   
#   myMap<-get_map(location = c(lon=lon, lat=lat), zoom=zoom, maptype=maptype)
#   p <- ggmap(myMap)
#   
#   #line segements centered on sensor location
#   if(colorscale=='discrete'){
#     #scale u and v so that speed = 1, maintaining u:v ratio
#     #plot vectors of equal length, but oriented in the correct direction
#     u_scaled<-mapply(speed2u, 2, df$obs_dir)
#     v_scaled<-mapply(speed2v, 2, df$obs_dir)
#     speed_bracket <- binSpeeds(df$obs_speed)
#     df <- cbind(df, u_scaled, v_scaled, speed_bracket)
#     p <- p + geom_segment(data=df, aes(x=lon+u_scaled/scaling_factor, y=lat+v_scaled/scaling_factor,
#                                        xend = lon-u_scaled/scaling_factor, yend = lat-v_scaled/scaling_factor, 
#                                        colour = speed_bracket), arrow = arrow(ends="first", length = unit(0.2, "cm")), size = 0.7) +
#       scale_colour_manual(values = c("red", "darkorange", "darkgreen", "blue"), name="Speed (m/s)")
#   }
#   else{
#     p <- p + geom_segment(data=df, aes(x=lon+u/scaling_factor, y=lat+v/scaling_factor,
#                                        xend = lon-u/scaling_factor, yend = lat-v/scaling_factor, 
#                                        colour = obs_speed), arrow = arrow(ends="first", length = unit(0.2, "cm")), size = 0.7) +
#       scale_colour_gradient(limits=c(min(df$obs_speed),max(df$obs_speed)), name="Speed (m/s)", low="blue", high="red")
#   }
#   p <- p + theme(legend.title=element_text(size=14))
#   p <- p + theme(legend.text=element_text(size = 14))
#   p <- p + theme(strip.text.x=element_text(size = 18))
#   p <- p + theme(axis.text.x = element_text(size=18))
#   p <- p + theme(strip.text.y=element_text(size = 18))
#   p <- p + theme(axis.text.y = element_text(size=18))
#   p <- p + xlab("") + ylab("")
#   
#   if(axis_labels == TRUE){
#     p <- p + theme(axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16))
#   }
#   else{
#     p <- p + theme(axis.text.x = element_blank())
#     p <- p + theme(axis.ticks.x = element_blank())
#     p <- p + theme(axis.text.y = element_blank())
#     p <- p + theme(axis.ticks.y = element_blank())
#   }
#   
#   if(!is.null(df$hour)){
#     #don't do custom facet labeling for now... 
#     #p <- p + facet_grid(. ~ hour, labeller=facetLabeller)
#     p <- p + facet_wrap( ~ hour)
#   }
#   
#   return(p)
# }


#########################################################################################################################
#####  Server Setup
#########################################################################################################################

# Define server logic
shinyServer(function(input, output, session) {
  
  # Date range input
  output$setDates <- renderUI({ 
    if (input$site == 'SRC'){
      dateRangeInput('daterange', 'Date range:', 
                     start = "2011-08-10", #default start date
                     end = "2011-08-15", #default end date
                     min = src_mindate, 
                     max = src_maxdate)
    }
    else if (input$site == 'BSB'){
      dateRangeInput('daterange', 'Date range:', 
                     start = "2010-08-15", #default start date
                     end = "2010-08-20", #default end date
                     min = bsb_mindate,
                     max = bsb_maxdate)
    }
    else if (input$site == 'BIRCH'){
      dateRangeInput('daterange', 'Date range:', 
                     start = "2013-08-15", #default start date
                     end = "2013-08-20", #default end date
                     min = birch_mindate,
                     max = birch_maxdate)
    }
  })
  
  # Time range input
  output$setTimes <- renderUI({ 
    if (input$site == 'SRC'){
      dateRangeInput('timerange', 'Time range:', 
                     start = "2011-08-10", #default start date
                     end = "2011-08-15", #default end date
                     min = src_mindate, 
                     max = src_maxdate)
    }
    else if (input$site == 'BSB'){
      dateRangeInput('timerange', 'Time range:', 
                     start = "2010-08-15", #default start date
                     end = "2010-08-20", #default end date
                     min = bsb_mindate,
                     max = bsb_maxdate)
    }
    else if (input$site == 'BIRCH'){
      dateRangeInput('timerange', 'Time range:', 
                     start = "2013-08-15", #default start date
                     end = "2013-08-20", #default end date
                     min = birch_mindate,
                     max = birch_maxdate)
    }
  })
  
  # Sensor input selection
  output$selectUI <- renderUI({
    
    if(input$site == 'SRC'){
      selectInput('variable', 'Choose a sensor:', src_plot_ids)
    }
    else if(input$site == 'BSB'){
      selectInput('variable', 'Choose a sensor:', bsb_plot_ids)
    }
    else if(input$site == 'BIRCH'){
      selectInput('variable', 'Choose a sensor:', birch_plot_ids)
    }
  })
  
  # Multiple input selection for time series page
  output$multiselectUI <- renderUI({

    if(input$site == 'SRC'){
      selectInput('multi_var', 'Sensor(s)', src_plot_ids, multiple=TRUE, selectize=TRUE, selected = input$variable)
    }
    else if(input$site == 'BSB'){
      selectInput('multi_var', 'Sensor(s)', bsb_plot_ids, multiple=TRUE, selectize=TRUE, selected = input$variable)
    }
    else if(input$site == 'BIRCH'){
      selectInput('multi_var', 'Sensor(s)', birch_plot_ids, multiple=TRUE, selectize=TRUE, selected = input$variable)
    }
  })
  

  #######################################################################################################################
  ###  Map of Sites - Overview Page
  #######################################################################################################################
  
  # Overview map of site locations
  output$overviewMap <- renderPlot({
    library(maptools)
    library(maps)
    library(ggmap)
    library(sp)
    #-------------------------------------
    #   make a US map with sites labeled
    #-------------------------------------
    xlim<-c(-125, -110)
    ylim<-c(42, 49)
    domain<-map("state", regions = c("idaho","Montana","Wyoming","oregon", "washington", "california", "Nevada", "utah",      
                                     "colorado", "new mexico", "arizona"), plot = FALSE, fill = TRUE, warnings = FALSE)
    IDs<-sub("^idaho,", "", domain$names)
    domain_sp<-map2SpatialPolygons(domain, IDs, CRS("+proj=longlat"))
    sites<-cbind(-113.0283, 43.40202)
    sites<-rbind(sites, cbind(-116.2314, 45.40276))
    sites <- rbind(sites, cbind(-112.912886, 44.18216))
    sp<-SpatialPoints(sites, proj4string=CRS("+proj=longlat +datum=WGS84"))
    plot(domain_sp, axes = TRUE, xlim=xlim, ylim=ylim)
    plot(sp, add=TRUE, pch = 19)
    text(-113, 43, "BSB")
    text(-115.6, 45.1, "SRC")
    text(-112, 43.9, "BIRCH")
  })
  
  #####################################################################################################################
  ##### Map of Sensor Locations - Site Map Page
  #####################################################################################################################
  
  output$siteMap <- renderLeaflet({
    if (input$variable == "") {
          if (input$site == 'SRC' ) {
                updateSelectInput(session, 'variable', choices = src_plot_ids, selected = "K1")
          }
          if(input$site == 'BSB') {
                updateSelectInput(session, 'variable', choices = bsb_plot_ids, selected = "R1")
          }
          if(input$site == 'BIRCH') {
                updateSelectInput(session, 'variable', choices = birch_plot_ids, selected = "BC-1")
          }
    }
    
    if (input$site == 'SRC') {
      loc <- dbFetchSensorLocation(db_src, src_plot_ids)
      lat <- mean(loc$Latitude)
      lon <- mean(loc$Longitude)
      row <- loc[loc$Plot_id == input$variable, ]
      zoom = 12
    }
    else if (input$site == 'BSB'){
      loc <- dbFetchSensorLocation(db_bsb, bsb_plot_ids)
      lat <- mean(loc$Latitude)
      lon <- mean(loc$Longitude)
      row <- loc[loc$Plot_id == input$variable, ]
      zoom = 11
    }
    else if (input$site == 'BIRCH'){
      loc <- dbFetchSensorLocation(db_birch, birch_plot_ids)
      lat <- mean(loc$Latitude)
      lon <- mean(loc$Longitude)
      row <- loc[loc$Plot_id == input$variable, ]
      zoom = 10
    }

    leaflet(data = loc) %>%
      addProviderTiles(providers$Stamen.Terrain) %>%
      addCircleMarkers(~Longitude, ~Latitude,
                       label = ~as.character(Plot_id),
                       radius = 3,
                       color = "red",
                       stroke = FALSE,
                       fillOpacity = 0.8) %>%
      addCircleMarkers(data = row, ~Longitude, ~Latitude,
                       label = ~as.character(Plot_id),
                       radius = 3,
                       color = "black",
                       stroke = FALSE,
                       fillOpacity = 0.8) %>%
      setView(lng = lon, lat = lat, zoom = zoom)

  })

  # Update sensor selection based on map click
  observe({

    if (input$site == 'SRC') {
      loc <- dbFetchSensorLocation(db_src, src_plot_ids) #2
    }
    else if (input$site == 'BSB'){
      loc <- dbFetchSensorLocation(db_bsb, bsb_plot_ids)
    }
    else if (input$site == 'BIRCH'){
      loc <- dbFetchSensorLocation(db_birch, birch_plot_ids)
    }

    leafletProxy("siteMap")
    event <- input$siteMap_marker_click   # returns $lat, $lng
    if (is.null(event))
      return()

    site <- loc[loc$Latitude == event$lat,]
    updateSelectInput(session, 'variable', selected = site$Plot_id)

  })
  
    
  ###################################################################################################################
  ####   Vector Map 
  ###################################################################################################################
  
  output$vectorMap <- renderPlot({

    start <- paste0(input$startTime, ":00")
    end <- paste0(input$endTime, ":00")
    start_time <- paste(substr(min(input$daterange), 1, 10), start)
    end_time <- paste(substr(max(input$daterange), 1, 10), end)
    
    if(input$site == 'SRC'){
      s<-dbFetchWind(db_src, src_plot_ids, start_time, end_time, progressBar=TRUE)
      lat <- mean(s$lat)
      lon <- mean(s$lon)
      zoom <- 13
    }
    else if(input$site == 'BSB'){
      s<-dbFetchWind(db_bsb, bsb_plot_ids, start_time, end_time, progressBar=TRUE)
      lat <- mean(s$lat)
      lon <- mean(s$lon)
      zoom <- 12
    }
    else if(input$site == 'BIRCH'){
      s<-dbFetchWind(db_birch, birch_plot_ids, start_time, end_time, progressBar=TRUE)
      lat <- mean(s$lat)
      lon <- mean(s$lon)
      zoom <- 11
    }

    options(warn=-1) # Turn warnings off
    plot <- makeVectorMap(s, lat, lon, zoom, maptype = input$mapType, min_time=input$timeRange[1], max_time=input$timeRange[2], hourly_averaging = input$hourly_avg)
    options(warn=0) # Turn warnings on
    if (input$hourly_avg == TRUE) {
      plot <- plot + theme(axis.text.x = element_text(size = (18-10)/23*(23-(input$timeRange[2]-input$timeRange[1]))+10), 
                           axis.text.y = element_text(size = (18-10)/23*(23-(input$timeRange[2]-input$timeRange[1]))+10))  # Scale labels to # of images
    }

    print(plot)
    
  })
  
  
  
  ###########################################################################################################
  ########   Times Series Plot 
  ###########################################################################################################
  
  # Generate a plot of the requested sensor speed 
  output$speedPlot <- renderPlot({
    if (is.null(input$multi_var)) {
      if (input$site == 'SRC' ) {
        updateSelectInput(session, 'multi_var', choices = src_plot_ids, selected = "K1")
      }
      if(input$site == 'BSB') {
        updateSelectInput(session, 'multi_var', choices = bsb_plot_ids, selected = "R1")
      }
      if(input$site == 'BIRCH') {
        updateSelectInput(session, 'multi_var', choices = birch_plot_ids, selected = "BC-1")
      }
    }
   
    start <- paste0(input$startTime, ":00")
    end <- paste0(input$endTime, ":00")
    start_date <- paste(substr(min(input$daterange), 1, 10), start)
    end_date <- paste(substr(max(input$daterange), 1, 10), end)
     
    if (input$dataSorting == 'Raw') {
      if(input$site == 'SRC'){
        for (i in 1:length(input$multi_var)) {
          s<-dbFetchSensor(db_src, input$multi_var[i], start_date, end_date)
          if (i == 1) {
            master <- s
          }
          else {
            master <- rbind(master, s)
          }
        }
      }
      else if(input$site == 'BSB'){
        for (i in 1:length(input$multi_var)) {
          s<-dbFetchSensor(db_bsb, input$multi_var[i], start_date, end_date)
          colnames(s) <- c("plot_id", "date_time", "wind_speed", "wind_gust", "wind_dir", "quality", "sensor_quality")
          if (i == 1) {
            master <- s
          }
          else {
            master <- rbind(master, s)
          }
        }
      }
      else if(input$site == 'BIRCH'){
        for (i in 1:length(input$multi_var)) {
          s<-dbFetchSensor(db_birch, input$multi_var[i], start_date, end_date)
          if (i == 1) {
            master <- s
          }
          else {
            master <- rbind(master, s)
          }
        }      
      }
    }
    
    else if (input$dataSorting == 'Averaged') {
      if(input$site == 'SRC'){
        for (i in 1:length(input$multi_var)) {
            s <- dbFetchHourlyAvg(db_src, input$multi_var[i], start_date, end_date, avg_time=input$avgPer, align=input$avgJust)
            s <- data.frame(date_time=index(s), wind_speed = coredata(s))
            s$plot_id <- rep(input$multi_var[i], NROW(s))
            if (i == 1) {
               master <- s
            }
            else {
              master <- rbind(master, s)
            }
        }
      }
      else if(input$site == 'BSB'){
        for (i in 1:length(input$multi_var)) {
          s <- dbFetchHourlyAvg(db_bsb, input$multi_var[i], start_date, end_date, avg_time=input$avgPer, align=input$avgJust)
          s <- data.frame(date_time=index(s), wind_speed = coredata(s))
          s$plot_id <- rep(input$multi_var[i], NROW(s))
          if (i == 1) {
            master <- s
          }
          else {
            master <- rbind(master, s)
          }
        }
      }
      else if(input$site == 'BIRCH'){
        for (i in 1:length(input$multi_var)) {
          s <- dbFetchHourlyAvg(db_birch, input$multi_var[i], start_date, end_date, avg_time=input$avgPer, align=input$avgJust)
          s <- data.frame(date_time=index(s), wind_speed = coredata(s))
          s$plot_id <- rep(input$multi_var[i], NROW(s))
          if (i == 1) {
            master <- s
          }
          else {
            master <- rbind(master, s)
          }
        }
      }
    }
    
    

    # Convert wind speeds to m/s
    master$wind_speed <- master$wind_speed*1609.34/3600
    
    if(is.null(master))
      return()

    # Formating for single sensor      
    if (length(input$multi_var) == 1) {
      p<-ggplot(s, aes(x=date_time, y=wind_speed)) +
        geom_point(shape=19, size=1.5, color='blue') +
        theme_bw() +
        xlab("Time") +
        ylab("Wind Speed (m/s)") +
        ggtitle(master$plot_id)
    }
    else {   # Formating for multiple sensors
      p <- ggplot(master, aes(x = date_time, y = wind_speed, colour = plot_id)) + 
        geom_point(shape=19, size=1.5) + 
        theme_bw() +
        ylab(label="Wind Speed (m/s)") + 
        xlab("Date")
    }
    
    p<-p + scale_x_datetime(breaks=c(min(master$date_time),
                                     (max(master$date_time) - min(master$date_time))/4 + min(master$date_time),
                                     (max(master$date_time) - min(master$date_time))/4*2 + min(master$date_time),
                                     (max(master$date_time) - min(master$date_time))/4*3 + min(master$date_time),
                                     max(master$date_time)))
    
    p <- p + theme(axis.text.x = element_text(angle = 45))
    p <- p + theme(axis.text.x = element_text(vjust = 0.5))
    
    p <- p + theme(axis.text = element_text(size = 14))
    p <- p + theme(axis.title = element_text(size = 14))
    
    p <- p + theme(plot.margin=unit(c(1,1,1,1),"cm"))
    
    # Add line if line plot selected
    if (input$plotType == 'l') {
      if (length(input$multi_var) == 1){
        p <- p + geom_line(color='blue')
      }
      else {
        p <- p + geom_line()
      }
    }
    
    print(p)
    
  })
  
  ###########################################################################################################
  ########   Wind Direction Plot 
  ###########################################################################################################
  
  
  # Generate a plot of the requested sensor speed 
  output$dirPlot <- renderPlot({
    if (is.null(input$multi_var)) {
      if (input$site == 'SRC' ) {
        updateSelectInput(session, 'multi_var', choices = src_plot_ids, selected = "K1")
      }
      if(input$site == 'BSB') {
        updateSelectInput(session, 'multi_var', choices = bsb_plot_ids, selected = "R1")
      }
      if(input$site == 'BIRCH') {
        updateSelectInput(session, 'multi_var', choices = birch_plot_ids, selected = "BC-1")
      }
    }
    
    start <- paste0(input$startTime, ":00")
    end <- paste0(input$endTime, ":00")
    start_time <- paste(substr(min(input$daterange), 1, 10), start)
    end_time <- paste(substr(max(input$daterange), 1, 10), end)
    
      if(input$site == 'SRC'){
        for (i in 1:length(input$multi_var)) {
          s<-dbFetchSensor(db_src, input$multi_var[i], start_time, end_time)
          if (i == 1) {
            master <- s
          }
          else {
            master <- rbind(master, s)
          }
        }      
      }
      else if(input$site == 'BSB'){
        for (i in 1:length(input$multi_var)) {
          s<-dbFetchSensor(db_bsb, input$multi_var[i], start_time, end_time)
          colnames(s) <- c("plot_id", "date_time", "wind_speed", "wind_gust", "wind_dir", "quality", "sensor_quality")
          if (i == 1) {
            master <- s
          }
          else {
            master <- rbind(master, s)
          }
        }
      }
      else if(input$site == 'BIRCH'){
        for (i in 1:length(input$multi_var)) {
           s<-dbFetchSensor(db_birch, input$multi_var[i], start_time, end_time)
           if (i == 1) {
             master <- s
           }
           else {
             master <- rbind(master, s)
           }
        }
      }
    
      colnames(master)<-c("plot_id", "date_time", "wind_speed", "wind_gust", "wind_dir", "quality", "sensor_qual")
      
      if(is.null(master))
        return()

      # Formating for single sensor      
      if (length(input$multi_var) == 1) {
          p<-ggplot(s, aes(x=date_time, y=wind_dir)) +
            geom_point(shape=19, size=1.5, color='blue') +
            theme_bw() +
            xlab("Time") +
            ylab("Direction (degrees)") +
            ggtitle(master$plot_id)
      }
      else {   # Formating for multiple sensors
          p <- ggplot(master, aes(x = date_time, y = wind_dir, colour = plot_id)) + 
            geom_point(shape=19, size=1.5) + 
            theme_bw() +
            ylab(label="Direction (degrees)") + 
            xlab("Date")
      }
        
      p<-p + scale_x_datetime(breaks=c(min(master$date_time),
                                       (max(master$date_time) - min(master$date_time))/4 + min(master$date_time),
                                       (max(master$date_time) - min(master$date_time))/4*2 + min(master$date_time),
                                       (max(master$date_time) - min(master$date_time))/4*3 + min(master$date_time),
                                       max(master$date_time)))

      p <- p + theme(axis.text.x = element_text(angle = 45))
      p <- p + theme(axis.text.x = element_text(vjust = 0.5))
      
      p <- p + theme(axis.text = element_text(size = 14))
      p <- p + theme(axis.title = element_text(size = 14))
      
      p <- p + theme(plot.margin=unit(c(1,1,1,1),"cm"))
    
    # Add line if line plot selected
    if (input$plotType == 'l') {
      if (length(input$multi_var) == 1){
        p <- p + geom_line(color='blue')
      }
      else {
        p <- p + geom_line()
      }
    }
    
    print(p)
    
  })
  
  ###########################################################################################################################
  ####  Sensor Summary
  ###########################################################################################################################
  
  # Generate a summary of the sensor data            
  output$sensor_summary <- renderTable({
    if (input$variable == "") {
      if (input$site == 'SRC' ) {
        updateSelectInput(session, 'variable', choices = src_plot_ids, selected = "K1")
      }
      if(input$site == 'BSB') {
        updateSelectInput(session, 'variable', choices = bsb_plot_ids, selected = "R1")
      }
      if(input$site == 'BIRCH') {
        updateSelectInput(session, 'variable', choices = birch_plot_ids, selected = "BC-1")
      }
    }
    
    start <- paste0(input$startTime, ":00")
    end <- paste0(input$endTime, ":00")
    start_date <- paste(substr(min(input$daterange), 1, 10), start)
    end_date <- paste(substr(max(input$daterange), 1, 10), end)
    
    if (input$site == 'SRC'){
      s <- dbFetchWind(db_src, input$variable, start_date, end_date, progressBar = TRUE)
    }
    else if (input$site == 'BSB'){
      s <- dbFetchWind(db_bsb, input$variable, start_date, end_date, progressBar = TRUE)
    }
    else if (input$site == 'BIRCH'){
      s <- dbFetchWind(db_birch, input$variable, start_date, end_date, progressBar = TRUE)
    }
    
    if (input$summary_data_type == "wind_speed") {
      sum <- summary(s$obs_speed)
      t(c(sum))
    }
    else if (input$summary_data_type == "wind_gust") {
      sum <- summary(s$wind_gust)
      t(c(sum))
    }
    
  })
  
  # Histogram of the sensor summary data
  output$sensor_hist <- renderPlot({
    if (input$variable == "") {
      if (input$site == 'SRC' ) {
        updateSelectInput(session, 'variable', choices = src_plot_ids, selected = "K1")
      }
      if(input$site == 'BSB') {
        updateSelectInput(session, 'variable', choices = bsb_plot_ids, selected = "R1")
      }
      if(input$site == 'BIRCH') {
        updateSelectInput(session, 'variable', choices = birch_plot_ids, selected = "BC-1")
      }
    }

    start <- paste0(input$startTime, ":00")
    end <- paste0(input$endTime, ":00")
    start_date <- paste(substr(min(input$daterange), 1, 10), start)
    end_date <- paste(substr(max(input$daterange), 1, 10), end)
    
    if (input$site == 'SRC'){
      s <- dbFetchWind(db_src, input$variable, start_date, end_date, progressBar = TRUE)
    }
    else if (input$site == 'BSB'){
      s <- dbFetchWind(db_bsb, input$variable, start_date, end_date, progressBar = TRUE)
    }
    else if (input$site == 'BIRCH'){
      s <- dbFetchWind(db_birch, input$variable, start_date, end_date, progressBar = TRUE)
    }
    
    if (input$summary_data_type == "wind_speed") {
      hist_data <- s$obs_speed
      title <- "Wind Speed"
      xlab <- "Observed Speed (m/s)"
    }
    else if (input$summary_data_type == "wind_gust") {
      hist_data <- s$wind_gust
      title <- "Wind Gust"
      xlab <- "Observed Speed (m/s)"
    }
    else if (input$summary_data_type == "wind_dir") {
      hist_data <- s$obs_dir
      title <- "Wind Direction"
      xlab <- "Direction (0 = North, 90 = East, 180 = South, 270 = West)" 
    }
    
    if(is.na(hist_data) || is.null(hist_data))
      return()
    
    hist(hist_data,
         main = paste(input$site, input$variable, title),
         col = "#0000FF", border = "white", 
         xlab = xlab,
         ylab = "Measurements")
  })
  
  ###########################################################################################################################
  ####  Site Summary
  ###########################################################################################################################
  
  # Generate a summary of the site data            
  output$site_summary <- renderTable({ 
    if (input$variable == "") {
      if (input$site == 'SRC' ) {
        updateSelectInput(session, 'variable', choices = src_plot_ids, selected = "K1")
      }
      if(input$site == 'BSB') {
        updateSelectInput(session, 'variable', choices = bsb_plot_ids, selected = "R1")
      }
      if(input$site == 'BIRCH') {
        updateSelectInput(session, 'variable', choices = birch_plot_ids, selected = "BC-1")
      }
    }
    
    start <- paste0(input$startTime, ":00")
    end <- paste0(input$endTime, ":00")
    start_date <- paste(substr(min(input$daterange), 1, 10), start)
    end_date <- paste(substr(max(input$daterange), 1, 10), end)
    
    if (input$site == 'SRC'){
      s<-dbFetchWind(db_src, src_plot_ids, start_date, end_date, progressBar = TRUE)
    }
    else if (input$site == 'BSB'){
      s<-dbFetchWind(db_bsb, bsb_plot_ids, start_date, end_date, progressBar = TRUE)
    }
    else if (input$site == 'BIRCH'){
      s<-dbFetchWind(db_birch, birch_plot_ids, start_date, end_date, progressBar = TRUE)
    }
    
    if (input$summary_data_type == "wind_speed") {
      sum <- summary(s$obs_speed)
      t(c(sum))
    }
    else if (input$summary_data_type == "wind_gust") {
      sum <- summary(s$wind_gust)
      t(c(sum))
    }
  })
  
  # Histogram of the site summary data
  output$site_hist <- renderPlot({
    if (input$variable == "") {
      if (input$site == 'SRC' ) {
        updateSelectInput(session, 'variable', choices = src_plot_ids, selected = "K1")
      }
      if(input$site == 'BSB') {
        updateSelectInput(session, 'variable', choices = bsb_plot_ids, selected = "R1")
      }
      if(input$site == 'BIRCH') {
        updateSelectInput(session, 'variable', choices = birch_plot_ids, selected = "BC-1")
      }
    }
    
    start <- paste0(input$startTime, ":00")
    end <- paste0(input$endTime, ":00")
    start_date <- paste(substr(min(input$daterange), 1, 10), start)
    end_date <- paste(substr(max(input$daterange), 1, 10), end)
    
    if (input$site == 'SRC'){
      s<-dbFetchWind(db_src, src_plot_ids, start_date, end_date, progressBar = TRUE)
    }
    else if (input$site == 'BSB'){
      s<-dbFetchWind(db_bsb, bsb_plot_ids, start_date, end_date, progressBar = TRUE)
    }
    else if (input$site == 'BIRCH'){
      s<-dbFetchWind(db_birch, birch_plot_ids, start_date, end_date, progressBar = TRUE)
    }
    
    if (input$summary_data_type == "wind_speed") {
      hist_data <- s$obs_speed
      title <- "Wind Speed"
      xlab <- "Observed Speed (m/s)"
    }
    else if (input$summary_data_type == "wind_gust") {
      hist_data <- s$wind_gust
      title <- "Wind Gust"
      xlab <- "Observed Speed (m/s)"
    }
    else if (input$summary_data_type == "wind_dir") {
      hist_data <- s$obs_dir
      title <- "Wind Direction"
      xlab <- "Direction (0 = North, 90 = East, 180 = South, 270 = West)"
    }
    
    if(is.na(hist_data) || is.null(hist_data))
      return()
    
    hist(hist_data,
         main = paste(input$site, title),
         col = "#0000FF", border = "white", 
         xlab = xlab,
         ylab = "Measurements")
  })
  
  ###########################################################################################################################
  #######  Data Table
  ###########################################################################################################################
  
  # Generate an HTML table view of the data                           # Really slow to display all data
  output$table <- DT::renderDataTable({  
    if (input$variable == "") {
      if (input$site == 'SRC' ) {
        updateSelectInput(session, 'variable', choices = src_plot_ids, selected = "K1")
      }
      if(input$site == 'BSB') {
        updateSelectInput(session, 'variable', choices = bsb_plot_ids, selected = "R1")
      }
      if(input$site == 'BIRCH') {
        updateSelectInput(session, 'variable', choices = birch_plot_ids, selected = "BC-1")
      }
    }
    
    start <- paste0(input$startTime, ":00")
    end <- paste0(input$endTime, ":00")
    start_date <- paste(substr(min(input$daterange), 1, 10), start)
    end_date <- paste(substr(max(input$daterange), 1, 10), end)
    
    if (input$site == 'SRC'){
      s<-dbFetchWind(db_src, input$variable, start_date, end_date, progressBar = TRUE)
    }
    else if (input$site == 'BSB'){
      s<-dbFetchWind(db_bsb, input$variable, start_date, end_date, progressBar = TRUE)
    }
    else if (input$site == 'BIRCH'){
      s<-dbFetchWind(db_birch, input$variable, start_date, end_date, progressBar = TRUE)
    }
    
    s$datetime <- as.character(s$datetime)
    s$obs_speed <- round(s$obs_speed, 2)
    s$wind_gust <- round(s$wind_gust, 2)
    s$lat <- round(s$lat, 3)
    s$lon <- round(s$lon, 3)
    colnames = c("Sensor ID", "Date/Time", "Wind Speed (m/s)", "Wind Gust (m/s)", "Direction", "Quality", "Status", "Latitude", "Longitude")
    datatable(s,
              colnames = colnames,
              options = list(
                lengthMenu = list(c(50, 100, 500, 1000, -1), c('50', '100', '500', '1,000', 'All')),
                pageLength = 50,
                searching = TRUE,
                align = "center"
              ))
  })
  
  ####################################################################################################################
  ###### Download Data
  ####################################################################################################################
  
  # Download time series data for sensor(s) from selected date range to .csv file (time series plot)
  multiSensorDataset <- reactive({
    start <- paste0(input$startTime, ":00")
    end <- paste0(input$endTime, ":00")
    start_date <- paste(substr(min(input$daterange), 1, 10), start)
    end_date <- paste(substr(max(input$daterange), 1, 10), end)
    
    if(input$site == 'SRC'){
      for (i in 1: length(input$multi_var)) {
          s<-dbFetchWind(db_src, input$multi_var[i], start_date, end_date)
          if (i == 1 ) {
            data <- s
          }
          else {
            data <- rbind(data, s)
          }
      }
    }
    else if(input$site == 'BSB'){
      for (i in 1: length(input$multi_var)) {
        s<-dbFetchWind(db_bsb, input$multi_var[i], start_date, end_date)
        if (i == 1 ) {
          data <- s
        }
        else {
          data <- rbind(data, s)
        }
      }
    }
    else if(input$site == 'BIRCH'){
      for (i in 1: length(input$multi_var)) {
        s<-dbFetchWind(db_birch, input$multi_var[i], start_date, end_date)
        if (i == 1 ) {
          data <- s
        }
        else {
          data <- rbind(data, s)
        }
      }
    }
    return(data)
  })
  
  output$multiSensorDownload <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(multiSensorDataset(), file, row.names = FALSE)   # wind speed in m/s
    }
  )
  
  ##################################################################################################################
  
  # Download time series data for sensor from selected date range to .csv file (Sensor Summary and Table pages)
  sensorDataset <- reactive({
    start <- paste0(input$startTime, ":00")
    end <- paste0(input$endTime, ":00")
    start_date <- paste(substr(min(input$daterange), 1, 10), start)
    end_date <- paste(substr(max(input$daterange), 1, 10), end)
    
    if(input$site == 'SRC'){
        s<-dbFetchWind(db_src, input$variable, start_date, end_date)
    }
    else if(input$site == 'BSB'){
        s<-dbFetchWind(db_bsb, input$variable, start_date, end_date)
    }
    else if(input$site == 'BIRCH'){
        s<-dbFetchWind(db_birch, input$variable, start_date, end_date)
    }
    return(s)
  })
  
  output$sensorDownload <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(sensorDataset(), file, row.names = FALSE)   # wind speed in m/s
    }
  )
  
  ##################################################################################################################
  
  
  # Download site data from selected date range to .csv file
  siteDataset <- reactive({
    start <- paste0(input$startTime, ":00")
    end <- paste0(input$endTime, ":00")
    start_date <- paste(substr(min(input$daterange), 1, 10), start)
    end_date <- paste(substr(max(input$daterange), 1, 10), end)
    
    if(input$site == 'SRC'){
      s<-dbFetchWind(db_src, src_plot_ids, start_date, end_date)
    }
    else if(input$site == 'BSB'){
      s<-dbFetchWind(db_bsb, bsb_plot_ids, start_date, end_date)
    }
    else if(input$site == 'BIRCH'){
      s<-dbFetchWind(db_birch, birch_plot_ids, start_date, end_date)
    }
  })
  
  output$siteDownload <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(siteDataset(), file, row.names = FALSE)   # wind speed in m/s
    }
  )
  
  #############################################################################################################################3
  
  # Download wind vector data from selected date range to .csv file
  vectorDataset <- reactive({
    start <- paste0(input$startTime, ":00")
    end <- paste0(input$endTime, ":00")
    start_date <- paste(substr(min(input$daterange), 1, 10), start)
    end_date <- paste(substr(max(input$daterange), 1, 10), end)
    
    if(input$site == 'SRC'){
      s <- dbFetchWind(db_src, src_plot_ids, start_date, end_date)
      if (input$hourly_avg == TRUE) {s <- buildHourlyAverages2(s, min_time = input$timeRange[1], max_time = input$timeRange[2])}
      else {s <- buildAverages(s)}
    }
    else if(input$site == 'BSB'){
      s <- dbFetchWind(db_bsb, bsb_plot_ids, start_date, end_date)
      if (input$hourly_avg == TRUE) {s <- buildHourlyAverages2(s, min_time = input$timeRange[1], max_time = input$timeRange[2])}
      else {s <- buildAverages(s)}
    }
    else if(input$site == 'BIRCH'){
      s <- dbFetchWind(db_birch, birch_plot_ids, start_date, end_date)
      if (input$hourly_avg == TRUE) {s <- buildHourlyAverages2(s, min_time = input$timeRange[1], max_time = input$timeRange[2])}
      else {s <- buildAverages(s)}
    }
  })
  
  output$vectorDownload <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(vectorDataset(), file, row.names = FALSE)    # Wind speed in m/s
    }
  )
  
})

