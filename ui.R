library(shiny)
library(leaflet)
library(shinyTime)



# Define UI for high-res wind application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel(tags$b("High-Resolution Wind Data Access")),
  
  sidebarPanel(
    width = 3,
    
    
    conditionalPanel(condition= 'input.dataset === "Overview"',
                     h3(tags$b('Databases'), align = "left"),
                     hr(),
                     h4('Big Southern Butte (BSB)', align = "left"),
                     tags$ul(
                         tags$li('Tall isolated mountain'),
                         tags$li('57 sensors'),
                         tags$li('June 13, 2010 - September 10, 2010')
                     ),
                     h4('Salmon River Canyon (SRC)', align = "left"),
                     tags$ul(
                       tags$li('Steep river canyon'),
                       tags$li('28 sensors'),
                       tags$li('July 13, 2011 - September 14, 2011')
                     ),
                     h4('Birch Creek (BIRCH)', align = "left"),
                     tags$ul(
                       tags$li("Cute lil' stream"),
                       tags$li('74 sensors'),
                       tags$li('June 11, 2013 - October 3, 2013')
                     )
    ),
    
    
    conditionalPanel(condition= 'input.dataset === "Site Map" ||
                     input.dataset === "Vector Plot" ||
                     input.dataset === "Time Series Plot" ||
                     input.dataset === "Sensor Summary" ||
                     input.dataset === "Site Summary" ||
                     input.dataset === "Table"',
                     selectInput("site", "Choose a site:",
                                    list("BSB" = "BSB", 
                                         "SRC" = "SRC",
                                         "BIRCH" = "BIRCH"))

    ),
    
    conditionalPanel(condition= 'input.dataset === "Time Series Plot"',
                     htmlOutput("multiselectUI")
                     ),

    conditionalPanel(condition= 'input.dataset === "Site Map" ||
                     input.dataset === "Sensor Summary" ||
                     input.dataset === "Table"',
                     htmlOutput("selectUI")
                     ),
    

    conditionalPanel(condition= 'input.dataset === "Time Series Plot" ||
                     input.dataset === "Vector Plot" ||
                     input.dataset === "Sensor Summary" ||
                     input.dataset === "Site Summary" ||
                     input.dataset === "Table"',
                     htmlOutput("setDates")
                    ),
    
    conditionalPanel(condition = 'input.dataset === "Time Series Plot"',
                     checkboxInput("advFilter", "Advanced Filter", value = FALSE)
                    ),
    
    conditionalPanel(condition = 'input.dataset === "Time Series Plot" && input.advFilter === true',
                     sliderInput("seriesTimeRange", "Time Range", min = 0,                               
                                 max = 23, value = c(8, 11))
                    ),
                     
    
    conditionalPanel(condition = 'input.dataset === "Vector Plot"',
                     #timeInput("vectorTime", "Time (tz)", seconds = FALSE, value = strptime("08:00", "%R")),  # Somewhat irrelavant on this page
                     selectInput("mapType", "Map Type", 
                                 list("Terrain"="terrain",
                                      "Hybrid"="hybrid",
                                      "Satellite"="satellite",
                                      "Roadmap"="roadmap")),
                     checkboxInput("hourly_avg", "Hourly Averaging", value = FALSE)
                     ),
    
    conditionalPanel(condition = 'input.dataset === "Time Series Plot"',
                     radioButtons("plotType", "Plot Type",
                                  c("Scatter"="p", "Line"="l"), inline = TRUE),
                     radioButtons("dataSorting", "Data Type",
                                  c("Raw", "Averaged"), inline = TRUE)
                     ),
    
    conditionalPanel(condition = 'input.dataSorting === "Averaged" && input.dataset === "Time Series Plot"',
                     numericInput("avgPer", "Averaging Period (minutes)", value = 10, min = 1), 
                     selectInput("avgJust", "Alignment",
                                 list("Center" = "center",
                                      "Right" = "right",
                                      "Left" = "left"))
                     ),
    
    conditionalPanel(condition = 'input.dataset === "Sensor Summary" ||
                                  input.dataset === "Site Summary"',
                     selectInput("summary_data_type", "Data Set",
                                 list("Wind Speed" = "wind_speed", 
                                      "Wind Gust" = "wind_gust",
                                      "Wind Direction" = "wind_dir"))
                     ), 

  
    conditionalPanel(condition = 'input.dataset === "Vector Plot" && input.hourly_avg === true',        # Might be better as two selectInputs to prevent 
                     sliderInput("timeRange", "Time Range (tz)", min = 0,                               #  unintentional time selection and easier single
                                 max = 23, value = c(8, 11))                                            #  hour selection
                     ),
    
    conditionalPanel(condition = 'input.dataset === "Vector Plot"',
                     downloadButton('vectorDownload', 'Download'),
                     helpText("Downloaded data is what is displayed in vector plot.  Wind speeds are in m/s.
                              Time is something.")
                    ),
    
    conditionalPanel(condition = 'input.dataset === "Time Series Plot"',
                     downloadButton('multiSensorDownload', 'Download'),
                     helpText("Downloaded data is for single sensor between date range specified
                              with wind and gust speeds in m/s. Time is something.")
                    ),
    
    conditionalPanel(condition = 'input.dataset === "Sensor Summary" ||
                                  input.dataset === "Table"',
                     downloadButton('sensorDownload', 'Download'),
                     helpText("Downloaded data is for single sensor between date range specified
                              with wind and gust speeds in m/s. Time is something.")
    ),
    

    conditionalPanel(condition = 'input.dataset === "Site Summary"',
                     downloadButton('siteDownload', 'Download'),
                     helpText("Downloads data for all sensors.  Wind speeds are in m/s. Time is something.")
                    )
  ),
  

  mainPanel(
    # Suppress on-screen user warnings
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
  tabsetPanel( 
      id = 'dataset',
      tabPanel("Overview", 
                   h5("This page provides access to near-surface wind data collected from a tall isolated mountain, 
                            Big Southern Butte (BSB), a steep river canyon, Salmon River Canyon (SRC), and a cute lil' stream, Birch Creek (BIRCH).",
                            tags$br(), tags$br(),
                            "These data were collected during field campaigns led by the Missoula Fire Sciences Laboratory 
                            in coordination with Washington State University and the NOAA Field Research Division. 
                            The data are intended to allow analysis of surface winds over different types of complex terrain 
                            and for evaluations of high-resolution wind models.",
                            tags$br(), tags$br(),
                            "The data displayed here are from 10-ft cup-and-vane anemometers. Sampling rate: 1-2 Hz, logging rate: 30s.
                            Additional sensors were also deployed during the field campaigns.
                            See ", tags$a(href = "http://firelab.github.io/windninja/internal/select/Downscale.html", "ACP"),
                            "for details and ", tags$a(href = "https://collab.firelab.org/software/projects/wind-obs/repository", "Repository"), 
                            "for access to the full database and additional sensors.", 
                            sep = "", align = "left"),
                  align = "center",
                  plotOutput("overviewMap", width="80%")),
      
      tabPanel("Site Map", 
               align = "center",
               leafletOutput("siteMap")
               ),
      
      tabPanel("Vector Plot", 
               align = "center",
               plotOutput("vectorMap", width="1000px", height="1000px")),
      
      tabPanel("Time Series Plot", 
               align = "center",
               plotOutput("speedPlot"),
               plotOutput("dirPlot"),
               h6('*Averages performed at top of hour', align = "left")
               ), 
      
      tabPanel("Sensor Summary",
                     align = "center",
                     plotOutput("sensor_hist"),
                     tableOutput("sensor_summary")
               ),
      
      tabPanel("Site Summary",
               align = "center",
               plotOutput("site_hist"),
               tableOutput("site_summary")
      ),
      
      tabPanel("Table", 
                       hr(),
                       DT::dataTableOutput("table"))
               )
      )
    )
  )

