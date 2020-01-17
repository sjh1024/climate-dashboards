# app.R
#####################################################################################################################
# UPDATED VERSION PUSHED TO shinyapps.io 3-22-2019
######################################################################################################################
# @author mem, sjh1024 
#

library(shiny)
library(dygraphs)
library(datasets)
library(xts)
library(lubridate)
library(shinyBS)
#Initialize datasets; read csv files into values RShiny can reference later and put into graphs.
#Also reads in dates from files.
precipitationHourly <- read.csv("http://hbrsensor.sr.unh.edu/data/hbrloggernet/shinyfiles/hbef_shiny_precipHouly.csv", header=TRUE)
precipitationHourly$DATETIME <- as.POSIXlt(precipitationHourly$DATETIME, tz = "America/New_York")
precipitationEvent <- read.csv("http://hbrsensor.sr.unh.edu/data/hbrloggernet/shinyfiles/hbef_shiny_precipeventWS1mm.csv", header=TRUE, col.names = c("DATETIME", "PRECIP_EVENT" ,  "WS2_EVENT" , "WS3_EVENT" ,
                                                                                                                                                      "WS4_EVENT" , "WS5_EVENT" , "WS6_EVENT" , "WS7_EVENT" , 
                                                                                                                                                      "WS8_EVENT", "WS9_EVENT" ))
precipitationEvent$DATETIME <- as.POSIXlt(precipitationEvent$DATETIME, tz = "America/New_York")
tempRHcols <- read.csv("http://hbrsensor.sr.unh.edu/data/hbrloggernet/shinyfiles/hbef_shiny_temperature.csv",header=TRUE )
temperature <- read.csv("http://hbrsensor.sr.unh.edu/data/hbrloggernet/shinyfiles/hbef_shiny_temperature.csv",header=TRUE ,col.names = c("DATETIME", "HB_WX1_TEMP" , "HB_WXHQ_TEMP", "HB_WX6_TEMP", "HB_WX14_TEMP", "HB_WX23_TEMP"))
temperature$DATETIME <- as.POSIXlt(temperature$DATETIME, tz = "America/New_York")
rh <- read.csv("http://hbrsensor.sr.unh.edu/data/hbrloggernet/shinyfiles/hbef_shiny_RH.csv", header=TRUE, col.names = c("DATETIME", "HB_WX1_RH" , "HB_WXHQ_RH", "HB_WX6_RH", "HB_WX14_RH", "HB_WX23_RH"))
rh$DATETIME <- as.POSIXlt(rh$DATETIME, tz = "America/New_York")
wind <- read.csv("http://hbrsensor.sr.unh.edu/data/hbrloggernet/shinyfiles/hbef_shiny_wind.csv" ,header=TRUE)
wind$DATETIME <- as.POSIXlt(wind$DATETIME, tz = "America/New_York")
streamflowheight <- read.csv("http://hbrsensor.sr.unh.edu/data/hbrloggernet/shinyfiles/HBR_streamflow_height_all.csv", header=TRUE)
streamflowheight$TIMESTAMP <- as.POSIXlt(streamflowheight$TIMESTAMP, tz = "America/New_York")
streamflowdischarge<- read.csv("http://hbrsensor.sr.unh.edu/data/hbrloggernet/shinyfiles/HBR_streamflow_discharge_all.csv", header=TRUE)
streamflowdischarge$TIMESTAMP <- as.POSIXct(streamflowdischarge$TIMESTAMP, tz = "America/New_York")
solar <- read.csv("http://hbrsensor.sr.unh.edu/data/hbrloggernet/shinyfiles/hbef_shiny_solar.csv", header=TRUE)
solar$DATETIME <- as.POSIXct(solar$DATETIME, tz = "America/New_York")
snowDepth <- read.csv("http://hbrsensor.sr.unh.edu/data/hbrloggernet/shinyfiles/hbef_shiny_snowdepth.csv")
snowDepth$DATETIME <- as.POSIXlt(snowDepth$DATETIME, tz = "America/New_York")
snowWater <- read.csv("http://hbrsensor.sr.unh.edu/data/hbrloggernet/shinyfiles/hbef_shiny_snowwater.csv")
snowWater$DATETIME <- as.POSIXlt(snowWater$DATETIME, tz = "America/New_York")
soilTemp<- read.csv("http://hbrsensor.sr.unh.edu/data/hbrloggernet/shinyfiles/hbef_shiny_soilT.csv")
soilTemp$DATETIME <- as.POSIXlt(soilTemp$DATETIME, tz = "America/New_York")
phenocam<- read.csv("http://hbrsensor.sr.unh.edu/data/hbrloggernet/shinyfiles/hbef_shiny_phenocam.csv", header=TRUE )
phenocam$date <- as.POSIXlt(phenocam$date, tz = "America/New_York" )

# Define the overall UI
ui<-fluidPage(  
  tags$head(includeHTML("google-analytics.html")),
  title = "Hubbard Brook Ecosystem Study",
  #Button that returns user to Homepage of Hubbard Brook Website upon clicking.
  actionButton("return", "Return to Homepage" , align = "left", height = 200, width = 300, onclick ="location.href='http://hubbardbrook.org';"),
  
  #Title panel contains Hubbard Brook logo on top of page.
  titlePanel( img(src='logo.png', align = "center", height = 75, width = 450)),
  
  #Graph options sidebar. ###NOTE: "hr()" defines a Horizontal Rule. This creates a faint line between defined sections. We can discuss making the line stronger or deleting it entirely. ###
  
  sidebarLayout
  (  
    sidebarPanel
    (
      width = 4,
      h4("STEP 1: Select Graphs to Display"),
      fluidRow(
        #column(1, bsButton("temprhtip", label = "", icon = icon("question"), style = "info", size = "extra-small")),
        column(2, checkboxInput("th", "Temperature/Relative Humidity", TRUE) )
        
      ),
      
      fluidRow(
        #column(1, bsButton("preciptip", label = "", icon = icon("question"),  style = "info", size = "extra-small")),
        column(2, checkboxInput("p", "Precipitation", TRUE))
        
      ),
      fluidRow(
        #column(1, bsButton("windtip", label = "", icon = icon("question"), style = "info", size = "extra-small")),
        column(2, checkboxInput("w", "Wind Speed", TRUE) )
        
      ),
      fluidRow(
        #column(1, bsButton("strfltip", label = "", icon = icon("question"), style = "info", size = "extra-small")),
        column(2, checkboxInput("sf", "Stream Flow", TRUE)) ),
      
      
      fluidRow(
        #column(1, bsButton("solradtip", label = "", icon = icon("question"), style = "info", size = "extra-small")),
        column(2,  checkboxInput("sr", "Solar Radiation", FALSE)) 
        
      ),
      fluidRow(
        #column(1, bsButton("snowdtip", label = "", icon = icon("question"), style = "info", size = "extra-small")),
        column(2, checkboxInput("sd", "Snow Depth", FALSE))
      ),
      
      fluidRow(
        #column(1, bsButton("soiltemptip", label = "", icon = icon("question"), style = "info", size = "extra-small")),
        column(2, checkboxInput("st", "Soil Temperature", FALSE)) 
      ),
      fluidRow(
        #column(1, bsButton("soiltemptip", label = "", icon = icon("question"), style = "info", size = "extra-small")),
        column(2, checkboxInput("phe", "Phenology Greenness Index", FALSE)) 
      ),
      
      
      #bsButton("preciptip", label = "", icon = icon("question"),  style = "info", size = "extra-small"),
      #checkboxInput("p", "Precipitation", TRUE),
      
      
      h4("STEP 2: Configure Graph Displays"),
      
      #radioButtons("units", "Select Measurement System: ",
       
                   #           c("English" = "en",
      #            "Metric" = "me")),
      
      
      #bsPopover(id = "temprhtip", title = "Temperature and Relative Humidity",content = paste0("Temp/RH info goes here"), placement = "right", trigger = "focus", options = list(container = "body")),
      #bsPopover(id = "preciptip", title = "Precipitation",content = paste0("Precipitation info goes here"), placement = "right", trigger = "focus", options = list(container = "body")),
      #bsPopover(id = "windtip", title = "Wind Speed",content = paste0("Wind Speed info goes here"), placement = "right", trigger = "focus", options = list(container = "body")),
      #bsPopover(id = "strfltip", title = "Streamflow",content = paste0("Streamflow info goes here"), placement = "right", trigger = "focus", options = list(container = "body")),
      #bsPopover(id = "solradtip", title = "Solar Radiation",content = paste0("Solar Radiation info goes here"), placement = "right", trigger = "focus", options = list(container = "body")),
      #bsPopover(id = "snowdtip", title = "Snow Depth",content = paste0("Snow Depth info goes here"), placement = "right", trigger = "focus", options = list(container = "body")),
      
      #Select a Site for Temp/RH
      selectizeInput("temprhControls", "Select A Site (Temperature/Relative Humidity):", choices=colnames(tempRHcols)[2:6], selected = cbind ("HB_WX1", "HB_WX23"),  multiple = TRUE, options = NULL),
      checkboxInput("r", "Show Relative Humidity", FALSE),
      hr(),
      ##################################################################################################################################
      
      #Graph controls for second graph (Precipitation)
      ######################################################################################################################################
      selectizeInput("precipControls", "Select A Site (Precipitation) :", choices=colnames(precipitationHourly)[2:3], selected = cbind ("HB_WX1", "HB_WX23"),  multiple = TRUE,
                     options = NULL),
      
      hr(),
      #Graph controls for third graph (Streamflow)
      #####################################################################################################################################
      radioButtons("streamflowUnits", "Streamflow units:",
                   c("Stage Height(ft)" = "ht",
                     "Discharge(mm/hr)" = "ds")),
      
      checkboxInput("ev", "Show Event-Based Precipitation Data", TRUE),
      hr(),
      
      selectizeInput("strflowControls", "Select A Site (Streamflow):", choices=colnames(streamflowheight)[2:10], selected = cbind ("WS1"),  multiple = TRUE,
                     options = NULL),
      hr(),
      
      
      #########################################################################################################################################
      hr(),
      
      #Graph controls for Wind Measure
      #########################################################################################################################################
      selectizeInput("windControls", "Select A Site (Wind Speed):", choices=colnames(wind)[2:5], selected = "HQ_avg", "HQ_max", multiple = TRUE,
                     options = NULL),
      hr(),
      #Graph controls for Solar Radiation
      #####################################################################################################################################
      
      selectizeInput("solarControls", "Select A Site (Solar Radiation):", choices=colnames(solar)[2:6], selected = cbind ("HB_HQ", "HB_WX1"),  multiple = TRUE,
                     options = NULL),
      hr(),
      #########################################################################################################################################
      #Graph controls for Snow Depth
      #####################################################################################################################################
      radioButtons("snowUnits", "Snow depth units:",
                   c("Depth(cm)" = "snod",
                     "Water Content(cm)" = "snowc")),
      #checkboxInput("swe", "Show Water Content", FALSE),
      hr(),
      
      selectizeInput("snowControls", "Select A Site (Snow Depth):", choices=colnames(snowDepth)[2:4], selected = cbind ("SC19", "SCHQ"),  multiple = TRUE,
                     options = NULL),
      hr(),
      ##########################################################################################################################################
      #Graph controls for Soil Temperature
      ##########################################################################################################################################
      selectizeInput("soilControls", "Select A Site (Soil Temperature):", choices=colnames(soilTemp)[2:3], selected = cbind ("SoilT_10cm"),  multiple = TRUE,
                     options = NULL),
      hr(),
      ##########################################################################################################################################
      #Graph controls for Phenocam Greenness
      ##########################################################################################################################################
      #selectizeInput("phenoControls", "Select A Site (Phenocam Greenness):", choices=list(paste(colnames(phenocam)[9], "_HB", sep = ""), paste(colnames(phenoSoft)[9], "_othssw", sep = "") ,paste(colnames(phenoHard)[9], "_othshw", sep = ""), selected = cbind ("midday_gcc_HB")),  multiple = TRUE, options = NULL),
      #selectizeInput("phenoControls", "Select A Site (Phenology Greenness Index):", choices=colnames(phenocam)[9], selected = cbind ("midday_gcc"),  multiple = TRUE, options = NULL),
      #hr(),
      #########################################################################################################################################
      #General graph controls.
      #############################################################################################################################################
      #Toggle gridlines on or off
      checkboxInput("showgrid", label = "Show Grid", value = TRUE),
      hr(),
      #Select a date range
      dateRangeInput('dateRange',
                     label = 'Date range input: yyyy-mm-dd',
                     start = Sys.Date()%m+% months(-1), end = Sys.Date()
      ),
      helpText("Drag scroll bar below graph to view data from a specific date range")
      #Select graphs you want to display; some graphs are selected by default.
    ),
    mainPanel
    (
      conditionalPanel(
        condition = "input.th == 1",
        fluidRow(column(12,dygraphOutput("dygraphTemperatureRH", width = "100%", height = "300px"))),
        fluidRow(column(10,offset=2,tags$div(id="trh")))
      ),
      conditionalPanel(
        condition = "input.p == 1",
        fluidRow(column(12,dygraphOutput("dygraphPrecipitation", width = "100%", height = "300px"))),
        fluidRow(column(10,offset=2,tags$div(id="pre")))
      ),
      conditionalPanel(
        condition = "input.sf == 1",
        fluidRow(column(12,dygraphOutput("dygraphStreamflow", width = "100%", height = "300px"))),
        fluidRow(column(10,offset=2,tags$div(id="stf")))
      ),
      conditionalPanel(
        condition = "input.w == 1",
        fluidRow(column(12,dygraphOutput("dygraphWindSpeed" , width = "100%", height = "300px"))),
        fluidRow(column(10,offset=2,tags$div(id="wi")))
      ),
      conditionalPanel(
        condition = "input.sr == 1",       
        fluidRow(column(12,dygraphOutput("dygraphSolarRadiation", width = "100%", height = "300px"))),
        fluidRow(column(10,offset=2,tags$div(id="solr")))
      ),
      conditionalPanel(
        condition = "input.sd == 1",   
        fluidRow(column(12,dygraphOutput("dygraphSnowDepth", width = "100%", height = "300px"))),
        fluidRow(column(10,offset=2,tags$div(id="snd")))
      ),
      conditionalPanel(
        condition = "input.st == 1",   
        fluidRow(column(12,dygraphOutput("dygraphSoilTemp", width = "100%", height = "300px"))),
        fluidRow(column(10,offset=2,tags$div(id="soit")))
      ),
      conditionalPanel(
        condition = "input.phe == 1",   
        fluidRow(column(12,dygraphOutput("dygraphPhenocam", width = "100%", height = "300px"))),
        fluidRow(column(10,offset=2,tags$div(id="pcam")))
      )
    )
  )
)







# Define a server for the Shiny app
server <- function(input, output, session) {
  #Temperature/RH dygraph. Currently appears by default. RH can be toggled on and off.
  output$dygraphTemperatureRH<-renderDygraph({
    
    selectedTemperature = temperature[c(paste(input$temprhControls, '_TEMP', sep = ""))]
    selectedRH = rh[c(paste(input$temprhControls, '_RH', sep = ""))] 
    if (input$r == TRUE)
    {
      t <- cbind(selectedTemperature,selectedRH)
      y=xts(t,order.by = temperature$DATETIME)
      dygraph(y, main=("Temperature and Relative Humidity"), group = "hubbard") %>%
        dyOptions(colors = RColorBrewer::brewer.pal(6, "Set2"))%>%
        dyOptions(drawGrid = input$showgrid) %>%
        dyAxis("y",  valueRange = c(-40, 150), label = "Temp(F), RH(%)") %>%
        dyLegend(labelsDiv = 'trh') %>%
        dyRangeSelector(height = 40,dateWindow = input$dateRange)   
    }
    else
    {
      t<- selectedTemperature
      y=xts(t,order.by = temperature$DATETIME)
      dygraph(y, main=paste("Temperature"), group = "hubbard") %>%
        dyOptions(colors = RColorBrewer::brewer.pal(6, "Set2"))%>%
        
        dyOptions(drawGrid = input$showgrid) %>%
        dyAxis("y", label = "Temp(F)") %>%
        dyLegend(labelsDiv = 'trh') %>%
        dyRangeSelector(height = 40,dateWindow = input$dateRange)
      #dySeries()
    } 
    
    
  })
  #Precipitation dygraph. Currently appears by default.
  output$dygraphPrecipitation<-renderDygraph({
    selectedPrecipHourly = precipitationHourly[c(input$precipControls)]
    t<- selectedPrecipHourly
    y=xts(t,order.by = precipitationHourly$DATETIME)
    dygraph(y, main=paste("Precipitation"), group = "hubbard") %>%
      dyOptions(colors = RColorBrewer::brewer.pal(6, "Set2"))%>%
      dyOptions(drawGrid = input$showgrid) %>%
      dyAxis("y", label = "Amount(in)") %>%
      dyLegend(labelsDiv='pre') %>%
      dyRangeSelector(height = 40,dateWindow = input$dateRange)
    
  })
  
  #Streamflow dygraph. Currently appears by default.
  output$dygraphStreamflow<-renderDygraph({
    
    selectedSfHeight = streamflowheight[c(input$strflowControls)] 
    selectedSfDisch = streamflowdischarge[c(input$strflowControls)]
    
    
    if(input$ev == FALSE && input$streamflowUnits == "ht")
    {
      t<- selectedSfHeight  
      y<-xts(t, order.by = streamflowheight$TIMESTAMP)
      dygraph(y, main=paste("Streamflow(ft)"), group = "hubbard") %>%
        dyOptions(colors = RColorBrewer::brewer.pal(6, "Set2"))%>%  
        #dyHighlight(highlightSeriesOpts = list(strokeWidth = 1)) %>%
        dyOptions(drawGrid = input$showgrid) %>%
        dyAxis("y",  label = "Stage Height(ft)") %>%
        dyLegend(labelsDiv = 'stf') %>%
        dyRangeSelector(height = 40,dateWindow = input$dateRange) 
    }
    else if(input$ev == TRUE && input$streamflowUnits == "ds" )
    {
      selectedPrecipEvent = precipitationEvent[2]
      t<- cbind(selectedSfDisch, selectedPrecipEvent)  
      y<-xts(t, order.by = streamflowdischarge$TIMESTAMP)
      dygraph(y, main=paste("Streamflow(mm/hr)"), group = "hubbard") %>%
        dyOptions(colors = RColorBrewer::brewer.pal(6, "Set2"))%>%  
        #dyHighlight(highlightSeriesOpts = list(strokeWidth = 1)) %>%
        dyOptions(drawGrid = input$showgrid) %>%
        dyAxis("y", label = "Discharge(mm/hr)") %>%
        dyLegend(labelsDiv = 'stf') %>%
        dyRangeSelector(height = 40,dateWindow = input$dateRange) 
    }
    else if( input$ev == TRUE && input$streamflowUnits == "ht" )
    {
      
      selectedPrecipEvent = precipitationEvent[2]
      t<- cbind(selectedSfHeight, selectedPrecipEvent)  
      y<-xts(t, order.by = streamflowheight$TIMESTAMP)
      dygraph(y, main=paste("Streamflow(ft)"), group = "hubbard") %>%
        dyOptions(colors = RColorBrewer::brewer.pal(6, "Set2"))%>%  
        #dyHighlight(highlightSeriesOpts = list(strokeWidth = 1)) %>%
        dyOptions(drawGrid = input$showgrid) %>%
        dyAxis("y",  label = "Stage Height(ft)") %>%
        dyLegend(labelsDiv = 'stf') %>%
        dyRangeSelector(height = 40,dateWindow = input$dateRange) 
    }
    else if( input$ev == FALSE && input$streamflowUnits == "ds" )
    {
      t<- selectedSfDisch  
      y<-xts(t, order.by = streamflowdischarge$TIMESTAMP)
      dygraph(y, main=paste("Streamflow(mm/hr)"), group = "hubbard") %>%
        dyOptions(colors = RColorBrewer::brewer.pal(6, "Set2"))%>%  
        #dyHighlight(highlightSeriesOpts = list(strokeWidth = 1)) %>%
        dyOptions(drawGrid = input$showgrid) %>%
        dyAxis("y", label = "Discharge(mm/hr)") %>%
        dyLegend(labelsDiv = 'stf') %>%
        dyRangeSelector(height = 40,dateWindow = input$dateRange) 
    }
    
  })
  #Wind speed dygraph. Currently appears by default.
  
  output$dygraphWindSpeed<-renderDygraph({
    selectedWind = wind[c(input$windControls)]
    if(input$w == TRUE){
      k = xts(selectedWind, order.by = wind$DATETIME)
      dygraph(k, main=paste("Wind Speed"), group = "hubbard") %>%
        dyOptions(colors = RColorBrewer::brewer.pal(6, "Set2"))%>%
        dyOptions(drawGrid = input$showgrid) %>%
        dyAxis("y", label = "Speed (mph)") %>%
        dyLegend(labelsDiv = 'wi') %>%
        dyRangeSelector(height = 40,dateWindow = input$dateRange) }
    
  })
  
  #Solar radiation dygraph. DOES NOT APPEAR by default.
  
  output$dygraphSolarRadiation<-renderDygraph({
    selectedSolar = solar[c(input$solarControls)]
    if(input$sr == TRUE){
      k = xts(selectedSolar, order.by = solar$DATETIME)
      dygraph(k, main=paste("Solar Radiation"), group = "hubbard") %>%
        dyOptions(colors = RColorBrewer::brewer.pal(6, "Set2"))%>%
        dyOptions(drawGrid = input$showgrid) %>%
        dyAxis("y", label = "Intensity(W/m<sup>2</sup>)") %>%
        dyLegend(labelsDiv = 'solr') %>%
        dyRangeSelector(height = 40,dateWindow = input$dateRange) }
    
  })
  
  #Snow depth dygraph. DOES NOT APPEAR by default.
  
  output$dygraphSnowDepth<-renderDygraph({
    selectedSnowDepth = snowDepth[c(input$snowControls)]
    selectedSnowWater = snowWater[c(input$snowControls)]
    if(input$snowUnits == "snod"){
      t<- selectedSnowDepth
      y<-xts(t, order.by = snowDepth$DATETIME )
      dygraph(y, main=paste("Snow"), group = "hubbard") %>%
        dyOptions(colors = RColorBrewer::brewer.pal(6, "Set2"))%>%  
        #dyHighlight(highlightSeriesOpts = list(strokeWidth = 1)) %>%
        dyOptions(drawGrid = input$showgrid) %>%
        
        dyAxis("y", label = "Depth(cm)") %>%
        dyLegend(labelsDiv = 'snd') %>%
        dyRangeSelector(height = 40,dateWindow = input$dateRange) 
      
    }
    else
    {
      t<- selectedSnowWater
      y<-xts(t, order.by = snowWater$DATETIME )
      dygraph(y, main=paste("Snow"), group = "hubbard") %>%
        dyOptions(colors = RColorBrewer::brewer.pal(6, "Set2"))%>%  
        #dyHighlight(highlightSeriesOpts = list(strokeWidth = 1)) %>%
        dyOptions(drawGrid = input$showgrid) %>%
        dyAxis("y", label = "Water Content(cm)") %>%
        dyLegend(labelsDiv = 'solr') %>%
        dyRangeSelector(height = 40,dateWindow = input$dateRange) 
      
    }
  })
  
  output$dygraphSoilTemp<-renderDygraph({
    selectedSoil= soilTemp[c(input$soilControls)]
    k = xts(selectedSoil, order.by = soilTemp$DATETIME)
    dygraph(k, main=paste("Soil Temperature"), group = "hubbard") %>%
      dyOptions(colors = RColorBrewer::brewer.pal(6, "Set2"))%>%
      dyOptions(drawGrid = input$showgrid) %>%
      dyAxis("y", label = "Temperature(F)") %>%
      dyLegend(labelsDiv = 'soit') %>%
      dyRangeSelector(height = 40,dateWindow = input$dateRange) 
    
  })
  output$dygraphPhenocam<-renderDygraph({
    selectedPheno= phenocam[9]
    k = xts(selectedPheno, order.by = phenocam$date)
    dygraph(k, main=paste("Phenology Greenness Index"), group = "hubbard") %>%
      dyOptions(colors = RColorBrewer::brewer.pal(6, "Set2"))%>%
      dyOptions(drawGrid = input$showgrid) %>%
      dyAxis("y", label = "Daily gcc (90th percentile) ") %>%
      dyLegend(labelsDiv = 'pcam') %>%
      dyRangeSelector(height = 40,dateWindow = input$dateRange) 
    
  })
  
}  

#Define the shiny app
shinyApp(ui, server)