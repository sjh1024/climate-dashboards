##############################################################################################################################
# Map for SWE data
# Current features:
# >Different colored points on map to indicate snow survey
#   >Legend in bottom left corner
# >Clicking one point "selects" it, highlights it red and displays site information on sidebar
#   >Sidebar contains:
#     >Site ID
#     >Site coordinates (Latitude, Longitude)
#     >Data Source
#     >Period of Record
#     >Number of Observations
#     >Average sampling frequency
# >Selected sites display Observed AND modeled SWE data
# >Modeled SWE data can display RCP 4.5 and/or RCP 8.5 data, if available along with historical data
# >Date range input for graphs is present as a slider
# >A Download button for site data (.csvs of observed data, graphs of modeled and observed data as seen on screen)
#   >Raster image of modeled data w/ date selection for that data
#   >Raster image has a legend with title to indicate units
#   >Raster image opacity can be changed with a slider
# >Filter points by data source
###################################################################################################################
# Coming Soon:
# >Elevation data in Station Information sidebar
# >Corrected sidebar info?
# >Ability to get modeled data from any point
# >Link to EDI archive
# >Filter sites by period of record and recording frequency
# @author sjh1024
#
#######################################################################################################################################
#Import the libraries required to run the application.
#"leaflet" is the package that makes maps possible in R Shiny.
#"dygraphs", "xts" and "dplyr" is for the graph displays
#"raster" is for the raster image of data on the maps
#"ncdf4" is for reading the files to create the raster images
#"leaflet.opacity" is for the opacity slider for the raster image
#"rgdal" is for generating the raster image
#"magrittr" is necessary for the NY error (see loading of NY file)
#"maptools" is for loading in shapefiles.
library(shiny)
library(leaflet)
library(dygraphs)
library(xts)
library(raster)
library(ncdf4)
library(leaflet.opacity)
library(rgdal)
library(tidyr)
library(tibble)
library(dplyr)
library(magrittr)
library(maptools)
library(stringr)
library(sf)
####################################################################LOAD SCRIPTS FROM SOURCE##################################################################################

#This command grabs the "wbm_load" function from the file "wbm_load.R" so it can be used later to make a RasterStack of netCDF files. 
source('/srv/shiny-server/dashboards/SWE/wbm_load.R')
#This command grabs the "dygraph-extra-shiny" R script so it can be used to implement graph download functionality.
source('/srv/shiny-server/dashboards/SWE/dygraph-extra-shiny.R')

##############################################################LOAD SHAPEFILES##############################################################################################
#Load shapefiles w/coordinates and site IDs.
#Leaflet will use this data to initialize a map with circles @ each location
stationsMaine <- readOGR("/srv/shiny-server/dashboards/SWE/shapefiles/ME_snow_survey_stations.shp")
stationsMaine@data["SITE_ID"] <- paste0("ME_", stationsMaine@data$SITE_ID) #Paste a little "header" to each column name so each point "knows" what file to reference for observed data

stationsNY <- readOGR("/srv/shiny-server/dashboards/SWE/shapefiles/NY_snow_survey_stations.shp")
stationsNY@data["ID"] <- paste0("NY_", stationsNY@data$ID)

stationsGHCND <-readOGR("/srv/shiny-server/dashboards/SWE/shapefiles/GHCND_stations.shp")
#NOTE: GHCND does not require a "header" to each column name because site IDs already start with the "GHCND:" identifier.

#NOTE: SCAN sites must be loaded in separately due the fact that they are in different shapefiles but will later be considered to be in the same "group", SCAN.
stationsSCANHB <- readOGR("/srv/shiny-server/dashboards/SWE/shapefiles/SCAN_HubbardBrook_stations.shp")
stationsSCANHB@data["SITE_ID"] <- paste0("SCANHB_", stationsSCANHB@data$SITE_ID)

stationsSCANMR <- readOGR("/srv/shiny-server/dashboards/SWE/shapefiles/SCAN_MascomaRiver_stations.shp")
stationsSCANMR@data["SITE_ID"] <- paste0("SCANMR_", stationsSCANMR@data$SITE_ID)

stationsSCANMM <- readOGR("/srv/shiny-server/dashboards/SWE/shapefiles/SCAN_MtMansfield_stations.shp")
stationsSCANMM@data["SITE_ID"] <- paste0("SCANMM_", stationsSCANMM@data$SITE_ID)

################################################################LOAD SITE DATA FILES##############################################################################################################
#Load site data .rds (RData) files.
#This data will be used for observed SWE data graphs at each site, when a site is selected.
observedME<-read.csv("/srv/shiny-server/dashboards/SWE/datafiles/observed/ME_snow_survey_SWE.csv", header = TRUE, check.names= FALSE)
names(observedME) <- paste0("ME_", names(observedME)) #Need to paste headers to the site IDs on the observed data files as well
observedME$ME_DATE <- as.POSIXct(observedME$ME_DATE, tz="America/New_York") #Convert the "DATE" field to a date object so dygraphs can use it to create a line plot

observedNY<-readRDS("/srv/shiny-server/dashboards/SWE/datafiles/observed/NY_SWE.rds")
names(observedNY) <- paste0("NY_", names(observedNY))
observedNY$NY_DATE <- as.POSIXct(observedNY$NY_DATE, tz="America/New_York")
#Something went weird in the NY data; some columns filled with only NA had "logical" values
#and would throw an error. This conversion fixes that error.
observedNY %<>% mutate_if(is.logical, as.numeric)

observedGHCND<-readRDS("/srv/shiny-server/dashboards/SWE/datafiles/observed/GHCND.rds")
observedGHCND$DATE <- as.POSIXct(observedGHCND$DATE)

#Again, SCAN sites are loaded in separately but are still in the same "group"
observedSCANHB<-readRDS("/srv/shiny-server/dashboards/SWE/datafiles/observed/HB_SWE.rds")
names(observedSCANHB) <- paste0("SCANHB_", names(observedSCANHB))
observedSCANHB$SCANHB_DATE <- as.POSIXct(observedSCANHB$SCANHB_DATE, tz="America/New_York")

observedSCANMR<-readRDS("/srv/shiny-server/dashboards/SWE/datafiles/observed/MR_SWE.rds")
names(observedSCANMR) <- paste0("SCANMR_", names(observedSCANMR))
observedSCANMR$SCANMR_DATE <- as.POSIXct(observedSCANMR$SCANMR_DATE, tz="America/New_York")

observedSCANMM<-readRDS("/srv/shiny-server/dashboards/SWE/datafiles/observed/MM_SWE.rds")
names(observedSCANMM) <- paste0("SCANMM_", names(observedSCANMM))
observedSCANMM$SCANMM_DATE <- as.POSIXct(observedSCANMM$SCANMM_DATE, tz="America/New_York")

#Convert all of the data.frames to tibbles for easy column extraction later on (For downloads)
MEtibble <- as_tibble(observedME)
NYtibble <- as_tibble(observedNY)
#ghcnd tibble goes here
HBtibble <- as_tibble(observedSCANHB)
MRtibble <- as_tibble(observedSCANMR)
MMtibble <- as_tibble(observedSCANMM)
#########################################################LOAD FILES CONTAINING METADATA###################################################
#This metadata will be displayed on the sidebar when a site is selected.
MEmetadata<-readRDS("/srv/shiny-server/dashboards/SWE/datafiles/observed/site_metadata/ME_sitestats.rds")
MEmetadata$site <- paste0("ME_", MEmetadata$site) #Need to paste headers to the site IDs on the observed data files as well

NYmetadata<-readRDS("/srv/shiny-server/dashboards/SWE/datafiles/observed/site_metadata/NY_sitestats.rds")
NYmetadata$site  <- paste0("NY_", NYmetadata$site) #Need to paste headers to the site IDs on the observed data files as well

GHCNDmetadata<-readRDS("/srv/shiny-server/dashboards/SWE/datafiles/observed/site_metadata/GHCND_sitestats.rds")
GHCNDmetadata$site <- paste0("GHCND:", substring(GHCNDmetadata$site, first=7))

#SCANHB 

#SCANMM

#SCANMR
####################################################################LOAD NETCDF FILES #############################################################################
#Load the netCDF file(s) for the modeled SWE data
#First, create RasterStacks (basically stacking the netCDF files) for easy, fast access to data

#This uses the wbm_load script in Danielle Grogan's WBMr repository: https://github.com/daniellegrogan/WBMr
#wbmHist<-wbm_load("/data1/modeledNcdfs/historical/daily", "snowPack", NA) #Load ALL ncdf files in that directory, variable name (the variable that contains SWE), NA because we want to read all files in the file path. This is historical data.
#wbm45 <-wbm_load("/data1/modeledNcdfs/rcp45/daily", "snowPack", NA) #RCP 4.5 data
#wbm85 <-wbm_load("/data1/modeledNcdfs/rcp85/daily", "snowPack", NA) #RCP 8.5 data
load("/data1/modeledNcdfs/wbmHist.RData")
load("/data1/modeledNcdfs/wbm45.RData")
load("/data1/modeledNcdfs/wbm85.RData")

###################################################################LOAD MODELED DATA AS CSVS FOR GRAPH DISPLAY##########################################
maineHist <-readRDS("/srv/shiny-server/dashboards/SWE/datafiles/mergedModeled/maineHist.rds")
maineHist$ME_DATE <- as.POSIXct(maineHist$ME_DATE, tz = "America/New_York" )

maine45 <-readRDS("/srv/shiny-server/dashboards/SWE/datafiles/mergedModeled/maine45.rds")
maine45$ME_DATE <- as.POSIXct(maine45$ME_DATE, tz = "America/New_York" )

maine85 <-readRDS("/srv/shiny-server/dashboards/SWE/datafiles/mergedModeled/maine85.rds")
maine85$ME_DATE <- as.POSIXct(maine85$ME_DATE, tz = "America/New_York" )

nyHist <-readRDS("/srv/shiny-server/dashboards/SWE/datafiles/mergedModeled/nyHist.rds")
nyHist$NY_DATE <- as.POSIXct(nyHist$NY_DATE, tz = "America/New_York" )

ny45 <-readRDS("/srv/shiny-server/dashboards/SWE/datafiles/mergedModeled/ny45.rds")
ny45$NY_DATE <- as.POSIXct(ny45$NY_DATE, tz = "America/New_York" )

ny85 <-readRDS("/srv/shiny-server/dashboards/SWE/datafiles/mergedModeled/ny85.rds")
ny85$NY_DATE <- as.POSIXct(ny85$NY_DATE, tz = "America/New_York" )

scanHist <-readRDS("/srv/shiny-server/dashboards/SWE/datafiles/mergedModeled/scanHist.rds")
scanHist$SCAN_DATE <- as.POSIXct(scanHist$SCAN_DATE, tz = "America/New_York" )

scan45 <-readRDS("/srv/shiny-server/dashboards/SWE/datafiles/mergedModeled/scan45.rds")
scan45$SCAN_DATE <- as.POSIXct(scan45$SCAN_DATE, tz = "America/New_York" )

scan85 <-readRDS("/srv/shiny-server/dashboards/SWE/datafiles/mergedModeled/scan85.rds")
scan85$SCAN_DATE <- as.POSIXct(scan85$SCAN_DATE, tz = "America/New_York" )

ghcndHist <-readRDS("/srv/shiny-server/dashboards/SWE/datafiles/mergedModeled/ghcndHist.rds")
ghcndHist$DATE <- as.POSIXct(ghcndHist$DATE, tz = "America/New_York" )

ghcnd45 <-readRDS("/srv/shiny-server/dashboards/SWE/datafiles/mergedModeled/ghcnd45.rds")
ghcnd45$DATE <- as.POSIXct(ghcnd45$DATE, tz = "America/New_York" )

ghcnd85 <-readRDS("/srv/shiny-server/dashboards/SWE/datafiles/mergedModeled/ghcnd85.rds")
ghcnd85$DATE <- as.POSIXct(ghcnd85$DATE, tz = "America/New_York" )


###########################################################################END FILE LOADING; BEGIN SHINY APP########################################################################################
shinyApp(
  
  #UI part of Shiny application.
  ui <- fluidPage( title="Snow Water Equivalent (SWE)", style= "padding-top: 40px; padding-left: 40px; padding-right: 40px;", #This padding is for the map display
  #The "title" value is what is displayed in a browser tab when this application is opened on the web.
 ########################### Sidebar Layout means that there will be a "main panel" and a "sidebar panel".############################################################################################
 #The main panel contains the map and site data graph(s)
 #The sidebar contains site info and options
 #######################################################################################################################################################################################################    
 #This is needed for the UI to source the script that handles dygraph downloads.
 tags$head(
   tags$script(src = "dygraph-extra.js")
 ),
 sidebarLayout(
   mainPanel(
     
     leafletOutput("mymap"), #Need to put this in UI to make the map display. The map is named "mymap" and this name is needed to manipulate it in the server function.
     
     ################################ The following are conditionalPanels (containing the graphs) which only display under certain conditions. ################################################################
     # One condition is that no data exists for the site. In this condition, no data will display.
     conditionalPanel(
       condition = "output.hasData == 1",
       tags$h4("No data available for this site.")
     ),
     # Another condition is that a site is not selected (Startup of the application). No data will display.
     conditionalPanel(
       condition = "output.hasData == 2",
       tags$h4("Select a Site")
     ),
     # Another condition is that a site has data to display. In this case, two conditional panels will display that show graphs.
     # Might need to change the "conditions" later if a site has one type of data, but not both.
     conditionalPanel(
       condition = "output.hasData == 3", 
       fluidRow(column(12,dygraphOutput("snowWaterEquivalent", width = "100%", height = "300px"))),
       fluidRow(column(10,offset=2,tags$div(id="swe")))
     ),
     conditionalPanel(
       condition = "output.hasData == 3", 
       fluidRow(column(12,dygraphOutput("snowWaterEquivalentModeled", width = "100%", height = "300px"))),
       fluidRow(column(10,offset=2,tags$div(id="swem")))
     )
   ),
   sidebarPanel(
     ###############################The sidebar contains a title, some information on the currently selected point, and some graph/application options.##########################################################
     #"htmlOutput" allows lines of text with html to be assigned in the server function.
     #They need to be assigned in the server function and not the UI because the reactiveValue "select"
     #cannot be accessed in the UI part of the application.
     #The reactive value "select" contains data on the currently selected site.
     #
     #Use the names of each htmlOutput in your server function to assign them.
     #
     #Lines of text will appear in order as they are written in the UI function.
     ############################################################################################################################################################################################################
     width = 3, #Sets the width of the sidebar panel
     h2("Snow Water Equivalent (SWE) Data Viewer"), #Application title
     #Application description below title
     tags$h5("Visualize and download measured and modeled snow water equivalent (SWE) data across northeastern North America. 
Measured SWE values come from over 3,700 individual study sites and span over 100 years.
These measured SWE values were used to validate modeled SWE generated from the University of New Hampshire Water Balance Model.
This model was then used to project future SWE through 2099 under two different climate change scenarios."),
     #Link to EDI archive: needs real URL and may be moved later
     tagList("Link to EDI archive: ", "zero.sr.unh.edu:3838/index.html"),
     hr(), #hr = horizontal rule. It makes a subtle "line" to divide sections.
     radioButtons("rasterModel", label= "Select model to display", choices = c("Lower Climate Scenario (RCP 4.5)", "Higher Climate Scenario (RCP 8.5)"), selected = "Lower Climate Scenario (RCP 4.5)"),
     dateInput("date", value = "2007-01-01", format = "yyyy.mm.dd", label = h4("Select date for modeled SWE between 1950.1.1 and 2099.12.31") ), #Date input for selecting modeled SWE raster image.
     
     #####################################################################STATION INFORMATION SECTION################################################################################################################        
     tags$h4("Station Information"), #Station info header
     htmlOutput("siteName"),
     htmlOutput("coords"),
     htmlOutput("elev"), 
     htmlOutput("dataSource"),
     htmlOutput("perOfRecord"),
     htmlOutput("numOfObs"),
     htmlOutput("sampFreq"),
     hr(),
     ############################################################################DATA DOWNLOAD SECTION###########################################################################################################################
     tags$h4("Download this site's data"),
     uiOutput("downloadCsv"), #UI output for csv download button
     tags$h5("Graph output will download as it appears on-screen:"),
     dyDownload("snowWaterEquivalent", "Observed SWE Graph (.png)", asbutton = TRUE, usetitle = TRUE), # .png download button for observed SWE
     dyDownload("snowWaterEquivalentModeled", "Modeled SWE Graph (.png)", asbutton = TRUE, usetitle = TRUE), #.png download button for modeled SWE
     ###############################################################################DATE RANGE INPUT SECTION############################################################################        
     hr(),
     #Select a date range for the graph output
     sliderInput('dateSlider', min = as.POSIXlt("1980-01-01"), max = as.POSIXlt("2099-12-31"), label = h4("Select date range"), timeFormat = "%F", value = c(as.POSIXlt("2015-01-01"), as.POSIXlt("2020-01-01"))),
     hr(),
     #################################################################################SOURCE SELECTION SECTION############################################################################################################
     #Check or uncheck these boxes to toggle display of certain snow survey points. 
     tags$h4("Select Sources to Display on Map"),
     checkboxInput("mss", label = "Maine Snow Survey", value = TRUE),
     checkboxInput("nyss", label = "New York Snow Survey", value = TRUE),
     checkboxInput("ghcndss", label = "GHCND", value = TRUE),
     checkboxInput("scnss", label = "SCAN Sites", value = TRUE),
     ######################################################################################SOURCE FILTER SECTION######################################################################################################
     hr(),
     tags$h4("Filter source points by:"),
     tags$b("Period of Record"),
     dateRangeInput('por', label = NULL, start = "2000-01-01", end = "2018-01-01"),
     tags$b("Sample Density"),
     checkboxInput("sfreqlow", label = "Low", value = FALSE),
     checkboxInput("sfreqmed", label = "Medium", value = FALSE),   
     checkboxInput("sfreqhi", label = "High", value = FALSE),   
     hr(),
     ########################################################################################SWE MODEL SELECT SECTION####################################################################################################
     #Show RCP 4.5, 8.5, both, or neither
     tags$h4("Select SWE Model:"),
     
     checkboxInput("fourfive", label = "Lower Climate Scenario (RCP 4.5)", value = TRUE),
     checkboxInput("eightfive", label = "Higher Climate Scenario(RCP 8.5)", value = FALSE),
     hr(),
     #######################################################################################OPTIONS SECTION######################################################################################################################
     checkboxInput("showgrid", label = "Show Grid", value = TRUE), #A checkbox to select whether or not you want to see gridlines on the data graphs
     checkboxInput("showRaster", label = "Show Modeled SWE", value = TRUE), #This doesn't have a function right now, but it will later be used to turn the modeled SWE output on the map on or off.
     hr(),
     #citations/acknowledgements
     tags$h6("SWE data compilation and modeling activities were supported by an NSF Macrosystems Biology award (#1802726).
The development of this web platform was supported by
a University of New Hampshire Earth Systems Research Center Iola Hubbard Endowment award.") 
   )
 )
  ),
  ################################################################################################################################################################################
  server <- function(input, output, session) {
    #The selected point must be saved as a reactiveValues so it will save every time a change is made to the application (Clicking a point, moving a graph, etc.)
    #Right now, the initial selected value is a "null-like" value until I can figure out a more elegant way to handle this, or make one point be initially selected on the map itself.
    select <-reactiveValues("site" = as.character("Select a Site"),
                            "lng" = as.numeric(NaN), "lat" = as.numeric(NaN), 
                            "short" = "INIT", "data" = "NA", 
                            "src" = "NA", 
                            "startDate" = "NA", 
                            "endDate" = "NA", 
                            "nObs" = "NA",
                            "sFreq" = "NA")
    val <- reactiveValues("tibble" = as_tibble(NULL))
    model45max <- reactiveValues("mod45max" = 0);
    model85max <- reactiveValues("mod85max" = 0);
    modelhistmax <- reactiveValues("modhistmax" = 0);
    obsrMax <- reactiveValues("obsMax" = 0);
    curGraph <-reactiveValues("cur" = "NONE")
    
    #Initialize the map with all points. 
    output$mymap <-renderLeaflet ({
      #dateText is a formatted character string from the date input object for the raster image. This is used to get the correct layer/date within the layer in the RasterStack.
      dateText = format(input$date, '%Y%m%d')
      #Get the layer from the correct raster stack for raster image display based on date. 
      if(input$rasterModel == "Lower Climate Scenario (RCP 4.5)" && as.POSIXct(input$date) > as.POSIXct("2004-12-31")){
        #4.5 AND future
        rasterSample <- raster::subset(wbm45, grep(dateText, names(wbm45), value = TRUE))
      }
      else if(input$rasterModel == "Lower Climate Scenario (RCP 4.5)" && as.POSIXct(input$date) <= as.POSIXct("2004-12-31")){
        #4.5 AND historical
	dateText = format(input$date, '%y.%m.%d')
        rasterSample <- raster::subset(wbmHist, grep(dateText, names(wbmHist), value = TRUE))
      }
      else if(input$rasterModel == "Higher Climate Scenario (RCP 8.5)" && as.POSIXct(input$date) <= as.POSIXct("2004-12-31")){
       	dateText = format(input$date, '%y.%m.%d')
	 #8.5 AND historical
        rasterSample <- raster::subset(wbmHist, grep(dateText, names(wbmHist), value = TRUE))
      }
      else if(input$rasterModel == "Higher Climate Scenario (RCP 8.5)" && as.POSIXct(input$date) > as.POSIXct("2004-12-31")){
        #8.5 AND future
        rasterSample <- raster::subset(wbm85, grep(dateText, names(wbm85), value = TRUE))
      }
      else{
       
      }
     
      #Create a color palette object for the colors of the raster image display.
      palette = colorNumeric(palette = "Spectral", domain = values(rasterSample))
      
      leaflet(padding = 5) %>% 
        addTiles() %>%  #Needed to show the "map" underneath the points
        #Add image to map from the rasterSample object.
        addRasterImage(rasterSample, colors = RColorBrewer::brewer.pal(11, "Spectral"),  opacity = 0.8, layerId = "rastImg") %>% #Adds the modeled data raster image to the map. Currently not being used.
        #Opacity slider for raster image
        addOpacitySlider(layerId = "rastImg") %>% #Adds a slider to adjust the opacity of the modeled data. Currently not in use.
        #Legend for raster image w/ units/title
        leaflet::addLegend(pal = palette, values = values(rasterSample), title = "SWE (mm)") %>% #Adds a legend for the raster image data. Currently not in use.
        #Legend for colors of map points by data source
        leaflet::addLegend("bottomleft",
                           colors = c("mediumblue", "cyan", "limegreen", "purple"),
                           labels=c("Maine Snow Survey", "New York Snow Survey", "GHCND", "SCAN"),
                           title="Legend"
                           #layerId="mapLegend"
        )%>%
        addCircles( #"Circles" are the points on the map. Initalizes them with the lats/longs in the Locations csv and the layerId as the Site in the locations csv. 
          data = stationsMaine,
          radius = 5000,
          fillColor = "mediumblue",
          fillOpacity = 0.5,
          color = "mediumblue",
          weight = 2,
          layerId = as.character(stationsMaine$SITE_ID),#layerID is just the ID of each individual point. It's used to register the current selected point and to get data for graphs.
          highlightOptions = highlightOptions(color = "mediumseagreen",
                                              opacity = 1.0,
                                              weight = 2,
                                              bringToFront = TRUE))%>%
        addCircles( #You need to do a separate "addCircles" block for every shapefile, this ensures the different points are different colors and different groups
          data = stationsNY,
          radius = 5000,
          fillColor = "cyan",
          fillOpacity = 0.5,
          color = "cyan",
          weight = 2,
          layerId = as.character(stationsNY$ID),
          highlightOptions = highlightOptions(color = "mediumseagreen",
                                              opacity = 1.0,
                                              weight = 2,
                                              bringToFront = TRUE)) %>%
        addCircles(
          data = stationsGHCND,
          radius = 5000,
          fillColor = "limegreen",
          fillOpacity = 0.5,
          color = "limegreen",
          weight = 2,
          layerId = as.character(stationsGHCND$SITE_ID),
          highlightOptions = highlightOptions(color = "mediumseagreen",
                                              opacity = 1.0,
                                              weight = 2,
                                              bringToFront = TRUE)) %>%
        addCircles(
          data = stationsSCANHB,
          radius = 5000,
          fillColor = "purple",
          fillOpacity = 0.5,
          color = "purple",
          weight = 2,
          layerId = as.character(stationsSCANHB$SITE_ID),
          highlightOptions = highlightOptions(color = "purple",
                                              opacity = 1.0,
                                              weight = 2,
                                              bringToFront = TRUE)) %>%
        addCircles(
          data = stationsSCANMR,
          radius = 5000,
          fillColor = "purple",
          fillOpacity = 0.5,
          color = "purple",
          weight = 2,
          layerId = as.character(stationsSCANMR$SITE_ID),
          highlightOptions = highlightOptions(color = "mediumseagreen",
                                              opacity = 1.0,
                                              weight = 2,
                                              bringToFront = TRUE)) %>%
        addCircles(
          data = stationsSCANMM,
          radius = 5000,
          fillColor = "purple",
          fillOpacity = 0.5,
          color ="purple",
          weight = 2,
          layerId = as.character(stationsSCANMM$SITE_ID),
          highlightOptions = highlightOptions(color = "mediumseagreen",
                                              opacity = 1.0,
                                              weight = 2,
                                              bringToFront = TRUE))
      
      
      
    })
    
    #Render the text output for the selected site's data.
    output$siteName <- renderText({
      paste("<b>Site ID:</b>", as.character(select$site))
    })
    output$coords <- renderText({
      paste("<b>Coordinates: </b>", "(", as.character(select$lat), ", ", as.character(select$lng), ")", sep = "")
    })
    #Currently, Elevation and Data Source are not displayed because they will be shapefile metadata. Coming soon!
    output$elev <- renderText({
      paste("<b>Elevation: </b>", "NA", sep = "")
    })
    
    output$dataSource <- renderText({
      paste("<b>Data Source: </b>", select$src, sep = "")
    })
    output$perOfRecord <- renderText({
      paste("<b>Period of Record: </b>", select$startDate, " to ", select$endDate, sep = "")
    })
    
    output$numOfObs <- renderText({
      paste("<b>Number of Observations: </b>", select$nObs, sep = "")
    })
    
    output$sampFreq <- renderText({
      paste("<b>Average sampling frequency: </b>", "1 sample/", as.integer(select$sFreq), " days", sep = "")
    })
    
    
    ###########################################################################################################
    #GRAPH OUTPUTS
    ###########################################################################################################
    
    #SWE Graph: Observed Data 
    ###PLEASE CHECK THE FIRST GRAPH OF MODELED AND OBSERVED DATA FOR DETAILED COMMENTS###
    output$snowWaterEquivalent = renderDygraph({
      if(select$short != "NA" && select$short != "INIT") #Double check that there is plottable data available, otherwise do not display a graph
      {
        #These "if" statements all check the "header" for the site ID. It uses this value to select which .rds to extract observed data from.
        if(startsWith(select$site, "ME_")){
          #Get observed data by name of site from the rds
          selectObserved <- observedME[c(select$site)]
          #Calculate the max of the observed site; need this to make sure both Y axes are the same height.
          obsrMax$obsMax <- c(max(selectObserved, na.rm = TRUE))
          #Create a new vector that only contains the observed site data that you have selected
          t<- selectObserved
          #Change the colname of the vector to "OBSERVED_SWE" instead of the site name.
          colnames(t) <- "OBSERVED SWE"
          #Create a time series with "t" as the y axis data and the date column of the selected site as the x.
          y=xts(t, observedME$ME_DATE)
          dygraph(y, main=paste("Snow Water Equivalent (Observed) "), group = "hubbard") %>% #graph title
            dyOptions(colors = RColorBrewer::brewer.pal(6, "Set2"))%>%  #line/point colors
            dyOptions(drawGrid = input$showgrid) %>% #Draws a grid on the graph if "Show Grid" is checked
            dyOptions(drawPoints = TRUE, pointSize = 3) %>% #Since the data are sometimes sparse, placing a "point" at individual data samples makes the output easier to read.
            dyAxis("y", label = "SWE(mm)", valueRange = c(0,max(c(obsrMax$obsMax, model45max$mod45max, model85max$mod85max, modelhistmax$modhistmax)))) %>% #Label the y axis and set its min to 0 and the max to the max of the modeled and observed data of the selected point so the axes match
            dyLegend(labelsDiv='swe') %>% #This is what causes the value of the point that the user is hovering over to appear underneath the graph.
            dyRangeSelector(height = 40,dateWindow = input$dateSlider) %>%  #This creates the date range selector under the graph. The default date window is the dateRange selection from the sidebar, but this can be adjusted. 
            dyCallbacks(drawCallback = dyRegister()) #Need this for graph downloads
        }
        
        else if(startsWith(select$site, "NY_")){
          selectObserved <- observedNY[c(select$site)]
          obsrMax$obsMax <- c(max(selectObserved, na.rm = TRUE))
          
          t<- selectObserved
          colnames(t) <- "OBSERVED SWE"
          y=xts(t, observedNY$NY_DATE)
          dygraph(y, main=paste("Snow Water Equivalent (Observed) "), group = "hubbard") %>%
            dyOptions(colors = RColorBrewer::brewer.pal(6, "Set2"))%>%
            dyOptions(drawGrid = input$showgrid) %>%
            dyOptions(drawPoints = TRUE, pointSize = 3) %>%
            dyAxis("y", label = "SWE(mm)", valueRange = c(0,max(c(obsrMax$obsMax, model45max$mod45max, model85max$mod85max, modelhistmax$modhistmax)))) %>%
            dyLegend(labelsDiv='swe') %>%
            dyRangeSelector(height = 40,dateWindow = input$dateSlider) %>%
            dyCallbacks(drawCallback = dyRegister())
        }
        #NOTE: selectObserved <- observedSCANExample[2] is used here because the SCAN data is kept in separate data.frames with only 2 columns: date(col1) and the data(col2)
        else if(startsWith(select$site, "SCANHB_")){
          selectObserved <- observedSCANHB[2]
          obsrMax$obsMax <- c(max(selectObserved, na.rm = TRUE))
          
          t<- selectObserved
          colnames(t) <- "OBSERVED SWE"
          y=xts(t, observedSCANHB$SCANHB_DATE)
          dygraph(y, main=paste("Snow Water Equivalent (Observed) "), group = "hubbard") %>%
            dyOptions(colors = RColorBrewer::brewer.pal(6, "Set2"))%>%
            dyOptions(drawGrid = input$showgrid) %>%
            
            dyAxis("y", label = "SWE(mm)", valueRange = c(0,max(c(obsrMax$obsMax, model45max$mod45max, model85max$mod85max, modelhistmax$modhistmax)))) %>%
            dyLegend(labelsDiv='swe') %>%
            dyRangeSelector(height = 40,dateWindow = input$dateSlider) %>%
            dyCallbacks(drawCallback = dyRegister())
        }
        else if(startsWith(select$site, "SCANMR_")){
          selectObserved <- observedSCANMR[2]
          obsrMax$obsMax <- c(max(selectObserved, na.rm = TRUE))
          
          t<- selectObserved
          colnames(t) <- "OBSERVED SWE"
          y=xts(t, observedSCANMR$SCANMR_DATE)
          dygraph(y, main=paste("Snow Water Equivalent (Observed) "), group = "hubbard") %>%
            dyOptions(colors = RColorBrewer::brewer.pal(6, "Set2"))%>%
            dyOptions(drawGrid = input$showgrid) %>%
            
            dyAxis("y", label = "SWE(mm)", valueRange = c(0,max(c(obsrMax$obsMax, model45max$mod45max, model85max$mod85max, modelhistmax$modhistmax)))) %>%
            dyLegend(labelsDiv='swe') %>%
            dyRangeSelector(height = 40,dateWindow = input$dateSlider) %>%
            dyCallbacks(drawCallback = dyRegister())
        }
        else if(startsWith(select$site, "SCANMM_")){
          selectObserved <- observedSCANMM[2]
          obsrMax$obsMax <- c(max(selectObserved, na.rm = TRUE))
          
          t<- selectObserved
          colnames(t) <- "OBSERVED SWE"
          y=xts(t, observedSCANMM$SCANMM_DATE)
          dygraph(y, main=paste("Snow Water Equivalent (Observed) "), group = "hubbard") %>%
            dyOptions(colors = RColorBrewer::brewer.pal(6, "Set2"))%>%
            dyOptions(drawGrid = input$showgrid) %>%
            
            dyAxis("y", label = "SWE(mm)", valueRange = c(0,max(c(obsrMax$obsMax, model45max$mod45max, model85max$mod85max, modelhistmax$modhistmax)))) %>%
            dyLegend(labelsDiv='swe') %>%
            dyRangeSelector(height = 40,dateWindow = input$dateSlider) %>%
            dyCallbacks(drawCallback = dyRegister())
        }
        else if(startsWith(select$site, "GHCND")){
          val = select$site
          #Need to do this so the GHCND data is read correctly based on the format of the dataframe
          val = str_replace(val, ":", ".")
          selectObserved <- observedGHCND[c(val)]
          obsrMax$obsMax <- c(max(selectObserved, na.rm = TRUE))
          
          t<- selectObserved
          colnames(t) <- "OBSERVED SWE"
          y=xts(t, observedGHCND$DATE)
          dygraph(y, main=paste("Snow Water Equivalent (Observed) "), group = "hubbard") %>%
            dyOptions(colors = RColorBrewer::brewer.pal(6, "Set2"))%>%
            dyOptions(drawGrid = input$showgrid) %>%
            dyOptions(drawPoints = TRUE, pointSize = 3) %>%
            dyAxis("y", label = "SWE(mm)", valueRange = c(0,max(c(obsrMax$obsMax, model45max$mod45max, model85max$mod85max, modelhistmax$modhistmax)))) %>%
            dyLegend(labelsDiv='swe') %>%
            dyRangeSelector(height = 40,dateWindow = input$dateSlider) %>%
            dyCallbacks(drawCallback = dyRegister())
        }
      }
    })
    #SWE Graph: Modeled Data 
    output$snowWaterEquivalentModeled = renderDygraph({
      if(select$short == "ME_"){
        #load in both datasets, make sure to paste the "trailing" suffix so that the data can be read from the data.frame based on its format
        selectModeled = maineHist[c(paste0(select$site, "_HIST"))] 
        selectModeled45 = maine45[c(paste0(select$site, "_4.5"))] 
        selectModeled85 = maine85[c(paste0(select$site, "_8.5"))] 
        #get max values from the data frame to calculate max y for the y axes on both graphs
        model45max$mod45max <- c(max(selectModeled45, na.rm = TRUE))
        model85max$mod85max <- c(max(selectModeled85, na.rm = TRUE))
        modelhistmax$modhistmax <- c(max(selectModeled, na.rm = TRUE))
        #Change colnames in graph display legend
        colnames(selectModeled45) <- "MODELED_RCP_4.5_FUTURE"
        colnames(selectModeled85) <- "MODELED_RCP_8.5_FUTURE"
        colnames(selectModeled) <- "MODELED_HISTORICAL"
        #These "if" statements are checking which checkboxes are selected for modeled output (4.5, 8.5, both, or neither are your options)
        if(input$fourfive == TRUE && input$eightfive == TRUE){
          
          t <- cbind(selectModeled, selectModeled45, selectModeled85)
          
          y=xts(t, order.by = maine45$ME_DATE)
          dygraph(y, main=paste("Snow Water Equivalent (Modeled, RCP 4.5 and RCP 8.5)"), group = "hubbard") %>%
            dyOptions(colors = RColorBrewer::brewer.pal(6, "Set2"))%>%
            dyOptions(drawGrid = input$showgrid) %>%
            dyAxis("y", label = "SWE(mm)", valueRange = c(0,max(c(obsrMax$obsMax, model45max$mod45max, model85max$mod85max, modelhistmax$modhistmax)))) %>%
            dyLegend(labelsDiv='swem') %>%
            dyRangeSelector(height = 40,dateWindow = input$dateSlider)%>%
            dyCallbacks(drawCallback = dyRegister())
          
        }
        else if(input$fourfive == TRUE && input$eightfive == FALSE){
          t <- cbind(selectModeled, selectModeled45)
          y<-xts(t, order.by = maine45$ME_DATE)
          dygraph(y, main=paste("Snow Water Equivalent (Modeled, RCP 4.5)"), group = "hubbard") %>%
            dyOptions(colors = RColorBrewer::brewer.pal(6, "Set2"))%>%
            dyOptions(drawGrid = input$showgrid) %>%
            dyAxis("y", label = "SWE(mm)", valueRange = c(0,max(c(obsrMax$obsMax, model45max$mod45max, model85max$mod85max, modelhistmax$modhistmax)))) %>%
            dyLegend(labelsDiv='swem') %>%
            dyRangeSelector(height = 40,dateWindow = input$dateSlider) %>%
            dyCallbacks(drawCallback = dyRegister())
          
        }
        else if(input$fourfive == FALSE && input$eightfive == TRUE){
          t <- cbind(selectModeled, selectModeled85)
          y<-xts(t, order.by = maine85$ME_DATE)
          dygraph(y, main=paste("Snow Water Equivalent (Modeled, RCP 8.5)"), group = "hubbard") %>%
            dyOptions(colors = RColorBrewer::brewer.pal(6, "Set2"))%>%
            dyOptions(drawGrid = input$showgrid) %>%
            dyAxis("y", label = "SWE(mm)", valueRange = c(0,max(c(obsrMax$obsMax, model45max$mod45max, model85max$mod85max, modelhistmax$modhistmax)))) %>%
            dyLegend(labelsDiv='swem') %>%
            dyRangeSelector(height = 40,dateWindow = input$dateSlider) %>%
            dyCallbacks(drawCallback = dyRegister())
          
        }
        else if(input$fourfive == FALSE && input$eightfive == FALSE){
          
          t <- cbind(selectModeled)
          
          y=xts(t, order.by = maineHist$ME_DATE)
          dygraph(y, main=paste("Snow Water Equivalent (Modeled, Historical Only)"), group = "hubbard") %>%
            dyOptions(colors = RColorBrewer::brewer.pal(6, "Set2"))%>%
            dyOptions(drawGrid = input$showgrid) %>%
            dyAxis("y", label = "SWE(mm)", valueRange = c(0,max(c(obsrMax$obsMax, model45max$mod45max, model85max$mod85max, modelhistmax$modhistmax)))) %>%
            dyLegend(labelsDiv='swem') %>%
            dyRangeSelector(height = 40,dateWindow = input$dateSlider)%>%
            dyCallbacks(drawCallback = dyRegister())
        }
      }
      else if(select$short == "NY_"){
        selectModeled = nyHist[c(paste0(select$site, "_HIST"))] 
        selectModeled45 = ny45[c(paste0(select$site, "_4.5"))] 
        selectModeled85 = ny85[c(paste0(select$site, "_8.5"))] 
        
        colnames(selectModeled45) <- "MODELED_RCP_4.5_FUTURE"
        colnames(selectModeled85) <- "MODELED_RCP_8.5_FUTURE"
        colnames(selectModeled) <- "MODELED_HISTORICAL"
        
        model45max$mod45max <- c(max(selectModeled45, na.rm = TRUE))
      
        model85max$mod85max <- c(max(selectModeled85, na.rm = TRUE))
        modelhistmax$modhistmax <- c(max(selectModeled, na.rm = TRUE))
        
        #These "if" statements are checking which checkboxes are selected for modeled output (4.5, 8.5, both, or neither are your options)
        if(input$fourfive == TRUE && input$eightfive == TRUE){
          t <- cbind(selectModeled, selectModeled45, selectModeled85)
          y=xts(t, order.by = ny45$NY_DATE)
          dygraph(y, main=paste("Snow Water Equivalent (Modeled, RCP 4.5 and RCP 8.5)"), group = "hubbard") %>%
            dyOptions(colors = RColorBrewer::brewer.pal(6, "Set2"))%>%
            dyOptions(drawGrid = input$showgrid) %>%
            dyAxis("y", label = "SWE(mm)", valueRange = c(0,max(c(obsrMax$obsMax, model45max$mod45max, model85max$mod85max, modelhistmax$modhistmax, model85max$mod85max, modelhistmax$modhistmax)))) %>%
            dyLegend(labelsDiv='swem') %>%
            dyRangeSelector(height = 40,dateWindow = input$dateSlider)%>%
            dyCallbacks(drawCallback = dyRegister())
          
        }
        else if(input$fourfive == TRUE && input$eightfive == FALSE){
          t <- cbind(selectModeled, selectModeled45)
          y<-xts(t, order.by = ny45$NY_DATE)
          
          dygraph(y, main=paste("Snow Water Equivalent (Modeled, RCP 4.5)"), group = "hubbard") %>%
            dyOptions(colors = RColorBrewer::brewer.pal(6, "Set2"))%>%
            dyOptions(drawGrid = input$showgrid) %>%
            dyAxis("y", label = "SWE(mm)", valueRange = c(0,max(c(obsrMax$obsMax, model45max$mod45max, model85max$mod85max, modelhistmax$modhistmax)))) %>%
            dyLegend(labelsDiv='swem') %>%
            dyRangeSelector(height = 40,dateWindow = input$dateSlider) %>%
            dyCallbacks(drawCallback = dyRegister())
          
        }
        else if(input$fourfive == FALSE && input$eightfive == TRUE){
          t <- cbind(selectModeled, selectModeled85)
          y<-xts(t, order.by = ny85$NY_DATE)
          dygraph(y, main=paste("Snow Water Equivalent (Modeled, RCP 8.5)"), group = "hubbard") %>%
            dyOptions(colors = RColorBrewer::brewer.pal(6, "Set2"))%>%
            dyOptions(drawGrid = input$showgrid) %>%
            dyAxis("y", label = "SWE(mm)", valueRange = c(0,max(c(obsrMax$obsMax, model45max$mod45max, model85max$mod85max, modelhistmax$modhistmax)))) %>%
            dyLegend(labelsDiv='swem') %>%
            dyRangeSelector(height = 40,dateWindow = input$dateSlider) %>%
            dyCallbacks(drawCallback = dyRegister())
          
        }
        else if(input$fourfive == FALSE && input$eightfive == FALSE){
          t <- cbind(selectModeled)
          
          y=xts(t, order.by = nyHist$NY_DATE)
          dygraph(y, main=paste("Snow Water Equivalent (Modeled, Historical Only)"), group = "hubbard") %>%
            dyOptions(colors = RColorBrewer::brewer.pal(6, "Set2"))%>%
            dyOptions(drawGrid = input$showgrid) %>%
            dyAxis("y", label = "SWE(mm)", valueRange = c(0,max(c(obsrMax$obsMax, model45max$mod45max, model85max$mod85max, modelhistmax$modhistmax)))) %>%
            dyLegend(labelsDiv='swem') %>%
            dyRangeSelector(height = 40,dateWindow = input$dateSlider)%>%
            dyCallbacks(drawCallback = dyRegister())
        }
      }
      else if(startsWith(select$short, "SCAN")){
        trimmed = str_replace(select$site, "(SCAN..)", "SCAN")
        selectModeled = scanHist[c(paste0(trimmed, "_HIST"))] 
        selectModeled45 = scan45[c(paste0(trimmed, "_4.5"))] 
        selectModeled85 = scan85[c(paste0(trimmed, "_8.5"))] 
        
        colnames(selectModeled45) <- "MODELED_RCP_4.5_FUTURE"
        colnames(selectModeled85) <- "MODELED_RCP_8.5_FUTURE"
        colnames(selectModeled) <- "MODELED_HISTORICAL"
        
        model45max$mod45max <- c(max(selectModeled45, na.rm = TRUE))
      
        model85max$mod85max <- c(max(selectModeled85, na.rm = TRUE))
        modelhistmax$modhistmax <- c(max(selectModeled, na.rm = TRUE))
        #These "if" statements are checking which checkboxes are selected for modeled output (4.5, 8.5, both, or neither are your options)
        if(input$fourfive == TRUE && input$eightfive == TRUE){
          t <- cbind(selectModeled, selectModeled45, selectModeled85)
          y=xts(t, order.by = scan45$SCAN_DATE)
          dygraph(y, main=paste("Snow Water Equivalent (Modeled, RCP 4.5 and RCP 8.5)"), group = "hubbard") %>%
            dyOptions(colors = RColorBrewer::brewer.pal(6, "Set2"))%>%
            dyOptions(drawGrid = input$showgrid) %>%
            dyAxis("y", label = "SWE(mm)", valueRange = c(0,max(c(obsrMax$obsMax, model45max$mod45max, model85max$mod85max, modelhistmax$modhistmax)))) %>%
            dyLegend(labelsDiv='swem') %>%
            dyRangeSelector(height = 40,dateWindow = input$dateSlider)%>%
            dyCallbacks(drawCallback = dyRegister())
          
        }
        else if(input$fourfive == TRUE && input$eightfive == FALSE){
          t <- cbind(selectModeled, selectModeled45)
          y<-xts(t, order.by = scan45$SCAN_DATE)
          dygraph(y, main=paste("Snow Water Equivalent (Modeled, RCP 4.5)"), group = "hubbard") %>%
            dyOptions(colors = RColorBrewer::brewer.pal(6, "Set2"))%>%
            dyOptions(drawGrid = input$showgrid) %>%
            dyAxis("y", label = "SWE(mm)", valueRange = c(0,max(c(obsrMax$obsMax, model45max$mod45max, model85max$mod85max, modelhistmax$modhistmax)))) %>%
            dyLegend(labelsDiv='swem') %>%
            dyRangeSelector(height = 40,dateWindow = input$dateSlider) %>%
            dyCallbacks(drawCallback = dyRegister())
          
        }
        else if(input$fourfive == FALSE && input$eightfive == TRUE){
          t <- cbind(selectModeled, selectModeled85)
          y<-xts(t, order.by = scan85$SCAN_DATE)
          dygraph(y, main=paste("Snow Water Equivalent (Modeled, RCP 8.5)"), group = "hubbard") %>%
            dyOptions(colors = RColorBrewer::brewer.pal(6, "Set2"))%>%
            dyOptions(drawGrid = input$showgrid) %>%
            dyAxis("y", label = "SWE(mm)", valueRange = c(0,max(c(obsrMax$obsMax, model45max$mod45max, model85max$mod85max, modelhistmax$modhistmax)))) %>%
            dyLegend(labelsDiv='swem') %>%
            dyRangeSelector(height = 40,dateWindow = input$dateSlider) %>%
            dyCallbacks(drawCallback = dyRegister())
          
        }
        else if(input$fourfive == FALSE && input$eightfive == FALSE){
          t <- cbind(selectModeled)
          
          y=xts(t, order.by = scanHist$SCAN_DATE)
          dygraph(y, main=paste("Snow Water Equivalent (Modeled, Historical Only)"), group = "hubbard") %>%
            dyOptions(colors = RColorBrewer::brewer.pal(6, "Set2"))%>%
            dyOptions(drawGrid = input$showgrid) %>%
            dyAxis("y", label = "SWE(mm)", valueRange = c(0,max(c(obsrMax$obsMax, model45max$mod45max, model85max$mod85max, modelhistmax$modhistmax)))) %>%
            dyLegend(labelsDiv='swem') %>%
            dyRangeSelector(height = 40,dateWindow = input$dateSlider)%>%
            dyCallbacks(drawCallback = dyRegister())
        }
      }
      else if(startsWith(select$short, "GHCND")){
        val = select$site
        selectModeled = ghcndHist[c(paste0(val, "_HIST"))] 
        selectModeled45 = ghcnd45[c(paste0(val, "_4.5"))] 
        selectModeled85 = ghcnd85[c(paste0(val, "_8.5"))] 
        
        colnames(selectModeled45) <- "MODELED_RCP_4.5_FUTURE"
        colnames(selectModeled85) <- "MODELED_RCP_8.5_FUTURE"
        colnames(selectModeled) <- "MODELED_HISTORICAL"
        
        model45max$mod45max <- c(max(selectModeled45, na.rm = TRUE))
      
        model85max$mod85max <- c(max(selectModeled85, na.rm = TRUE))
        modelhistmax$modhistmax <- c(max(selectModeled, na.rm = TRUE))
        
        #These "if" statements are checking which checkboxes are selected for modeled output (4.5, 8.5, both, or neither are your options)
        if(input$fourfive == TRUE && input$eightfive == TRUE){
          t <- cbind(selectModeled, selectModeled45, selectModeled85)
          y=xts(t, order.by = ghcnd45$DATE)
          dygraph(y, main=paste("Snow Water Equivalent (Modeled, RCP 4.5 and RCP 8.5)"), group = "hubbard") %>%
            dyOptions(colors = RColorBrewer::brewer.pal(6, "Set2"))%>%
            dyOptions(drawGrid = input$showgrid) %>%
            dyAxis("y", label = "SWE(mm)", valueRange = c(0,max(c(obsrMax$obsMax, model45max$mod45max, model85max$mod85max, modelhistmax$modhistmax)))) %>%
            dyLegend(labelsDiv='swem') %>%
            dyRangeSelector(height = 40,dateWindow = input$dateSlider)%>%
            dyCallbacks(drawCallback = dyRegister())
          
        }
        else if(input$fourfive == TRUE && input$eightfive == FALSE){
          t <- cbind(selectModeled, selectModeled45)
          y<-xts(t, order.by = ghcnd45$DATE)
          dygraph(y, main=paste("Snow Water Equivalent (Modeled, RCP 4.5)"), group = "hubbard") %>%
            dyOptions(colors = RColorBrewer::brewer.pal(6, "Set2"))%>%
            dyOptions(drawGrid = input$showgrid) %>%
            dyAxis("y", label = "SWE(mm)", valueRange = c(0,max(c(obsrMax$obsMax, model45max$mod45max, model85max$mod85max, modelhistmax$modhistmax)))) %>%
            dyLegend(labelsDiv='swem') %>%
            dyRangeSelector(height = 40,dateWindow = input$dateSlider) %>%
            dyCallbacks(drawCallback = dyRegister())
          
        }
        else if(input$fourfive == FALSE && input$eightfive == TRUE){
          t <- cbind(selectModeled, selectModeled85)
          y<-xts(t, order.by = ghcnd85$DATE)
          dygraph(y, main=paste("Snow Water Equivalent (Modeled, RCP 8.5)"), group = "hubbard") %>%
            dyOptions(colors = RColorBrewer::brewer.pal(6, "Set2"))%>%
            dyOptions(drawGrid = input$showgrid) %>%
            dyAxis("y", label = "SWE(mm)", valueRange = c(0,max(c(obsrMax$obsMax, model45max$mod45max, model85max$mod85max, modelhistmax$modhistmax)))) %>%
            dyLegend(labelsDiv='swem') %>%
            dyRangeSelector(height = 40,dateWindow = input$dateSlider) %>%
            dyCallbacks(drawCallback = dyRegister())
          
        }
        else if(input$fourfive == FALSE && input$eightfive == FALSE){
          t <- cbind(selectModeled)
          
          y=xts(t, order.by = ghcndHist$DATE)
          dygraph(y, main=paste("Snow Water Equivalent (Modeled, Historical Only)"), group = "hubbard") %>%
            dyOptions(colors = RColorBrewer::brewer.pal(6, "Set2"))%>%
            dyOptions(drawGrid = input$showgrid) %>%
            dyAxis("y", label = "SWE(mm)", valueRange = c(0,max(c(obsrMax$obsMax, model45max$mod45max, model85max$mod85max, modelhistmax$modhistmax)))) %>%
            dyLegend(labelsDiv='swem') %>%
            dyRangeSelector(height = 40,dateWindow = input$dateSlider)%>%
            dyCallbacks(drawCallback = dyRegister())
        }
      }
      
    })
    
    #Download link for csvs.
    output$downloadCsv <-renderUI({
      req(!is.null(val$tibble)) #Do not allow download of "nothing", or download button to download anything if no site has been selected
      downloadButton('downloadCsvHandler', paste0("Observed SWE data (.csv)" )) #Download Link that the user can click to download observed data 
    })
    
    output$downloadCsvHandler <- downloadHandler(
      filename = function() {
        paste('observedSWE-', select$site, '.csv', sep='')
      },
      content = function(con) {
        write.csv(val$tibble, row.names=FALSE, con)
      }
    )
    #NOTE: DOWNLOAD LINKS FOR THE GRAPHS ARE IN THE "dyDownload" OBJECTS!
    #ONLY THE DOWNLOAD LOGIC FOR CSVS IS HERE.
    #The reasoning is that downloadCsv is a different function than the "dygraph-extra-shiny" function needed
    #to capture and download pngs of the graphs
    
    
    
    
    #Observes map for changes in snow survey output options. 
    #This will turn circles on or off depending on what snow surveys are selected/deselected.
    observe({
      proxy <- leafletProxy("mymap") 
      proxy %>% clearShapes() 
      #Now, re-draw every deselected circle on the map.
      if(input$mss == TRUE){ 
        
        proxy <- leafletProxy("mymap") 
        proxy %>% addCircles(
          data = stationsMaine,
          radius = 5000,
          fillColor = "mediumblue",
          fillOpacity = 0.5,
          color = "mediumblue",
          weight = 2,
          layerId = as.character(stationsMaine$SITE_ID),
          highlightOptions = highlightOptions(color = "mediumseagreen",
                                              opacity = 1.0,
                                              weight = 2,
                                              bringToFront = TRUE))  
      }
      if(input$nyss == TRUE){
        proxy <- leafletProxy("mymap") 
        proxy %>% addCircles(
          data = stationsNY,
          radius = 5000,
          fillColor = "cyan",
          fillOpacity = 0.5,
          color = "cyan",
          weight = 2,
          layerId = as.character(stationsNY$ID),
          highlightOptions = highlightOptions(color = "mediumseagreen",
                                              opacity = 1.0,
                                              weight = 2,
                                              bringToFront = TRUE))
      }
      if(input$ghcndss == TRUE){
        proxy <- leafletProxy("mymap") 
        proxy %>% addCircles(
          data = stationsGHCND,
          radius = 5000,
          fillColor = "limegreen",
          fillOpacity = 0.5,
          color = "limegreen",
          weight = 2,
          layerId = as.character(stationsGHCND$SITE_ID),
          highlightOptions = highlightOptions(color = "mediumseagreen",
                                              opacity = 1.0,
                                              weight = 2,
                                              bringToFront = TRUE))
      }
      if( input$scnss == TRUE){
        proxy <- leafletProxy("mymap") 
        proxy %>% addCircles(
          data = stationsSCANHB,
          radius = 5000,
          fillColor = "purple",
          fillOpacity = 0.5,
          color = "purple",
          weight = 2,
          layerId = as.character(stationsSCANHB$SITE_ID),
          highlightOptions = highlightOptions(color = "purple",
                                              opacity = 1.0,
                                              weight = 2,
                                              bringToFront = TRUE)) 
        proxy <- leafletProxy("mymap") 
        proxy %>% addCircles(
          data = stationsSCANMR,
          radius = 5000,
          fillColor = "purple",
          fillOpacity = 0.5,
          color = "purple",
          weight = 2,
          layerId = as.character(stationsSCANMR$SITE_ID),
          highlightOptions = highlightOptions(color = "mediumseagreen",
                                              opacity = 1.0,
                                              weight = 2,
                                              bringToFront = TRUE)) 
        proxy <- leafletProxy("mymap") 
        proxy %>% addCircles(
          data = stationsSCANMM,
          radius = 5000,
          fillColor = "purple",
          fillOpacity = 0.5,
          color ="purple",
          weight = 2,
          layerId = as.character(stationsSCANMM$SITE_ID),
          highlightOptions = highlightOptions(color = "mediumseagreen",
                                              opacity = 1.0,
                                              weight = 2,
                                              bringToFront = TRUE)) 
      }
      
    })
    #Handles events where a circle-shaped point is clicked on a map. Changes colors of selected/deselected points.  
    observeEvent(input$mymap_shape_click, {
      #If a selected point exists, delete it
      proxy <- leafletProxy("mymap")
      proxy %>% removeMarker(layerId = "SELECTED")
      #Add a new selected point
      proxy <- leafletProxy("mymap") 
      proxy %>% addCircles(data = paste(as.character(input$mymap_shape_click$lat), as.character(input$mymap_shape_click$lng)),
                           radius = 5000,
                           lat = input$mymap_shape_click$lat,
                           lng = input$mymap_shape_click$lng,
                           fillColor = "red",
                           fillOpacity = 1,
                           color = "red",
                           weight = 2,
                           layerId = as.character("SELECTED"),
                           highlightOptions = highlightOptions(color = "mediumseagreen",
                                                               opacity = 1,
                                                               weight = 10,
                                                               bringToFront = TRUE))
      #Change "select"'s values so that it contains the data of the selected point. 
      #These values are used so the .rds can find the data corresponding to the correct location, and to update the sidebar's station information.
      select$site = input$mymap_shape_click$id
      select$lat = input$mymap_shape_click$lat
      select$lng =  input$mymap_shape_click$lng
      
      
      #"short" stores a short prefix to determine what file the data is coming from. This is important in the download function.
      if(startsWith(select$site, "ME_")){
        select$short = "ME_"
        select$src = "Maine Snow Survey"
        val$tibble = MEtibble %>% dplyr::select(ME_DATE, select$site)
        select$startDate = MEmetadata[MEmetadata$site == select$site, 3]
        select$endDate = MEmetadata[MEmetadata$site == select$site, 4]
        select$nObs =  MEmetadata[MEmetadata$site == select$site, 2]
        select$sFreq =  MEmetadata[MEmetadata$site == select$site, 7]
        
      }
      else if(startsWith(select$site, "NY_")){
        select$short = "NY_"
        select$src = "New York Snow Survey"
        val$tibble = NYtibble %>% dplyr::select(NY_DATE, select$site)
        select$startDate = NYmetadata[NYmetadata$site == select$site, 3]
        select$endDate = NYmetadata[NYmetadata$site == select$site, 4]
        select$nObs =  NYmetadata[NYmetadata$site == select$site, 2]
        select$sFreq =  NYmetadata[NYmetadata$site == select$site, 7]
        
      }
      else if(startsWith(select$site, "GHCND")){
        select$short =  "GHCND:"
        select$src = "GHCND"
        #val$tibble = GHCNDtibble %>% dplyr::select(DATE, select$site)
        select$startDate = GHCNDmetadata[GHCNDmetadata$site == select$site, 3]
        select$endDate = GHCNDmetadata[GHCNDmetadata$site == select$site, 4]
        select$nObs =  GHCNDmetadata[GHCNDmetadata$site == select$site, 2]
        select$sFreq =  GHCNDmetadata[GHCNDmetadata$site == select$site, 7]
      }
      else if(startsWith(select$site, "SCANHB_")){
        select$src = "SCAN"
        select$short = "SCANHB_"
        val$tibble = HBtibble %>% dplyr::select(SCANHB_DATE, SCANHB_SWE)
      }
      else if(startsWith(select$site, "SCANMR_")){
        select$src = "SCAN"
        select$short = "SCANMR_"
        val$tibble = MRtibble %>% dplyr::select(SCANMR_DATE, SCANMR_SWE)
      }
      else if(startsWith(select$site, "SCANMM_")){
        select$src = "SCAN"
        select$short = "SCANMM_"
        val$tibble = MMtibble %>% dplyr::select(SCANMM_DATE, SCANMM_SWE)
      }
      
      
    })
    
    #This function checks to see if the point has data, based on "select"s value. The conditionalPanel will take the result from this reactive function and determine whether or not to display graphs.
    output$hasData <- reactive({
      if(select$short == "NA" ){ return(1) } #no data for selected site case
      else if( select$short == "INIT"){return(2)} #no site selected case
      else{ return(3) } #site is selected AND has data
    })
    outputOptions(output, "hasData", suspendWhenHidden = FALSE) #output is based on the value of hasData, initialized above.
  }
  
  
)


