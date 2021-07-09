# JRA Widget 2.0
# Git https://github.com/ChesapeakeCommons/JRAWidget_v2.0.git
# Readme https://docs.google.com/document/d/1dHaQ7w8Ttfirp27Yuaoji84n7KgoQs-Qo83T9WIb8-Q/edit
# Created 05.19.2021
## Program Structure 
## UI Side 
## Server Side 
##   Imports and Variable Declerations 
##   API Request Functions 
##   Helper Functions 
##   Map 
##   Modal 
##   Rendered Components 
##       Parameter Select
##       Station Image 
##       Station Station Status 
##       Station Text
##       Gage Chart 
##       Trends Chart 

library(shiny)
library(shinyjs)
library(plotly)
library(leaflet)
library(RCurl)
library(tidyverse)
library(XML)
library(ggplot2)
library(reactlog)
library(rgdal)
library(shinybusy)
library(shinycssloaders)
library(ggimage)
library(rlist)
library(httr)
library(jsonlite)
library(plotrix)
library(googlesheets4)


#### Declaring for using ShinyJS
js <- "

$(window).resize(function() {
  
  /*
  let w = $('#Map').outerWidth();
  $('#Map').css('width : '+ $('window').width);
  $('#header').css('width : '+ $('window').width);
*/
  console.log('resize');

});
$( document ).ready(function() {
  if($('#Map')){
    console.log('map')
  }
  
  $('#Map').css('height : 300px');
  $('#Map').height('calc(100vh - 75px)');
});
"
shinyjs::useShinyjs()

####### UI #######
ui <- fluidPage(theme = "styler.css",
              
                
                ### Header
                div(id = "header",
                    div(id = "header-title",
                    ),
                    uiOutput("RenderFlag"),
                    HTML("<a href='https://www.jamesriverwatch.org/' target='blank' style='margin-top: -8px;'>"),
                    div(id = "header-logo" ),
                    HTML("</a>")
                    
                ),     
                
                ### Map 
                leafletOutput("Map", height = 'calc(100vh - 75px)', width = '100%'),
                

                ### Temperature Select
                div( class = "temp-overlay-container",
                     div (class = "temp-overlay",
                          div(class = "temp-title", HTML("Temperature Unit")),
                          div( style='margin: 0 auto; border: 0px solid green;',
                               radioButtons("TempUnit", "Unit:", c("F°" = "F", "C°" = "C"), selected = "F")
                          )
                     )
                ),
                
                
                ### Key overlay 
                div( class= "key-overlay-container",
                     div(class = "key-overlay",
                     )
                )
)



### SERVER SECTION ###
server <- function(input, output,session) {
    

    
###### ###### ###### ###### ###### ####
###### IMPORTS AND VAR DECLRATIONS ####
###### ###### ###### ###### ###### ####
  
#Removes requirement for Gsheet Authentification 
gs4_deauth()

#### WATER REPORTER VAR DECLERATION ### 
#Parameter Sets
Parameters <- c("E Coli Concentration","Enterococcus Bacteria Concentration","Air Temperature", "Water Temperature", "Turbidity") 
EnteroParameters <- c("Enterococcus Bacteria Concentration", "Air Temperature", "Water Temperature", "Turbidity")
ColiParameters <- c("E Coli Concentration", "Air Temperature", "Water Temperature", "Turbidity")
  
# Station names that should should show Enteroccocus instead of E coli by default 
EnteroStations <- c("J05","P05","C01","CC01","CC02","VDH-AP","VDH-HTP","VDH-KL", "VDH-HB")
  
# List of max values used for WR data 
MaxValue <- c(2419,2419,45,50,250) 
  
##Bind of Parameters and Max Values 
MaxParamValue <- data.frame(Parameters,MaxValue)
  
#Parameter Codes for API Requests
APICode <- c(2586,2587,2583,2584,2589)
  
#Bind of the parameter list and the codes
APIParameterCodes <- data.frame(Parameters,APICode)
  
# API Token 
Token <- "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9.eyJ1IjoiZXJlaWxseUB0aGVqYW1lc3JpdmVyLm9yZyJ9.FILPCCWQquQm-qP1itvgKEi5gPduTNR5S4KNaBaL_yOkCs6oefy15C8fiUngkUThL5YRWMCl46UlwTVftQ-3PjJXqzBzpzST-2gGd0KB0jeF5xGTo_dEP5raCGiVi8MXmhu_sErwd8PxdxAUzrtOQxKdr9RTu0IA1ZA2LTDUOfJZAxQq3no8se5XHXdvfa3n_lYixPG043f8t2e9PKljN6_8OgSEHTZbr49brHkgFzVHRvAwQ32AuIV4FWH9MBwZzV7mmc5VOp9oF9ll7r0uwT0-kvVfHF-Kw7okzDYPcKHD5k1tICTYOA4rApTdQALn5OFFpX8F1cfU0xJgvIpU2w"

### Thresholds for WR Stations using NOAA Data ### 
#WRNOAAThresholds <- read_csv("www/WRNOAAThresholds_v1.csv")

WRNOAAThresholds <- read_sheet("https://docs.google.com/spreadsheets/d/1lxxNuAPJzJJSqXGWGENSlkrAwHb3rhRBkhBMdJICsfg/edit#gid=35911052")

### NOAA VAR DECLERATION + FILE IMPORT ### 
### Hardcoded list of NOAA stations and their lat longs. 
NOAAStationsList <- read_sheet("https://docs.google.com/spreadsheets/d/119EBdkkskx6Hc9_TXTbl2JwE4t_vTWo0qfkH7EUtyF4/edit#gid=207141266")
    
### !! For turning on and off NOAA Data !! ###
NOAAData <- read_csv("www/NOAAData_v1.csv")

### Values for controlling NOAA Min Max chart settings ###
NOAAStationsMaxMin <- read_csv("www/NOAAStationsMaxMin_v1.csv")

### NOAA Stations Threshold ### 
NOAAThresholds <- read_csv("www/NOAAThresholds_v2.csv")

#NOAA square iconset for use in Map 
#Note that WR stations are circles - a default leaflet shape.
IconSet <- iconList(
  Red = makeIcon(iconUrl = "www/images/NOAAred.png", iconWidth = 15, iconHeight = 15),
  Yellow = makeIcon(iconUrl = "www/images/NOAAyellow.png", iconWidth = 15, iconHeight = 15),
  Green = makeIcon(iconUrl = "www/images/NOAAgreen.png", iconWidth = 15, iconHeight = 15),
  Grey = makeIcon(iconUrl = "www/images/NOAAgrey.png", iconWidth = 15, iconHeight = 15))

### LOGICAL COMPONENETS DECLERATION ### 
##StationDataReactive$df is the selected data based on map marker click, and the parameter select 
StationDataReactive <- reactiveValues(df = data.frame())

## Stores the curent list of Parameter Choices 
ParamListReactive <- reactiveValues(X = as.vector(NULL))

## Used to prevent uneeded re renders 
RenderFlag <- reactiveValues(X = as.character("FALSE"))



#### UI VAR DECLERATION ###
#List of Icon Colors 
IconColors <- c("Green", "Grey", "Red", "Yellow") 

#List of Color #'s 
ColorHex <- c("#008000","#999999","#da222b","#ffea2e")
#Bind of Colors and Color Hexs
ColorSet <- data.frame(IconColors,ColorHex)

#List of aspect ratios for threshold text 
Ratio <- c(2.3449,4.7375,4.797,3.31)

#Bind of Icon Colors and Ratio
IconRatio <- data.frame(IconColors,Ratio)

### Huc 8 GeoJson ### 
Hucs <- rgdal::readOGR("www/HUC8s_v2.geojson", verbose = FALSE)

### River GeoJson ###
River <- suppressWarnings(rgdal::readOGR("www/JamesRiverArea_v2.geojson", verbose = FALSE))

### Image used when no station image is available 
DefaultImage <- "https://www.savethesound.org/wp-content/uploads/2021/05/orient-point-state-park_SM_HeyNardo_FINAL.jpg"

###### ###### ###### ###### ###### ####
### END IMPORTS AND VAR DECLARTIONS ###
###### ###### ###### ###### ###### ####



###### ###### ######  ###
###### API FUNCTIONS ####
###### ###### ######  ###

## GetWRStations
## Returns a data frame containing station_id, station_name, station_API_id, station_key, Latitude, Longitude and Type
## Used in GetAllStations
GetWRStations <- function()
{   
    ## Gets the Water Reporter Station List for Swimming Conditions 2021, id 1227.
    Request <- GET(paste("https://api.waterreporter.org/stations?sets=1227&geo_format=xy&access_token=",Token, sep = ""))
    
    ##Converting to dataframe 
    jsonRequestText <- content(Request,as="text")
    parsed <- fromJSON(jsonRequestText)
    Data <- data.frame(parsed$features)
    
    ## Selecting only the columns we need
    StationData <- data.frame(Data$raw_id,Data$name,Data$id,Data$lat,Data$lng,Data$description,Data$image_url) 
    
    #Cleaning Var Names 
    colnames(StationData) <- gsub('Data.','',colnames(StationData))
    
    StationData <- StationData %>%
        dplyr::rename("station_id"= raw_id,
                      "station_name" = name,
                      "station_API_id"= id,
                      "Latitude" = lat,
                      "Longitude" = lng,
                      "Image" = image_url)%>%
                       mutate(Type = 0)
    
    ### We need to get the colors for each station now which is a seperate per station and parameter request ###
    
    ### Wrapping this in a withProgress on startup because it takes a bit for each request
    withProgress(message = 'Fetching Water Reporter Data: ', value = nrow(StationData), {
      
    for (row in 1:nrow(StationData))
    {   
        # The Parameter ID is either for Enterococcus or Ecoli, depending on the station name hardcoded here.
        Parameter_ID <- ifelse(StationData$station_id[row] %in% EnteroStations,2587,2586)
        
        # Making the request 
        URL <- paste("https://api.waterreporter.org/readings?station_id=",StationData$station_API_id[row],"&parameter_id=",Parameter_ID,"&limit=5&access_token=",Token, sep = "")
        
        #Getting it
        Request <- GET(URL)
        
        #Parsing
        jsonRequestText <- content(Request,as="text")
        parsed <- fromJSON(jsonRequestText)
        
        #If the color is null, set to grey
        StationData$ColorHex[row] <- as.character(ifelse(!is.null(parsed$data$color),parsed$data$color,"#999999"))
        
        #Post progress
        incProgress(1/row, detail = HTML(paste(StationData$station_name[row], " | ", row, " of ", nrow(StationData), sep = "")))
    }
 })
   
return(StationData)
}

## GetWRData
## Takes StationID or ParameterName
## Returns Date, Value, ColorHex, ThresholdValue
## Used in  observeEvent(input$Map_marker_click
GetWRData <- function(StationID,ParameterName)
{
    ## Parsing Parameter Name 
    ParameterCode <- APIParameterCodes %>%
        filter(Parameters == ParameterName)%>%
        pull(APICode)
    
    ## Parsing the Station Name 
    #Getting the station_API_id for calling the WRData
    station_API_id <- Stations %>%
        filter(station_id == StationID)%>%
        pull(station_API_id)
    
    ## Assembling Request
    URL <- paste("https://api.waterreporter.org/readings?parameter_id=",ParameterCode,"&station_id=",station_API_id,"&access_token=",Token, sep = "")
    
    ## Request
    Request <- GET(URL)

    ## Parsing
    jsonRequestText <- content(Request,as="text")
    parsed <- fromJSON(jsonRequestText)

    ## Assembling data
    if(!is.null(parsed$dataset))
    {
    ## Main Components from API Response
    Data <- data.frame(parsed$data$collection_date,parsed$data$value,parsed$data$color)%>%
                rename("Date" = parsed.data.collection_date,
                       "Value" = parsed.data.value,
                       "ColorHex" = parsed.data.color)%>%
                mutate(ColorHex = as.character(ColorHex))%>%
                ## Sometimes WR sends a #616161 or a null color, convert to grey here
                mutate(ColorHex = as.character(ifelse(ColorHex == "#616161" | is.null(ColorHex), "#999999",ColorHex)))%>%
                mutate(Date = as.POSIXct(Date, origin = "1970-01-01", tz = "UTC"))
    
    ### Calculating ThresholdValue 
    ### Used in the Gauge Plot and Trends plot.
    # Need the current reading
    CurrentReading <- Data %>%
                      slice_head()%>%
                      pull(Value)

    ### Getting the threshold logic from WR
    UpperBound <- parsed$parameter$chart_schema$ranges$upper_bound
    
    #turning into a list
    ThresholdValueList <- UpperBound[!is.na(UpperBound)]

    
    #selecting only ones which are above the current reading
    ThresholdValueListTrimmed <- list.filter(ThresholdValueList, . >= CurrentReading)
    
    #Geting the min absulute value threshold line
    ThresholdValue <- as.numeric(ThresholdValueListTrimmed[which.min(abs(CurrentReading-ThresholdValueListTrimmed))])

    # Checking to see if value is over threshold, setting to max threshold value
    if(purrr::is_empty(ThresholdValue))
    {
    ThresholdValue <- max(ThresholdValueList)
    }
    # Entering into the Data frame
    Data$ThresholdValue <- ThresholdValue
    
    # Lastly adding the station id
    Data$station_id <- StationID
    
    }
    
    ##### If the request for data comes back empty, we handle it here ###
    else
    {
    Data <- data.frame(matrix(ncol = 5, nrow = 1))
    headers <- c("station_id", "Date", "Value","ColorHex","ThresholdValue")
    colnames(Data) <- headers
    Data$station_id <- StationID
    Data$ColorHex <- "#999999"
    Data$Date <- Sys.time()
    }
    return(Data)
}

## NOAA Data Request
## Takes NOAA StationID 
## Returns Date, Stage, Flow 
## Used in NOAADataPull
NOAADataRequest <- function(StationID)
{ 
    #Getting Sys.Date Year to append to data 
    Year <- format(Sys.Date(), "%Y")
    
    #URL
    url <- paste("https://water.weather.gov/ahps2/hydrograph_to_xml.php?gage=", StationID ,"&output=tabular", sep = "")
    
    #Getting URL
    urlCurled <- getURL(url)
    
    #Reading HTML
    dfRawHtml <- readHTMLTable(urlCurled, stringAsFactors = FALSE)
    
    #Getting Correct List
    dfRaw <- data.frame(dfRawHtml[1])
    
    #Cleaning Data
    dfCleaned <- dfRaw %>%
        rename("Date" = NULL.V1,
               "Stage" = NULL.V2,
               "Flow" =  NULL.V3)%>%
        ##Filtering out unneeded rows leftover from parsing
        filter(!grepl('Forecast',Date))%>%
        filter(!grepl('Observed',Date))%>%
        filter(!grepl('Date',Date))%>%
        mutate(Date = as.POSIXct(paste(Year,"/",as.character(Date), sep = ""), tz="UTC"))%>%
        mutate(Stage = as.numeric(str_remove(Stage, "ft")))%>%
        #Detecting the unit and converting cfs to kcfs 
        mutate(Flow = ifelse(str_detect(Flow, "kcfs"), as.numeric(str_remove(Flow, "kcfs")), as.numeric(str_remove(Flow, "cfs"))/1000))%>%
        #Handling no data value
        mutate(Flow = ifelse(Flow < -900, NA, Flow))%>%
        mutate(Stage = ifelse(Stage < -900, NA, Stage))%>%
        mutate(station_id = StationID)
    
    return(dfCleaned)
}
## END NOAA DATA PARSER ##


### NOAADataPull
# Constructs the NOAA Data Frame from the NOAAStationsList using the NOAADataRequest function
# !!!! Uncomment line below to run loading, comment out to skip loading for testing ####
NOAADataPull <- function()
{
    ## Assembling blank dataframe 
    NOAAData <- data.frame(station_id = character(),
                           Date = as.character(),
                           Flow = double(),
                           Stage = double())
    
       #We have a with progress here because it takes awhile 
       withProgress(message = 'Fetching Predictive NOAA Data: ', value = nrow(NOAAStationsList), {
        
         ## Loops 
        for (row in 1:nrow(NOAAStationsList))
        {
                incProgress(1/row, detail = HTML(paste(NOAAStationsList$station_name[row], " | ", row, " of ", 14, sep = "")))
                 
                # Runs the request, and binds it to the data 
                NOAAData <- rbind(NOAAData,NOAADataRequest(NOAAStationsList$station_id[row]))
        }
        return(NOAAData)
    })
}
NOAAData <- NOAADataPull()
#write.csv(NOAAData,"www/NOAAData_v1.csv")


### Get NOAA Data 
### Takes StationID, ParameterName
### Returns Date, Value, ColorHex, ThresholdValue
### Used in  observeEvent(input$Map_marker_click
GetNOAAData <- function(StationID,ParameterName)
{
  if(StationID %in% WRNOAAThresholds$WRstation_id)
  {
   NOAAStation_ID <- WRNOAAThresholds %>%
                filter(WRstation_id == StationID)%>%
                slice_head()%>%
                pull(NOAAstation_id)
  }
  else
  {
  NOAAStation_ID <- StationID  
  }
  

    NOAAStationData <- NOAAData %>%
                    filter(station_id == NOAAStation_ID)%>%
                    select(Date,ParameterName)%>%
                    rename("Value" = ParameterName)%>%
                    mutate(ColorHex = "")
    
  ## Getting the correct ColorHex
  for (row in 1:nrow(NOAAStationData))
  {
  NOAAStationData$ColorHex[row] <- GetColorHex(GetColor(StationID,ParameterName,NOAAStationData$Value[row]))
  }

 CurrentReading <- GetCurrentReading(NOAAStation_ID,ParameterName,NOAAData)

 Threshold <- GetThreshold(StationID, ParameterName)
 
#Threshold is set to the threshold value closets to the current value because we can't show all of them due to Plotly limitations
if(nrow(Threshold) != 0 && !is.na(Threshold$ValueOne))
{
  ThresholdValueList <- Threshold %>%
    select_if(~ !any(is.na(.)))%>%
    select_if(is.numeric)%>%
    pivot_longer(everything())%>%
    pull(value)%>%
    as.vector()
  
  #"Price is Right" rule - chooses the threshold value thats above the current reading
  ThresholdValueList <- list.filter(ThresholdValueList, . >= CurrentReading)
  ThresholdValue <- ThresholdValueList[which.min(abs(CurrentReading-ThresholdValueList))]
}
else
{
  ThresholdValue <- 0
}

 NOAAStationData$ThresholdValue <- ThresholdValue
 NOAAStationData$station_id <- NOAAStation_ID

  return(NOAAStationData)
}
###### ###### ######  ###
###### END API FUNCTIONS #
###### ###### ######  ###



###### ###### ######  ###
###### HELPER FUNCTIONS #
###### ###### ######  ###

## Get All Stations 
## Gets the correct Colors, and Current Readings for NOAA Stations and WR Stations for use in the map and Gauge plot. 
## Declared above leaflet Map as Stations
GetAllStations <- function()
{ 
  
  NOAAStations <- NOAAStationsList %>%
    mutate(ColorHex = "")%>%
    mutate(station_API_id = "")%>%
    mutate(CurrentReading = 0)
  
  #We need to use a helper function, Current Reading to get the colors for the NOAA Data. 
  for (row in 1:nrow(NOAAStations))
  {
    NOAAStations$CurrentReading[row] <- GetCurrentReading(NOAAStations$station_id[row],"Stage",NOAAData)
    NOAAStations$ColorHex[row] <- as.character(GetColor(NOAAStations$station_id[row],"Stage",NOAAStations$CurrentReading[row]))
  }
  
  NOAAStations <- NOAAStations %>%
    select(-c(CurrentReading))
  
  Stations <- rbind(NOAAStations,GetWRStations())
  return(Stations)
}

# Get Station Type 
# Takes Station_ID 
# Returns Station Type (1 for NOAA station and 0 for WR station)
GetStationType <- function(Station_ID)
{
    
    StationType <- Stations %>%
        filter(station_id == Station_ID)%>%
        select(Type)%>%
        pull()
}

# Get Threshold 
# Takes Station_ID, ParameterName
# Returns the correct row with threshold instructions
GetThreshold <- function(Station_ID, ParameterName)
{
  
  if(Station_ID %in% WRNOAAThresholds$WRstation_id)
  {
    Threshold <- WRNOAAThresholds[0,]
    
    if(ParameterName %in% WRNOAAThresholds$Parameter)
    {
      Threshold <- WRNOAAThresholds %>%
                  filter(Parameter == ParameterName)%>%
                  filter(WRstation_id == Station_ID)
    }
  }
  else
  {
    Threshold <- NOAAThresholds[0,]
    
        if(Station_ID %in% NOAAThresholds$station_id)
        {
            if(ParameterName %in% NOAAThresholds$Parameter)
            {
                Threshold <- NOAAThresholds %>%
                    filter(Parameter == ParameterName)%>%
                    filter(station_id == Station_ID) 
            }
        }
  }

    return(Threshold)
}

## Get Color 
## Takes Station_ID, Parameter, Value 
## Returns the color name 
GetColor <- function(Station_ID,Parameter,Value)
{
 
        Color <- "Grey"
        
        ## Checks to see if the station is in the the WRNOAA Data 
        if(Station_ID %in% WRNOAAThresholds$WRstation_id)
        {
          ## Checks to see if Parameter is as well 
          if(Parameter %in% WRNOAAThresholds$Parameter)
          {
           ## Gets the Threshold
           Threshold <- GetThreshold(Station_ID,Parameter)
           
           if(!is.na(Threshold$ValueOne))
           {
           Color <- ifelse(Value <= Threshold$ValueOne,"Green",Color)
           Color <- ifelse(Value > Threshold$ValueOne,Threshold$ValueOneColor,Color)
           }
           if(!is.na(Threshold$ValueTwo))
           {
             Color <- ifelse(Value > Threshold$ValueTwo,Threshold$ValueTwo$Color,Color)
           }
          }
        }
       else
       {
        if(Station_ID %in% NOAAThresholds$station_id)
        {
            if(Parameter %in% NOAAThresholds$Parameter)
            {
                Threshold <- GetThreshold(Station_ID,Parameter)
                
                Color <- ifelse(Value < Threshold$ValueOne,Threshold$ValueOneColor,Threshold$ValueTwoColor)
                Color <- ifelse(Value < Threshold$ValueThree,Color,Threshold$ValueThreeColor)
                
                if(!is.na(Threshold$ValueFour))
                {
                    Color <- ifelse(Value < Threshold$ValueFour,Color,Threshold$ValueFourColor)
                }
            }
         }
       }
       

        return(Color)
}




#Gets the Color Hex 
#Takes a color name 
GetColorHex <- function(Color)
{
    Hex <- ColorSet %>%
        filter(IconColors == Color)%>%
        mutate(ColorHex = as.character(ColorHex))%>%
        select(ColorHex)%>%
        slice_head()%>%
        pull()
    return(Hex)
}

#Gets the color name
#Takes ColorHex
GetColorName <- function(inColorHex)
{
    ColorName <- ColorSet %>%
        filter(ColorHex == inColorHex)%>%
        mutate(IconColors = as.character(IconColors))%>%
        select(IconColors)%>%
        slice_head()%>%
        pull()
    return(ColorName)
}

#Gets the most recent reading
# Takes a station ID, Parameter, and DF
GetCurrentReading <- function(Station_ID,Parameter,df)
{
        CurrentValue <- df %>%
            filter(Date < Sys.time())%>%
            arrange()%>%
            filter(station_id == Station_ID) %>%
            filter(!is.na(.))%>%
            slice_head()%>%
            pull(Parameter)
        return(CurrentValue)
}

#Gets the current Unit
# Takes a Parameter and a F or C unit
GetUnit <- function(inParameter,inUnit)
{
  
  if(inUnit == "F")
  {
    Units <- c("CFU/100mL","CFU/100mL","F°","F°","NTU","ft","kcfs")
  }
  else
  {
    Units <- c("CFU/100mL","CFU/100mL","C°","C°","NTU","ft","kcfs")
  }
  Parameters <- c("E Coli Concentration","Enterococcus Bacteria Concentration","Air Temperature", "Water Temperature", "Turbidity","Stage","Flow")
  UnitsParam <- data.frame(Units,Parameters)
  
  
  Unit <- UnitsParam %>%
    filter(Parameters == inParameter)%>%
    select(Units)%>%
    pull()
  return(Unit)
}


###### ###### ######  ###
###### END HELPER FUNCTIONS #
###### ###### ######  ###




###### ###### ###### ### 
###### LEAFLET MAP #####
###### ###### ###### ### 


### Renders the Leaflet Map
Stations <- GetAllStations()

output$Map <- renderLeaflet({

    WRStations <- Stations %>%
                  filter(Type == 0)
    
    NOAAStations <-  Stations %>%
                 filter(Type == 1)  %>%
                 mutate(Color = as.character(ColorHex))
    
    leaflet("Map")%>%
        addProviderTiles("CartoDB.VoyagerLabelsUnder", group = "Streets")%>%
        addPolygons(data = Hucs, color = "#76cdae", weight = 3, label = "James River Watershed")%>%
        addPolygons(data = River, color = "#104a77", opacity = 1, stroke = TRUE, weight = 1, label = "James River and Tributaries")%>%
        addCircleMarkers(data = WRStations, lng = ~Longitude, lat = ~Latitude, layerId = ~ station_id, label = ~station_name, fillColor = ~ColorHex, color = "black", fillOpacity = 1, weight = 1)%>%
        addMarkers(data = NOAAStations, lng = ~Longitude, lat = ~Latitude, layerId = ~ station_id, label = ~station_name, icon = ~IconSet[ColorHex])
})

## Reacts to Map Marker Click and sets the StationDataReactive and ParamListReactive
observeEvent(input$Map_marker_click, ignoreNULL = TRUE,{
  click <- input$Map_marker_click
  
  ## If its a NOAA Station, set the correct parameter list and get the noaa data
  if(GetStationType(click$id) == 1)
  {
    ParamListReactive$X <- c("Stage","Flow")
    StationDataReactive$df <- GetNOAAData(click$id,"Stage")
  }
  else
  {
     # If the station should show enterococuss by default, this will be the OG modal data, otherwise, E Coli
      if(click$id %in% EnteroStations)
       {
        # If the station needs stage or flow data, add those options here 
        if(click$id %in% WRNOAAThresholds$WRstation_id)
          {
          ParamListReactive$X <- c(EnteroParameters,"Stage","Flow")
          }
          else
          {
          ParamListReactive$X <- EnteroParameters
          }
      
           #Fetch Data 
          StationDataReactive$df <- GetWRData(click$id,"Enterococcus Bacteria Concentration")
        }
        # If the station needs stage or flow data, add those options here 
        else
        {
         if(click$id %in% WRNOAAThresholds$WRstation_id)
          {
          ParamListReactive$X <- c(ColiParameters,"Stage","Flow")
          }
          else
          {
           ParamListReactive$X <- ColiParameters
          }
     
           ## Fetch Data
          StationDataReactive$df <- GetWRData(click$id,"E Coli Concentration")
        }
    
  }
  
  ## Render the Modal
  showModal(Modal)
  
  ## Set Render Flag to false so we don't trigger the ParamSelect observe event 
  RenderFlag$X <- "FALSE"
})

###### ###### ###### ### ###
###### END LEAFLET MAP #####
###### ###### ###### ### ###



###### ###### ######
###### MODAL  #####
###### ###### ###### 

### Modal Decleration 
Modal <-  modalDialog(
    uiOutput("StationImage"),
    div(class='model-info-wrapper',
         uiOutput("StationText"),
         div(uiOutput("ParameterSelect")),
        tabsetPanel(
            tabPanel("Latest Measurement",uiOutput("GaugeTitle"),plotlyOutput("GaugePlot", width = 450, height = 250)%>% withSpinner()),
            tabPanel("Trends",uiOutput("TrendsTitle"),plotOutput("TrendsPlot", hover = "plot_click", width = 550, height = 250)%>% withSpinner())
                   ),
    ),
    easyClose = TRUE,
    footer = NULL,
)

## Render UI for Parameter Select
output$ParameterSelect <- renderUI({
  selectInput("ParamSelect","", choices = ParamListReactive$X)
})


## Observes the ParamerSelect pulldown and sets the right StationDataReactive
observeEvent(input$ParamSelect,ignoreNULL = TRUE,ignoreInit = TRUE,{
    req(RenderFlag$X)
    click <- input$Map_marker_click

   if(RenderFlag$X == "TRUE")
   {
        if(input$ParamSelect != "")
         {
            if(GetStationType(click$id) == 1)
                 {
                  StationDataReactive$df <- GetNOAAData(click$id,input$ParamSelect)
                 }
                 else
                {
                if(input$ParamSelect == "Stage" | input$ParamSelect == "Flow")
                {
                 StationDataReactive$df <- GetNOAAData(click$id, input$ParamSelect)
                }
                else
                {
                StationDataReactive$df <- GetWRData(click$id,input$ParamSelect)
                }
                }
           }
   }
    RenderFlag$X <- "TRUE"
})

#Render Image for the station status indicator text 
output$StationImage <- renderUI({
    req(input$Map_marker_click)
    click <- input$Map_marker_click 
    ImgLink <- filter(Stations, station_id == click$id)%>%
    mutate(Image = ifelse(is.na(Image),DefaultImage,Image))%>%
    pull(Image)
    
    HTML('<div class=\'model-image\' style=\'background-image: url(\" ',ImgLink,' \"); \'></div>')
})

#Station Text Rendering
output$StationText <- renderUI({
    click <- input$Map_marker_click
    req(StationDataReactive$df)
    char0 <- character(0)
    
    LastSampled <- StationDataReactive$df %>%
                   as.tibble()%>%
                   filter(Date < as.POSIXlt(Sys.time(), tz = "ETC"))%>%
                   select(Date, Value)%>%
                   filter(!is.na(Value))%>%
                   arrange(Date)%>%
                   slice_tail()%>%
                   pull(Date)
    
    LastSampled <- format(LastSampled, format="%B %d %Y")
    
    if(identical(char0,LastSampled))
    {
        LastSampled <- "No Data Currently Available. Select a New Parameter."
    }
    else
    {
        LastSampled <- paste0("Last Monitored on ", LastSampled)
    }
    
    
    StationName <- filter(Stations, station_id %in% click$id)%>%
                   pull(station_name)
    
    NOAAStation_ID <- WRNOAAThresholds %>%
      filter(WRstation_id == click$id)%>%
      slice_head()%>%
      pull(NOAAstation_id)
    
    NOAAStationName <- filter(Stations, station_id %in% NOAAStation_ID)%>%
                       pull(station_name)
    
    StationDescription <- filter(Stations, station_id %in% click$id)%>%
                          pull(description)
    
    tagList(
        tags
        $div(id='model-info-title', style='float:left;border:0px solid red;',StationName),
        HTML("<div id='StationStatus_wrapper' style='float:right; display:block; border:0px solid green;'>"),
        
        imageOutput("StationStatus", width= '100px', height= '30px'),
        HTML("</div>"),
        
        div(style='float:left; display:block; width: 100%; border:0px solid red; margin-bottom: 15px',
            paste(LastSampled,".", sep = ""),
            HTML("<br/>"),
            paste(StationDescription),
            paste0(ifelse(identical(NOAAStationName,char0),"",paste("Stage and Flow data are from ",NOAAStationName,".", sep = ""))),
            HTML("<br/>"),
           uiOutput("MoreInfo")
        )
        
    )
})

#MoreInfo
# Used Above in Station Text
output$MoreInfo <- renderUI({
    click <- input$Map_marker_click
    req(StationDataReactive$df)
   
    if(!is.na(StationDataReactive$df$Value))
    {
    if(StationDataReactive$df$ColorHex[1] != "#999999")
    {
   Text <- ifelse(GetStationType(StationDataReactive$df$station_id[1]) == 1,"Thresholds were created by locals familiar with the area for a typical user.","Threshold is from EPA guidance on Recreational Water Use.")
   }
   else
   {
    Text <- "No Threshold Information available."
   }
    }
    else
    {
      Text <- "No Data Currently Available. Select a New Parameter."
    }
    tagList(
        Text
    )
})

## Station Status Image
output$StationStatus <- renderImage({
    req(input$Map_marker_click)
    click <- input$Map_marker_click
    char0 <- character(0)
    
    ColorHex <- StationDataReactive$df %>%
            filter(Date <= Sys.Date())%>%
            arrange()%>%
            slice_head()%>%
            select(ColorHex)%>%
            pull(ColorHex)
    
  ColorHex <-  ifelse(identical(char0,ColorHex),"#999999",ColorHex)
  Color <- GetColorName(ColorHex)

    Ratio <- IconRatio %>%
        filter(IconColors == Color)%>%
        pull(Ratio)

    ImgLink <- paste0("www/images/Threshold", Color,".png", sep = "")
    
    list(src=ImgLink, align = "right", width = Ratio *35, height = 35)

},deleteFile = FALSE)


###### ###### ######
###### END MODAL  #####
###### ###### ###### 


###### ###### ######
###### CHARTS  #####
###### ###### ###### 


## Gauge Chart Title 
output$GaugeTitle <- renderUI({
  req(input$ParamSelect)
  text <- paste(input$ParamSelect,"-",GetUnit(input$ParamSelect,input$TempUnit))
  tagList(
   h4(text, align = "center")
  )
})

## Gauge Plot Rendering
output$GaugePlot <- renderPlotly({
  req(StationDataReactive$df)
  req(input$ParamSelect)
  click <- input$Map_marker_click
  ChartData <- StationDataReactive$df
  CurrentReading <- GetCurrentReading(ChartData$station_id[1],"Value",ChartData)
  ThresholdValue <- ChartData$ThresholdValue 
  
  #Getting min and Max values for the Gage plot
  if(GetStationType(ChartData$station_id[1]) == 1)
  {
    Max <- NOAAStationsMaxMin %>%
      filter(station_id == ChartData$station_id[1]) %>%
      filter(Parameter == input$ParamSelect)%>%
      select(Max)%>%
      pull()

    Min <- NOAAStationsMaxMin %>%
      filter(station_id == ChartData$station_id[1]) %>%
      filter(Parameter == input$ParamSelect)%>%
      select(Min)%>%
      pull()
  }
  else
  {
  #Gets the max parameter value for WR Params
  Max <- MaxParamValue %>%
      filter(Parameters == input$ParamSelect)%>%
      select(MaxValue)%>%
      pull()
  
  ## Changes the threshold line and current reading if the temp is switched to F
  if(input$TempUnit == "F" && str_detect(input$ParamSelect, "Temperature"))
  {
    Max <- ((Max * 9/5) + 32)
    

      CurrentReading <- (CurrentReading * 9/5) + 32
      ThresholdValue <- (ThresholdValue * 9/5) + 32
  } 
    Min <- 0
  }
  
  ## Declaring font for the plot
  t <- list(
    family = "sofia-pro",
    size = 14,
    color = 'black')
  
  p <- plot_ly(
    domain = list(x = c(0, 1), y = c(0, 1)),
    value = CurrentReading,
    type = "indicator",
    mode = "gauge+number",
    gauge = list(
      axis = list(range = list(Min,Max)),
      bar = list(color = ChartData$ColorHex[1]),
      threshold = list(line= list(color = "black", width = 1),
                       thickness = 1.2,
                       value = ThresholdValue))
  ) %>%
    #config(displayModeBar = F) %>%
    layout(margin = list(l=20,r=30,t=10,b=0)) %>%
    layout(plot_bgcolor='transparent') %>%
    layout(paper_bgcolor='transparent')
})





## Trends Chart Title 
output$TrendsTitle <- renderUI({
  req(input$ParamSelect)
  text <- paste(input$ParamSelect,"-",GetUnit(input$ParamSelect,input$TempUnit))
  tagList(
    h4(text, align = "center")
  )
})

#Trends Chart Rendering 
output$TrendsPlot <- renderPlot({
  req(StationDataReactive$df)
  req(input$ParamSelect)
  click <- input$Map_marker_click

if(!is.na(StationDataReactive$df$Value[1]))
  {
  ChartData <- StationDataReactive$df %>%
  mutate(Shape = ifelse(Date < Sys.time(), 21,23))
  CurrentReading <- GetCurrentReading(ChartData$station_id[1],"Value",ChartData)
  
  # #Getting min and Max values for the Gage plot.
  # Dependent on Station type and parameter 
  if(GetStationType(ChartData$station_id[1]) == 1)
  {
    if(input$ParamSelect == "Stage")
    {
      Max <- NOAAStationsMaxMin %>%
        filter(station_id == ChartData$station_id[1]) %>% 
        filter(Parameter == input$ParamSelect)%>%
        select(Max)%>%
        pull()
      
      Min <- NOAAStationsMaxMin %>%
        filter(station_id == ChartData$station_id[1]) %>% 
        filter(Parameter == input$ParamSelect)%>%
        select(Min)%>%
        pull()
    }
    
    #If Flow 
    else
    {
      Max <- max(ChartData$Value)
      
      Min <- 0
    }
    
    ## Getting the correct ColorHex
    for (row in 1:nrow(ChartData))
    {
    ChartData$ColorHex[row] <- GetColorHex(GetColor(click$id,input$ParamSelect,ChartData$Value[row]))
    }
    
  }
  
  else
  {
    Max <- MaxParamValue %>%
      filter(Parameters == input$ParamSelect)%>%
      select(MaxValue)%>%
      pull()
    
    if(input$TempUnit == "F" && str_detect(input$ParamSelect, "Temperature"))
    {
      Max <- ((Max * 9/5) + 32)
      ChartData <- ChartData %>%
                    mutate(Value = (Value * 9/5) + 32)%>%
                    mutate(ThresholdValue = (Value * 9/5) + 32)
    } 
    Min <- 0
    }
  
  
  # Checks to see if the start and end date are the same and then addes some padding so the chart looks normal 
  if(identical(min(ChartData$Date),max(ChartData$Date)))
     {
   xlim <- as.POSIXct(c(min(ChartData$Date - 60*60*48 ),max(ChartData$Date + 60*60*48 )),  origin = "1970-01-01")
   }
   else
   {
   xlim <- as.POSIXct(c(min(ChartData$Date),max(ChartData$Date)),  origin = "1970-01-01")
   }
  
  suppressWarnings(ggplot(data = ChartData, aes_string(x=ChartData$Date, y=ChartData$Value, stroke = .25))+
                     geom_point(shape = ChartData$Shape, fill = ChartData$ColorHex, color = "black", size = 6)+
                     geom_hline(yintercept= ChartData$ThresholdValue, linetype="dotted", color = "grey") +
                     geom_vline(xintercept = Sys.time(), linetype="dashed", color = ifelse(GetStationType(ChartData$station_id[1]) == 1,"black","white")) + 
                     annotate("text", x = Sys.time()+6*60^2, y = Max, label = ifelse(GetStationType(ChartData$station_id[1]) == 1,"Today",""))+
                     ylab(GetUnit(input$ParamSelect,input$TempUnit))+
                     ylim(Min,Max)+
                     scale_x_datetime(limits = xlim)+
                     xlab("")+
                     theme(
                       plot.title = element_text(hjust = 0.5, size = 22 ),
                       panel.background =  element_rect(fill = "transparent"),
                       plot.background = element_rect(fill = "transparent", color = NA),
                       axis.title = element_text(size=15),
                       axis.text = element_text(size=12),
                       legend.position = "none"))
  }
})

outputOptions(output, "StationText", suspendWhenHidden = TRUE)
outputOptions(output, "StationStatus", suspendWhenHidden = TRUE)
outputOptions(output, "GaugePlot", suspendWhenHidden = TRUE)
outputOptions(output, "TrendsPlot", suspendWhenHidden = TRUE)
outputOptions(output, "StationImage", suspendWhenHidden = TRUE)


}

# Run the application 
shinyApp(ui = ui, server = server)
