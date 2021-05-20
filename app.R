#JRA Widget 
# Git https://github.com/ChesapeakeCommons/JRAWidget_v2.0.git
# Readme https://docs.google.com/document/d/1dHaQ7w8Ttfirp27Yuaoji84n7KgoQs-Qo83T9WIb8-Q/edit
# Created 05.19.2021




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

####### UI
ui <- fluidPage(theme = "styler.css",
                
                
                #Header
                div(id = "header",
                    div(id = "header-title",
                    ),
                    HTML("<a href='https://www.jamesriverwatch.org/' target='blank' style='margin-top: -8px;'>"),
                    div(id = "header-logo" ),
                    HTML("</a>")
                    
                ),     
                
                leafletOutput("Map", height = 'calc(100vh - 75px)', width = '100%'),
                div( class = "temp-overlay-container",
                     div (class = "temp-overlay",
                          div(class = "temp-title", HTML("Temperature Unit")),
                          div( style='margin: 0 auto; border: 0px solid green;',
                               radioButtons("TempUnit", "Unit:", c("F°" = "F", "C°" = "C"), selected = "F")
                          )
                     )
                ),
                
                # Key overlay 
                div( class= "key-overlay-container",
                     div(class = "key-overlay",
                     )
                )
)



# SERVER
server <- function(input, output,session) {
    
    
    
###### ###### ###### ###### ###### ####
###### IMPORTS AND VAR DECLRATIONS ####
###### ###### ###### ###### ###### ####
## For testing !!   
#Stations <- read_csv("www/StationsTesting_v5.csv")    

NOAAStationsList <- read_csv("www/NOAAStations_v1.csv")
    
### For Testing !! ###
NOAAData <- read_csv("www/NOAAData_v1.csv")

### More info for NOAA Stations ###
MoreInfo <- read_csv("www/MoreInfo_v1.csv")


### NOAA Min Max ###
NOAAStationsMaxMin <- read_csv("www/NOAAStationsMaxMin_v1.csv")

#List of Icon Colors 
IconColors <- c("Green", "Grey", "Red", "Yellow") 
#List of Color #'s 
ColorHex <- c("#008000","#999999","#da222b","#ffea2e")
ColorSet <- data.frame(IconColors,ColorHex)


### NOAA Stations Threshold ### 
NOAAThresholds <- read_csv("www/NOAAThresholds_v2.csv")

### Hucs GeoJson
Hucs <- rgdal::readOGR("www/HUC8s_v2.geojson", verbose = FALSE)

### River GeoJson
River <- suppressWarnings(rgdal::readOGR("www/JamesRiverArea_v2.geojson", verbose = FALSE))


#Noaa square iconset 
IconSet <- iconList(
    Red = makeIcon(iconUrl = "www/images/NOAAred.png", iconWidth = 15, iconHeight = 15),
    Yellow = makeIcon(iconUrl = "www/images/NOAAyellow.png", iconWidth = 15, iconHeight = 15),
    Green = makeIcon(iconUrl = "www/images/NOAAgreen.png", iconWidth = 15, iconHeight = 15),
    Grey = makeIcon(iconUrl = "www/images/NOAAgrey.png", iconWidth = 15, iconHeight = 15)
)

#StationDataReactive represents all the data for the selected station
StationDataReactive <- reactiveValues(df = data.frame())

#List of Icon Colors 
IconColors <- c("Green", "Grey", "Red", "Yellow")    
#List of aspect ratios for threshold text 
Ratio <- c(2.3449,4.7375,4.797,3.31)
#Bind of Icon Colors and Ratio
IconRatio <- data.frame(IconColors,Ratio)

#List of Parameters 
Parameters <- c("E Coli Concentration","Enterococcus Bacteria Concentration","Air Temperature", "Water Temperature", "Turbidity") 
EnteroParameters <- c("Enterococcus Bacteria Concentration", "Air Temperature", "Water Temperature", "Turbidity")
ColiParameters <- c("E Coli Concentration", "Air Temperature", "Water Temperature", "Turbidity")

#Station names that should sho
EnteroStations <- c("J05","P05","C01")

##!!List of max values used for WR data - will likely drop
MaxValue <- c(2419,2419,45,50,500) 
##Bind of Parameters and Max Values 
MaxParamValue <- data.frame(Parameters,MaxValue)

    

#Parameter Codes for API Request 
APICode <- c(711,1690,707,708,710)
APIParameterCodes <- data.frame(Parameters,APICode)

# API Token 
Token <- "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9.eyJ1Ijoic3VwcG9ydEB3YXRlcnJlcG9ydGVyLm9yZyJ9.fkSv5ZUeX0oK_PCJl5K5nPBnci7X2FJY0mSnfO3LTjSPr31hAdZUJkGnYe4hjztt-XF-RSTvn_OA8Gd14MXK1S-YJ3_zcI4SUF-HqwFPNfha39L0nHBJ2gSB3HQLxOkJ4WvF3bdtezqtLNDv4e1KeqGIPCjVBw_wL2k68Zu-Q-BJUwM9V7v5nwqFIr9Enj7Y5ggEXrPycq-XA33B61f9GXYG1MuWoo4w6zLN4N8v95Bwn2f-f1OjpCeahkRHpGKp27GCiTxkT222XuxOak4g3U1IUu_32mYNYbOl0IyGu-paj8CNpLk2e8pdu38ZpRNWS74ixdVHRzNUr18mkZttjw"



###### ###### ###### ###### ###### ####
### END IMPORTS AND VAR DECLARTIONS ###
###### ###### ###### ###### ###### ####



###### ###### ######  ###
###### API FUNCTIONS ####
###### ###### ######  ###

## Returns the station_id, station_name, station_API_id, station_key, Latitude, Longitude and Type
GetWRStations <- function()
{
    
    Request <- GET(paste("https://dev.api.waterreporter.org/stations?sets=863&geo_format=xy&access_token=",Token, sep = "")
    )
    print(Request)
    jsonRequestText <- content(Request,as="text")
    parsed <- fromJSON(jsonRequestText)
    Data <- data.frame(parsed$features)
    
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
    
    ### We need to get the colors which is a seperate per station and parameter request ###
    for (row in 1:nrow(StationData))
    {
        URL <- paste("https://dev.api.waterreporter.org/readings?station_id=",StationData$station_API_id[row],"&parameter_id=711&limit=1&acces_token=",Token, sep = "")
        
        Request <- GET(URL)
        
        jsonRequestText <- content(Request,as="text")
        parsed <- fromJSON(jsonRequestText)
        StationData$ColorHex[row] <- ifelse(!is.null(parsed$data$color),parsed$data$color,"#999999")
    }
    return(StationData)
}

## Station Data Request 
## Returns collection date, parametervalue, and color 
GetWRData <- function(Station_ID,ParameterName)
{
    ## $$ Parsing Parameter Name 
    ParameterCode <- APIParameterCodes %>%
        filter(Parameters == ParameterName)%>%
        pull(APICode)
    
    
    ## $$ Parsing the Station Name 
    #Getting the station_API_id for calling the WRData
    station_API_id <- Stations %>%
        filter(station_id == Station_ID)%>%
        pull(station_API_id)
    
    URL <- paste("https://dev.api.waterreporter.org/readings?parameter_id=",ParameterCode,"&station_id=",station_API_id,"&access_token=",Token, sep = "")
    
    Request <- GET(URL)
    
    print(Request)
    jsonRequestText <- content(Request,as="text")
    parsed <- fromJSON(jsonRequestText)
    
    Data <- data.frame(parsed$data$collection_date,parsed$data$value,parsed$data$color)%>%
                rename("Date" = parsed.data.collection_date,
                       "Value" = parsed.data.value,
                      "ColorHex" = parsed.data.color)%>%
               mutate(Date = as.POSIXct(Date, origin = "1970-01-01", tz = "UTC"))
    
    return(Data)
}


## NOAA DATA PARSER ##
#Data Parcing for the NOAA sensor data from NOAA
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
        #     #Renaming Vars
        rename("Date" = NULL.V1,
               "Stage" = NULL.V2,
               "Flow" =  NULL.V3)%>%
        #     #Filtering out unneeded rows leftover from parsing
        filter(!grepl('Forecast',Date))%>%
        filter(!grepl('Observed',Date))%>%
        filter(!grepl('Date',Date))%>%
        #Adding Year to Date
        mutate(Date = as.POSIXct(paste(Year,"/",as.character(Date), sep = ""), tz="UTC"))%>%
        mutate(Stage = as.numeric(str_remove(Stage, "ft")))%>%
        mutate(Flow = ifelse(str_detect(Flow, "kcfs"), as.numeric(str_remove(Flow, "kcfs"))*1000, as.numeric(str_remove(Flow, "cfs"))))%>%
        mutate(Flow = ifelse(Flow < -900, NA, Flow))%>%
        mutate(station_id = StationID)
    
    return(dfCleaned)
}
## END NOAA DATA PARSER ##


#Constructs the NOAA Data Frame and shows loading progress
#!!!! Uncomment to run loading, comment out to skip loading ####
NOAADataPull <- function()
{
    NOAAData <- data.frame(station_id = character(),
                           Date = as.character(),
                           Flow = double(),
                           Stage = double())
    
       withProgress(message = 'Loading Predictive NOAA Data: ', value = nrow(NOAAStationsList), {
        
        for (row in 1:nrow(NOAAStationsList))
        {
                incProgress(1/row, detail = HTML(paste("Station ", NOAAStationsList$station_id[row], " | ", row, " of ", 14, sep = "")))
                
                NOAAData <- rbind(NOAAData,NOAADataRequest(NOAAStationsList$station_id[row]))
        }
        return(NOAAData)
    })
}
NOAAData <- NOAADataPull()




GetNOAAData <- function(Station_ID,ParameterName)
{
NOAAStationData <- NOAAData %>%
                    filter(station_id == Station_ID)%>%
                    select(Date,ParameterName)%>%
                    rename("Value" = Stage)%>%
                    mutate(ColorHex = "")

for (row in 1:nrow(NOAAStationData))
{
    NOAAStationData$ColorHex[row] <- GetColorHex(GetColor(Station_ID,"Stage",NOAAStationData$Value[row]))
}
    return(NOAAStationData)
}

###### ###### ######  ###
###### END API FUNCTIONS #
###### ###### ######  ###


###### ###### ######  ###
###### HELPER FUNCTIONS #
###### ###### ######  ###

#Get Station Type 
GetStationType <- function(Station_ID)
{
    
    StationType <- Stations %>%
        filter(station_id == Station_ID)%>%
        select(Type)%>%
        pull()
}


GetThreshold <- function(Station_ID, Parameter)
{
    Threshold <- NOAAThresholds[0,]
    
        if(Station_ID %in% NOAAThresholds$station_id)
        {
            if(Parameter %in% NOAAThresholds$Parameter)
            {
                Threshold <- NOAAThresholds %>%
                    filter(Parameter == Parameter)%>%
                    filter(station_id == Station_ID) 
            }
        }

    return(Threshold)
}



# #Gets the color of the station 
# Lets re write this at somepoint, its very shitty 
GetColor <- function(Station_ID,Parameter,Value)
{
        Color <- "Grey"
        
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
        
        return(Color)
}

GetColorHex <- function(Color)
{
    Hex <- ColorSet %>%
        filter(IconColors == Color)%>%
        mutate(ColorHex = as.character(ColorHex))%>%
        select(ColorHex)%>%
        pull()
    return(Hex)
}


#Gets the most recent reading for the
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

###### ###### ######  ###
###### END HELPER FUNCTIONS #
###### ###### ######  ###




###### ###### ###### ### 
###### LEAFLET MAP #####
###### ###### ###### ### 


#Gets the correct Colors, and Current Readings for NOAA Stations and WR Stations for use in the map and Gauge plot. 
GetAllStations <- function()
{
    NOAAStations <- NOAAStationsList %>%
        mutate(description = "")%>%
        mutate(ColorHex = "")%>%
        mutate(station_API_id = "")%>%
        mutate(CurrentReading = 0)

    #We need to use a helper function, Current Reading to get the colors for the NOAA Data. 
    for (row in 1:nrow(NOAAStations))
    {
        
        NOAAStations$CurrentReading[row] <- GetCurrentReading(NOAAStations$station_id[row],"Stage",NOAAData)
        
        NOAAStations$ColorHex[row] <- GetColor(NOAAStations$station_id[row],"Stage",NOAAStations$CurrentReading[row])
        
    }
    
    NOAAStations <- NOAAStations %>%
        select(-c(CurrentReading))
    
    Stations <- rbind(NOAAStations,GetWRStations())
    
    return(Stations)
}

### Renders the Leaflet Map
Stations <- GetAllStations()
output$Map <- renderLeaflet({
    
     WRStations <- Stations %>%
                 filter(Type == 0)
    
    NOAAData <- Stations %>%
        filter(Type == 1)  %>%
        mutate(Color = as.factor(ColorHex))
    
    leaflet("Map")%>%
        addProviderTiles("CartoDB.VoyagerLabelsUnder", group = "Streets")%>%
        addPolygons(data = Hucs, color = "#76cdae", weight = 3, label = "James River Watershed")%>%
        addPolygons(data = River, color = "#104a77", opacity = 1, stroke = TRUE, weight = 1, label = "James River and Tributaries")%>%
        addCircleMarkers(data = WRStations, lng = ~Longitude, lat = ~Latitude, layerId = ~ station_id, label = ~station_name, fillColor = ~ColorHex, color = "black", fillOpacity = 1, weight = 1)%>%
        addMarkers(data = NOAAData, lng = ~Longitude, lat = ~Latitude, layerId = ~ station_id, label = ~station_name, icon = ~IconSet[ColorHex])
})

d


## Reacts to Map Marker Click to make data for the modal 
observeEvent(input$Map_marker_click, ignoreNULL = TRUE,ignoreInit = TRUE,{
    
    click <- input$Map_marker_click
    
    if(GetStationType(click$id) == 1)
    {
        updateSelectInput(session, "ParamSelect", choices = c("Stage","Flow"), selected = "Stage")
        StationDataReactive$df <- GetNOAAData(click$id,"Stage")
    }
    else
    {
       # If the station should show enterococuss by default, this will be the OG modal data, otherwise, E Coli 
       if(click$id %in% EnteroStations)
       {
            updateSelectInput(session, "ParamSelect", choices = EnteroParameters, selected = "Enterococcus Bacteria Concentration")
            StationDataReactive$df <- GetWRData(click$id,"Enterococcus Bacteria Concentration")
       }
       else
       {
        updateSelectInput(session, "ParamSelect", choices = ColiParameters, selected = "E Coli Concentration")
        StationDataReactive$df <- GetWRData(click$id,"E Coli Concentration")
       }
    }

    
})

}

# Run the application 
shinyApp(ui = ui, server = server)
