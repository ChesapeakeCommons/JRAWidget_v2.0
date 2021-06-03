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
library(plotrix)



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
                
              #  add_busy_spinner(spin = "fading-circle"),
                
                #Header
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

WRNOAAThresholds <- read_csv("www/WRNOAAThresholds_v1.csv")

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

RenderFlag <- reactiveValues(X = as.character("FALSE"))
ParamListReactive <- reactiveValues(X = as.vector(NULL))

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
EnteroStations <- c("J05","P05","C01", "VDH-AP","VDH-HTP","VDH-KL")

##!!List of max values used for WR data - will likely drop
MaxValue <- c(2419,2419,45,50,500) 
##Bind of Parameters and Max Values 
MaxParamValue <- data.frame(Parameters,MaxValue)

#Parameter Codes for API Request 
APICode <- c(2586,2587,2583,2584,2589)
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
    
    Request <- GET(paste("https://dev.api.waterreporter.org/stations?sets=1227&geo_format=xy&access_token=",Token, sep = "")
    )

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
        Parameter_ID <- ifelse(StationData$station_id[row] %in% EnteroStations,2587,2586)
        
        URL <- paste("https://dev.api.waterreporter.org/readings?station_id=",StationData$station_API_id[row],"&parameter_id=",Parameter_ID,"&limit=1&access_token=",Token, sep = "")
        
        Request <- GET(URL)

        jsonRequestText <- content(Request,as="text")
        parsed <- fromJSON(jsonRequestText)

        StationData$ColorHex[row] <- as.character(ifelse(!is.null(parsed$data$color),parsed$data$color,"#999999"))

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

    jsonRequestText <- content(Request,as="text")
    parsed <- fromJSON(jsonRequestText)

    # Threshold <- data.frame(parsed$parameter$chart_schema$ranges$color,parsed$parameter$chart_schema$ranges$description)%>%
    #                       rename("ColorHex" = parsed.parameter.chart_schema.ranges.color,
    #                              "ThresholdDescription" = parsed.parameter.chart_schema.ranges.description)
    

    if(!is.null(parsed$dataset))
    {
    Data <- data.frame(parsed$data$collection_date,parsed$data$value,parsed$data$color)%>%
                rename("Date" = parsed.data.collection_date,
                       "Value" = parsed.data.value,
                      "ColorHex" = parsed.data.color)%>%
                      mutate(ColorHex = as.character(ColorHex))%>%
                  #    mutate(ColorHex = as.character(ifelse(!is.null(ColorHex) || ColorHex == "#aaacab",ColorHex,"#999999")))%>%
                      mutate(Date = as.POSIXct(Date, origin = "1970-01-01", tz = "UTC"))#%>%
                   #   left_join(Threshold)
    
    ### Getting the correct thresholds and the threshold value for charts
    CurrentReading <- Data %>%
                      slice_head()%>%
                      pull(Value)

    UpperBound <- parsed$parameter$chart_schema$ranges$upper_bound
    ThresholdValueList <- UpperBound[!is.na(UpperBound)]
    ThresholdValueListTrimmed <- list.filter(ThresholdValueList, . >= CurrentReading)
    ThresholdValue <- as.numeric(ThresholdValueListTrimmed[which.min(abs(CurrentReading-ThresholdValueListTrimmed))])

    # Checking to see if value is over threshold, setting to max threshold value
    if(purrr::is_empty(ThresholdValue))
    {
    ThresholdValue <- max(ThresholdValueList)
    }

    Data$ThresholdValue <- ThresholdValue
    Data$station_id <- Station_ID
    
    }
    else
    {
    Data <- data.frame(matrix(ncol = 5, nrow = 1))
    
    headers <- c("station_id", "Date", "Value","ColorHex","ThresholdValue")

    colnames(Data) <- headers
    Data$station_id <- Station_ID
    }

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
    
       withProgress(message = 'Fetching Predictive NOAA Data: ', value = nrow(NOAAStationsList), {
        
        for (row in 1:nrow(NOAAStationsList))
        {
                incProgress(1/row, detail = HTML(paste("Station ", NOAAStationsList$station_id[row], " | ", row, " of ", 14, sep = "")))
                
                NOAAData <- rbind(NOAAData,NOAADataRequest(NOAAStationsList$station_id[row]))
        }
        return(NOAAData)
    })
}
NOAAData <- NOAADataPull()
#write.csv(NOAAData,"www/NOAAData_v1.csv")
GetNOAAData <- function(Station_ID,ParameterName)
{
  if(Station_ID %in% WRNOAAThresholds$WRstation_id)
  {
   NOAAStation_ID <- WRNOAAThresholds %>%
                filter(WRstation_id == Station_ID)%>%
                slice_head()%>%
                pull(NOAAstation_id)
  }
  else
  {
  NOAAStation_ID <- Station_ID  
  }
  
    NOAAStationData <- NOAAData %>%
                    filter(station_id == NOAAStation_ID)%>%
                    select(Date,ParameterName)%>%
                    rename("Value" = ParameterName)%>%
                    mutate(ColorHex = "")
  for (row in 1:nrow(NOAAStationsList))
  {
  NOAAStationData$ColorHex[row] <- GetColorHex(GetColor(Station_ID,ParameterName,NOAAStationData$Value[row]))
  }

 CurrentReading <- GetCurrentReading(NOAAStation_ID,ParameterName,NOAAData)

 Threshold <- GetThreshold(Station_ID, ParameterName)
 
#Threshold is set to the threshold value closets to the current value because we can't show all of them due to Plotly limitations
if(nrow(Threshold) != 0)
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
  if(Station_ID %in% WRNOAAThresholds$WRstation_id)
  {
    Threshold <- WRNOAAThresholds[0,]
    
    if(Parameter %in% WRNOAAThresholds$Parameter)
    {
      Threshold <- WRNOAAThresholds %>%
                  filter(Parameter == Parameter)%>%
                  filter(WRstation_id == Station_ID)
    }
  }
  else
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
  }

    return(Threshold)
}

# #Gets the color of the station 
# Lets re write this at somepoint, its very shitty 
GetColor <- function(Station_ID,Parameter,Value)
{
        Color <- "Grey"
        
        if(Station_ID %in% WRNOAAThresholds$WRstation_id)
        {
          if(Parameter %in% WRNOAAThresholds$Parameter)
          {
           Threshold <- GetThreshold(Station_ID,Parameter)
           Color <- ifelse(Value <= Threshold$ValueOne,"Green",Color)
           Color <- ifelse(Value > Threshold$ValueOne,Threshold$ValueOneColor,Color)
           
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

GetColorName <- function(ColorHex)
{
    ColorName <- ColorSet %>%
        filter(ColorHex == ColorHex)%>%
        mutate(Color = as.character(Color))%>%
        select(Color)%>%
        slice_head()%>%
        pull()
    return(ColorName)
}

#Gets the most recent reading
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
GetUnit <- function(inParameter,inUnit)
{
  
  if(inUnit == "F")
  {
    Units <- c("CFU/100mL","CFU/100mL","F°","F°","NTU","ft","cfs")
  }
  else
  {
    Units <- c("CFU/100mL","CFU/100mL","C°","C°","NTU","ft","cfs")
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
        NOAAStations$ColorHex[row] <- as.character(GetColor(NOAAStations$station_id[row],"Stage",NOAAStations$CurrentReading[row]))
        
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
        mutate(Color = as.character(ColorHex))
    
    leaflet("Map")%>%
        addProviderTiles("CartoDB.VoyagerLabelsUnder", group = "Streets")%>%
        addPolygons(data = Hucs, color = "#76cdae", weight = 3, label = "James River Watershed")%>%
        addPolygons(data = River, color = "#104a77", opacity = 1, stroke = TRUE, weight = 1, label = "James River and Tributaries")%>%
        addCircleMarkers(data = WRStations, lng = ~Longitude, lat = ~Latitude, layerId = ~ station_id, label = ~station_name, fillColor = ~ColorHex, color = "black", fillOpacity = 1, weight = 1)%>%
        addMarkers(data = NOAAData, lng = ~Longitude, lat = ~Latitude, layerId = ~ station_id, label = ~station_name, icon = ~IconSet[ColorHex])
})





## Reacts to Map Marker Click to make data for the modal
observeEvent(input$Map_marker_click, ignoreNULL = TRUE,{
  #RenderFlag$X <- "FALSE"

  click <- input$Map_marker_click
  
  if(GetStationType(click$id) == 1)
  {
    ParamListReactive$X <- c("Stage","Flow")
  #  updateSelectInput(session, "ParamSelect", choices = c("Stage","Flow"), selected = "Stage")
    StationDataReactive$df <- GetNOAAData(click$id,"Stage")
  }
  else
  {
    # If the station should show enterococuss by default, this will be the OG modal data, otherwise, E Coli
    if(click$id %in% EnteroStations)
    {
      
      if(click$id %in% WRNOAAThresholds$WRstation_id)
      {
        ParamListReactive$X <- c(EnteroParameters,"Stage","Flow")
      }
      else
      {
        ParamListReactive$X <- EnteroParameters
      }
      
      StationDataReactive$df <- GetWRData(click$id,"Enterococcus Bacteria Concentration")
    }
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
     
      StationDataReactive$df <- GetWRData(click$id,"E Coli Concentration")
    }
    
  }
  showModal(Modal)
  RenderFlag$X <- "FALSE"
})

###### ###### ###### ### ###
###### END LEAFLET MAP #####
###### ###### ###### ### ###



###### ###### ######
###### MODAL  #####
###### ###### ###### 

#   withProgress(message = 'Loading Station Card', {
Modal <-  modalDialog(
    uiOutput("StationImage"),
    div(class='model-info-wrapper',
         uiOutput("StationText"),
        div(
          uiOutput("ParameterSelect")
           # selectInput("ParamSelect","", choices = "", selectize = TRUE),
        ),
        tabsetPanel(
            tabPanel("Latest Measurement",uiOutput("GaugeTitle"),plotlyOutput("GaugePlot", width = 450, height = 250)%>% withSpinner()),
            tabPanel("Trends",uiOutput("TrendsTitle"),plotOutput("TrendsPlot", hover = "plot_click", width = 550, height = 250)%>% withSpinner())
        #     #  uiOutput("ChartKey"))
        #     
         ),
        
    ),
    easyClose = TRUE,
    footer = NULL,
  # footer = NULL
)

output$RenderFlag <- renderUI({
  tagList(
    paste(RenderFlag$X)
  )
})


output$ParameterSelect <- renderUI({
  selectInput("ParamSelect","", choices = ParamListReactive$X)
})

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
    mutate(Image = ifelse(is.na(Image),"https://www.savethesound.org/wp-content/uploads/2021/05/orient-point-state-park_SM_HeyNardo_FINAL.jpg",Image))%>%
    pull(Image)

    
    HTML('<div class=\'model-image\' style=\'background-image: url(\" ',ImgLink,' \"); \'></div>')
})

#Text Rendering 
output$StationText <- renderUI({
    
    #withProgress(message = 'Loading Station Card', {
    click <- input$Map_marker_click
    req(StationDataReactive$df)
    
    
    char0 <- character(0)
    #   if(sum(StationDataReactive$df)))
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
    
    tagList(
        tags
        $div(id='model-info-title', style='float:left;border:0px solid red;',StationName),
        HTML("<div id='StationStatus_wrapper' style='float:right; display:block; border:0px solid green;'>"),
        
     #   imageOutput("StationStatus", width= '100px', height= '30px'),
        HTML("</div>"),
        
        div(style='float:left; display:block; width: 100%; border:0px solid red; margin-bottom: 15px',
            paste(LastSampled),
            HTML("<br/>"),
            paste0("This site is monitored by ", 
                   ifelse(GetStationType(click$id) == 1, "the National Oceanic and Atmospheric Administration (NOAA)", "James River Association Volunteers")),
            HTML("<br/>"),
           uiOutput("MoreInfo")
        )
        
    )
})


#GetColor    #station_id param, value 
output$MoreInfo <- renderUI({

    click <- input$Map_marker_click
    req(StationDataReactive$df)
   
    ## !!!! ###
    if(!is.na(StationDataReactive$df$Value))
    {
   if(StationDataReactive$df$ColorHex != "#aaacab" || StationDataReactive$df$ColorHex != "#999999")
   {
   Text <- ifelse(GetStationType(click$id) == 1,"Thresholds were created by consulting with locals familiar with the area for an average user.","Threshold is from EPA guidance on Recreational Water Use")
   }
   else
   {
    Text <- "No Threshold Information"
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

output$StationStatus <- renderImage({
    req(input$Map_marker_click)
    click <- input$Map_marker_click

    ColorHex <- StationDataReactive$df %>%
            filter(Date < Sys.Date())%>%
            arrange()%>%
            slice_head()%>%
            select(ColorHex)%>%
            pull(ColorHex)
        
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


## Gauge Title 
output$GaugeTitle <- renderUI({
  req(input$ParamSelect)
  text <- paste(input$ParamSelect,"-",GetUnit(input$ParamSelect,input$TempUnit))
  tagList(
   h4(text, align = "center")
  )
})

# #Gauge Plot Rendering
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
  
  t <- list(
    family = "sofia-pro",
    size = 14,
    color = 'black')
  
  p <- plot_ly(
    domain = list(x = c(0, 1), y = c(0, 1)),
    value = CurrentReading,
   # title = list(text = paste(input$ParamSelect,"-",GetUnit(input$ParamSelect,input$TempUnit)), font = t),
    type = "indicator",
    mode = "gauge+number",
    gauge = list(
      axis = list(range = list(Min,Max)),
      bar = list(color = ChartData$ColorHex),
      threshold = list(line= list(color = "black", width = 1),
                       thickness = 1.2,
                       value = ThresholdValue))
  ) %>%
    #  config(displayModeBar = F) %>%
    layout(margin = list(l=20,r=30,t=10,b=0)) %>%
    layout(plot_bgcolor='transparent') %>%
    layout(paper_bgcolor='transparent')
  #will also accept paper_bgcolor='black' or paper_bgcolor='transparent'
})





## Chart Title 
output$TrendsTitle <- renderUI({
  req(input$ParamSelect)
  text <- paste(input$ParamSelect,"-",GetUnit(input$ParamSelect,input$TempUnit))
  tagList(
    h4(text, align = "center")
  )
})

#Trends Rendering 
output$TrendsPlot <- renderPlot({
  req(StationDataReactive$df)
  req(input$ParamSelect)
  click <- input$Map_marker_click
  
  if(!is.na(StationDataReactive$df$Value[1]))
     {
        ChartData <- StationDataReactive$df %>%
         mutate(Shape = ifelse(Date < Sys.time(), 21,23))
  
  CurrentReading <- GetCurrentReading(ChartData$station_id[1],"Value",ChartData)
  # #Getting min and Max values for the Gage plot
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
    else
    {
      Max <- ChartData %>%
        mutate(Max = max(Value, na.rm = FALSE))%>%
        slice_head()%>%
        pull(Max)
      
      Max <- max(ChartData$Value)
      
      Min <- 0
    }
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
  
  xlim <- as.POSIXct(c(min(ChartData$Date),max(ChartData$Date)),  origin = "1970-01-01")
  suppressWarnings(ggplot(data = ChartData, aes_string(x=ChartData$Date, y=ChartData$Value, stroke = .25))+
                     geom_point(shape = ChartData$Shape, fill = ChartData$ColorHex, color = "black", size = 6)+
                     geom_hline(yintercept= ChartData$ThresholdValue, linetype="dotted", color = "grey") +
                     geom_vline(xintercept = Sys.time(), linetype="dashed", color = ifelse(ChartData$station_id[1] == 1,"grey","white")) + 
                     annotate("text", x = Sys.time()+5*60^2, y = Max, label = ifelse(ChartData$station_id[1] == 1,"Today",""))+
                     ylab(GetUnit(input$ParamSelect,input$TempUnit))+
                     ylim(Min,Max)+
                     scale_x_datetime(limits = xlim)+
                     xlab("Date")+
                     #ggtitle(input$ParamSelect)+
                     theme(
                       # panel.background = element_rect(fill = "transparent"), # bg of the panel
                       # plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
                       # panel.grid.major = element_blank(), # get rid of major grid
                       # panel.grid.minor = element_blank(), # get rid of minor grid
                       # legend.background = element_rect(fill = "transparent"), # get rid of legend bg
                       # legend.box.background = element_rect(fill = "transparent"), # get rid of legend panel bg         
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
