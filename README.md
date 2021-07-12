JRA RiverWatch 2.0 README
---------------

This document provides an overview of the James RiverWatch 2.0 application, its contents, and how it functions.
---------------

Quick Links
---------------

[Git](https://github.com/ChesapeakeCommons/JRAWidget_v2.0)

[Application](https://jamesriver.shinyapps.io/Riverwatch/)  

[Data](https://drive.google.com/drive/u/0/folders/1DsVJ0CLUZBJyEHH34rJhhjjJelVywnvn)

[Shinyapps.io](https://www.shinyapps.io/)

[This document](https://docs.google.com/document/d/1dHaQ7w8Ttfirp27Yuaoji84n7KgoQs-Qo83T9WIb8-Q/)



Version Control and Access 
---------------

You can download the application from the [git repository.](https://github.com/ChesapeakeCommons/JRAWidget_v2.0) After downloading, please fork a new branch, and name it your last name, and date. e.g. Reilly_07_08. Make any changes you'd like to this branch - it is yours. If there is a bug or problem with the Master branch, please perform a pull request to make changes to the core application and to be subsequently approved by the Commons team.

Application Publishing
---------------

To publish the application, use the whirlpool icon in Rstudio to publish via [Shinyapps.io](https://www.shinyapps.io/). You can see a step by step process of this [here](https://www.r-bloggers.com/2021/05/push-button-publishing-for-shiny-apps/). For now (07/09/2021), make sure to always publish to the same URL from the same machine. Publishing from different machines and environments could potentially cause issues.

Application Format
---------------

The application is broken into three distinct scripts. In app.R, there is the UI side and Server side, and a styler.css stylesheet. Note that some basic styling does occur in UI side of app.R

The styler.css and UI side depend on the server side. The server side has two major visual components, the Map and the Modal. It has two major data streams, Water Reporter data - (WR), and data from the National Oceanic and Atmospheric Administration, (NOAA). Throughout the application, there will be similarities and differences between how the WR data and the NOAA data are handled. These differences will be noted when applicable.

App.R Server Side Flow
---------------

The application first loads the NOAA stations and data directly from a webtable, and also requests the WR stations from the WR API. The Stations file combines both types and is used for displaying in the Map, StationImage, and StationText, Note that NOAA stations and WRNOAA stage color information is calculated in the application whereas WR data color is provided  by the API.  Upon map click, observeEvent(input$Map_marker_click, sets the StationDataReactive and the ParamListReactive to their correct values depending on what type of station is clicked. Then the Modal is presented. The Modal contains the rendered components, ParamSelect, StationImage, StationText, StationStatus, and TabsetPanel (GageChart and TrendsChart). These different sections utilize a variety of helper methods and API functions to display the correct information.

App.R Server Side Components
---------------

Full Documentation for each component can be found in app.R server side by using Ctr + F and searching for the component by name.

#### Imports and Variable Decelerations 

##### Water Reporter 

* Parameters

* EnteroParameters

* ColiParameters

* EnteroStations

* MaxValue

* MaxParamValue

* APICode

* APIParameterCodes

* Token

##### NOAA

* NOAAStationsList 

* NOAAData 

* NOAAStationsMaxMin

* NOAAThresholds

* IconSet

#### Logical 

* StationDataReactive

* ParamListReactive

* RenderFlag

#### UI 

* IconColors

* ColorHex

* ColorSet

* Ratio

* IconRatio

* Hucs

* River

* DefaultImage

#### API Request Functions 

* GetWRStations

* GetWRData

* NOAADataRequest

* NOAADataPull

* GetNOAAData

* GetAllStations

#### Helper Functions 

* GetStationType

* GetThreshold

* GetColor

* GetColorHex

* GetColorName

* GetCurrentReading

* GetUnit

#### Map 

Map_Marker Observe

#### Modal 

* Parameter Select

  * ParmSelect Observe

* Station Image 

* Station Status 

* Station Text

* Gage Chart 

* Trends Chart
