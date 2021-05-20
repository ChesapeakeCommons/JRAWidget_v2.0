


library(dplyr)
library(plyr)
library(tidyverse)


InputData <- read_csv("www/JRADataMarch30.csv")


#Removing the pesky column markers         
colnames(InputData) <- gsub("\\[.*?\\]",'',colnames(InputData))

names(InputData) <- trimws(names(InputData))
      
Entero <- InputData %>%
  filter(!is.na(`Enterococcus Bacteria Concentration`))


InputData_v2 <- InputData %>% 
             select(station_id,station_name,latitude,longitude,collection_date,
                    `E Coli Concentration`,`Enterococcus Bacteria Concentration`,
                    `Air Temperature`,`Water Temperature`,`Turbidity` )%>%
                     filter(!is.na(latitude))%>%
                     #filter_at(vars(`E Coli Concentration`:`Turbidity`), all_vars(is.na(.)))%>%
                     rename("Date" = collection_date)%>%
                     filter(station_id != "VDH-HB")%>%
                     filter(station_id != "VDH-HTP")%>%
                     filter(station_id != "VDH-KL")%>%
                     filter(station_id != "VDH-AP")

write.csv(InputData_v2,"www/WRInputData_v3.csv", row.names = FALSE)

Stations <- InputData_v2 %>%
            distinct(station_id, .keep_all = TRUE)%>%
            select(station_id, station_name, latitude, longitude)


#Some stations have no data whatso ever, removing these 
# StationSampleCount <- InputData_v2 %>%
#                       select(-c(latitude,longitude,Date,station_name))%>%
#                       group_by(station_id)%>%
#                       summarise_if(is.numeric, sum, na.rm = TRUE)


