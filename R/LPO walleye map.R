#set working directory
setwd("S:/Fishery/Research/Rust/Hoyt/Walleye Map (blog)/Data")
#packages required
library(readxl)
library(purrr)
library(dplyr)
library(tidyverse)
library(mapview)
library(leaflegend)
library(crosstalk)
library(DT)

#import excel files
ramps<- read_excel("IDFG boat launch.xlsx")
landmarks <-read_excel("landmarks.xlsx") 
last2weeks <- read_excel("last two weeks.xlsx")
receivers<- read_excel("receiver info.xlsx")

#splits "last two weeks" into list by receiver 
#(last two weeks is last two weeks of data from "Tracking summary 1 hr residency" filtered for WAE)
seperated<-split(last2weeks, last2weeks$Receiver)
seperated

#Count specific fish at each receiver as a list
rec.fish<- map(seperated, ~.x %>%
                 group_by(Receiver)%>%
                 summarise(unique_number = n_distinct(Transmitter_TagID)))

#turns list into data frame
fishcount<- do.call(rbind.data.frame, rec.fish)
fishcounts<- fishcount %>% remove_rownames %>% column_to_rownames(var = "Receiver")


#makes receiver file into df
as.data.frame(receivers)


#change receiver to row name in receivers df to match merging df (fishcounts)
receiver<- receivers %>% remove_rownames %>% column_to_rownames(var = "Receiver")

#combine receiver info and fish counts
final.na<- merge(receiver,fishcounts, by=0, all=TRUE)
final.na
#get rid of n/a's (receivers that were not checked in last two weeks)
final <- na.omit(final.na)
final
#rename column headers
colnames(final) <- c("Receiver", "Latitude", "Longitude", "Location", "Fish.Count")
final

####################################################################
#creates table for map
#finaltable<- final[ , c("Location", "Fish.Count")]  
#rename column headers
#colnames(finaltable) <- c("Location", "Walleye Count")
#finaltable
#sorts table
#finaltable<- finaltable[order(-finaltable$'Walleye Count'), ]
#finaltable <- as.data.frame(finaltable)
#finaltable<- finaltable %>% remove_rownames %>% column_to_rownames(var = "Location")
#finaltable

########################################################################
#adds active tracking data
#AT <- read_excel("Active Tracking.xlsx") 

#Process Active Tracking Data (count specific fish at each tracking location) #splits into list by location
#AT.count <- AT %>%
 # group_by(Long) %>%
  #mutate(count = n())
#AT.count

#creates color
#ATpal <- colorFactor(
  #palette = ('#D3D3D3'),
 # domain = AT$`Active Track`)

####################################################

#desktop version of map
#install.packages("leaflet")
library(leaflet)

#opens map
leaflet()%>%addTiles()

#starts color gradient for receiver locations
colors<- c("yellow","red")
pal<-colorBin(colors,final$Fish.Count, bins = 5)

#adds boat launch icons
boat.icon <- makeIcon(
  iconUrl = "S:/Fishery/Research/Rust/Hoyt/Walleye Map (blog)/Data/boat_icon.png",
  iconWidth = 20, iconHeight = 40,
  iconAnchorX = 7, iconAnchorY = 32
)

#popup info for boat launches (interactive text)
ramps<- ramps%>%mutate(popup_info=paste(Site_Name))
ramps$popup_info

#popup info for receiver locations(interactive text)
final<- final%>%mutate(popup_info=paste(Location,"<br/>", "Fish Count = ",Fish.Count))
final$popup_info

#creates map with lat/long points with color gradient markers for fish density and markers for boat ramps
walleyemap<- leaflet()%>%addTiles()%>%addProviderTiles('Esri.WorldTopoMap') %>%
  
  
  #adds receiver locations
  addCircleMarkers(data=final,
                   #adds lat/long
                   lat = ~Latitude, lng = ~Longitude,
                   #adjusts size of points
                   radius = ~6,
                   #adds color gradient
                   color = ~pal(Fish.Count), fillOpacity = 1,
                   #adds popup info
                   popup = ~popup_info) %>%
    
  
  #adds boat launch locations
  addMarkers(data=ramps,
                   #adds lat/long
                   lat = ~Latitude, lng = ~Longitude,
                   #adjusts size of points
                   #adds color
                   icon = boat.icon,
                   popup = ~popup_info)    %>%
  

  
  #adds text to map for landmarks but not actual markers
  addLabelOnlyMarkers ( data = landmarks,
                      lat = ~Latitude, lng = ~Longitude, 
                      label = ~Landmark,
                      labelOptions = labelOptions(noHide = TRUE, direction = "center", textOnly = TRUE)) %>%
  
  #Add active tracking data
  #addCircleMarkers(data = AT.count, lat = ~Lat, lng = ~Long,
                  # stroke = FALSE, label = ~count,
                 #  radius = ~9,
                  # color = ~ATpal("Active Track"), fillOpacity = 1,
                  # labelOptions = labelOptions(noHide = T, textOnly = T)) %>% 
  
  #addLegend("topright",
           # pal = ATpal,
            #values = AT.count$'Active Track Fish Count',
            #opacity = 1) %>%

  #add legend for fish count
  addLegend("topright",
            pal = pal, opacity = 1,
            values = final$Fish.Count,
            title = "Fish Count") %>%
  
  #add legend for boat launch
  addLegendImage(images = "S:/Fishery/Research/Rust/Hoyt/Walleye Map (blog)/Data/boat_icon.png",
                 labels = ('Boat Launch'),width = 20, height = 40,
                 position = 'topright',
                 labelStyle = 'font-size: 15px; text-align: center;')

walleyemap



  #############################################################################################
  #Mobile version of walleye map
  
  #install.packages("leaflet")
  library(leaflet)
  
  #opens map
  leaflet()%>%addTiles()
  
  #starts color gradient for receiver locations
  colors<- c("yellow","red")
  pal<-colorBin(colors,final$Fish.Count, bins = 5)
  
  #adds boat launch icons
  boat.icon <- makeIcon(
    iconUrl = "S:/Fishery/Research/Rust/Hoyt/Walleye Map (blog)/Data/boat_icon.png",
    iconWidth = 45, iconHeight = 90,
    iconAnchorX = 18, iconAnchorY = 70
  )
  
  #creates color for boat launches
  #black <- ("black")
  #launchpal <- colorFactor(black, ramps$Boat_Launch)
  
  #popup info for boat launches (interactive text)
  ramps<- ramps%>%mutate(label_info=paste(Site_Name))
  ramps$label_info
  
  #label info for receiver locations(interactive text) (increased size for mobile version)
  final<- final%>%mutate(label_info= paste(Location, "Fish Count = ", Fish.Count)) 
  final$label_info
  
  #creates map with lat/long points with color gradient markers for fish density and markers for boat ramps
  mobilewalleyemap<- leaflet()%>%addTiles()%>%addProviderTiles('Esri.WorldTopoMap') %>%
    
    
    #adds receiver locations
    addCircleMarkers(data=final,
                     #adds lat/long
                     lat = ~Latitude, lng = ~Longitude,
                     #adjusts size of points
                     radius = ~15,
                     #adds color gradient
                     color = ~pal(Fish.Count), fillOpacity = 1,
                     label = ~label_info,
                     labelOptions = labelOptions(textsize = "30px"))%>%
    
    
    #adds boat launch locations
    addMarkers(data=ramps,
               #adds lat/long
               lat = ~Latitude, lng = ~Longitude,
               #adjusts size of points
               #adds color
               icon = boat.icon,
               label = ~label_info,
               labelOptions = labelOptions(textsize = "30px")) %>%
    
    
    
    #adds text to map for landmarks but not actual markers
    addLabelOnlyMarkers ( data = landmarks,
                          lat = ~Latitude, lng = ~Longitude, 
                          label = ~Landmark,
                          labelOptions = labelOptions(noHide = TRUE, direction = "center", textOnly = TRUE)) %>%
    
    #Add active tracking data
    #addCircleMarkers(data = AT.count, lat = ~Lat, lng = ~Long,
    # stroke = FALSE, label = ~count,
    #  radius = ~9,
    # color = ~ATpal("Active Track"), fillOpacity = 1,
    # labelOptions = labelOptions(noHide = T, textOnly = T)) %>% 
    
    #addLegend("topright",
    # pal = ATpal,
    #values = AT.count$'Active Track Fish Count',
  #opacity = 1) %>%
  
  #add legend for fish count
  addLegend("topright",
            pal = pal, opacity = 1,
            values = final$Fish.Count,
            title = "Fish Count") %>%
    
    #add legend for boat launch
    addLegendImage(images = "S:/Fishery/Research/Rust/Hoyt/Walleye Map (blog)/Data/boat_icon.png",
                   labels = ('Boat Launch'),width = 20, height = 40,
                   position = 'topright',
                   labelStyle = 'font-size: 15px; text-align: center;')
  
  
  
  mobilewalleyemap

    
