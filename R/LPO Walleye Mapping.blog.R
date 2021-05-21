#set working directory

#packages required
library(readxl)
library(purrr)
library(dplyr)
library(tidyverse)

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
####################################################

#install.packages("leaflet")
library(leaflet)

#opens map
leaflet()%>%addTiles()

#starts color gradient for receiver locations
colors<- c("yellow","red")
pal<-colorBin(colors,final$Fish.Count, bins = 4)

#creates color for boat launches
black <- ("black")
launchpal <- colorFactor(black, ramps$Boat_Launch)

#popup info for boat launches (interactive text)
ramps<- ramps%>%mutate(popup_info=paste(Site_Name))
ramps$popup_info

#popup info for receiver locations(interactive text)
final<- final%>%mutate(popup_info=paste(Location,"<br/>", "Fish Count = ",Fish.Count))
final$popup_info

#creates map with lat/long points with color gradient markers for fish density and markers for boat ramps
walleyemap<- leaflet()%>%addTiles()%>%

  #adds boat launch locations
  addCircleMarkers(data=ramps,

                   #adds lat/long
                   lat = ~Latitude, lng = ~Longitude,
                   #adjusts size of points
                   radius = ~2,
                   #adds color
                   color = ~launchpal(Boat_Launch),
                   fillOpacity = 0.6,
                   popup = ~popup_info)    %>%

  #adds receiver locations
  addCircleMarkers(data=final,
                   #adds lat/long
                   lat = ~Latitude, lng = ~Longitude,
                   #adjusts size of points
                   radius = ~4,
                   #adds color gradient
                   color = ~pal(Fish.Count), fillOpacity = 1,
                   popup = ~popup_info) %>%

  #adds text to map for landmarks but not actual markers
  addLabelOnlyMarkers ( data = landmarks,
                        lat = ~Latitude, lng = ~Longitude,
                        label = ~Landmark,
                        labelOptions = labelOptions(nohide = TRUE, direction = "center", textOnly = TRUE)) %>%


  addLegend("bottomleft",
            pal = pal, opacity = 1,
            values = final$Fish.Count,
            title = "Fish Count") %>%

  addLegend("bottomleft",
            pal = launchpal, opacity = 0.7,
            values = ramps$Boat_Launch)

walleyemap

