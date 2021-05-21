#set working directory
setwd("~/Desktop/blogwork/LPO.mapping/R")

#packages required
library(readxl)
library(purrr)
library(dplyr)
library(tidyverse)

#import file
read_excel("last two weeks.xlsx")
last2weeks <- read_excel("last two weeks.xlsx")
last2weeks

#splits into list by receiver
seperated<-split(last2weeks, last2weeks$Receiver)
seperated

#Count specific fish at each receiver as a list
rec.fish<- map(seperated, ~.x %>%
                 group_by(Receiver)%>%
                 summarise(unique_number = n_distinct(Transmitter_TagID)))

#turns list into data frame
fishcount<- do.call(rbind.data.frame, rec.fish)
fishcounts<- fishcount %>% remove_rownames %>% column_to_rownames(var = "Receiver")


#import receiver info and makes df
receivers<- read_excel("receiver info.xlsx")
as.data.frame(receivers)


#change receiver to row name in receivers df to match merging df
receiver<- receivers %>% remove_rownames %>% column_to_rownames(var = "Receiver")

#combine receiver info and fish counts
final.na<- merge(receiver,fishcounts, by=0, all=TRUE)
final.na
#get rid of n/a's
final <- na.omit(final.na)
final
#rename column headers
colnames(final) <- c("Receiver", "Latitude", "Longitude", "Location", "Fish.Count")
final
####################################################

#install.packages("leaflet")
library(leaflet)
################################
#loads in boat launch info
library(readxl)

ramps<- read_excel("IDFG boat launch.xlsx")

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
leaflet()%>%addTiles()%>%

  #adds receiver locations
  addCircleMarkers(data=final,

                   #adds lat/long
                   lat = ~Latitude, lng = ~Longitude,
                   #adjusts size of points
                   radius = ~3,
                   #adds color gradient
                   color = ~pal(Fish.Count), fillOpacity = 1,
                   popup = ~popup_info) %>%

  #adds boat launch locations
  addCircleMarkers(data=ramps,

                   #adds lat/long
                   lat = ~DD_Y, lng = ~DD_X,
                   #adjusts size of points
                   radius = ~2,
                   #adds color
                   color = ~launchpal(Boat_Launch),
                   fillOpacity = 0.6,
                   popup = ~popup_info)    %>%


  addLegend("bottomleft",
            pal = pal, opacity = 1,
            values = final$Fish.Count,
            title = "Fish Count") %>%

  addLegend("bottomleft",
            pal = launchpal, opacity = 0.7,
            values = ramps$Boat_Launch)

###########################################
