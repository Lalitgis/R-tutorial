#Making maps using R 2 min tutorial
install.packages("ggplot2")   #only do this once
install.packages("tidyverse") #only do this once
install.packages('maps')
library(ggplot2)              #needs to be done each r session
library(tidyverse)            #needs to be done each r session
library(maps)

EUvax <- read.csv("EUvaccine.csv") #####this reads in the data file I made you will need to change the path to your computer
View(EUvax)

mapdata <- map_data("world") ##ggplot2
View(mapdata)
mapdata <- left_join(mapdata, EUvax, by="region")
View(mapdata)

mapdata1<-mapdata %>% filter(!is.na(mapdata$Perc_vaccinated)) #remove anything that is null
View(mapdata1)

map1<-ggplot(mapdata1, aes( x = long, y = lat, group=group)) +
  geom_polygon(aes(fill = Perc_vaccinated), color = "black")
map1

map2 <- map1 + scale_fill_gradient(name = "% Vaccinated", low = "yellow", high =  "red", na.value = "grey50")+
  labs(title = 'Percentage vaccination in Europe')+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        plot.title = element_text(size = 18, face = 'bold', color = 'steelblue', hjust = 0.5),
        rect = element_blank())
map2

install.packages("cowplot")
install.packages('magick')
library(cowplot)
library(magick)
ggdraw() +
  draw_image("narrow.png",  x = 0.35, y = 0.3, scale = .2) +
  draw_plot(map2)
#You need to downlaod the northarrow and upload using cowplot
