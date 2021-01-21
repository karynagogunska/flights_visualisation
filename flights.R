library(ggplot2)
library(maps)
library(rgeos)
library(maptools)
library(ggmap)
library(geosphere)
library(tidyverse)
library(plyr)
library(gganimate)
library(readODS)
#install.packages("readODS")
library(lubridate)
library(dplyr)
library(stringr)
library(gapminder)
library(gganimate)
library(gifski)

setwd("C:/Users/karishoolik/Desktop/flights_visualisation")

#load file with data and create a dataset
myflights <- read.csv("karyna_flights.csv")

#convert dates to data format
myflights$date.forward <- dmy(myflights$date.forward)
myflights$date.backwards <- dmy(myflights$date.backwards)

#sort from earliest flights to latest
myflights = arrange(myflights,date.forward)

#load city coordinates
coords <- read.csv("worldcities.csv")
coords = select(coords,city_ascii,lat,lng)



myflights <- myflights %>%
  select(date.forward, from, to) %>%
  mutate(from = str_extract(from, "[A-Za-z ]*"), # Removing unnecessary data and symbols from city names
         to = str_extract(to, "[A-Za-z ]*")) %>%
  left_join(coords, by = c("from" = "city_ascii")) %>% # adding coordinates
  left_join(coords, by = c("to" = "city_ascii")) %>%
  distinct(date.forward, to, from, .keep_all = TRUE)

eu.countries <-c ("Austria","Belgium","Bulgaria","Croatia","Cyprus",
                "Czech Rep.","Denmark","Estonia","Finland","France",
                "Germany","Greece","Hungary","Ireland","Italy","Latvia",
                "Lithuania","Luxembourg","Malta","Netherlands","Poland",
                "Portugal","Romania","Slovakia","Slovenia","Spain",
                "Sweden","United Kingdom", "Ukraine", "Belarus", "Switzerland", "Moldova", "Russia","Japan")

worldmap <- map_data("world", region = eu.countries)
wrld <- c(geom_polygon(aes(long,lat,group=group), 
                       size = 0.1, 
                       colour= "#1a1a1a", 
                       fill="#404040", alpha=0.8, data=worldmap))         
fortify.SpatialLinesDataFrame = function(model, data, ...){
  ldply(model@lines, fortify)
}


routes <- gcIntermediate(myflights[,c('lng.x', 'lat.x')], 
                         myflights[,c('lng.y', 'lat.y')], 50, 
                         breakAtDateLine = FALSE, 
                         addStartEnd = TRUE, sp=TRUE)
# Fortify routes to dataframe
fortifiedroutes <- fortify.SpatialLinesDataFrame(routes)

fortifiedroutes <- fortifiedroutes %>%
  arrange(group,order) %>%
  mutate(ord = rownames(.) %>% as.numeric()) 

#animation

anim <- ggplot() +
  wrld +
  geom_line(aes(long,lat, group = id), size=.4, data= fortifiedroutes,
            color = "#f2ee66",
            alpha = .3) +
  theme(panel.background = element_rect(fill='#1f1f1f',colour='#2b2b2b'), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "None",
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank()) + 
  transition_reveal(fortifiedroutes$ord)

animate(anim, fps = 20, width = 1980, height = 1080, 
        duration = 30, end_pause = 40, renderer = gifski_renderer())
anim_save("flights.gif")

