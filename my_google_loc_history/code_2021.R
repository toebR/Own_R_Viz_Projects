##Script by: Tobias Stalder
#date: 30.01.2022
#creds to Martjin van Vreeden: https://martijnvanvreeden.nl/analysing-google-location-data/

#load libraries
library(sf)
library(mapview)
library(tidyverse)
library(ggthemes)
library(ggpubr)
library(jsonlite)
library(lubridate)
library(stringr)
library(here)
library(Cairo)
options(scipen = 999)
# 
# dir <- c("C:\\Users\\tobia\\OneDrive\\Desktop\\ownprojects\\R\\own_projects\\google_location_tracker")
# setwd(dir)

#read in shapefile for CH for later
CH <- st_read(dsn= paste0(here(), "/swissboundaries3d.shp/SHAPEFILE_LV95_LN02/swissBOUNDARIES3D_1_3_TLM_LANDESGEBIET.shp"))
CH <- st_transform(CH, 4236)

#read json file
system.time(df_list <- fromJSON(paste0(here(),"/location_data/Takeout/Location History/Records.json")))
df <- df_list$location
head(df)

#create columns for dates
# as.POSIXct(as.numeric(df$timestamp)/1000, tz='GMT', origin = "1970-01-01") -> df$time
substr(df$timestamp, 1, 4) -> df$year
substr(df$timestamp, 6, 7) -> df$month
substr(df$timestamp, 9, 10) -> df$day

df$date <- as.POSIXct(paste0(df$year, "-", df$month, "-", df$day))


#Create proper lat lon columns for ggmap
df$lat <- df$latitudeE7 / 1e7
df$lon <- df$longitudeE7 / 1e7

#some summary
n_count <- nrow(df) #number of rows in dataframe / number of location pings
n_count
n_days <- df$date %>%
  n_distinct() #number of days in dataframe
n_days
n_avg_day <- round(n_count/n_days,2) #average number of datapoints per day
n_avg_day
round(n_avg_day / 24,2) #average number of datapoints per hour

#convert to shapefile!
shape <- st_as_sf(df,coords = c("lon", "lat"), crs = 4326)
shape

# try to build a smoothed line geometry..

shape = shape %>%
  arrange(date)

multipoint <- shape %>% group_by(year, source) %>%
  summarise(do_union = F)

multipoint$year <- as.character(multipoint$year)
vec_points = npts(multipoint, by_feature = TRUE)
multipoint$n_points <- vec_points

multipoint %>%
  mutate(source = factor(source, levels = c(NA, "GPS", "CELL", "WIFI"))) %>%
  filter(source != "UNKNOWN")-> multipoint

multipoint %>%
  mutate(scale  = case_when(
    source == "WIFI" ~ 1,
    source == "CELL" ~ 2,
    source == "GPS" ~ 3
  )) -> multipoint


# plot function -----------------------------------------------------------

#create plotting subset for function dev
# multipoint %>%
#   filter(year == "2017") -> dat2016

plot_map <- function(data, baseshape){
  
  ggplot() +
    geom_sf(data = baseshape, aes(group = NAME), fill = "lightgrey", color = "lightgrey", size = 1)+
    geom_sf(data = data, aes(color = source, size = scale), alpha = 0.2,show.legend = FALSE) +
    scale_color_manual(values = c("#990000","#1380A1","#FAAB18")) +
    scale_size_continuous(range = c(0.5, 2))+
    theme(plot.background = element_rect(fill= "white"),
          panel.background = element_rect(fill= "white"),
          legend.background = element_rect(fill= "white"),
          legend.position = "right",
          legend.title = element_blank(),
          panel.grid.major = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank()
          )+
    coord_sf(xlim = c(6, 11), ylim = c(45.5,52))

}

# plot_map(data = multipoint, baseshape = CH, years = "2017")


plot_col <- function(data, baseshape, years) {
  data %>%
    filter(year == years) -> dat
  
  dat %>%
    st_drop_geometry() %>%
    ggplot() +
    geom_col(aes(x = source, y = n_points, fill = source), show.legend = FALSE) +
    xlab("")+
    ylab("Nr. of logged points")+
    theme_classic2()+
    theme(axis.ticks = element_blank())+
    scale_y_continuous(expand = c(0,0), labels= function(x) format(x, big.mark = "'", decimal.mark = ",", scientific = FALSE))+
    scale_fill_manual(values = c("#990000","#1380A1","#FAAB18", "green"))  -> P_col
  ggsave(P_col, filename = paste0(here(), "/exports/", years, "_col.png"), height = 7, width = 7, dpi = 300, unit = "cm", bg = "transparent",
         type = "cairo-png")
}

# plot_col(data = multipoint, baseshape = CH, years = "2017")


# plot all maps and col charts --------------------------------------------

#iterator
Years <- c("2016", "2017", "2018", "2019", "2020", "2021")

#for loop that executes functions per year

for (i in Years) {
 plot_col(data = multipoint, baseshape = CH, years = i)
}

multipoint %>%
  filter(year == "2017") -> dat2017
multipoint %>%
  filter(year == "2018") -> dat2018
multipoint %>%
  filter(year == "2019") -> dat2019
multipoint %>%
  filter(year == "2020") -> dat2020
multipoint %>%
  filter(year == "2021") -> dat2021


plot_map(data = dat2017, baseshape = CH) -> P2017
ggsave(P2017, filename = paste0(here(), "/exports/", "2017", "_map.png"), height =10, width = 10, dpi = 600, unit = "cm", bg = "transparent",
       type = "cairo-png")

plot_map(data = dat2018, baseshape = CH) -> P2017
ggsave(P2017, filename = paste0(here(), "/exports/", "2018", "_map.png"), height = 10, width = 10, dpi = 600, unit = "cm", bg = "transparent",
       type = "cairo-png")

plot_map(data = dat2019, baseshape = CH) -> P2017
ggsave(P2017, filename = paste0(here(), "/exports/", "2019", "_map.png"), height = 10, width = 10, dpi = 600, unit = "cm", bg = "transparent",
       type = "cairo-png")

plot_map(data = dat2020, baseshape = CH) -> P2017
ggsave(P2017, filename = paste0(here(), "/exports/", "2020", "_map.png"), height = 10, width = 13, dpi = 600, unit = "cm", bg = "transparent",
       type = "cairo-png")

plot_map(data = dat2021, baseshape = CH) -> P2017
ggsave(P2017, filename = paste0(here(), "/exports/", "2021", "_map.png"), height = 10, width = 10, dpi = 600, unit = "cm", bg = "transparent",
       type = "cairo-png")







