##Script by: @toeb18, toebR
#creds to Martjin van Vreeden: https://martijnvanvreeden.nl/analysing-google-location-data/

#load libraries
library(sf)
library(tidyverse)
library(ggthemes)
library(ggpubr)
library(jsonlite)
library(lubridate)
library(stringr)

dir <- c("C:\\Users\\tobia\\OneDrive\\Desktop\\ownprojects\\R\\own_projects\\google_location_tracker")
setwd(dir)

#read in shapefile for CH for later
CH <- st_read(dsn = "CH.shp")
CH <- st_transform(CH, 4236)

#read json file
system.time(df_list <- fromJSON("Location_History.json"))
df <- df_list$location
head(df)

#create columns for dates
as.POSIXct(as.numeric(df$timestampMs)/1000, tz='GMT', origin='1970-01-01') -> df$time
as.Date(df$time) -> df$date
isoweek(df$date) -> df$week
isoyear(df$date) -> df$year

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

#test variable for customizing plot
loc20 <- shape%>%
  filter(year == 2020| year == 2017)

min(loc20$year)
nrow(loc20)

shape$year <- as.character(shape$year)

#Overall plot
ggplot() +
  geom_sf(data = shape, aes(color = as.character(year)),color=alpha("cyan", 0.3), show.legend = FALSE) +
  geom_sf(data = CH, aes(group = NAME), fill = "transparent", color = "#969696", size = 1) +
  ggtitle("My Google Location History", subtitle = "look at 2020!") +
  labs(caption = "plot by @toeb18 ¦ Source: Google GPS Tracker") +
  facet_wrap(.~year) +
  theme(plot.background = element_rect(fill= "#444B5A"),
        panel.background = element_rect(fill = "#444B5A"),
        legend.background = element_rect(fill = "#444B5A"),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        plot.title = element_text(size = 16, color = '#969696'),
        plot.subtitle  = element_text(size = 12, color = '#969696'),
        # axis.title = element_blank(),
        # axis.text = element_blank(),
        axis.title = element_text(size = 12, color = '#969696'),
        axis.text = element_text(color = "#969696")) -> history_years

ggsave(history_years, filename = "history_years.png", width = 10, height = 20, units = "cm")

#maybe create separate plots of all years and patch together with patchwork because of the island holidays in 2016^^

#create empty list
list <- list()

#iterator
Years <- c("2016", "2017", "2018", "2019", "2020")

#for loop that stores the plots per year in an empty list

for (i in Years) {
  filt <- shape %>%
    filter(str_detect(year, i))
  local({
    i<-i
    auxplot <- ggplot() +
    geom_sf(data = filt, aes(color = year),color=alpha("cyan", 0.3), show.legend = FALSE) +
    geom_sf(data = CH, aes(group = NAME), fill = "transparent", color = "#969696", size = 1) +
    ggtitle("",subtitle = paste("Google Location History: ",i)) +
      labs(caption = "plot by @toeb18 ¦ Source: Google GPS Tracker") +
      theme(plot.background = element_rect(fill= "#444B5A"),
            panel.background = element_rect(fill = "#444B5A"),
            legend.background = element_rect(fill = "#444B5A"),
            legend.position = "bottom",
            legend.title = element_blank(),
            panel.grid.major = element_blank(),
            plot.title = element_text(size = 16, color = '#969696'),
            plot.subtitle  = element_text(size = 12, color = '#969696'),
            plot.caption = element_text(color = '#969696'),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())
  list[[i]] <<- auxplot
  })
}
p2016 <- list[[1]]
p2017 <- list[[2]]
p2018 <- list[[3]]
p2019 <- list[[4]]
p2020 <- list[[5]]

p2020
#export:
file_name = "2010.png"
png(file_name, width=15, height=15, units = "cm", res = 300)
print(last_plot())
dev.off()



#plot by month end of 2019 vs start of 2020
corona <- shape %>%
  filter(year == 2020 | year == 2019)

Years

    