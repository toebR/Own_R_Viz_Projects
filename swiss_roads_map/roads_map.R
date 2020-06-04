#making a map on the Swiss road net
#by @toeb18, Rtoeb
#June 2020

library(sf)
library(tidyverse)
library(ggtext)

#set working directory
dir <- "C:\\Users\\tobia\\OneDrive\\Desktop\\ownprojects\\R\\own_projects\\swiss_roads"
setwd(dir)

#load the geodata that I need for the map (was a bit tweaked in QGis =P)
CH <- st_read(dsn = "swissBOUNDARIES3D_1_3_TLM_LANDESGEBIET.shp")
cantons <- st_read(dsn = "swissBOUNDARIES3D_1_3_TLM_KANTONSGEBIET.shp")
roads <- st_read(dsn = "gis_osm_roads_fclass2.shp")

national_roads <- roads %>%
  filter(fclass == "National")

ggplot() +
  geom_sf(roads, mapping = aes(group = fclass), color = "cyan", size = 0.3) +
  geom_sf(national_roads, mapping = aes(group = fclass), color = "cyan", size = 1, alpha = 0.5) +
  geom_sf(national_roads, mapping = aes(group = fclass), color = "cyan", size = 2.5, alpha = 0.3) +
  geom_sf(CH, mapping = aes(group = NAME), size = 1, fill = "transparent", color = "#969696") +
  # scale_color_manual(values = c("cyan", "yellow", "blue")) +
  ggtitle("Roads in Switzerland", subtitle = "National Roads and Major Regional Networks") +
  labs(caption = "plot by @toeb18 ¦ Source: Open Street Map") +
  theme(text = element_text(family = 'Gill Sans', color = "#969696"),
        plot.background = element_rect(fill= "#444B5A", color = "#444B5A"),
        panel.background = element_rect(fill = "#444B5A"),
        legend.background = element_rect(fill = "#444B5A"),
        legend.position = "none",
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle  = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 12, color = '#969696'),
        axis.text = element_text(color = "#969696")) -> map_roads
ggsave(map_roads, filename = "Swiss_roads2.png", width = 27, height = 25, units = "cm")

