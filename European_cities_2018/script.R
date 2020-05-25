##Script by: @toeb18, Rtoeb

#load libraries
library(sf)
library(tidyverse)
library(ggthemes)
library(ggpubr)


#set directory where shapes are located
dir <- c("C:\\Users\\tobia\\OneDrive\\Desktop\\ownprojects\\R\\own_projects\\eurostat\\city_audits_2018")
setwd(dir)

#read in shapes (already in 3035 geographic coordinate system!)
shape <- st_read(dsn = "Urban_audit_2018.shp")

Europe <- st_read(dsn = "europe_poly.shp")

World <- st_read(dsn = "CNTR_RG_01M_2016_3035.shp")

head(shape)

#map on European cities (3035 Projected)
ggplot() +
  geom_sf(data = shape, aes(fill = area_km2),
          fill = alpha("cyan", 0.4),
          color = "cyan",
          size = 0.2,
          # color = "transparent",
          show.legend = TRUE) +
  geom_sf(data = Europe, aes(color = Europe$CNTR_ID),
          fill = "transparent", color = "#969696",size = 0.5,
          show.legend = FALSE) +
  ggtitle("Mapping 1751 Cities and Villages in Europe") +
  labs(caption = "plot by @toeb18 ¦ Source: Eurostat 2018") +
  theme(text = element_text(color = "#969696"),
        plot.background = element_rect(fill= "#444B5A"),
        panel.background = element_rect(fill = "#444B5A"),
        legend.background = element_rect(fill = "#444B5A"),
        legend.position = "left",
        panel.grid.minor = element_line(color = '#4d5566'),
        panel.grid.major = element_line(color = '#586174'),
        plot.title = element_text(size = 16),
        plot.subtitle  = element_text(size = 12),
        axis.title = element_text(size = 12, color = '#969696'),
        axis.text = element_text(size = 8, color = '#969696')) -> map
map
ggsave(map, filename = "map.png", dpi = 300, width = 20, height = 20, units = "cm")

#Globe map in 3035 Projection 
ggplot() +
  geom_sf(data = World, aes(color = World$CNTR_ID),
          fill = "transparent", color = "#969696",size = 0.5,
          show.legend = FALSE) +
  geom_sf(data = shape, aes(fill = area_km2),
          fill = alpha("cyan", 0.4),
          color = "cyan",
          size = 0.2,
          # color = "transparent",
          show.legend = TRUE) +
  labs(caption = "plot by @toeb18 ¦ Source: Eurostat 2018") +
  theme(text = element_text(color = "#969696"),
        plot.background = element_rect(fill= "#444B5A"),
        panel.background = element_rect(fill = "#444B5A"),
        legend.background = element_rect(fill = "#444B5A"),
        legend.position = "left",
        panel.grid.minor = element_line(color = '#4d5566'),
        panel.grid.major = element_line(color = '#586174'),
        plot.title = element_text(size = 16),
        plot.subtitle  = element_text(size = 12),
        axis.title = element_text(size = 12, color = '#969696'),
        axis.text = element_text(size = 8, color = '#969696')) -> globe
globe
ggsave(globe, filename = "globe.png", dpi = 300, width = 20, height = 20, units = "cm")



