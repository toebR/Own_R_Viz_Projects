##lorraine map for png for later lorraine map poster (map cropped to a circle to look like a microscope image)

library(tidyverse)
library(sf)
library(Cairo)

#load data
build <- st_read(dsn = r"(C:\Users\tobia\OneDrive\Desktop\ownprojects\lorraine_map\lorraine_buildings.shp)")
rail <- st_read(dsn = r"(C:\Users\tobia\OneDrive\Desktop\ownprojects\lorraine_map\railways_lorraine.shp)")
road <- st_read(dsn = r"(C:\Users\tobia\OneDrive\Desktop\ownprojects\lorraine_map\roads_lorraine.shp)")

#reproject data
st_transform(build, 2056) -> build
st_transform(rail, 2056) -> rail
st_transform(road, 2056) -> road

build1 <- build
rail1 <- rail
raod1<-road

#dissolve to new index
build %>%
  mutate(index = 1) %>%
  group_by(index) %>%
  summarise(index = first(index))  -> build

rail %>%
  mutate(index = 1) %>%
  group_by(index) %>%
  summarise(index = first(index))  -> rail

road %>%
  mutate(index = 1) %>%
  group_by(index) %>%
  summarise(index = first(index))  -> road

#make map
ggplot() +
  geom_sf(data = build, aes(fill = index, color = index), show.legend = FALSE) +
  geom_sf(data = road, aes(color = index), color = "grey", size = .5) +
  geom_sf(data = rail, aes(color = index), color = "white") +
  theme_void() +
  theme(plot.background = element_rect(color = "#2e4053", fill = "#2e4053")) +
  ggsave(path = r"(C:\Users\tobia\OneDrive\Desktop\ownprojects\lorraine_map)",
         filename = "darkblue_map.png", dpi = 1000,
         width = 20, height = 20, type = "cairo")


ggplot() +
  geom_sf(data = build, aes(fill = index, color = index), color = "#c1003d", fill = "#c1003d", show.legend = FALSE) +
  geom_sf(data = road, aes(color = index), color = "#322b2e", size = .5) +
  geom_sf(data = rail, aes(color = index), color = "black") +
  theme_void() +
  theme(plot.background = element_rect(color = "#f3b100", fill = "#f3b100")) +
  ggsave(path = r"(C:\Users\tobia\OneDrive\Desktop\ownprojects\lorraine_map)",
         filename = "yellow_map.png", dpi = 1000,
         width = 20, height = 20, type = "cairo")


ggplot() +
  geom_sf(data = build, aes(fill = index, color = index), color = "black", fill = "black", show.legend = FALSE) +
  geom_sf(data = road, aes(color = index), color = "#808b96", size = .5) +
  geom_sf(data = rail, aes(color = index), color = "black") +
  theme_void() +
  theme(plot.background = element_rect(color = "white", fill = "white")) +
  ggsave(path = r"(C:\Users\tobia\OneDrive\Desktop\ownprojects\lorraine_map)",
         filename = "black_map.png", dpi = 1000,
         width = 20, height = 20, type = "cairo")

build1 %>%
  group_by(type) %>%
  summarise(ID = mean(as.numeric(osm_id), na.rm = TRUE)) -> build2
raod1 %>%
  group_by(maxspeed) %>%
  summarise(ID = mean(as.numeric(osm_id), na.rm = TRUE)) -> road2
rail1 %>%
  group_by(bridge) %>%
  summarise(ID = mean(as.numeric(osm_id), na.rm = TRUE)) -> rail2


#gradient map
ggplot() +
  geom_sf(data = build2, aes(fill = ID, color = ID), show.legend = FALSE) +
  geom_sf(data = road2, aes(color = ID), show.legend = FALSE, size = .5) +
  geom_sf(data = rail2, aes(color = ID), color = "#372869", show.legend = FALSE) +
  scale_color_gradientn(colours = c("#372869", "#d50a80", "#37b0c3")) +
  scale_fill_gradientn(colors = c("#372869", "#d50a80", "#37b0c3"))+
  # scale_color_viridis_c(option = "plasma") +
  # scale_fill_viridis_c(option = "plasma")+
  theme_void() +
  theme(plot.background = element_rect(color = "#88c1b8", fill = "#88c1b8")) +
  ggsave(path = r"(C:\Users\tobia\OneDrive\Desktop\ownprojects\lorraine_map)",
         filename = "gradient_map_on_mint.png", dpi = 1000,
         width = 20, height = 20, type = "cairo")


ggplot() +
  geom_sf(data = build2, aes(fill = ID, color = ID), show.legend = FALSE) +
  geom_sf(data = road2, aes(color = ID), show.legend = FALSE, size = .5) +
  geom_sf(data = rail2, aes(color = ID), color = "#00bd92", show.legend = FALSE) +
  scale_color_viridis_c() +
  scale_fill_viridis_c()+
  theme_void() +
  theme(plot.background = element_rect(color = "black", fill = "black")) +
  ggsave(path = r"(C:\Users\tobia\OneDrive\Desktop\ownprojects\lorraine_map)",
         filename = "gradient_map_on_black.png", dpi = 1000,
         width = 20, height = 20, type = "cairo")



#export to png -> crop to round, then import to inkscape and make a poster.