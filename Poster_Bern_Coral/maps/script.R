library(here)
library(tidyverse)
library(sf)


# data --------------------------------------------------------------------
datsrc <- paste0(here(), "/data")
build <- sf::st_read(dsn = paste0(datsrc,"/BernBuildings.shp"))
build %>%
  mutate(fill = "a") -> build
street <- sf::st_read(dsn = paste0(datsrc,"/BernStrassen.shp"))%>% mutate(fill = "a")
river <-sf::st_read(dsn = paste0(datsrc,"/BernGew.shp"))

river %>%
  mutate(fill = "a") -> river
rail <- sf::st_read(dsn = paste0(datsrc,"/BernEisenbahn.shp")) %>% mutate(fill = "a")
HK <- sf::st_read(dsn = paste0(datsrc,"/BernHK.shp")) %>% mutate(fill = "a")

unique(street$BELAGSART)


# test --------------------------------------------------------------------
small <- c(3, 4, 7, 11, 12, 14, 15, 16, 17, 18, 19)
medium <- c(1, 5, 6, 8, 9, 10, 20)
large <- 2

largestreet <- street %>%
  filter(OBJEKTART %in% large)
mediumstreet <- street %>%
  filter(OBJEKTART %in% medium)
smallstreet <- street %>%
  filter(OBJEKTART %in% small)

ggplot(smallstreet)+
  geom_sf(aes(color = factor(OBJEKTART)))

unique(street$OBJEKTART)

# map ---------------------------------------------------------------------

ggplot()+
  geom_sf(data = HK, aes(fill = fill), color = "#F89F5B")+

  geom_sf(data = river, aes(fill = fill), fill = "#288994", color = "#288994") +
  geom_sf(data = rail, aes(fill = fill), color = "#653780") +
  geom_sf(data = smallstreet, aes(fill = fill), size = 0.1, fill = "transparent", color = "black") +
  geom_sf(data = mediumstreet, aes(fill = fill), size = 0.2, fill = "transparent", color = "black") +
  geom_sf(data = largestreet, aes(fill = fill), size = 0.4, fill = "transparent", color = "black") +
  geom_sf(data = build, aes(fill = fill), color = "transparent", fill = "#E53F71")+

  theme_void()+
  theme(legend.position = "none") -> map


ggsave(filename = paste0(here(), "/maps/test.svg"), plot = map, units = "cm",
       height = 77, width = 58, bg = "#e9e5d9")
