#visualisation on submarine cables and landing points
#data source: https://public.opendatasoft.com/explore/?sort=modified&q=submarine

#by Tobias Stalder
#November 2020

library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(Cairo)
library(extrafont)
loadfonts(device = "win")
options(scipen=999)

setwd(r"(C:\Users\tobia\Desktop\ownprojects\dataviz\submarine connections)")

#load data:
cables <- st_read(dsn = r"(C:\Users\tobia\Desktop\ownprojects\dataviz\submarine connections\data\submarine-cables-cables.shp)")
points <- st_read(dsn = r"(C:\Users\tobia\Desktop\ownprojects\dataviz\submarine connections\data\submarine-cables-landing-points.shp)")



# world map cables --------------------------------------------------------


ne_countries(scale = 110, type = "countries", continent = NULL,
             country = NULL, geounit = NULL, sovereignty = NULL,
             returnclass = c("sf")) -> world

world %>%
  filter(continent != "Antarctica") -> world


ggplot(world) +
  geom_sf(aes(fill = scalerank), fill = "black", color = "black") +
  geom_sf(data = cables, aes(alpha = length, color = length), size = .3) +
  scale_color_gradientn("Length [Km]", colours = c("#00838f", "#00acc1", "#26c6da", "#80deea"))+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  theme_void() +
  theme(plot.background = element_rect(fill = "#293133", color = "#293133"),
        legend.position = "none",
        text = element_text(family = "Nirmala UI", color = "darkgrey")) +
  guides(color = guide_colorsteps(barwidth = 10, barheight = .5),
         alpha = guide_none())+
  ggsave(filename = "map.png", dpi = 600, width = 29.7, height = 11.84, units = "cm", type = "cairo")



# data analysis cables dataset --------------------------------------------
#1) cummulative total length of cables (approx. 1428243 KM to 2020) -> text
cables %>%
  filter(rfs %in% c(seq(1989, 2020, 1))) -> to2020
sum(to2020$length, na.rm = TRUE)

#2) longest cable (SeaMeWe-3, rfs 1999, 39000 KM, landing points in Egypt, Indonesia, Phillipines, Greece, India, vietnam, hong-kong, china, Taiwan, Djbouti, UK, Saudi Arabia..., multiple ownsers!)
cables %>%
  st_drop_geometry() %>%
  filter(length == max(length, na.rm = TRUE)) -> longcab

#2) how many owners, which are the biggest, how many km do they own -> text
#result:307 owners (partnerships and individual owners.), XL Axiata owns the most cables as a single owner, specifically 11.
cables %>%
  count(owners) %>%
  arrange(n, "desc")-> owncount


#3) lineplot rfs, through years show how many cables were ready and how many km -> plot
cables$rfs <- as.numeric(cables$rfs)

cables %>%
  st_drop_geometry() %>%
  group_by(rfs) %>%
  summarise(length = sum(length, na.rm = TRUE)) %>%
  mutate(cumlength = cumsum(length))-> cumsum

ggplot(cumsum) +
  geom_area(aes(x = rfs, y = cumlength), color = "#00838f", size = 1, fill = "#00838f", alpha = .3) +
  xlab("Year") +
  ylab("Cummulative Length [Km]") +
  scale_x_continuous(limits = c(1989, 2020)) +
  theme(plot.background = element_rect(fill = "#293133", color = "#293133"),
        panel.background = element_rect(fill = "#293133", color = "#293133"),
        panel.grid = element_blank(),
        legend.position = "bottom",
        axis.line = element_line(color = "darkgrey"),
        axis.text = element_text(color = "darkgrey"),
        text = element_text(family = "Nirmala UI", color = "darkgrey", size = 16)) +
  ggsave(filename = "plot_rfs.png", dpi = 600, width = 12, height = 10, units = "cm", type = "cairo")


# data analysis land point dataset ----------------------------------------
#1) total landing points (1183)
nrow(points)

#2) list of countries with the most landing points -> gtable or bar plot
points %>% 
  st_drop_geometry() %>%
  count(landing_poi) -> points_n
  
top_n(points_n, 10) -> top_n
filter <- c(top_n$landing_poi)


points_n %>%
  filter(landing_poi %in% filter) -> points_n_fin


ggplot(data = points_n_fin, aes(x = reorder(landing_poi, n), y = n)) +
  geom_col(fill = "#00838f", alpha = .5) +
  xlab("") +
  ylab("Amount of Landing Points")+
  geom_text(aes(label = landing_poi), angle = 90, family ="Nirmala UI", color = "grey", nudge_y = -7) +
  theme(plot.background = element_rect(fill = "#293133", color = "#293133"),
        panel.background = element_rect(fill = "#293133", color = "#293133"),
        panel.grid = element_blank(),
        legend.position = "bottom",
        axis.line = element_line(color = "darkgrey"),
        axis.text.y = element_text(color = "darkgrey"),
        axis.text.x = element_blank(),
        text = element_text(family = "Nirmala UI", color = "darkgrey", size = 16)) +
  ggsave(filename = "plot_points.png", dpi = 600, width = 12, height = 10, units = "cm", type = "cairo")
