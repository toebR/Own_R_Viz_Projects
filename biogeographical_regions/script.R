setwd(r"(C:\Users\tobia\Desktop\ownprojects\dataviz\biogeographical regions europe)")

library(sf)
library(tidyverse)
library(extrafont)

loadfonts(device = "win")
shape <- st_read(dsn = r"(C:\Users\tobia\Desktop\ownprojects\dataviz\biogeographical regions europe\BiogeoRegions2016.shp)")

europe <- shape %>%
  filter(code != "Outside")
europe$general = 1

europe_base <- europe
europe_base$code = NULL

data.frame(st_coordinates(europe_base)) -> coords
max(coords$X)
min(coords$X)

ggplot() +
  geom_sf(data = europe_base, aes(fill = general), fill = "black", color = "transparent", show.legend = FALSE) +
  geom_sf(data = europe, aes(fill = code), color = "transparent" ,show.legend = FALSE, fill ="#138d75" ) +
  facet_wrap(.~ code, ncol = 4) +
  scale_x_continuous(limits = c(943611, 8500000))+
  ggtitle("The Diverse Biogeographical Regions of Europe")+
  labs(caption = "Map by @toeb18\nData: european environmental agency\nGithub: https://github.com/toebR",
       subtitle = "Europe contains 11 distinct biogeographical regions defined by the European Environmental Agency.\nThe biodiversity in each region is threatened by different factors.\n") +
  theme_dark() +
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = "#293133"),
        plot.background = element_rect(fill = "#293133"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(), 
        plot.title = element_text(color = "lightgrey", family = "Bahnschrift", size = 18),
        plot.subtitle = element_text(color = "lightgrey", size = 10),
        plot.caption = element_text(color = "lightgrey", size = 8),
        text = element_text(family = "Bahnschrift", color = "lightgrey", hjust = .5),
        strip.background = element_rect(fill ="#293133", color = "#293133")) -> map

  ggsave(map, filename = "map.png", width = 20, height = 18, units = "cm", dpi = 300)
