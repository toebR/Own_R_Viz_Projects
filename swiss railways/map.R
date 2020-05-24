#male a map of traffic, railroad, roads etc. in switzerland for twitter :))
library(sf)
library(shinyjs)
library(tmaptools)
library(tidyverse)
library(ggthemes)
library(cowplot)
library(ggpubr)
library(patchwork)

palette_explorer()
print(get_brewer_pal("YlOrRd", n = 7))
blues <- get_brewer_pal("Blues", n = 30, contrast = c(0.39, 0.95))
greys <-get_brewer_pal("Greys", n = 13, contrast = c(0.33, 1))
print(greys)

dir <- c("C:\\Users\\tobia\\OneDrive\\Desktop\\ownprojects\\R\\own_projects\\mapping_swiss_rails\\OSM_CH")
setwd(dir)
getwd()

shps <- list.files(dir, pattern = "\\.shp$")
shps

CH <- st_read(dsn = "CH.shp")
CH <- st_transform(CH, 4236)


railways <- st_read(dsn = "gis_osm_railways_free_1.shp")
head(railways)




lengthM <- st_length(railways)
lengthKM <- sum(as.numeric(lengthM/1000))

railways$lengthKM <- as.numeric(st_length(railways)/1000)

#sum by group
classes <- data.frame(aggregate(railways$lengthKM, by = list(Category = railways$fclass), FUN = sum))

colnames(classes) <- c("Category", "Length [KM]")
classes$`Length [KM]` <- round(classes$`Length [KM]`)

#order dataframe
classes <- classes[order(classes$`Length [KM]`),]


#barplot
bar <- ggplot(classes) +
  geom_bar(aes(x = reorder(Category ,classes$`Length [KM]`), y = classes$`Length [KM]`),
           stat = "identity",
           color = "cyan",
           fill = "cyan",
           width = .9,
           position = "dodge",
           alpha = 0.8) +
  ylab("Total Length [Km]") +
  xlab("Category") +
  coord_flip() +
  rotate_x_text() +
  ggtitle("Kilometers of Swiss Railway") +
  labs(caption = "plot by @toeb18 ¦ Source: Open Street Map") +
  theme(text = element_text(family = 'Gill Sans', color = "#969696"),
        plot.background = element_rect(fill= "#444B5A"),
        panel.background = element_rect(fill = "#444B5A"),
        legend.background = element_rect(fill = "#444B5A"),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        plot.title = element_text(size = 16),
        plot.subtitle  = element_text(size = 12),
        axis.title = element_blank(),
        axis.text = element_text(color = "#969696"),
        panel.grid = element_blank())
bar
ggsave(bar, filename = "barplot_length.png", width = 15, height = 10, units = "cm")

#map

map <- ggplot() +
  geom_sf(data = CH, aes(group = NAME), fill = "transparent", color = "#969696", size = 1) +
  geom_sf(data = railways, aes(color = tunnel), color = alpha("cyan", 0.3), size = 1) +
  ggtitle("Railways in Switzerland", subtitle = paste("A whooping total of", round(lengthKM), "kilometers")) +
  labs(caption = "plot by @toeb18 ¦ Source: Open Street Map") +
  theme(text = element_text(family = 'Gill Sans', color = "#969696"),
        plot.background = element_rect(fill= "#444B5A"),
        panel.background = element_rect(fill = "#444B5A"),
        legend.background = element_rect(fill = "#444B5A"),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        plot.title = element_text(size = 16),
        plot.subtitle  = element_text(size = 12),
        axis.title = element_text(size = 12, color = '#969696'),
        axis.text = element_text(color = "#969696"),
        plot.margin = margin(c(.5,.5,.5,.5),"cm"))
ggsave(map, filename = "map_cyan.png", width = 20, height = 20, units = "cm")
