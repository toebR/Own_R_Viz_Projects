#male a map of traffic, railroad, roads etc. in switzerland for twitter :))
library(sf)
library(shinyjs)
library(tmaptools)
library(tidyverse)
library(ggthemes)
library(cowplot)
library(ggpubr)

palette_explorer()
print(get_brewer_pal("YlOrRd", n = 7))
blues <- get_brewer_pal("Blues", n = 9, contrast = c(0.39, 0.95))
greys <-get_brewer_pal("Greys", n = 13, contrast = c(0.33, 1))
print(greys)

dir <- c("C:\\Users\\tobia\\OneDrive\\Desktop\\ownprojects\\R\\mapping\\OSM_CH")
setwd(dir)

shps <- list.files(dir, pattern = "\\.shp$")
shps

CH <- st_read(dsn = "CH.shp")
CH <- st_transform(CH, 4236)

waterbodies <- st_read(dsn = "clc2018_waterbodies.shp")
head(waterbodies)

railways <- st_read(dsn = "gis_osm_railways_free_1.shp")
head(railways)

lengthM <- st_length(railways)
lengthKM <- sum(as.numeric(lengthM/1000))
lengthKM

map <- ggplot() +
  geom_sf(data = CH, aes(group = NAME), fill = "#A8A8A8", color = "#A8A8A8") +
  geom_sf(data=waterbodies, aes(group = LABEL1), color = "#3686BF", fill = "#3686BF") +
  # geom_sf(data = railways, aes(color = tunnel), color = "#FEC05B", size = 1.3) +
  geom_sf(data = railways, aes(color = tunnel), color = "black", size = 0.5) +
  ggtitle("Railways in Switzerland", subtitle = paste("A whooping total of", round(lengthKM), "Kilometers")) +
  labs(caption = "plot by @toeb18 ¦ Source: Open Street Map") +
  theme(plot.background = element_rect("white"),
        panel.background = element_rect("white"),
        plot.title = element_text(color = "#202020", hjust = - 0.41, vjust = 22),
        plot.subtitle = element_text(color = "#202020", hjust = - 0.465, vjust = 26),
        plot.caption = element_text(color = "#202020", hjust = 1.2), 
        panel.grid = element_blank(),
        axis.text = element_blank(),
        plot.margin = unit(c(5,5,1,10),"lines"))


map


#make barplot or some shit on flcass attribute of railways and put on map with ggdraw
head(railways)
summary(railways$fclass)

railways$lengthKM <- as.numeric(st_length(railways)/1000)
#sum by group
classes <- data.frame(aggregate(railways$lengthKM, by = list(Category = railways$fclass), FUN = sum))

colnames(classes) <- c("Category", "Length [KM]")
classes$`Length [KM]` <- round(classes$`Length [KM]`)

#order dataframe
classes <- classes[order(classes$`Length [KM]`),]


#calculate count per fclass in new dataframe and make plot!
bar <- ggplot(classes) +
  geom_bar(aes(x = reorder(Category ,classes$`Length [KM]`), y = classes$`Length [KM]`),
           stat = "identity",
           color = "#A8A8A8",
           fill = "#A8A8A8",
           width = .8,
           position = "dodge") +
  ylab("Total Length [Km]") +
  xlab("Category") +
  coord_flip() +
  rotate_x_text() +
  ggtitle("", subtitle = "Kilometers of Railway") +
  theme(plot.background = element_rect("transparent"),
        panel.background = element_rect("transparent"),
        plot.title = element_text(color = "#A8A8A8"),
        plot.subtitle = element_text(color = "#A8A8A8"),
        plot.caption = element_text(color = "#A8A8A8"), 
        panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(color = "#A8A8A8", size = 10),
        axis.text.x = element_blank(),
        # axis.title.x = element_text(color = "#202020", size = 10),
        axis.title = element_blank(),
        axis.line = element_blank(),
        axis.ticks.length = unit(0, "pt"),
        panel.border = element_blank(),
        panel.spacing = unit(0, "pt"),
        panel.grid = element_blank(),
        plot.margin = margin(0,0,0,0,"null"))
bar
plot <- cowplot::ggdraw() +
  draw_plot(map, 0,0,1,1) +
    draw_plot(bar,0.025, 0.45, 0.32, 0.4) 
plot
ggsave(plot, filename = "test1.png", width = 40, height = 20, units = "cm")


