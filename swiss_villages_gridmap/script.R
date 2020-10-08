#Author: Tobias Stalder

#map on points of interest for Switzerland

#load libraries
library(sf)
library(tidyverse)
library(colorspace)
library(data.table)
library(extrafont)
extrafont::fonttable()


#load shapefile from geogabrik.de
CH_PLACES = st_read(dsn = r"(C:\Users\tobia\Documents\geodat\OSM_switzerland\gis_osm_places_free_1.shp)")

#load administrative data from AU and CH
CH_CANTON <- st_read(dsn = r"(C:\Users\tobia\Documents\geodat\admin_switzerland\BOUNDARIES_2020\DATEN\swissBOUNDARIES3D\SHAPEFILE_LV95_LN02\swissBOUNDARIES3D_1_3_TLM_KANTONSGEBIET.shp)")



#POI mapping classes: hospital, school, museum, zoo, library


CH_PLACES <- st_transform(CH_PLACES, 2056)


CH_VILLAGES = CH_PLACES %>%
  filter(fclass == "village")


# create fishnet grid to later join the points count to (attention, dont use wgs84!) ---------------------------------------------------------

#code for a fishnet map:
grid_all <- st_make_grid(CH_CANTON,
                     cellsize =5 * 1000,
                     # Kms
                     
                     what = "polygons",
                     square = TRUE
)

# plot(grid)
st_as_sf(grid_all) -> gridSF




#get join count of villages per polygon on grid

gridSF$villages <- lengths(st_intersects(gridSF, CH_VILLAGES))

ggplot() +
  geom_sf(data = gridSF, aes(fill = villages), color = "darkgrey") +
  scale_fill_gradientn(colors = c("#040404",
                                  "#1E1431",
                                  "#3D1C49",
                                  "#60235D",
                                  "#832C6B",
                                  "#A53B70",
                                  "#C4516C",
                                  "#DE6C5B",
                                  "#F08D3C",
                                  "#F5B247",
                                  "#F8D76C",
                                  "#FFFE9E"),
                       breaks=c(0,2,4,6,8,10,12), 
                       limits = c(0,14))+
  ggtitle("Swiss villages with <10'000 inhabitants") +
  labs(subtitle = "3546 individual villages are mapped on OSM",
       caption = "plot by @toeb18 | Data: Swisstopo, Geofabrik.de, OpenStreetMap")+
  guides(fill = guide_colorsteps(barwidth = 20, barheight = .5))+ 
  theme_void() +
  theme(legend.position = "bottom",
                       panel.background = element_rect(fill = "white", color = "white"),
                       plot.background = element_rect(fill = "white", color = "white"),
                       text = element_text(color = "black", family = "Bahnschrift"),
                       plot.title = element_text(family = "Bahnschrift"),
        legend.title = element_blank())
