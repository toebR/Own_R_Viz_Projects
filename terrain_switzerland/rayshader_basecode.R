
# meta --------------------------------------------------------------------

# Dataviz on Switzerlands Topography
# By Tobias Stalder
# March 2022
# behance.org/tobiasstalder
# github: toebR



# libraries ---------------------------------------------------------------

memory.limit(size=4000000000) #set memory limit

library(tidyverse) #wrangling and ggplot
library(sf) #sf features for polygons etc
library(rayshader) #3d rendering
library(here) #working directory management
library(raster) #raster wrangling
library(rgdal) #gis engine
library(rgl) #3d rendering engine
library(rayrender) #3d rendering



# #data (cannot be share due to copyright but can be downloaded for free--------

# geotiff of elevation for transect (pre-processed in qgis for crs reasons)
elev_img <- raster::raster(paste0(here(), "\\data\\DHM25_MM_ASCII_GRID\\ASCII_GRID_1part\\dhm25_rect_WGS84.tif"))

#geotiff of elevation for transec in 200m resolution (lower res than elev_img)
elev_img_200 <- raster::raster(paste0(here(), "\\data\\DHM25_MM_ASCII_GRID\\ASCII_GRID_1part\\dhm200_transect_wgs84.tif"))

#geotiff elevation data for all of Switzerland in 200m res for minimap
CH_elev <- raster::raster(paste0(here(), "\\data\\DHM25_MM_ASCII_GRID\\ASCII_GRID_1part\\dhm200_CH_WGS84.tif"))

#transect bounding box as polygon for minimap
transect <- st_read(paste0(here(), "\\data\\data_gemeinden\\polygon square raster.shp"))


#transform geotiff to matrix
elev_matrix <- matrix(
  raster::extract(elev_img, raster::extent(elev_img), buffer = 1000), 
  nrow = ncol(elev_img), ncol = nrow(elev_img)
)


dim(elev_matrix) #check matrix dimensions

#calculate rayshade and ambient shadow for transect
ambmat <- ambient_shade(elev_matrix, zscale = 10)
raymat <- ray_shade(elev_matrix, zscale = 10, lambert = TRUE)

#check if dimensions of shadow layers and the elevation matrix match!
dim(elev_matrix)
dim(ambmat)
dim(raymat)


#plot transect 2d for check
elev_matrix %>%
  sphere_shade(texture=create_texture("#6D597A","#355070",
                                      "#B56576","#E56B6F","#EAAC8B"),
               sunangle = 45) %>%
  add_shadow(raymat, max_darken = 0.2) %>%
  add_shadow(ambmat, max_darken = 0.2) %>%
  plot_map()

# plot 3D with rayshader
elev_matrix %>%
  sphere_shade(texture=create_texture("#6D597A","#355070",
                                      "#B56576","#E56B6F","#EAAC8B"),
               sunangle = 45) %>%
  add_shadow(raymat, max_darken = 0.5) %>%
  add_shadow(ambmat, max_darken = 0.3) %>%
  plot_3d(elev_matrix, zscale =10, windowsize = c(2000, 600), zoom = 0.5)
render_snapshot(filename = paste0(here(),"/terrain_model.png")) #render snapshot (rgl window was enlarged since the output quality of the
                                                                #snapshot depends on monitor resolition



# ggplot minimap ----------------------------------------------------------

#calculate hillshade from CH_elev 200m resolution
slope <- terrain(CH_elev, opt='slope')
aspect <- terrain(CH_elev, opt='aspect')
hill <- hillShade(slope, aspect, 40, 270)

#convert to dataframe and remove NA (since hill will have a lot of NaN due to rectangular extent calculation)
as.data.frame(hill, xy=TRUE) -> CH_hill_df
CH_hill_df %>%
  drop_na(layer) -> CH_hill_df

#calculate contour of transect
rasterToContour(elev_img_200) -> elev_img_200_contour
plot(elev_img_200_contour)
st_as_sf(elev_img_200_contour) %>%
  mutate(level = as.numeric(level))-> elev_img_200_contour

#plot ggplot2: Switzerland hillshade + contour of transect

  ggplot(CH_hill_df)+
  geom_raster(aes(x = x, y =y, fill = layer))+
  geom_sf(data = elev_img_200_contour, aes(color = level))+
  scale_color_gradient(low = "#6D597A", high = "#B56576")+
  geom_sf(data = transect, aes(fill = id), fill = "transparent", color = "gray16") +
  scale_fill_gradient(low = "#696969", high = "#DCDCDC")+
  theme_void() +
    theme(legend.position = "none") -> minimap

  ggsave(minimap, height = 10, width = 15, filename = paste0(here(), "/minimap.svg")) #save as svg
  
  #composition of viz in inkscape
