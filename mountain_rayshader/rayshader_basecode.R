
# meta --------------------------------------------------------------------

# Dataviz on Elevation of Dente Blanche
# By Tobias Stalder
# April 2022
# behance.org/tobiasstalder
# github: toebR
#data is a geotiff of the Swissalti3D product by swisstopo


# libraries ---------------------------------------------------------------

memory.limit(size=4000000000) #set memory limit

library(tidyverse) #wrangling and ggplot
library(rayshader) #3d rendering
library(here) #working directory management
library(raster) #raster wrangling
library(rgdal) #gis engine
library(rgl) #3d rendering engine
library(rayrender) #3d rendering



# #data (cannot be share due to copyright but can be downloaded for free--------

# geotiff of elevation for transect (pre-processed in qgis for crs reasons)
elev_img <- raster::raster(paste0(here(), "\\data\\dent blanche\\dent_blanche_swissalti3D_2m.tif"))
plot(elev_img)



#transform geotiff to matrix
elev_matrix <- matrix(
  raster::extract(elev_img, raster::extent(elev_img), buffer = 1000), 
  nrow = ncol(elev_img), ncol = nrow(elev_img)
)


dim(elev_matrix) #check matrix dimensions

#calculate rayshade and ambient shadow for transect
ambmat <- ambient_shade(elev_matrix, zscale = 5)
raymat <- ray_shade(elev_matrix, zscale = 5, lambert = TRUE)

#check if dimensions of shadow layers and the elevation matrix match!
dim(elev_matrix)
dim(ambmat)
dim(raymat)


#plot 2D for check (good sunangle etc)
elev_matrix %>%
  sphere_shade(texture=create_texture("#22223b","#4a4e69","#9a8c98","#c9ada7", "#f2e9e4"),
               sunangle = 10) %>%
  add_shadow(raymat, max_darken = 0.2) %>%
  add_shadow(ambmat, max_darken = 0.2) %>%
  plot_map()

# plot 3D with rayshader (repeated with water depths corresponding to elevation of mount everest and dente blanche to later re-align in inkscape to the right plain dimensions)
elev_matrix %>%
  sphere_shade(texture=create_texture("#393d3f","#fdfdff",
                                      "#c6c5b9","#62929e","#546a7b"),
               sunangle = 80) %>%
  add_shadow(raymat, max_darken = 0.5) %>%
  add_shadow(ambmat, max_darken = 0.3) %>%
  plot_3d(elev_matrix, zscale =3, windowsize = c(1500, 1500), zoom = 0.7, phi = 30, theta = 120,
          # baseshape = "hex",
          water = TRUE, waterdepth = 0, wateralpha = 0, watercolor = "transparent",
          waterlinecolor = "black", waterlinealpha = 0.1
          )
render_snapshot(filename = paste0(here(),"/model_sea level_.png")) #render snapshot (rgl window was enlarged since the output quality of the
                                                                #snapshot depends on monitor resolition
  
  #composition of viz in inkscape
