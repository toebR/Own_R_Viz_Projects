#the beatles on spotify
#by tobias stalder nov 2020

# devtools::install_github('charlie86/spotifyr')


# load libraries ----------------------------------------------------------


library(tidyverse)
library(spotifyr)
library(lubridate)
library(Cairo)
library(extrafont)
loadfonts(device = "win")

options(scipen = 999)

# install spotify API -----------------------------------------------------

id <- 'your spotify client/app id'
secret <- 'your spotify secret id'
Sys.setenv(SPOTIFY_CLIENT_ID = id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = secret)
access_token <- get_spotify_access_token()



#get data on the beatles (Joshua de la Bruere @delaBJL helped a lot with this!)


get_artist_albums("3WrFJ7ztbogyGnTHbHJFl2",include_groups = c("album" ,
                                                              "single" ,
                                                              "appears_on",
                                                              "compilation"))-> beatle_albums

get_artist_albums("3WrFJ7ztbogyGnTHbHJFl2",include_groups = c("compilation"))-> beatle_compilations



# make the viz  ------------------------------------------------------------

# data prep: get nr of tracks per year ------------------------------------


beatles <- rbind(beatle_albums, beatle_compilations)


beatles$release_date <- as.POSIXct(beatles$release_date)
beatles %>%
  filter(id != "2gUfkZ9jhhYinKIunu7wxo") %>% #get rid of a double entry
  mutate(year = lubridate::year(release_date)) %>%
  group_by(year) %>%
  summarise(tracks = sum(total_tracks)) -> beatles_years


# make the viz look like a vinyl plate ------------------------------------------------------------


beatles_years %>%
  ggplot(aes(x = year, y = tracks)) +
  
  #background
  annotate(geom = "rect",
           fill = "gray4",
           ymin = -35,
           ymax = 130,
           xmin = 1960,
           xmax = 2020)+

  #scale for nr of tracks
  geom_hline(yintercept = 20, color = "gray15", size = .6)+
  geom_hline(yintercept = 60, color = "gray15", size = .6)+
  geom_hline(yintercept = 100, color = "gray15", size = .6)+
  
  #point for number of tracks in data
  geom_point(color = "#1db954", size = 2.5) +

  #seperator for the years
  geom_vline(xintercept = 1960,
             color = "white", linetype = "dashed")+
  
  #segment for lollipop look
  geom_segment(aes(x = year,
                   y =-3, 
                   xend=  year, 
                   yend=tracks),
               color = "#1db954",
               size = .8) +

  #inner green circle for the vinyl plate looks
  annotate(geom = "rect",
           fill = "#1db954",
           ymin = -35,
           ymax = -5,
           xmin = 1960,
           xmax = 2020)+
  

  #annotated scale for nr of tracks
  annotate(geom = "text", x = 1980, y = 26, label = "20", color = "#1db954", angle = -30, size = 4, family = "Verdana")+
  annotate(geom = "text", x = 1980, y = 66, label = "60", color = "#1db954", angle = -30, size = 4,family = "Verdana")+
  annotate(geom = "text", x = 1980, y = 108, label = "100", color = "#1db954", angle = -30, size = 4, family = "Verdana")+
  
  #annotated scale for the most important years
  annotate(geom = "text", x = 1967, y = 120, label = "1967\n\n\n", color = "gray8", angle = -41, size = 4, family = "Verdana")+
  annotate(geom = "text", x = 2018, y = 120, label = "2018\n\n\n", color = "gray8", angle = 12, size = 4, family = "Verdana")+
  annotate(geom = "text", x = 1996, y = 120, label = "\n\n\n1996", color = "gray8", angle = -35, size = 4, family = "Verdana")+
  
  #annotate scale title for nr of tracks
  annotate(geom = "text", x = 1980, y = 130, label = "\n\nReleased\nTracks", color = "gray12", angle = 63, size = 4, family = "Verdana")+
  
  #title, subtitle, caption
  ggtitle("\nThe Beatles")+
  labs(subtitle = "\nTrack Releases And Re-Releases\nin Albums and Compilations Through Time\n",
       caption = "Data Visualisation by Tobias Stalder\ntobias-stalder.netlify.app\nSource: Spotify API via {spotifyr}")+

  #scale breaks just for transformation, we dont see it on plot
  scale_y_continuous(limits = c(-40, 130), expand = c(0,0), breaks = c(20,
                                                                       40,
                                                                       60,
                                                                       80,
                                                                       100,
                                                                       120))+
  scale_x_continuous(expand = c(0,0))+
  
  coord_polar() +
  
  #theming
  theme(panel.background = element_rect(fill = "white", color = "transparent"),
        panel.grid.major.x  = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.background = element_rect(color = "white"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major.y  = element_line(color = "white"),
        axis.title = element_blank(),
        text = element_text(color = "black"),
        plot.title = element_text(size = 50, hjust = .5, family = "Bell MT", color = "gray8"),
        plot.subtitle = element_text(size = 20, hjust = .5, family = "Bell MT", color = "gray8"),
        plot.caption = element_text(size = 10, hjust = .5, family = "Bell MT", color = "gray8"))  -> p
p

#save output
ggsave(plot =p, filename = "vinyl_visualisation_beatles.png", path = r"(C:\Users\tobia\Desktop\ownprojects\dataviz\spotify_titles)",
       width = 20, height = 25, units = "cm", dpi = 300, type = "cairo")
