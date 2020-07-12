#author: Tobias Stalder
#note: most of the code was transformed out of a script that can be found here https://github.com/isabellabenabaye/life-chart


dir = "C:\\Users\\tobia\\OneDrive\\Desktop\\ownprojects\\R\\own_projects\\CV_graphic"
setwd(dir)

library(tidyverse)
library(lubridate)
library(tmaptools)
library(prismatic)


# Create the data-----
life_data <-
  tibble(months = factor(rep(month.abb[1:12], 29), levels=month.abb[1:12]),
         age = rep(0:28, each = 12),
         era = "text")  %>%
  rowid_to_column("row_name") %>%
  mutate(era = fct_inorder(case_when(row_name < 234 ~ "School",
                                     row_name < 262 ~ "Warehouse Specialist",
                                     row_name < 312 ~ "Software Test Engineer",
                                     row_name < 349 ~ "Project Engineer in R&D"))) %>%
  mutate(ed = fct_inorder(case_when(row_name<69 ~ "Nothing",
                                    row_name < 222 ~ "School",
                                     row_name < 241 ~ "Detour in Biomedical Sciences",
                                     row_name < 304 ~ "BSc in Geography and Sustainable Development",
                                     row_name < 349 ~ "MSc in Physical Geography & GIS")))

life_data = life_data[-(1:11),]
life_data = life_data[-(333:337),]
life_data = data.frame(life_data)
view(life_data)
tail(life_data)
head(life_data)
#plot on job experience

ggplot(life_data) +
  geom_point(aes(x = months, y = age, color = era), size = 5, shape = 15, show.legend = FALSE) +
  scale_color_manual(values = c("#B0B0B0", "#41A1D9", "#C260DB", "#F5AD4F"))+
  theme_void() +
ggsave(filename = "job_plot.png", height = 15, width = 6, units = "cm") 

#plot on education

ggplot(life_data) +
  geom_point(aes(x = months, y = age, color = ed), size = 5, shape = 15, show.legend = FALSE) +
  scale_color_manual(values = c("#B0B0B0","#4DD68B", "#41A1D9", "#C260DB", "#F5AD4F"))+
  theme_void() +
  ggsave(filename = "ed_plot.png", height = 15, width = 6, units = "cm") 

#post processing and design will be done in inkscape (no time to create textgrobs and labels in ggplot2..)
