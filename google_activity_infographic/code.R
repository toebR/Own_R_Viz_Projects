#Author: Tobias Stalder
#Date: 25.03.2022
#Description: Script to analyse and plot data of google activity from google takeout



# libraries ---------------------------------------------------------------

library(lubridate)
library(rvest)
library(tm)
library(tidyverse)
library(sysfonts)
library(showtext)

font_add_google("work sans")

# data --------------------------------------------------------------------

fileHTML <- r"(D:\data\google_activity\Takeout\My Activity\Search\My Activity.html)"
mySearchFile <- read_html(fileHTML, encoding = "UTF-8")



# wrangling ---------------------------------------------------------------


## html scrap --------------------------------------------------------------


## The following webscraping steps of the html file are based on a tutorial by Saúl Buentello Sánchez with minor modifications in the regex expressions
# https://towardsdatascience.com/explore-your-activity-on-google-with-r-how-to-analyze-and-visualize-your-search-history-1fb74e5fb2b6

# SCRAPPING SEARCH DATE AND TIME
dateSearch <- mySearchFile %>% 
  html_nodes(xpath = '//div[@class="mdl-grid"]/div/div') %>% 
  str_extract(pattern = "<br>\\s*(.*?)\\s*CET</div>") %>%
  str_sub(5) %>%
  str_sub(1,nchar(.)-10) 
gsub(",", "",dateSearch) -> dateSearch
dmy_hms(dateSearch) -> dateSearch #3 failed to parse (ok for me)


# SCRAPING SEARCH TEXT
textSearch <- mySearchFile %>% 
  html_nodes(xpath = '//div[@class="mdl-grid"]/div/div') %>%
  str_extract(pattern = '(?<=<a)(.*)(?=</a>)') %>% 
  str_extract(pattern = '(?<=\">)(.*)')
textSearch[1:5]
# SCRAPING SEARCH TYPE
searchType <- mySearchFile %>% 
  html_nodes(xpath = '//div[@class="mdl-grid"]/div/div') %>% 
  str_extract(pattern = "(?<=mdl-typography--body-1\">)(.*)(?=<a)") %>% 
  str_extract(pattern = "(\\w+)(?=\\s)")
searchType[1:5]
# CREATE DATA FRAME USING SCRAPED DATA
searchedData <- tibble(timestamp = dateSearch,
                       date = as_date(dateSearch),
                       year = year(dateSearch),
                       month = month(dateSearch, label = TRUE),
                       day = weekdays(dateSearch),
                       hour = hour(dateSearch),
                       type = searchType,
                       search = textSearch)
searchedData$day <- factor(searchedData$day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday","Thursday", "Friday", "Saturday"))
searchedData <- na.omit(searchedData)
head(searchedData)

nrow(searchedData)

searchedData %>%
  filter(type == "Searched") %>%
  mutate(day = factor(day,levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) -> searchedData


# Analysis ----------------------------------------------------------------

#counts by date
searchedData %>%
  group_by(date, day, hour) %>%
  summarise(count = n()) %>%
  ungroup() -> date_count

#summarise counts per daytime hour over all weekdays (mean)
date_count %>%
  group_by(day, hour,) %>%
  summarise(mean = mean(count)) -> hour_mean


#summarise counts over the months and years (searched) -> timeline
searchedData %>%
  group_by(month, year) %>%
  summarise(count = n())  -> month_total

#monthly mean to see seasonal activity
month_total %>%
  group_by(month) %>%
  summarise(mean = mean(count, na.rm = TRUE)) -> month_mean #highest: May with 184 mean counts and lowest August with a mean of 70 counts.
  


# read out highest search events:

#highest search count in:
# year
searchedData %>%
  group_by(year) %>%
  summarise(n()) # 2020, with 2495 searches

#month
month_total %>%
  arrange(desc(count)) #march 2020 with 475 searches

#day (date)
searchedData %>%
  group_by(date) %>%
  summarise(count =n()) %>%
  arrange(desc(count)) #2020-05-27 and 2020-09-27 with the highest search counts of 63



# plots -------------------------------------------------------------------

showtext_auto() #needed for work sans to work in plot window

#barplot of total counts per month over the whole timespand
ggplot()+
  geom_col(data = month_total, aes(x = month, y = count, fill = count), color = "transparent", show.legend = FALSE) +
  ggforce::facet_row(~year,  scales = "free_x",  space = "free", strip.position="left") +
  scale_y_continuous(expand =  c(0,0.1)) +
  scale_x_discrete(breaks = c("Jan", "Apr", "Aug", "Dec"))+
  scale_fill_gradient(low = "#0091ad", high = "#b7094c") +
  xlab("")+
  ylab("Total Search Count")+
  theme(axis.ticks = element_blank(),
        text = element_text(family = "work sans"),
        strip.background = element_rect(fill = "transparent", color = "transparent"),
        panel.background = element_rect(fill = "transparent", color = "transparent"),
        strip.text = element_text(hjust = 0, size = 8),
        axis.text.x = element_text(hjust = 0.5,vjust=0.3, size=7, angle = 90),
        axis.text.y = element_text(angle = 90),
        panel.grid.major.y = element_line(color = "darkgrey", linetype = "solid"),
        plot.background = element_rect(color = "transparent", fill = "transparent")) -> P1
P1
ggsave(P1, filename = "p1.svg", width = 27.5, height = 10, units = "cm", dpi = 300)


#heatmaps of density of search counts over the hours of a day. Not sure if it will be incoorporated in the viz.
ggplot()+
  geom_density2d_filled(data = hour_mean, aes(x = hour, y = mean), bins = 10, alpha = 0.8)+
  ggforce::facet_col(~day,  scales = "free_x",  space = "free", strip.position="left") +
  scale_y_continuous(expand =  c(0,0)) +
  scale_x_continuous(breaks = c(2,6,10,14,18, 22), expand = c(0, 0))+
  scale_alpha_discrete(range = c(0.5,1))+
 scale_fill_manual("",values = c("#00667b", "#0091ad",  "#1780a1",  "#2e6f95",  "#455e89", 
                              "#5c4d7d",  "#723c70",  "#892b64",  "#a01a58", "#b7094c")) +
  xlab("Hour")+
  ylab("Mean Search Count")+
  theme(legend.position = "bottom",
        legend.text = element_blank(),
        axis.ticks = element_blank(),
              text = element_text(family = "work sans"),
              strip.background = element_rect(fill = "transparent", color = "transparent"),
              panel.background = element_rect(fill = "transparent", color = "transparent"),
              strip.text = element_text(hjust = 0, size = 8),
              axis.text.x = element_text(hjust = 0.5,vjust=0.3, size=8),
        axis.text.y = element_text(size=8),
              plot.background = element_rect(color = "transparent", fill = "transparent"))+ 
  guides(alpha = "none",
         fill = guide_colorsteps(barwidth = 10, barheight = 0.25)) -> P2

ggsave(P2, filename = "p2.svg", width = 5, height = 25, units = "cm", dpi = 300)
  

#re-write levels of factors so that the plot starts with january at the top.
month_mean %>%
  mutate(month = factor(month, levels = c("Dec", "Nov", "Oct", "Sep", "Aug", "Jul", "Jun", "May", "Apr", "Mar", "Feb", "Jan"),
                        labels = c("Dec", "Nov", "Oct", "Sep", "Aug", "Jul", "Jun", "May", "Apr", "Mar", "Feb", "Jan"))) -> month_mean

month_total %>%
  mutate(month = factor(month, levels = c("Dec", "Nov", "Oct", "Sep", "Aug", "Jul", "Jun", "May", "Apr", "Mar", "Feb", "Jan"),
                        labels = c("Dec", "Nov", "Oct", "Sep", "Aug", "Jul", "Jun", "May", "Apr", "Mar", "Feb", "Jan"))) -> month_total

#slider plot with additional individual observations.
ggplot()+
  geom_point(data = month_mean, aes(x = mean, y = month, color = mean, size = mean), show.legend = FALSE) +
  geom_point(data = month_total, aes(x = count, y = month), alpha = 0.4, color = "black")+
  scale_color_gradient(low = "#0091ad", high = "#b7094c") +
  scale_x_continuous(expand = c(0,0))+
  scale_size_continuous(range = c(3,7))+
  xlab("")+
  ylab("Mean Search Count")+
  theme(text = element_text(family = "work sans"),
        legend.position = "bottom",
         legend.text = element_blank(),
         axis.ticks = element_blank(),
         strip.background = element_rect(fill = "transparent", color = NA),
         panel.background = element_rect(fill = "transparent", color = NA),
         axis.text.x = element_text(hjust = 0.5,vjust=0.3, size=8),
         plot.background = element_rect(color = NA, fill = "transparent"),
        panel.grid.major = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank()) -> P3
P3

ggsave(P3, filename = "p3.svg", width = 6, height = 27.5, units = "cm", dpi = 600, bg = "transparent")


# plots will be post-processed and arranged in inkscape for the final viz-------------------


