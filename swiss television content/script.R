library(tidyverse)
library(readxl)
library(reshape2)
library(ggthemes)
library(ggpubr)
library(gridExtra)
path <- "C:\\Users\\tobia\\OneDrive\\Desktop\\ownprojects\\R\\tidytuesday\\srf\\bfs_origdat_to_shit_around.xlsx"

dat <- data.frame(read_xlsx(path = path, col_names = TRUE))
dat

setwd("C:\\Users\\tobia\\OneDrive\\Desktop\\ownprojects\\R\\tidytuesday\\srf")
#first make new colnames so that they are right

years<- as.character(seq(from = 1998, to = 2018, by = 1))
colnames(dat) <- c("Category", years)

#then Make new long variable columns for "erstausstrahlung vs. wiederholung e.g.
dat$Type[1:17] <- c("First Broadcast")
dat$Type[18:34] <- c("Repetition")


dat$Category <- as.factor(dat$Category)
levels(dat$Category)


#translate category
#dat$Category[dat$Category == "Fiktion"] <- Fiction

levels(dat$Category)<- c("Education",
                                 "Documentaries",
                         "First Broadcast",
                                 "Fiction",
                                 "Information",
                                 "Kids programs",
                                 "Culture",
                                 "Music",
                                 "Not Assigned",
                                 "Non-Fictional Entertainment",
                                 "Religion",
                                 "Service",
                                 "Sports",
                                 "Else",
                                 "House Advertising",
                                 "Advertising",
                                 "Repetitions",
                                 "Science")



dat

#melt somehow into long formate
datfiltered <- filter(dat,dat$Category != c("First Broadcast", "Repetitions"))
datfiltered2 <- filter(datfiltered,datfiltered$Category != c("Else", "Not Assigned"))
datfiltered3 <- filter(datfiltered2,datfiltered2$Category != c("Service"))

dat_long <- melt(datfiltered3, id.vars = c("Category", "Type"))
colnames(dat_long) <- c("Category", "Type", "Year", "Hours/Year")

#view(dat_long)

dat_long$`Hours/Year`<- round(as.numeric(dat_long$`Hours/Year`), digits = 0)
dat_long$Year <- as.character(dat_long$Year)

#get a wide casted df:
# dat_wide <- reshape2::dcast(dat_long, Year ~Type + Category)
# dat_wide


#Detailplot! make the fill colors look nice!

p1 <- ggplot(dat_long,aes(x = Year, y = dat_long$`Hours/Year`, fill = Type)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Category) +
  theme_solarized(light = FALSE) +
  theme(axis.text = element_text(color = "grey")) +
  xlab("Year")+
  ylab("Streamed Hours") +
  scale_fill_manual(values = c("#D2A75C", "#62B7AD")) +
  rotate_x_text() +
  theme(legend.position = c(0.8,0.04)) +
  theme(legend.direction = "horizontal",
        legend.title=element_text(size=10), 
        legend.text=element_text(size=9)) +
  labs(caption = "Source: Swiss Federal Statistics Office (FSO)\n*Uncategorized and Unclear Data is not Displayed\n*Switzerland has a Multilingual State Television\nService offering many differnt Channels") +
  labs(title = "Program Streaming in Swiss State Television Channels",
       subtitle = "Streamed Hours per Year and Content Category*")


#overview plot!!
p2 <-  ggplot(dat_long) +
  geom_bar(stat = "identity", fill = "#FC8D62", aes(x = Year, y = dat_long$`Hours/Year`)) +
  theme_solarized(light = FALSE) +
  theme(axis.text = element_text(color = "grey")) +
  ylab("Streamed Hours") +
  rotate_x_text() +
  labs(caption = "Source: Swiss Federal Statistics Office (FSO)\n*Uncategorized and Unclear Data is not Displayed\n*Switzerland has a Multilingual State Television\nService offering many different Channels") +
  labs(title = "Program Streaming in Swiss\nState Television Channels",
       subtitle = "Streamed Hours per Year*")
print(p2)
g <- arrangeGrob(p2, p1, nrow = 1, widths = c(2,6))
plot(g)
ggsave(file = "test_srf.png", g, width = 15, height = 10)

p3 <- ggplot(dat_long,aes(x = Category, y = dat_long$`Hours/Year`, fill = Type)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Year) +
  theme_solarized(light = FALSE) +
  theme(axis.text = element_text(color = "grey")) +
  xlab("Year")+
  ylab("Streamed Hours") +
  scale_fill_manual(values = c("#D2A75C", "#62B7AD")) +
  rotate_x_text() +
  theme(legend.position = c(0.8,-0.2)) +
  theme(legend.direction = "horizontal",
        legend.title=element_text(size=10), 
        legend.text=element_text(size=9)) +
  labs(caption = "Source: Swiss Federal Statistics Office (FSO)\n*Uncategorized and Unclear Data is not Displayed\n*Switzerland has a Multilingual State Television\nService offering many differnt Channels") +
  labs(title = "Program Streaming in Swiss State Television Channels",
       subtitle = "Streamed Hours per Content and Year Category*")
p3
ggsave(file = "perY.png", p3, width = 9, height = 9)

dat_long$Year <- as.numeric(dat_long$Year)
typeof(dat_long$Year)

p4 <- ggplot(dat_long,aes(x = Year, y = dat_long$`Hours/Year`, fill = Type)) +
  geom_area() +
  facet_wrap(~Category, scales = "free_y") +
  theme_solarized(light = FALSE) +
  theme(axis.text = element_text(color = "grey")) +
  scale_x_continuous(limits = c(1998, 2018), breaks = c(1998, 2018)) +
  xlab("Year")+
  ylab("Streamed Hours") +
  scale_fill_manual(values = c("#D2A75C", "#62B7AD")) +
  rotate_x_text() +
  theme(legend.position = c(0.8,0)) +
  theme(legend.direction = "horizontal",
        legend.title=element_text(size=10),
        legend.text=element_text(size=9)) +
  labs(caption = "Source: Swiss Federal Statistics Office (FSO)\n*Uncategorized and Unclear Data is not Displayed\n*Switzerland has a Multilingual State Television\nService offering many differnt Channels") +
  labs(title = "Program Streaming in Swiss State Television Channels",
       subtitle = "Streamed Hours per Content and Year Category*")
p4
ggsave(file = "geom_area_version.png", p4, width = 15, height = 10)
dat_long
p5 <- ggplot(dat_long,aes(x = Year, y = dat_long$`Hours/Year`, color = Type)) +
  geom_point() +
  geom_line()  +
  facet_wrap(~Category, scales = "free_x") +
  theme_solarized(light = FALSE) +
  theme(axis.text = element_text(color = "grey")) +
  scale_x_continuous(limits = c(1998, 2018), breaks = c(1998, 2010, 2018)) +
  xlab("Year")+
  ylab("Streamed Hours") +
  scale_color_manual(values = c("#D2A75C", "#62B7AD")) +
  rotate_x_text() +
  theme(legend.position = c(0.8,0)) +
  theme(legend.direction = "horizontal",
        legend.title=element_text(size=10),
        legend.text=element_text(size=9)) +
  labs(caption = "Source: Swiss Federal Statistics Office (FSO)\n*Uncategorized and Unclear Data is not Displayed\n*Switzerland has a Multilingual State Television\nService offering many differnt Channels") +
  labs(title = "Program Streaming in Swiss State Television Channels",
       subtitle = "Streamed Hours per Content and Year Category*")
p5
ggsave(file = "geom_lineplot_version.png", p5, width = 10, height = 10)

