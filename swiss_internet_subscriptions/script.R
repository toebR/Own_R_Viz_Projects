#First specify the packages of interest
packages = c("tidyverse", 
             "gridExtra",
             "grid")


## Now load or install&load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

dir <- c("C:\\Users\\tobia\\OneDrive\\Desktop\\ownprojects\\R\\tidytuesday\\internetsubs")
setwd(dir)

data <- read.csv2("bfsdata_internet_subs.csv")
data

data100 <- read.csv2("bfsdata_internet_subs_mob.csv")
data100

p1 <-ggplot(data, aes(x = Year, y=subscriptions, color = Connection_type)) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = seq(from = 2000, to = 2018, by = 2)) +
  scale_color_manual(values = c("#1b9e77",
                                "#d95f02",
                                "#7570b3",
                                "#e7298a"), "Technology") +
  scale_colour_discrete(guide = 'none') +
  scale_x_discrete(expand=c(0, 1)) +
  geom_dl(aes(label = State), method = list(dl.combine("first.points", "last.points"), cex = 0.8))
  labs(caption = "Source: Swiss Federal Statistics Office (FSO)") +
  labs(title = "Internet Access Technologies in Switzerland",
       subtitle = "Subscriptions through Time")
print(p1)
ggsave("Evolution_of_internet_CH.png")

p2 <-ggplot(data100) +
  geom_line(aes(x = Year, y=subscriptionS.per.100.people, color = Connection_type),size = 1) +
  scale_x_continuous(breaks = seq(from = 2000, to = 2018, by = 2)) +
  scale_color_manual(values = c("#1b9e77",
                                "#d95f02",
                                "#7570b3",
                                "#e7298a",
                                "#66a61e"), "Technology") +
  #theme(legend.position = c(0.157,0.85)) +
  labs(caption = "Source: Swiss Federal Statistics Office (FSO)") +
  labs(title = "Internet Access Technologies in Switzerland",
       subtitle = "Subscriptions through Time") +
  ylab("subscriptions per 100 people")
print(p2)
ggsave("Evolution_of_internet_CH_100.png")

