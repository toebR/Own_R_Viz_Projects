
library(reshape2)
library(tmaptools)
library(shinyjs)
library(tidyverse)
library(ggpubr)
library(ggthemes)
library(gghighlight)
library(glue)
library(gganimate)

palette_explorer()

print(get_brewer_pal("Greys", n = 15))


dir <- c("C:\\Users\\tobia\\OneDrive\\Desktop\\ownprojects\\R\\inequality series\\HDI_GDP")
setwd(dir)

hdi_dat <- data.frame(read.csv2(file = c("hdi_dat.csv"), sep = "", header = TRUE, stringsAsFactors = TRUE))

hdi_dat <- data.frame(hdi_dat[,c("HDI.Rank..2018..", "Country", "X1990","X1991","X1992", "X1993", "X1994" , "X1995", "X1996", "X1997",
                   "X1998", "X1999", "X2000", "X2001", "X2002", "X2003", "X2004", "X2005", "X2006", "X2007", "X2008" ,
                  "X2009", "X2010", "X2011", "X2012", "X2013", "X2014", "X2015", "X2016", "X2017", "X2018")])

hdi_dat$HDI.Rank..2018.. <- as.numeric(hdi_dat$HDI.Rank..2018..)



colnames(hdi_dat) <- c(  "HDIRank 2018", "Country"    ,      "1990"   ,         "1991"      ,      "1992"    ,        "1993"   ,        
                         "1994"      ,      "1995"      ,      "1996"      ,      "1997"       ,     "1998"      ,      "1999"   ,        
                         "2000"       ,     "2001"      ,      "2002"     ,       "2003"      ,      "2004"     ,       "2005"   ,        
                         "2006"      ,      "2007"      ,      "2008"     ,       "2009"      ,      "2010"      ,      "2011"   ,        
                         "2012"      ,      "2013"      ,      "2014"      ,      "2015"    ,        "2016"     ,       "2017"   ,        
                         "2018"  )



hdi_long <-melt(hdi_dat, id.vars = c("Country", "HDIRank 2018")) %>%
  filter(value != "..")

hdi_long$value <- as.numeric(hdi_long$value)

as.character(hdi_long$variable)
# 
# selected <- hdi_long %>%
#   filter(variable == "2018") %>%
#   filter(value > quantile(hdi_long$value, 0.99)| value < quantile(hdi_long$value, 0.1)) %>%
#   arrange(value)
# 
# selected
# 
# 
# 
# fincountries <- hdi_long %>%
#   filter(hdi_long$Country %in% selected$Country)
# 
# fincountries$variable <- as.numeric(fincountries$variable) #change to numeric to enable continous scaling of the x axis in plot
# str(fincountries$variable)
# 
# ggplot(fincountries, aes(x=variable, y=value, group = Country, color = Country))+
#   geom_line() +
#   geom_point() +
#   scale_colour_manual(values = viridisLite::viridis(14)) +
#   scale_x_continuous(limits = c(1990, 2018), breaks = seq(from = 1990, to = 2018, by = 4)) +
#   theme_solarized(light = FALSE) +
#   theme(axis.text = element_text(color = "grey")) +
#   theme(legend.key = element_rect(fill = "#01262d", color = NA)) +
#   rotate_x_text() +
#   xlab("Year") +
#   ylab("HDI") +
#   labs(title = "The 'Inequality Series' #1\nHDI Development of top and bottom countries",
#        subtitle = "*top: HDI Index 2018 > 0.99 Quantile | *bottom: HDI Index 2018 < 0.1 Quantile") +
#   labs(caption = "Author: @toeb18\nSource: United Nations Development Programme") +
#   theme(plot.title = element_text(size = 14, face = "bold", lineheight = 1.5),
#         plot.subtitle = element_text(size = 10))
# 
# ggsave(filename = "HDI_ineq_series_1.png", width = 19.5, height = 13, units = "cm")


#gghighlight plot
str(hdi_long$value)
ggplot(hdi_long, aes(x = as.character(variable), y = value, group = Country, color = Country)) +
  # geom_point() +
  geom_line(size = 2) +
  # gghighlight(max(value) > 0.98) +
  gghighlight(max(value) > 0.9, label_key = Country,  max_highlight = 5,
              unhighlighted_params = list(size = 0.5,color = alpha("grey", 0.5))) +
  facet_wrap(~Country, scales = "free_x") +
  scale_x_discrete(breaks = c("1990", "2000", "2010", "2018")) +
  ylab("Human Development Index") +
  xlab("Year") +

  labs(title = "Plotting Global Inequality\nHDI Development since 1990",
              subtitle = "Human development Index of Countries with maximum value > 0.9") +
       labs(caption = "Author: @toeb18\nSource: United Nations Development Programme") +
  theme_bw() +
  theme(text = element_text(family = 'Gill Sans', color = "#969696"),
        plot.background = element_rect(fill= "#2A2A2A"),
        panel.background = element_rect(fill = "#444B5A"),
        panel.grid.minor = element_line(color = '#4d5566'),
        panel.grid.major = element_line(color = '#586174'),
        plot.title = element_text(size = 20),
        plot.subtitle  = element_text(size = 14),
        # plot.caption = element_text(color = "white"),
        axis.title = element_text(size = 14, color = '#969696'),
        axis.text = element_text(color = "#969696"),
        strip.background = element_rect(fill = "#444B5A", color = "#444B5A"),
        strip.text = element_text(color ="white")) +
  rotate_x_text() -> gridhd1
gridhdi
ggsave(gridhdi, filename = "HDIgrid.png", width = 30, height = 30, units = "cm")


        