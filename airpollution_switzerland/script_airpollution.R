library(tidyverse)
library(reshape2)
library(ggpubr)
library(zoo)
library(ggthemes)

dir <- c("C:\\Users\\tobia\\OneDrive\\Desktop\\ownprojects\\R\\own_projects\\air_pollution_CH")
setwd(dir)

#import and variable transormation
Bern <- read.csv("BER.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)
Jung <- read.csv("JUN.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)
Bern

Bern$Datum.Zeit <- strptime(Bern$Datum.Zeit, format = "%d.%m.%Y")
Bern$Datum.Zeit <- as.POSIXct(Bern$Datum.Zeit)
Bern$Standort <- "Bern - Urban Area"

Jung$Datum.Zeit <- strptime(Jung$Datum.Zeit, format = "%d.%m.%Y")
Jung$Datum.Zeit <- as.POSIXct(Jung$Datum.Zeit)
Jung$Standort <- "Jungfraujoch - Mountain Site"

#drop carbon monoxide
Bern$CO..mg.m3. <- NULL
Jung$CO..mg.m3. <- NULL


#bind dataframes
alldat <- data.frame(rbind(Bern, Jung))
colnames(alldat) <- c("Date", "O3", "NO2", "PM10", "NOx \n(eq. NO2)", "Measurement Station")
alldat

#convert to long format
datlong <- melt(alldat, idvars = c("Measurement Station", "Date"),
     measure.vars = c("O3", "NO2", "PM10", "NOx \n(eq. NO2)"))


#some plotting
plot <- ggplot(datlong) +
  # geom_point(aes(x = Date, y = value, color = `Measurement Station`)) +
  geom_line(aes(x = Date, y = value, color = `Measurement Station`, group = `Measurement Station`), alpha = .5) +
  geom_smooth(aes(x = Date, y = value, color = `Measurement Station`), size = 0.5) +
  ggtitle("Air Pollution in Switzerland", subtitle = "Selected Pollutants from June 2019 to June 2019") +
  labs(caption = "plot by @toeb18 ¦ Source: Feder Office for the Environment FOEN") +
  scale_color_manual(values = c("cyan", "yellow")) +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%m-%Y") +
  facet_wrap(.~variable, scales = "free_y") +
  ylab("Concentration [ug/m*3]") +
  theme_solarized_2(light = FALSE) +
  rotate_x_text() +
  theme(legend.position = "bottom")


plot




#save
ggsave(plot, filename = "plot2.png", width = 30, height = 20, units = "cm")

