#script by tobias stalder
#jan 2021
#tobias-stalder.netlify.app

library(here)
library(ggplot2)
library(dplyr)
library(gganimate)
library(ggtext)
library(palmerpenguins)
library(co)

here()

# rebuild what are your summary statistics hiding by alison horst based on the palmer penguins datasets

# sepearate data for mean body mass of penguins
penguins %>%
  group_by(species) %>%
  summarise(mean_bodymass = mean(body_mass_g, na.rm = TRUE)) -> penguin_bodymass_grp


#plot
ggplot(penguins) +
  geom_col(data = penguin_bodymass_grp, aes(x = species, y = mean_bodymass, fill = species, color = species, alpha = .5, width = .7)) +
  geom_jitter(aes(x = species, y = body_mass_g, color = species), alpha = .9, width = .3, height = 0) +
  geom_boxplot(aes(x = species, y = body_mass_g, fill = species), color = "#a6acaf", outlier.colour = "transparent", width = .3) +
  scale_color_manual(values = c("#ff7400","#c75bcb", "#067476"))+
  scale_fill_manual(values = c("#ff7400","#c75bcb", "#067476"))+
  xlab("Penguin Species") +
  ylab("Body Mass [g]")+
  labs(title = "What are you hiding with your summary statistics?",
       subtitle = "Bar vs. scatter & boxplot",
       caption = "**Plot by** Tobias Stalder<br>
       *tobias-stalder.netlify.app*<br><br>
              **Inspired by:** 'Are your summary statistics hiding something interesting?',<br>
       Illustration by Allison Horst. *github.com/allisonhorst/stats-illustrations*<br><br>
       **Data:** Horst AM, Hill AP, Gorman KB (2020). palmerpenguins: <br>Palmer
       Archipelago (Antarctica) penguin data. R package version 0.1.0.<br><br>
       **Packages:** {palmerpenguins}, {dplyr}, {ggplot2}, {gganimate}, {ggtext}") +
  theme_bw() +
  theme(legend.position = "none",
        plot.caption = element_markdown(hjust = 0, color = "#909497", size = 6),
        panel.border = element_blank(),
        axis.line = element_line(color = "#a6acaf"),
        plot.background = element_rect(fill = "#303841", color = "#303841"),
        panel.background = element_rect(fill = "#303841", color = "#303841"),
        text = element_text(color = "#a6acaf"),
        axis.text  = element_text(color = "#909497"),
        panel.grid = element_blank()) +
  ggsave(filename = "palmer_penguins.png", dpi =300)




# animation



ggplot(penguins) +
  geom_col(data = penguin_bodymass_grp, aes(x = species, y = mean_bodymass, fill = species, color = species, alpha = .5, width = .7)) +
  geom_jitter(aes(x = species, y = body_mass_g, color = species), alpha = .9, width = .3, height = 0) +
  geom_boxplot(aes(x = species, y = body_mass_g, fill = species), color = "#a6acaf", outlier.colour = "transparent", width = .3) +
  scale_color_manual(values = c("#ff7400","#c75bcb", "#067476"))+
  scale_fill_manual(values = c("#ff7400","#c75bcb", "#067476"))+
  xlab("Penguin Species") +
  ylab("Body Mass [g]")+
  labs(title = "What are you hiding with your summary statistics?",
       subtitle = "Bar vs. scatter & boxplot",
       caption = "**Plot by** Tobias Stalder<br>
       *tobias-stalder.netlify.app*<br><br>
              **Inspired by:** 'Are your summary statistics hiding something interesting?',<br>
       Illustration by Allison Horst. *github.com/allisonhorst/stats-illustrations*<br><br>
       **Data:** Horst AM, Hill AP, Gorman KB (2020). palmerpenguins: <br>Palmer
       Archipelago (Antarctica) penguin data. R package version 0.1.0.") +
  theme_bw() +
  theme(legend.position = "none",
        plot.caption = element_markdown(hjust = 0, color = "#909497", size = 8),
        panel.border = element_blank(),
        plot.title = element_text(size = 18),
        axis.line = element_line(color = "#a6acaf"),
        plot.background = element_rect(fill = "#303841", color = "#303841"),
        panel.background = element_rect(fill = "#303841", color = "#303841"),
        text = element_text(color = "#a6acaf"),
        axis.text  = element_text(color = "#909497"),
        panel.grid = element_blank()) +

transition_layers(layer_length = 1, transition_length = 2,
                  from_blank = TRUE, keep_layers = c(5, 2, 2)) +
  enter_grow() + enter_fade() + enter_grow() +
  exit_shrink()  + exit_fade() + exit_shrink()  -> anim_peng

animate(anim_peng, end_pause = 0, rewind = FALSE,  res = 100, height = 7, width = 7, units = "in")

anim_save(filename = "palmer_penguins_statistics.gif")
