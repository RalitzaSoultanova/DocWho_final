library(tidyverse)
library(ggalt)
library(cowplot)
library(magick)
library(grid)
library(png)
require(ggimage)


tuesdata <- tidytuesdayR::tt_load('2021-11-23')
episodes <- tuesdata$episodes 


episodesCl <- episodes %>% 
  select(-serial_title, -era) %>% 
  mutate(episode = paste0(season_number, ".", coalesce(as.character(episode_number), "X"), " ", episode_title))%>% 
  arrange(!is.na(episode)) %>%
  arrange(!is.na(uk_viewers)) %>%
  filter(season_number<13) %>%
  arrange(first_aired) %>%
  mutate(season = factor(season_number)) %>%
  group_by(season) 


#plot

#img <- readJPEG("background2.jpeg")

p <- ggplot(episodesCl, aes(season, rating)) +
  geom_boxplot(color="black", fill="darkgrey", alpha=0.3)+ 
  labs(title = "Doctor Who - Evolution of Rating per Season", x = "Season", y="Rating")

#add backround image
p1 <- p + theme(plot.background = element_rect(fill = "darkgrey"), 
                plot.title = element_text(color="white", size=16, face="bold.italic", hjust=0.5),
                axis.title.x = element_text(color="white", size=12),
                axis.title.y = element_text(color="white", size=12), 
                panel.grid.minor.y = element_blank())

p1
#save
ggsave(p1, filename="23112021.jpg")

