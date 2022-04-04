# 30 Day Chart Challenge, Week 1: Comparisons, Day 3: Historical

# -----Load Packages----------------------------------

library(tidyverse)
library(showtext) # for specific fonts; font_add_google
library(ggtext) # package to interpret HTML with element_markdown to color titles

# -----Load Fonts-------------------------------

font_add_google("Fira Sans Condensed")
showtext_auto()


# -----DataViz----------------------------------------

eq <- function(x){(-x^2)}
  
ggplot() + xlim(-10,40) +
  geom_function(fun=eq, color = "#809848", size = 2) +
  labs(title = "My house<span style='color:#809848;'>plants</span> willingness to <span style='color:#809848;'>live</span>",
       caption = "Viz: @braeuNERD",
       x = "The love I give my plants",
       y = "My plants  
       willingness  
       to live") +
  theme(plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.title = element_markdown(family = "Fira Sans Condensed",color = "#171717", face="bold", size = 25, margin = margin(20,0,10,0)),
        plot.caption = element_text(family = "Roboto", hjust = 0.95, color = "grey60", size = 11, margin = margin(0,0,20,0)),
        axis.title.x = element_markdown(family = "Fira Sans Condensed", color = "#809848", size = 15),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(color = "#171717", size = 1.5),
        axis.title.y = element_markdown(family = "Fira Sans Condensed", color = "#809848", angle = 0, vjust=0.5, size = 15))

