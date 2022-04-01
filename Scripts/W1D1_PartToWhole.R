# 30 Day Chart Challenge, Week 1: Comparisons, Day 1: Part-to-whole

# -----Load Packages----------------------------------

library(tidyverse)
library(treemapify) 
library(showtext) # for specific fonts; font_add_google
library(ggtext) # package to interpret HTML with element_markdown to color titles

# -----Load Data--------------------------------------

epistemas <- read_csv("Data/Carreras_Area.csv")

# -----Load Brand Fonts-------------------------------


font_add_google("Chau Philomene One")
font_add_google("Roboto")
showtext_auto()

# -----Organize Data----------------------------------

epistemas$Pregrado <- as.factor(epistemas$Pregrado)
epistemas$Area <- as.factor(epistemas$Area)

data_orden <- epistemas%>%
  group_by(Area, Pregrado) %>%
  summarize(n = n(),
            prop = (n/49)*100)

# -----DataViz----------------------------------------

# Brand colors HEX
## Yellow = #f5dd3a
## Light blue = #6bcbe3
## Brown = #986538

# Srow layout option:
ggplot(data_orden, aes(area = n, fill = Area, subgroup = Area, subgroup2 = Pregrado, label = Pregrado)) +
  geom_treemap(start = "topleft", layout = "srow") +
  geom_treemap_subgroup2_border(color = "#000000", size = 2, start = "topleft", layout = "srow") +
  geom_treemap_subgroup_border(color = "#000000", size = 10, start = "topleft", layout = "srow") +
  geom_treemap_subgroup_text(color = "#6E6E6E", alpha = 0.1, angle = 30, grow=T, place = "center", fontface="italic", family = "Chau Philomene One", start = "topleft", layout = "srow") +
  geom_treemap_subgroup2_text(color = "#000000", alpha = 0.7, size = 16, reflow=T, place = "center", fontface="bold", family = "Chau Philomene One", start = "topleft", layout = "srow") +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("#f5dd3a", "#6bcbe3","#986538")) +
  labs(title = "<span style='color:#f5dd3a;'>30%</span> de invitados e invitadas del primer año del podcast Epi<span style='color:#f5dd3a;'>S</span><span style='color:#6bcbe3;'>T</span><span style='color:#986538;'>E</span><span style='color:#f5dd3a;'>M</span>as  
       tienen un pregrado en <span style='color:#f5dd3a;'>biología</span>.",
       subtitle = "30% of guests of the first year of EpiSTEMas podcast (a LatAm STEM Podcast) are Biologists.",
       caption = "Data: EpiSTEMas Podcast  |   Viz: @braeuNERD") +
  theme(plot.background = element_rect(fill = "#000000"),
        plot.title = element_markdown(family = "Chau Philomene One",color = "white", size = 20),
        plot.subtitle = element_markdown(family = "Chau Philomene One", color = "grey60", size = 14, face = "italic"),
        plot.caption = element_text(family = "Roboto", hjust = 0.5, color = "grey80", size = 12))
