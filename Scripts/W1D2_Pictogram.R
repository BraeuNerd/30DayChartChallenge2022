# 30 Day Chart Challenge, Week 1: Comparisons, Day 2: Pictogram

# -----Load Packages----------------------------------

library(tidyverse)
library(lubridate)
library(showtext) # for specific fonts; font_add_google
library(ggtext) # package to interpret HTML with element_markdown to color titles
library(emojifont) # for the hearts on this viz

# -----Load Data--------------------------------------

mycal <- read_csv("Data/MeinKalender010422.csv")

# -----Load Brand Fonts-------------------------------

font_add_google("Roboto")
showtext_auto()

# -----Organize Data----------------------------------

names(mycal) <- c("Date","Period")
mycal$Date <- as.Date(mycal$Date, "%d-%m-%y")
mycal$Period <- as.factor(mycal$Period)

# Separate dates in 3 cols: days, months, years
  
mycal2 <- mycal %>%
    mutate_at(vars(Date), funs(year, month, day)) %>% #This separates my Date in 3 columns: year, month, day
    mutate(Month = month(Date, label=T)) # This turns the month (1-12) into Month by characters (i.e. Jan, Feb...)

# Take only the 2021 (for no reason other than it looks prettier than with all 5 years and I couldn't manage to split the months and years the way I wanted with the time I had XD) 
mycal_final <- mycal2 %>%
  filter(Date >= "2021-01-01" & Date < "2022-01-01")

# -----DataViz----------------------------------------


ggplot(mycal_final, aes(x=day, y=Month, color = Period)) +
  geom_text(family="EmojiOne", size=8, label = emoji("droplet")) +
  theme(legend.position = "none") +
  scale_y_discrete(limits=rev) +
  scale_x_continuous(breaks = seq(1, 31, by = 1)) +
  scale_color_manual(label = c("1","0"),
                     values = c("#d8867b","#ff1e1e")) +
  scale_fill_manual(values = c("#d8867b","#ff1e1e")) +
  labs(title = "During 2021 my <span style='color:#960018;'>menstrual cycle</span> was more or less regular",
       subtitle = "at a ~25.5 day cycle",
       caption = "Data: my data from My Calendar App  |   Viz: @braeuNERD") +
  theme(plot.background = element_rect(fill = "#171717", color = "#171717"),
        panel.background = element_rect(fill = "#171717"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_markdown(family = "Roboto",color = "#d8867b", size = 20, margin = margin(20,0,0,0)),
        plot.subtitle = element_markdown(family = "Roboto", color = "#d8867b", size = 14, margin = margin(8,0,0,0)),
        plot.caption = element_text(family = "Roboto", hjust = 0.95, color = "#d8867b", size = 12, face = "bold", margin = margin(0,0,20,0)),
        axis.title.x = element_blank(),
        axis.text.x = element_text(color = "grey70", size = 11, margin = margin(0,20,20,0)),
        axis.text.y = element_text(color = "grey70", size = 11, margin = margin(0,0,0,20)),
        axis.title.y = element_blank())
