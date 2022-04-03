# 30 Day Chart Challenge, Week 1: Comparisons, Day 3: Historical

# -----Load Packages----------------------------------

library(tidyverse)
library(showtext) # for specific fonts; font_add_google
library(ggtext) # package to interpret HTML with element_markdown to color titles
library(glue)

# -----Load Data--------------------------------------

latamrd <- read_csv("Data/WorldBank_GDP_RD.csv")

# -----Load Brand Fonts-------------------------------

font_add_google("Roboto")
showtext_auto()

# -----Organize Data----------------------------------

#remove country code and indicator name columns (slightly easier to pivot_longer in the next step):
latamrd <- latamrd[,-c(2,3)] 

# pivot from "cartesian" data to index/longform:
latamrd_long <- latamrd %>%
  pivot_longer(
    cols = !`Country Name`,
    names_to = "Year",
    values_to = "%GDP")

latamrd_long$Year <- as.ordered(latamrd_long$Year)
latamrd_long$`Country Name` <- as.factor(latamrd_long$`Country Name`)

#exclude all other regions, leave only Continental Latin America
LAC <- latamrd_long %>%
  filter(`Country Name` %in% c("México", "Guatemala", "El Salvador", "Honduras", "Nicaragua", "Costa Rica", "Panamá", 
                               "Colombia", "Venezuela", "Ecuador", "Perú", "Bolivia", "Paraguay", "Brasil", "Chile",
                               "Uruguay", "Argentina")) %>%
  filter(Year >=2000 & Year < 2019)

names(LAC) <- c("name", "year", "%GDP")

# small table for labels:
LAC %>%
  filter(year == 2018)

# -----DataViz----------------------------------------

# New facet label names for countries
names.labs <- c("Argentina (2018: 0.494%)",
                "Bolivia (2009: 0.157%)",
                "Brasil (2018: 1.16%)",
                "Chile (2017: 0.356%)",
                "Colombia (2018: 0.235%)",
                "Costa Rica (2018: 0.383%)",
                "Ecuador (2014: 0.443%)",
                "Guatemala (2018: 0.0295%)",
                "Honduras (2017: 0.0399%)",
                "México (2018: 0.313%)",
                "Nicaragua (2015: 0.107)",
                "Panamá (2017: 0.147%)",
                "Perú (2018: 0.127%)",
                "Paraguay (2018: 0.146%)",
                "El Salvador (2018: 0.165%)",
                "Uruguay (2018: 0.419%)",
                "Venezuela (2014: 0.337%)")

names(names.labs) <- c("Argentina","Bolivia","Brasil","Chile",
                "Colombia","Costa Rica","Ecuador","Guatemala",
                "Honduras","México","Nicaragua","Panamá",
                "Perú","Paraguay","El Salvador","Uruguay","Venezuela")


LAC2 <- LAC %>%
  mutate(name2 = name)

LAC2 %>%
  ggplot(aes(x = year, y = `%GDP`)) +
    geom_line( data=LAC2 %>% select(-name), aes(group = name2), color = "white", size = 0.3, alpha = 0.3) +
    geom_line(aes(color = name, group=name), color = "#FED459", size = 1) +
    geom_point(aes(color = name, group=name), color = "#FED459", size = 1.2) +
    theme(legend.position = "none") +
    facet_wrap(~name, labeller = labeller(name = names.labs)) +
    scale_x_discrete(breaks = seq(2000,2018,by=2)) +
  labs(title = "<span style='color:#009c3b;'>Brazil</span> invests the highest % of their GDP in Research & Development at <span style='color:#009c3b;'>1.16%</span>  
       compared to other countries from continental Latin America",
       subtitle = "<span style='color:#4997D0;'>Guatemala</span> the lowest (<span style='color:#4997D0;'>0.0295%</span>  by 2018)  
       <span style='color:#b0b3b8;'>(countries not shown = data not available)</span>",
       caption = "Data: WorldBank  |   DataViz: @braeuNERD") +
  theme(plot.background = element_rect(fill = "#171717", color = "#171717"),
        panel.background = element_rect(fill = "#171717"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_markdown(family = "Roboto",color = "white", size = 18, margin = margin(20,0,0,0)),
        plot.subtitle = element_markdown(family = "Roboto", color = "white", size = 12, margin = margin(5,0,10,0)),
        plot.caption = element_text(family = "Roboto", hjust = 0.95, color = "grey70", size = 11, margin = margin(0,0,20,0)),
        axis.title.x = element_blank(),
        axis.text.x = element_text(color = "grey70", size = 8, margin = margin(0,20,20,0), angle = 90),
        axis.text.y = element_text(color = "grey70", size = 11),
        axis.title.y = element_blank(),
        strip.background = element_rect(fill = "#171717"),
        strip.text.x = element_text(color = "white"))
