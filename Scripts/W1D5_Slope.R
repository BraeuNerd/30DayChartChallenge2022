# 30 Day Chart Challenge, Week 1: Comparisons, Day 3: Historical

# -----Load Packages----------------------------------

library(tidyverse)
library(showtext) # for specific fonts; font_add_google
library(ggtext) # package to interpret HTML with element_markdown to color titles
library(CGPfunctions)

# -----Load Data--------------------------------------

latamrd <- read_csv("Data/WorldBank_GDP_RD.csv")

# -----Organize Data----------------------------------

#remove country code and indicator name columns (slightly easier to pivot_longer in the next step):
latamrd <- latamrd[,-c(2,3)] 

# pivot from "cartesian" data to index/longform:
latamrd_long <- latamrd %>%
  pivot_longer(
    cols = !`Country Name`,
    names_to = "Year",
    values_to = "GDP")

latamrd_long$Year <- as.ordered(latamrd_long$Year)
latamrd_long$`Country Name` <- as.factor(latamrd_long$`Country Name`)

#exclude all other regions, leave only CentAm
LAC <- latamrd_long %>%
  filter(`Country Name` %in% c("Guatemala", "El Salvador", "Honduras", "Nicaragua", "Costa Rica", "Panamá")) %>%
  filter(!is.na(GDP))

names(LAC) <- c("name", "year", "GDP")

# Not all countries have a value for all years, and the oldest and most recent years are not all reported.
## Couldn't figure out how to do this "faster" so I'm filtering manually:

a1 <- LAC %>% filter(name == "Costa Rica" & year == 1996) %>% mutate(EL = as.factor("1990s"))
a2 <- LAC %>% filter(name == "Costa Rica" & year == 2018) %>% mutate(EL = as.factor("2018"))

b1 <- LAC %>% filter(name == "Panamá" & year == 1996) %>% mutate(EL = as.factor("1990s"))
b2 <- LAC %>% filter(name == "Panamá" & year == 2017) %>% mutate(EL = as.factor("2018"))

c1 <- LAC %>% filter(name == "Nicaragua" & year == 1997) %>% mutate(EL = as.factor("1990s"))
c2 <- LAC %>% filter(name == "Nicaragua" & year == 2015) %>% mutate(EL = as.factor("2018"))

d1 <- LAC %>% filter(name == "El Salvador" & year == 1998) %>% mutate(EL = as.factor("1990s"))
d2 <- LAC %>% filter(name == "El Salvador" & year == 2018) %>% mutate(EL = as.factor("2018"))

e1 <- LAC %>% filter(name == "Honduras" & year == 2000) %>% mutate(EL = as.factor("1990s"))
e2 <- LAC %>% filter(name == "Honduras" & year == 2017) %>% mutate(EL = as.factor("2018"))

f1 <- LAC %>% filter(name == "Guatemala" & year == 2005) %>% mutate(EL = as.factor("1990s"))
f2 <- LAC %>% filter(name == "Guatemala" & year == 2018) %>% mutate(EL = as.factor("2018"))


slope_CentAm <- rbind(a1,a2,b1,b2,c1,c2,d1,d2,e1,e2,f1,f2)


# -----DataViz----------------------------------------


newggslopegraph(dataframe = slope_CentAm,
                Times = EL,
                Measurement = GDP,
                Grouping = name,
                Title = "Spending on R&D in Central America* as % of GDP",
                SubTitle = "*Data for Belize not available \n (Last reported values for NQ are from 2015; HO & PN 2017; rest 2018)",
                Caption = "Data: WorldBank | Viz: @braeuNERD",
                YTextSize = 4,
                DataTextSize = 4,
                LineThickness = 1,
                TitleTextSize = 18,
                ThemeChoice = "wsj")
