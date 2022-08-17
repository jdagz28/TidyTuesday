#09-08-2022
#TidyTuesday - Week 32 Ferris Wheels

setwd("~/Jdagz Documents/TidyTuesday/Week 32")

# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

tuesdata <- tidytuesdayR::tt_load('2022-08-09')
#tuesdata <- tidytuesdayR::tt_load(2022, week = 32)

wheels <- tuesdata$wheels

# Or read in the data manually

#wheels <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-09/wheels.csv')


# Libraries
library(tidyverse)
library(dplyr)
library(sysfonts)
library(skimr)
library(ggforce)


# Font
font_add_google("Roboto", "roboto")
font_add_google("Roboto Condensed", "roboto condensed")
font_add_google("Oswald", "oswald")


# Data Check
glimpse(wheels)
skim(wheels)
colnames(wheels)

# Data Wrangling
dataset <- wheels %>%
    select(name, height, diameter, number_of_cabins, opened, status, country) %>%
    drop_na(c(diameter, height)) %>%
    mutate(year=lubridate::year(opened),
           rad = diameter / 2,
           h = height - rad,
           lab=glue::glue("{name}, {country}"))

coming <- dataset %>%
    arrange(desc(height)) %>% 
    slice(1:3) 

currenttop3 <- dataset %>%
    arrange(desc(height)) %>%
    slice(4:6)

top1 <- dataset %>%
    arrange(desc(height)) %>% 
    slice(1) %>% pull(lab)

top2 <- dataset %>%
    arrange(desc(height)) %>% 
    slice(2) %>% pull(lab)

top3 <- dataset %>%
    arrange(desc(height)) %>% 
    slice(3) %>% pull(lab)


# Plotting
ggplot() + 
    geom_circle(aes(x0 = rad, y0 = rad, r = rad), data = dataset, size = 0.2, color = "grey60") + 
    geom_circle(aes(x0 = rad, y0 = rad, r = rad), data = coming, size = 0.8, color = "#819FCC") +
    geom_text(aes(x=690, y=630, label = top1), size = 2, family = "Oswald") + 
    geom_text(aes(x=670, y=560, label = top2), size = 2, family = "Oswald") + 
    geom_text(aes(x=510, y=460, label = top3), size = 2, family = "Oswald") +
    ggrepel::geom_text_repel(currenttop3, mapping = aes(x = diameter - 10, y = rad+150, label= lab), size = 1.5, family = "Oswald")+
    scale_x_continuous("Radius (ft)", limits=c(0, 800), expand=c(.05,.05)) +
    labs(y = "Height (ft)") + 
    theme_minimal() + 
    theme(text=element_text(family="Roboto"),
          panel.grid.major = element_blank(),
          axis.title.x=element_text(hjust=.42),
          axis.ticks.length = unit(.3,"lines"),
          axis.ticks=element_line(color="black", size=.3),
          axis.title=element_text(size=7),
          axis.text.x=element_text(size=5),
          axis.text.y=element_text(size=5),
          axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
          axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
          plot.title.position = "plot",
          plot.title=element_text(face="bold", size=10),
          plot.subtitle=element_text(size=7),
          plot.caption.position = "plot",
          plot.caption = element_text(hjust=1, size=4),
          #plot.margin=margin(10,10,10,0)
    ) + 
    labs(caption="Data: {ferriswheels} package by Emil Hvitfeldt | #TidyTuesday Week 32",
            title="Coming soon: Bigger Ferris Wheels",
            subtitle="Under development are the top 3 biggest ferris wheels.")

# Export plot
ggsave("Week32.png", height = 1100, width=1100, units="px", bg="white") 


