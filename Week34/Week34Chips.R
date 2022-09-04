#23-08-2022
#TidyTuesday - Week 34 The CHIP dataset

setwd("~/Jdagz Documents/TidyTuesday/Week 34")

# Get the Data

#tuesdata <- tidytuesdayR::tt_load('2022-08-23') - SUMMARY

#chips <- tuesdata$chips


# Full data downloaded from (https://chip-dataset.vercel.app/)
chips <- read.csv("chip_dataset.csv", header = TRUE, sep=",", check.names = FALSE)


# Libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)
library(cowplot)


# Font
font_add_google("Roboto", "roboto")
font_add_google("Roboto Condensed", "roboto condensed")
font_add_google("Oswald", "oswald")


# Color Palette
pal <- c("AMD" = "#bd0000", "ATI" = "#ef0707", "Intel" = "#127CC1", "NVIDIA" = "#76b900", "Other" = "grey50")

# Data Check
glimpse(chips)
colnames(chips)
summary(chips)


# Rename column names
chips <- chips %>%
    janitor::clean_names()

# Create ReleaseYear variable
chips$release_year <- substr(chips$`release_date`, 0, 4)
chips <- chips[chips$release_year != "NaT",]
chips$release_year <- as.numeric(chips$release_year)


# Convert Release Date column from numeric to date type
chips[,"release_date"] <- as_date(chips[,"release_date"])


# Plotting
ggplot(data = chips, aes(release_date, transistors_million)) +
    geom_jitter(aes(fill = vendor), size = 4.2, alpha = 0.5, width = 15, height = 0.05, color = "white", shape = 21) +
    scale_y_log10(position = "right", labels = label_number(suffix = " M", big.mark = ",")) +
    scale_x_date(limits = c(ymd("1999-01-01"), ymd("2022-12-31")), breaks = seq(ymd("2000-01-01"), ymd("2022-01-01"), "5 years"), labels = year)+
    scale_fill_manual(values = pal) +
    theme_minimal_hgrid(6) +
    guides(fill = guide_legend(label.position = "top",
                               keywidth = unit(3, "line"),
                               keyheight = unit(1, "line"),
                               nrow = 1,
                               override.aes = list(alpha = 1, fill = pal, size = 20))) +
    theme(axis.line.x = element_blank(),
          axis.text.x = element_text(family = "roboto condensed", size=rel(3)),
          axis.text.y = element_text(family = "roboto condensed", size=rel(3)),
          axis.title.x = element_blank(),
          axis.title.y = element_text(family = "roboto condensed", size=rel(2.5)),
          plot.title=element_text(family="oswald", face="bold", size=rel(7)),
          plot.subtitle = element_text(family="oswald", size=rel(3)),
          plot.caption=element_text(family="roboto condensed", size=rel(1.7), hjust = 1.07),
          legend.position = c(0.05, 0.8),
          legend.title = element_blank(),
          legend.text = element_text(family = "roboto", size=rel(3), face = "bold")) + 
    labs(y = "Transistors (Million)",
        caption="Data: The CHIP dataset (chip-dataset.vercel.app)  |  #TidyTuesday Week 33",
        title="Moore's Law still holds, especially in GPUs",
        subtitle="Moore's law is the observation that the number of transistors in a dense intergrated circuit doubles about every two years.\nIt is an observation and observation and projection of a historical trend. Microchip advancement is important for other aspects\nof technological porgress in computing - such as processing speed or the price of computers.")
   
   
# Export
ggsave("Week34.png", height = 3000, width=6000, units="px", bg="white") 
   
