# Load necessary libraries
library(tidytuesdayR)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)
library(skimr)
library(stringr)
library(ggbeeswarm)
library(sysfonts)
library(scales)

# Load Data
tuesdata <- tidytuesdayR::tt_load('2024-07-02')

# Data Exploration
tt_datasets <- tuesdata$tt_datasets
colnames(tt_datasets)
glimpse(tt_datasets)
skim(tt_datasets)

# tt_summary <- tuesdata$tt_summary
# colnames(tt_summary)
#glimpse(tt_summary)
#skim(tt_summary)

#tt_variables <- tuesdata$tt_variables
#colnames(tt_variables)
#glimpse(tt_variables)
#skim(tt_variables)

# Font
font_add_google("Roboto", "roboto")
font_add_google("Roboto Condensed", "roboto condensed")
font_add_google("Oswald", "oswald")


# Plotting 
plt <- tt_datasets %>%
  ggplot(aes(x = year ,y = variables,)) +
  geom_hline(yintercept = 0, col = "grey34", lty = "dashed") + 
  ggbeeswarm::geom_beeswarm(
    aes(fill = case_when(
      observations < 2000 ~ "< 1000",
      observations < 20000 ~ "< 10000",
      observations >= 20000 ~ "≥ 10000"
      #variables < 5 ~ "< 5",
      #variables < 10 ~ "< 10",
      #variables >= 10 ~ "≥ 10",
    )),
    cex = 4, size = 4, shape = 21, color = "white", priority = "random") + 
  #scale_fill_manual(values = c("< 5" = "#19a0aa", "< 10" = "#e0b23d", "≥ 10" = "#f15f36")) +
  scale_fill_manual(values = c("< 1000" = "#19a0aa", "< 10000" = "#e0b23d", "≥ 10000" = "#f15f36")) +
  coord_flip(ylim = c(0, 80)) +
  facet_wrap(~ year, ncol = 3, scales = "free_y") +
  guides(fill = guide_legend(nrow=1, reverse=FALSE,label.position = "right", override.aes = list(size = 8))) +
  theme(
    plot.title.position = "plot",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(family = "roboto", size = rel(1.25)),
    axis.title.x = element_blank(),
     # axis.title.x = element_text(size = rel(1.5)),
    panel.background = element_blank(),
    strip.background = element_rect(colour="white", fill="white"),
    strip.text = element_text(family = "roboto", size=rel(1.5), angle = 0, hjust = 0,  face = "bold",),
    plot.title=element_text(family="oswald", face="bold", size=rel(2.5)),
    plot.subtitle = element_text(family="oswald", size=rel(1.5)),
    plot.caption=element_text(family="roboto condensed", size=rel(0.9)),
    legend.title = element_text(size = rel(1.4)),
    legend.text = element_text(size = rel(1.4)),
    legend.position="bottom",
    legend.key=element_blank()) + 
  labs(title="#TidyTuesday Dataset Variables and Observations",
       subtitle="Distribution of variables and observations in #TidyTuesday datasets from 2018 to 2024", 
       caption=paste(
         "Joshua Dagoy | Data: {ttmeta} | #TidyTuesday Week 27"),
       x = NULL,
       y = NULL,
       fill = "No. of Observations" )         

plt

ggsave("Week27.png", width = 12, height = 7, dpi = 600)
