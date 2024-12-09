# Load necessary libraries
library(tidytuesdayR)
library(tidyverse)
library(skimr)
library(wordcloud)
library(extafont)

# Load Data
tuesdata <- tidytuesdayR::tt_load(2024, week = 28)

glimpse(tuesdata$drob_funs)
skim(tuesdata$drob_funs)

# Summarize function usage by package and function name
wordcloud_data <- tuesdata$drob_funs %>%
  group_by(pkgs, funs) %>%
  summarise(times_used = n(), .groups = "drop") %>%
  ungroup()


# Load Fonts in Windows
# loadfonts(device = "win")

png("Week28.png", width = 7, height = 7, units = "in", res = 600)
par(mar = c(1,2,4,2))  

# Create Wordcloud
wordcloud(
  words = wordcloud_data$funs,
  freq = wordcloud_data$times_used,
  scale = c(3.5, 0.4),
  colors = c("#5e5e5e", "#f0b64d", "#0064ab"),
  random.order = FALSE,
  rot.per = 0.2,
  family = "Roboto",
  font = 2,
  min.freq = 2
)

# Add title and caption
title(
  main = "Top R Functions Used by David Robinson",
  line = 2.7,
  cex.main = 1.7,
  font.main = 2,
  family = "Roboto"
)

mtext(
  "A word cloud visualizing the frequency of R functions used by \nDavid Robinson while exploring #TidyTuesday datasets.",
  side = 3,
  line = 0.5,
  cex = 1,
  family = "Roboto Condensed"
)

mtext(
  "Joshua Dagoy | Data: {funspotr} | #TidyTuesday 2024 - Week 28",
  side = 1,
  line = -0.2,
  adj = 0.95,
  cex = 0.75,
  family = "Roboto Condensed"
)

dev.off()
