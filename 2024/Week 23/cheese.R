library(tidytuesdayR)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)
library(skimr)
library(ggtext)
library(stringr)
library(forcats)
library(scales)
library(prismatic)

# Load Data
tuesdata <- tidytuesdayR::tt_load(2024, week = 23)
cheeses <- tuesdata$cheeses

# Data Check
colnames(harvest_2020)
glimpse(harvest_2020)
skim(harvest_2020)

colnames(planting_2020)
glimpse(planting_2020)
skim(planting_2020)