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

setwd("~/++documents/TidyTuesday/2024/Week 23")

# Load Data
tuesdata <- tidytuesdayR::tt_load(2024, week = 23)
cheeses <- tuesdata$cheeses

# Data Check
colnames(cheeses)
glimpse(cheeses)
skim(cheeses)


