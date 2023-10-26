setwd("~/Jdagz Documents/TidyTuesday/Week 26")

# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

tuesdata <- tidytuesdayR::tt_load('2022-06-28')
# tuesdata <- tidytuesdayR::tt_load(2022, week = 26)

paygap <- tuesdata$paygap

# Or read in the data manually

# paygap <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-28/paygap.csv')

# Libraries
library(tidyverse)
library(vroom)
library(fs)
library(openxlsx)
library(dplyr)
library(ggbeeswarm)
library(sysfonts)

# Font
font_add_google("Roboto", "roboto")
font_add_google("Roboto Condensed", "roboto condensed")
font_add_google("Oswald", "oswald")


# SIC code - Industry categories (https://resources.companieshouse.gov.uk/sic/)
SICcodes <- read.xlsx("SICCodes.xlsx")

# Separate and create multiple entries with companies registered in multiple industries
paygap <- separate_rows(paygap,sic_codes,sep=":")

# calculate year
paygap$year <- lubridate::year(as.Date(paygap$due_date))

# Add SIC code section and Industry
dataset <- paygap %>%
    filter(year == 2021) %>%
    
    left_join(SICcodes, select(Section, Industry), by = "sic_codes")

sum(is.na(dataset$Section))    

dataset <- dataset %>%
    drop_na(Section)

sum(is.na(dataset$Section))

# Plotting 

plt <- dataset %>%
    ggplot(aes(x = Section,y = diff_median_hourly_percent, xaxt = "n")) +
    geom_hline(yintercept = 0, col = "grey34", lty = "dashed") + 
    ggbeeswarm::geom_beeswarm(
        aes(fill = case_when(
            diff_median_hourly_percent > 0 ~ "Men",
            diff_median_hourly_percent < 0 ~ "Women",
            diff_median_hourly_percent == 0 ~ "Equal"
        )),
        cex = 3, size = 3, shape = 21, color = "white", priority = "random") + 
    scale_fill_manual(values = c("Men" = "#19a0aa", "Equal" = "#e0b23d", "Women" = "#f15f36")) +
    coord_flip(ylim = c(-20, 75)) +
    facet_wrap(~ Industry) +
    guides(fill = guide_legend(nrow=1, reverse=FALSE,label.position = "left", override.aes = list(size = 7))) +
    theme(
        plot.title.position = "plot",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(family = "roboto"),
        panel.background = element_blank(),
        strip.background = element_rect(colour="white", fill="white"),
        strip.text = element_text(family = "roboto", size=rel(1.2), angle = 0, hjust = 0),
        plot.title=element_text(family="oswald", face="bold", size=rel(3)),
        plot.subtitle = element_text(family="oswald", size=rel(1.5)),
        plot.caption=element_text(family="roboto condensed", size=rel(0.9)),
        legend.text = element_text(size= rel(1)),
        legend.position="bottom",
        legend.key=element_blank()) + 
    labs(title="UK Gender Pay Gap",
         subtitle="Median Hourly Pay difference in UK companies (2021)", 
         caption="Data: gender-pay-gap.service.gov.uk | #TidyTuesday Week 26",
         x = NULL,
         y = "Difference between median male and female hourly pay (%)",
         fill = NULL)         

# Export Plot

ggsave("plt.jpeg", plt, device = "jpeg", height = 3000, width = 6000, units = "px")



