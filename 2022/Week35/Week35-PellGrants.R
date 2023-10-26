setwd("~/Jdagz Documents/TidyTuesday/Week 35")

# Get the Data
tuesdata <- tidytuesdayR::tt_load('2022-08-30')

pell <- tuesdata$pell


# Libraries
library(tidyverse)
library(ggplot2)
library(cowplot)
library(skimr)
library(sysfonts)
library(scales)
library(geofacet)
library(gridExtra)
library(grid)


# Font
font_add_google("Roboto", "roboto")
font_add_google("Roboto Condensed", "roboto condensed")
font_add_google("Oswald", "oswald")


# Data Check
glimpse(pell)
skim(pell)
colnames(pell)


# Data Wrangling
pellTOTALS <- pell %>%
    group_by(YEAR) %>%
    summarize(total_award = sum(AWARD, na.rm = TRUE),
              total_recipient = sum(RECIPIENT)) %>%
    ungroup() %>%
    mutate(award_per_recipient = total_award / total_recipient) %>%
    pivot_longer(cols = -YEAR, names_to = "metric", values_to = "value") %>%
    mutate(metric = case_when(metric == "total_award" ~ "Total Awarded ($)",
                              metric == "total_recipient" ~ "Total Recipients",
                              TRUE ~ "Award per Recipient ($)"),
           metric = fct_relevel(metric, "Total Awarded ($)", "Total Recipients"))

pellbystate <- pell %>%
    group_by(YEAR, STATE) %>%
    summarise(total = sum(AWARD))

pellbyavg <- pell %>%
    group_by(YEAR, STATE) %>%
    summarize(total_award = sum(AWARD, na.rm = TRUE),
              total_recipient = sum(RECIPIENT)) %>%
    ungroup() %>%
    mutate(award_per_recipient = total_award / total_recipient)

pellbyavgALL <- pell %>%
    group_by(YEAR) %>%
    summarize(total_award = sum(AWARD, na.rm = TRUE),
              total_recipient = sum(RECIPIENT)) %>%
    ungroup() %>%
    mutate(award_per_recipient = total_award / total_recipient)

pellmaxALL <- pell %>%
    mutate(maxaward_per_recipient = AWARD / RECIPIENT) %>%
    group_by(YEAR) %>%
    summarize(max = max(maxaward_per_recipient, na.rm = TRUE))
    
    #with names
    #mutate(max = max(maxaward_per_recipient, na.rm = TRUE)) %>%
    #ungroup() %>%
    #filter(maxaward_per_recipient == max)

pellbyrecipient <- list(pellbyavgALL, pellmaxALL) %>%
    reduce(full_join, by="YEAR")
    


# Plotting
plt1 <- ggplot(pellbystate, aes(x = YEAR, y = total)) +
    geom_area(fill = "#2166ac") + 
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
    facet_geo(~STATE, grid = "us_state_grid2") +
    theme_bw() + 
    labs(y = "Award ($)", 
         x = "" ) + 
    theme(text=element_text(color="black", family="roboto"),
      panel.grid = element_blank(),
      axis.text.y=element_text(size=rel(1), family = "roboto condensed"),
      axis.text.x = element_text(size=rel(1), family = "roboto condensed"),
      axis.title.y = element_text(family = "roboto condensed", size=rel(1.5)),
      strip.text = element_text(size=rel(1)),
      plot.background=element_rect(fill="white", color=NA),
      plot.title=element_text(family="oswald", face="bold", size=rel(3)),
      plot.subtitle = element_text(family="oswald", size=rel(1.5)),
      plot.caption=element_text(family="roboto condensed", size=rel(1.7), hjust = 1),) + 
    labs(title = "Pell Grants, 1999 - 2017",
         subtitle = "The total amount of Pell Grants awarded by state and year. Pell Grants by the U.S Department of Education \nare awarded to undergraduate students with exceptional financial need and have not earned a degree.")
    

plt2 <- ggplot(pellbyrecipient, aes(x = YEAR)) +
    geom_line(aes(y = max,), size = 5, color = "#E51837") + 
    geom_line(aes(y = award_per_recipient), size = 5, color = "#2166ac") + 
    scale_y_log10(labels = label_dollar(accuracy = 1)) +
    annotate(geom = "text", x = 2014, y = 9000, label = "Maximum Pell Grant", hjust = "left", size=rel(6.5), color = "#E51837", family = "roboto" )+ 
    annotate(geom = "text", x = 2014, y = 4300, label = "Average Pell Grant", hjust = "left", size=rel(6.5), color = "#2166ac", family = "roboto" )+ 
    theme_minimal_hgrid(line_size = 2) +
    theme(
        axis.text.x = element_text(family = "roboto condensed", size=rel(1.3)),
        axis.text.y = element_text(family = "roboto condensed", size=rel(1.1)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(family = "roboto condensed", size=rel(1.5))
    ) +
    labs(y = "Award per recipient ($)",
         caption = "Data: Pell Awards {pell}  |  #TidyTuesday Week 35")

# Export plots
ggsave("Week35-1.png", plt1, height = 3500, width=6000, units="px", bg="white") 
ggsave("Week35-1.svg", plt1, height = 3500, width=6000, units="px", bg="white") 
ggsave("Week35-2.png", plt2, height = 2000, width=6000, units="px", bg="white") 
ggsave("Week35-2.svg", plt2, height = 2000, width=6000, units="px", bg="white") 

