setwd("~/Jdagz Documents/TidyTuesday/Week 33")

# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

tuesdata <- tidytuesdayR::tt_load('2022-08-16')
#tuesdata <- tidytuesdayR::tt_load(2022, week = 33)

characters <- tuesdata$characters
psych_stats <- tuesdata$psych_stats
myers_briggs <- tuesdata$myers_briggs

# Or read in the data manually

#characters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-16/characters.csv')


#Libraries
library(tidyverse)
library(fmsb)
library(ggplot2)
library(cowplot)



# Data Wrangling
# Check characters for Grey's Anatomy
GA_char <- characters %>%
    filter(uni_name == "Grey's Anatomy") %>%
    filter(name != c("Preston Burke", "Addison Montgomery"))

# Check questions and personality traits
distinct(data.frame(psych_stats$question))

GA_questions <- psych_stats %>% 
    filter(uni_name == "Grey's Anatomy") %>% 
    #filter(char_name != c("Preston Burke", "Addison Montgomery")) %>%
    group_by(personality, question) %>% 
    summarise(n = n()) %>%
    filter(n == 8) 


# Get personality traits and scores
scores <- psych_stats %>% 
    filter(uni_name == "Grey's Anatomy") %>% 
    filter(char_name != c("Preston Burke", "Addison Montgomery")) %>%
    select(c(char_id, personality, avg_rating)) %>% 
    pivot_wider(names_from = "personality", values_from = "avg_rating")

scores_pers <- scores %>%
    select(char_id,
           assertive, 
           motivated,
           competitive,
           direct,
           resourceful,
           decisive, 
           efficient, 
           rational)


# Join - Character Names
dataset <- characters %>% 
    filter(uni_name == "Grey's Anatomy") %>% 
    filter(name != c("Preston Burke", "Addison Montgomery")) %>%
    left_join(scores_pers, by = c("id" = "char_id")) %>% 
    select(-c(id, uni_id, uni_name, notability, link, image_link)) %>%
    column_to_rownames(var = "name") %>%
    replace(.=="NULL", 0) %>% 
    mutate(across(.fns = ~ if(all(!is.na(as.numeric(.x)))) as.numeric(.x) else .x))


max_min <- data.frame(assertive = c(100,0), 
                      motivated = c(100,0),
                      competitive = c(100,0),
                      direct = c(100,0),
                      resourceful = c(100,0),
                      decisive = c(100,0), 
                      efficient = c(100,0), 
                      rational = c(100,0))

rownames(max_min) <- c("Max", "Min")
dataset <- rbind(max_min, dataset)

# Plotting - FMSB radarchart 
create_beautiful_radarchart <- function(data, color = "#00AFBB", 
                                        vlabels = colnames(data), vlcex = 1,
                                        caxislabels = NULL, title = NULL, ...){
    radarchart(
        data, axistype = 1,
        # Customize the polygon
        pcol = color, pfcol = scales::alpha(color, 0.4), plwd = 3, plty = 1,
        # Customize the grid
        cglcol = "grey", cglty = 1, cglwd = 0.8,
        # Customize the axis
        axislabcol = "grey", 
        # Variable labels
        vlcex = vlcex, vlabels = vlabels,
        caxislabels = caxislabels, title = title
    )
}

par(mfrow = c(2, 4))

titles <- row.names(dataset[3:10,])

plt <- recordPlot()
for(i in 1:8){
    create_beautiful_radarchart(
        data = dataset[c(1, 2, i+2), ], caxislabels = c(0, 25, 50, 75, 100),
        color = "#3F7AB5", title = titles[i]
    ) 
}

