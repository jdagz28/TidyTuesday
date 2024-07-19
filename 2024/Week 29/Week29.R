# Load necessary libraries
library(tidytuesdayR)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(skimr)
library(extrafont)

# Load Data
tuesdata <- tidytuesdayR::tt_load(2024, week = 29)

setwd("~/++documents/TidyTuesday/2024/Week 28")
loadfonts(device = "win")

# Data Exploration
colnames(tuesdata$ewf_appearances)
glimpse(tuesdata$ewf_appearances)
skim(tuesdata$ewf_appearances)


colnames(tuesdata$ewf_matches)
glimpse(tuesdata$ewf_matches)
skim(tuesdata$ewf_matches)


colnames(tuesdata$ewf_standings)
glimpse(tuesdata$ewf_standings)
skim(tuesdata$ewf_standings)

# Data Preparation
dataset <- tuesdata$ewf_standings %>%
  mutate(
    team_name = gsub(" Ladies$", "", team_name), 
    team_name = gsub(" Women$", "", team_name)  
  ) %>%
  group_by(team_name, season) %>%
  summarise(
    season_goals = sum(goals_for, na.rm = TRUE),
    .groups = "keep"
  ) %>%
  arrange(team_name, season) %>%
  group_by(team_name) %>%
  mutate(
    total_goals = cumsum(season_goals)
  ) %>%
  ungroup()

top3 <- dataset %>%
  filter(team_name %in% c("Chelsea", "Manchester City", "Arsenal"))

rest <- dataset %>%
  filter(!team_name %in% c("Chelsea", "Manchester City", "Arsenal"))

top3_latest <- top3 %>%
  filter(season %in% "2023-2024")

# Define custom colors for the teams
top3_colors <- c(
  "Chelsea" = "#034694",
  "Manchester City" = "#6CABDD",
  "Arsenal" = "#EF0107"
)

# Create the shaded area graph
ggplot() +
  geom_line(data = rest, 
            aes(x = season, y = total_goals, group = team_name),
            linetype = "dashed",
            color = "#5e5e5e",
            size = 0.5,
            alpha = 0.3,
            show.legend = FALSE) +
  geom_line(data = top3, 
            aes(x = season, y = total_goals, color = team_name, group = team_name),
            size = 1.2,
            show.legend = FALSE) +
  geom_point(data = top3_latest,
             aes(x = season, y = total_goals, color = team_name),
             size = 4,
             show.legend = FALSE) + 
  scale_color_manual(values = top3_colors) + 
  scale_y_continuous(
    labels = scales::comma,
    breaks = seq(0, 600, by = 100)
  ) +
  scale_x_discrete(expand = c(0.01, 0)) +
  labs(
    title = "",
    x = "Season",
    y = "Goals",
    color = "Team"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_text(color = "grey40"),
    axis.text.x = element_text(size = 10, angle = 45, margin = margin(t = 5)),
    axis.text.y = element_text(size = 13, margin = margin(r = 5)),
    axis.ticks = element_line(color = "grey91", size = .5),
    axis.ticks.length.x = unit(1.3, "lines"),
    axis.ticks.length.y = unit(.7, "lines"),
    panel.grid = element_blank()
  )
