# Load necessary libraries
library(tidytuesdayR)
library(tidyverse)
library(ggplot2)
library(skimr)
library(ggtext)
library(ggrepel)

# Load Data
tuesdata <- tidytuesdayR::tt_load(2024, week = 29)

# Set Working Directory
setwd("~/++documents/TidyTuesday/2024/Week 29")

# Data Exploration
glimpse(tuesdata$ewf_appearances)
skim(tuesdata$ewf_appearances)

glimpse(tuesdata$ewf_matches)
skim(tuesdata$ewf_matches)

glimpse(tuesdata$ewf_standings)
skim(tuesdata$ewf_standings)

# Data Preparation
dataset <- tuesdata$ewf_standings %>%
  filter (tier %in% "1") %>%
  select(season, team_name, goals_for, position) %>%
  mutate(
    # Check and standardize team name (Chelsea Ladies, Chelsea Women)
    team_name = gsub(" Ladies$", "", team_name), 
    team_name = gsub(" Women$", "", team_name),
    # Format same year season (2011-2011 to 2011)
    season = ifelse(
      str_extract(season, "^\\d{4}") == str_extract(season, "\\d{4}$"),
      str_extract(season, "^\\d{4}"),  
      season  
    )
  ) %>% 
  group_by(team_name, season, position) %>%
  summarise(
    season_goals = sum(goals_for, na.rm = TRUE),
    .groups = "keep"
  ) %>%
  arrange(team_name, season) %>%
  group_by(team_name) %>%
  mutate(
    total_goals = cumsum(season_goals),
  ) %>%
  ungroup()

# Focus line graph on top 3 teams
top3 <- dataset %>%
  filter(team_name %in% c("Chelsea", "Manchester City", "Arsenal"))

# Grey out lines for non top 3 team
rest <- dataset %>%
  filter(!team_name %in% c("Chelsea", "Manchester City", "Arsenal"))

# Most recent season total_goals point
top3_latest <- top3 %>%
  filter(season %in% "2023-2024")

# Set team colors for top 3 teams
top3_colors <- c(
  "Chelsea" = "#034694",
  "Manchester City" = "#6CABDD",
  "Arsenal" = "#EF0107"
)

# Season winners
top1_season <- dataset %>%
  filter(position %in% "1") %>% 
  mutate(team_name = ifelse(team_name %in% names(top3_colors), team_name, NA))

liverpool <- dataset %>%
  filter(position %in% "1") %>% 
  filter(!team_name %in% c("Chelsea", "Manchester City", "Arsenal"))

rect_data <- top1_season %>%
  filter(position == 1) %>%
  mutate(
    start = as.numeric(as.factor(season)) - 0.5,
    end = as.numeric(as.factor(season)) + 0.5
  )

# Set theme settings
theme_set(theme_minimal(base_family = "sans", base_size = 22))
theme_update(
  axis.title = element_blank(),
  axis.text = element_text(color = "grey40"),
  axis.text.x = element_text(size = 10),
  axis.text.y = element_text(size = 13),
  axis.ticks = element_line(color = "grey", size = .5),
  panel.grid.minor = element_blank(),
  panel.grid.major.y = element_line(color = "grey", size = 0.5),
  panel.grid.major.x = element_blank(),
  plot.title = element_textbox_simple(
    size = rel(1.25), 
    face = "plain", 
    lineheight = 1.05, 
    fill = "transparent", 
    width = unit(10, "inches"), 
    box.color = "transparent", 
    margin = margin(0, 0, 5, 0), 
    hjust = 0, 
    halign = 0,
    family = "Oswald"
  ),
  plot.subtitle = element_markdown(
    size = rel(0.75), 
    margin = margin(0, 0, 10, 0),
    lineheight = 1.2,
    family = "Roboto Condensed"
  ),
  plot.caption = element_markdown(
    size = rel(.5), 
    color = "black", 
    hjust = 1, 
    margin = margin(t = 5, b = 0),
    family = "Roboto Condensed"
  ),
  plot.title.position = "plot",
  plot.caption.position = "plot",
  plot.margin = margin(30, 30, 10, 30),
  plot.background = element_rect(fill = "white", color = "white"),
  legend.position = "bottom"
)


ggplot() +
  geom_rect(data = rect_data,
            aes(xmin = start, xmax = end, fill = team_name), 
            ymin = -Inf, 
            ymax = Inf, 
            alpha = 0.4, 
            show.legend = FALSE) + 
  geom_line(data = rest, 
            aes(x = season, y = total_goals, group = team_name),
            linetype = "dashed",
            color = "#5e5e5e",
            size = 0.7,
            alpha = 0.3,
            show.legend = FALSE) +
  geom_line(data = top3, 
            aes(x = season, y = total_goals, color = team_name, group = team_name),
            size = 1.5,
            show.legend = FALSE) +
  geom_point(data = top3_latest,
             aes(x = season, y = total_goals, color = team_name),
             size = 4,
             show.legend = FALSE) +
  geom_point(data = liverpool,
             aes(x = season, y = total_goals),
             size = 4,
             color = "#FD1220",
             alpha = 0.4,
             show.legend = FALSE) + 
  geom_text_repel(data = top3_latest,
             aes(x = season, y = total_goals, label = team_name, color = team_name),
             nudge_x = -0.5, 
             hjust = -1,
             size = 6,
             show.legend = FALSE,
             direction = "y",
             max.overlaps = Inf,
             family = "Roboto Condensed",
             fontface = "bold",
             box.padding = 0.5,
             bg.color = "white",
             bg.r = 0.2,
             segment.color = NA,) + 
  scale_fill_manual(
    values = top3_colors,  
    na.value = "transparent") +
  scale_color_manual(values = top3_colors) + 
  scale_y_continuous(
    labels = scales::comma,
    breaks = seq(0, 650, by = 100),
    limits = c(0, 650),
    expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0)) +
  labs(
    title = paste(
      "<b style='color:#034694;'>The Blues takeover: Chelsea</b>",
      "overtakes <b style='color:#EF0107;'>Arsenal</b> in most goals in English Women's Football"
    ),
    subtitle = paste(
      "<b style='color:#EF0107;'>Arsenal</b>, despite winning only three seasons (2011, 2012, 2018-2019) since 2011,",
      "remained the top scorer <br> in English Women's Football for over a decade.",
      "<b style='color:#034694;'>Chelsea</b> overtook Arsenal last season to claim the title of top goal scorer."
    ),
    caption = paste(
      "<b>*Shaded areas indicate season winners. Liverpool won the 2013 and 2014 seasons.</b><br><br>",
      "Joshua Dagoy | Data: English Women's Football (EWF) Database  | #TidyTuesday 2024 - Week 29"
    )
  ) 

ggsave("Week29.png", width = 12, height = 7, dpi = 600)
