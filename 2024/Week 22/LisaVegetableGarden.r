# Load Libraries
library(tidytuesdayR)
library(tidyverse)
library(ggtext)
library(skimr)

# Load Data
tuesdata <- tidytuesdayR::tt_load(2024, week = 22)

harvest_2020 <- tuesdata$harvest_2020
harvest_2021 <- tuesdata$harvest_2021
planting_2020 <- tuesdata$planting_2020
planting_2021 <- tuesdata$planting_2021
spending_2020 <- tuesdata$spending_2020
spending_2021 <- tuesdata$spending_2021

# Data Check
glimpse(harvest_2020)
skim(harvest_2020)

glimpse(planting_2020)
skim(planting_2020)

glimpse(spending_2020)
skim(spending_2020)


# Data Manipulation - Summarize Harvest Data by Year
harvest2020 <- harvest_2020 %>%
  group_by(vegetable) %>%
  summarize(annual_harvest = sum(weight),
            avg_month_harvest = sum(weight) / 12) %>%
  mutate(year = 2020)

harvest2021 <- harvest_2021 %>%
  group_by(vegetable) %>%
  summarize(annual_harvest = sum(weight),
            avg_month_harvest = sum(weight) / 12) %>%
  mutate(year = 2021)

# Combined Harvest Data for Both Years
combined_harvest <- bind_rows(harvest2020, harvest2021)

# Reshape and Filter Data
reshaped_harvest <- combined_harvest %>%
  select(vegetable, year, annual_harvest) %>%
  pivot_wider(names_from = year, values_from = annual_harvest, names_prefix = "year_") 

# Exclude vegetables not planted in both years
vegetables_with_na <- reshaped_harvest %>%
  filter(is.na(year_2020) | is.na(year_2021)) %>%
  select(vegetable)

bothyears_harest <- reshaped_harvest %>%
  drop_na(year_2020, year_2021)


bothyears_harvest <- bothyears_harest %>%
  mutate(
    percent_diff = (year_2021 - year_2020) / year_2020 * 100
  )

# Plot Theme Setup
theme_set(theme_minimal(base_family = "sans", base_size = 22))
theme_update(
  axis.title = element_blank(),
  axis.text.y = element_text(hjust = 0, color = "grey25"),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  plot.title = element_textbox_simple(
    size = rel(1.25), face = "plain", lineheight = 1.05, 
    fill = "transparent", width = unit(8, "inches"), box.color = "transparent", 
    margin = margin(0, 0, 5, 0), hjust = 0, halign = 0
  ),
  plot.subtitle = element_text(size = rel(0.75), margin = margin(0, 0, 10, 0)),
  plot.caption = element_markdown(
    size = rel(.5), color = "grey50", hjust = 1, margin = margin(t = 20, b = 0),
    family = 'sans'
  ),
  plot.title.position = "plot",
  plot.caption.position = "plot",
  plot.margin = margin(25, 25, 10, 25),
  plot.background = element_rect(fill = "white", color = "white"),
  legend.position = "none"
)

# Prepare for plotting
filtered_data <- bothyears_harvest %>%
  gather(key = "year", value = "weight", year_2020, year_2021) %>%
  mutate(year = ifelse(year == "year_2020", 2020, 2021)) %>%
  select(-percent_diff)
#%>%  filter(!(vegetable %in% c("tomatoes", "strawberries", "radish", "cilantro", "asparagus")))
filtered_data$vegetable <- str_to_title(filtered_data$vegetable)

# Plot annual harvest
plt = ggplot(filtered_data, aes(x = weight, y = vegetable))
plt <- plt + geom_line(aes(group = vegetable), linewidth = 1)
plt <- plt + geom_point(data = filtered_data %>% filter(year == 2020), aes(color = "red"), size = 3)
plt <- plt + geom_point(data = filtered_data %>% filter(year == 2021), aes(color = "blue"), size = 3)
plt

# Focus analysis on harvest of tomato varieties
tomatoes_2020 <- harvest_2020 %>%
  filter(vegetable == "tomatoes") %>%
  group_by(variety) %>%
  summarize(annual_harvest = sum(weight),
            avg_month_harvest = sum(weight) / 12) %>%
  mutate(year = 2020)

tomatoes_2021 <- harvest_2021 %>%
  filter(vegetable == "tomatoes") %>%
  group_by(variety) %>%
  summarize(annual_harvest = sum(weight),
            avg_month_harvest = sum(weight) / 12) %>%
  mutate(year = 2021)

# Combined tomato harvests for both years
combined_tomatoes <- bind_rows(tomatoes_2020, tomatoes_2021)

# Tomato variety harvests
reshaped_tomatoes <- combined_tomatoes %>%
  select(variety, year, annual_harvest) %>%
  pivot_wider(names_from = year, values_from = annual_harvest, names_prefix = "year_") 

# Identify tomato varieties not planted in both years
tomatoes_with_na <- reshaped_tomatoes %>%
  filter(is.na(year_2020) | is.na(year_2021)) %>%
  select(variety)

# Identify tomato varieties planted in both years
bothyears_tomatoes <- reshaped_tomatoes %>%
  drop_na(year_2020, year_2021) %>%
  mutate(
    percent_diff = (year_2021 - year_2020) / year_2020 * 100
  )

# Prepare for visualization
tomatoes_for_plot <- bothyears_tomatoes %>%
  gather(key = "year", value = "weight", year_2020, year_2021) %>%
  mutate(year = ifelse(year == "year_2020", 2020, 2021)) %>%
  mutate(weight = weight / 1000) %>%
  #select(-percent_diff) %>%
  arrange(desc(percent_diff))
tomatoes_for_plot$variety <- factor(tomatoes_for_plot$variety, levels = unique(tomatoes_for_plot$variety))

# Plot tomato harvest trends
 plt= ggplot(tomatoes_for_plot, aes(x = weight, y = variety)) +
  geom_line(aes(group = variety), size = 3.5, color = "#E7E7E7") +
  geom_point(data = tomatoes_for_plot %>% filter(year == 2020), color = "#F99B7D", size = 6) +
  geom_point(data = tomatoes_for_plot %>% filter(year == 2021), color = "#E76161", size = 6) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(expand = expansion(add = c(2, 1)), guide = "none") +
  scale_y_discrete(limits = rev(levels(tomatoes_for_plot$variety))) + 
  geom_text(aes(label = round(weight, 1)),
            hjust = ifelse(tomatoes_for_plot$variety %in% c("Big Beef", "Mortgage Lifter") & tomatoes_for_plot$year == 2020, 1.5,
                           ifelse(tomatoes_for_plot$variety %in% c("Cherokee Purple", "Black Krim", "Bonny Best", "Old German", "Amish Paste") 
                                  & tomatoes_for_plot$year == 2021, 1.7, -0.7)),
            vjust = 0.5, size = 3) +
  geom_text(data = tomatoes_for_plot %>%
              group_by(variety) %>%
              summarize(mid_weight = sum(weight) / 2) %>%
              inner_join(tomatoes_for_plot %>% filter(!duplicated(variety))),
            aes(label = paste0(round(percent_diff, 1), "%"), x = mid_weight, y = variety, color = ifelse(percent_diff > 0, "#28A87D", "red")),
            hjust = 0.5, vjust = -0.7, size = 3, fontface = "bold") + 
  scale_color_manual(values = c("#28A87D", "#E76161")) +
  scale_fill_manual(values = "grey50") +
  labs(
    title = paste(
      "Analyzing Lisa Lendway's tomato harvest (kg).",
      "<b>Performance of the seven varieties of tomatoes in",
      "<b style='color:#F99B7D;'>2020</b> and",
      "<b style='color:#E76161;'>2021</b>.</b>"
    ),
    subtitle = paste(
      "A total of 19 tomato varieties were planted in two years.", 
      "\nBut only seven of these varieties were planted in both years."
    ),
    caption = paste(
      "Joshua Dagoy | Data: {gardenR} | #TidyTuesday 2024 - Week 22"
    )
  ) +
  theme(axis.text.y = element_text(face = "plain", size = 14, hjust = 0))

plt

setwd("~/++documents/TidyTuesday/2024/Week 22")
ggsave("Week22.png", width = 8.5, height = 7, dpi = 600)
