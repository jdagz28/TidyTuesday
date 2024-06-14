# Load necessary libraries
library(tidytuesdayR)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)
library(skimr)
library(stringr)

# Load Data
tuesdata <- tidytuesdayR::tt_load(2024, week = 23)
cheeses <- tuesdata$cheeses

# Clean and preprocess the data, removing rows with NA in 'family'
data <- cheeses %>%
  mutate(
    country_clean = map(strsplit(as.character(country), ", "), ~str_trim(.x) %>% str_to_title() %>% unlist())
  ) %>%
  unnest(country_clean) %>%
  filter(!is.na(country_clean) & !is.na(family))

# Calculate total cheese production per country
cheese_by_country <- data %>%
  count(country_clean) %>%
  group_by(country_clean) %>%
  summarise(total_cheese = sum(n)) %>%
  arrange(desc(total_cheese)) # Arrange by descending total cheese production

# Top 10 cheese producing countries
top_countries <- cheese_by_country %>%
  top_n(10, total_cheese) %>%
  pull(country_clean)  # Extract country names

# Group by country and family, summarize total cheese production
cheese_by_country_family <- data %>%
  count(country_clean, family) %>%
  group_by(country_clean, family) %>%
  summarise(total_cheese = sum(n)) %>%
  arrange(desc(total_cheese)) # Arrange by descending total cheese production

# Select top 5 families based on overall production
top_families <- data %>%
  count(family) %>%
  group_by(family) %>%
  summarize(total_cheese = sum(n)) %>%
  top_n(5, total_cheese) %>%
  pull(family)  # Extract top 5 families

# Filter data for top countries and top families
data_filtered <- data %>%
  filter(country_clean %in% top_countries & family %in% top_families)

# Count cheese production by country for the top 5 families
cheese_by_country_filtered <- data_filtered %>%
  count(country_clean, family) %>%
  group_by(country_clean, family) %>%
  summarise(total_cheese = sum(n)) %>%
  arrange(country_clean, desc(total_cheese))  # Arrange by country name and descending total cheese production per family

# Set theme settings to match previous project style
theme_set(theme_minimal(base_family = "sans", base_size = 22))
theme_update(
  axis.title = element_blank(),
  axis.text.y = element_text(color = "grey25", size = 14, hjust = 0),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  plot.title = element_text(
    size = 24, face = "plain", lineheight = 1.05,
    hjust = 0, vjust = 1,
    margin = margin(0, 0, 10, 0)
  ),
  plot.subtitle = element_text(size = 18, margin = margin(0, 0, 20, 0)),
  plot.caption = element_text(
    size = 14, color = "grey50", hjust = 1,
    margin = margin(t = 20, b = 0)
  ),
  plot.background = element_rect(fill = "white", color = "white"),
  legend.position = "none"
)

family_order <- c("Pecorino", "Gouda", "Brie", "Blue", "Cheddar")

# Plot cheese production by country for the top 5 families (y-axis stacked bar plot)
p <- ggplot(data = cheese_by_country_filtered, aes(x = total_cheese, y = factor(family, levels = family_order), fill = country_clean)) +
  geom_col(position = "stack") +
  scale_x_continuous(guide = "none") +
  theme(axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 14, color = "grey25", hjust = 0))


print(p)
