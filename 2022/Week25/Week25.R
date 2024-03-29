#28-06-2022
#TidyTuesday - Week 25 US Slave Trade

# Libraries
library(tidyverse)
library(sysfonts)
library(tidygeocoder)


# Font
font_add_google("Roboto", "roboto")
font_add_google("Roboto Condensed", "roboto condensed")
font_add_google("Oswald", "oswald")

# Get the Data

setwd("~/Jdagz Documents/TidyTuesday/Week 25")


#blackpast <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/blackpast.csv')
#census <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/census.csv')
slave_routes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/slave_routes.csv')
#african_names <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/african_names.csv')

# Or read in with tidytuesdayR package (https://github.com/thebioengineer/tidytuesdayR)

# Either ISO-8601 date or year/week works

# Install via devtools::install_github("thebioengineer/tidytuesdayR")

#tuesdata <- tidytuesdayR::tt_load('2020-06-16')
#tuesdata <- tidytuesdayR::tt_load(2020, week = 25)


#blackpast <- tuesdata$blackpast


# Data Prep - Remove unspecified ports
routes <- slave_routes %>%
    mutate(port_arrival = str_remove_all(string = port_arrival, pattern = ", port unspecified"),
           port_origin = str_remove_all(string = port_origin, pattern = ", port unspecified"),
           place_of_purchase = str_remove_all(string = place_of_purchase, pattern = ", port unspecified"))

# Geolocation of Ports
# (geocoding and dataprep operations from ShalvaKvi)
route_data_unique <- routes %>%
   select(port_arrival:port_origin) |> 
   pivot_longer(cols = port_arrival:port_origin, values_to = "location", names_to = NULL) |> 
   distinct()

route_data_unique <- route_data_unique %>% 
    mutate(location = ifelse(location == "Cap Francais", yes = "Cap-Ha?tien", no = location))

route_data_unique <- route_data_unique |> 
   tidygeocoder::geocode(address = location)

# combine routes
route_data <- routes |> 
    left_join(route_data_unique |>  select(location, "origin_lat" = lat, "origin_long" = long), 
              by = c("port_origin" = "location")) |> 
    left_join(route_data_unique |>  select(location, "purchase_lat" = lat, "purchase_long" = long), 
              by = c("place_of_purchase" = "location")) |> 
    left_join(route_data_unique |>  select(location, "arrival_lat" = lat, "arrival_long" = long), 
              by = c("port_arrival" = "location"))

# mutate trip names
route_data <- route_data |> 
    mutate(trip1 = paste(port_origin, "-", place_of_purchase),
           trip2 = paste(place_of_purchase, "-", port_arrival))

# drop NAs
route_data <- route_data |> 
    drop_na(origin_lat:arrival_long) |> 
    drop_na(n_slaves_arrived)

# Filter out routes which end at the same location, this causes errors with the plot
route_data <- route_data |> 
    filter(origin_long != purchase_long) |> 
    filter(purchase_long != arrival_long)

# Summarize route information
route_data_summ <- route_data |> 
    group_by(trip1, trip2) |> 
    summarise(n_trips = n(),
              n_slaves = sum(n_slaves_arrived)) |> 
    ungroup() |> 
    left_join(route_data |> select(trip1, origin_long, origin_lat, purchase_long, purchase_lat) |> distinct(),
              by = "trip1") |> 
    left_join(route_data |> select(trip2, arrival_long, arrival_lat) |> distinct(),
              by = "trip2")

# Map information, take out Antarctica
map <- map_data("world", interior = FALSE) |> 
    filter(region != "Antarctica")

# Take out trips that only occurred once, this is optional, makes final plot less hectic
route_data_summ <- route_data_summ |> 
    filter(n_trips > 1)


# Groups trips that occured more than once and sum the number of slaves
## For slave purchase (origin) location
route_data_purchase <- route_data_summ |> 
    group_by(purchase_long, purchase_lat) |> 
    summarise(n_slaves = sum(n_slaves))

route_data_purchase <- route_data_purchase |> 
    left_join(route_data |> select("loc_pur" = place_of_purchase, purchase_lat, purchase_long) |> distinct(),
              by = c("purchase_lat", "purchase_long"))

## For slave arrival location
route_data_arrival <- route_data_summ |> 
    group_by(arrival_long, arrival_lat) |> 
    summarise(n_slaves = sum(n_slaves))

route_data_arrival <- route_data_arrival |> 
    left_join(route_data |> select("loc_arr" = port_arrival, arrival_lat, arrival_long) |> distinct(),
              by = c("arrival_lat", "arrival_long"))



# Plot ----
plt <- ggplot() +
    
    #geoms
    ## Drawing map
    geom_polygon(data = map, aes(x = long, y = lat, group = group),
                 fill = "grey40", color = "grey40", size = 1)  +
    ## Drawing Curves
    geom_curve(data = route_data_summ, aes(x = purchase_long, y = purchase_lat,
                                           xend = arrival_long, yend = arrival_lat,
                                           group = trip2, alpha = n_slaves),
               color = "#2b2928", size = 1.2, show.legend = FALSE) +
    ## Drawing points of purchase
    geom_point(data = route_data_purchase, aes(x = purchase_long, y = purchase_lat),
               color = "black",shape = 19, alpha = 0.8) +
    ## Drawing points of arrival
    geom_point(data = route_data_arrival, aes(x = arrival_long, y = arrival_lat),
               color = "black", shape = 19, alpha = 0.8) +

    # scales
    scale_y_continuous(limits = c(-55, 100)) +
    scale_size_continuous(range = c(1,15)) +
    scale_alpha_continuous(range = c(0.05, 1)) +
    
    # Annotations
    ## Header
    annotate("text", x = 0, y = 95, size = 20, family = "oswald", color = "black", label = "Slave Trade Routes") +
    annotate("text", x = 0, y = 85, size = 14, family = "oswald", color = "black", label = "1562-1864") +
    
    ## Caption
    annotate("text", x = 160, y = -55, size = 4, alpha = 0.9, color = "#2b2928", family = "roboto", 
             label = "Data: Slave Voyages (slavevoyages.org) | #TidyTuesday Week 25") +
    
    # theme adjustments
    ggthemes::theme_map() +
    theme(panel.background = element_rect(fill = "white"),
          legend.position = "none")


plt 



# save
ggsave("plt.jpeg", plt, device = "jpeg", height = 3000, width = 6000, units = "px")


