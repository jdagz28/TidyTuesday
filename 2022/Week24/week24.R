#18-06-2022
#TidyTuesday - Week 24 US Drought

# Libraries
library(tidyverse)
library(geofacet)
library(ggnewscale)
library(sysfonts)
library(skimr)

# Load datasets from TidyTuesday Repo
drought <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-14/drought.csv')
drought_fips <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-14/drought-fips.csv')

# Data checks
glimpse(drought)
skim(drought)

glimpse(drought_fips)
skim(drought_fips)

# Data Processing
df1 = drought %>%
    mutate(DATE=str_remove(DATE,"d_"), # Remove Prefix d_ in dates
           date=lubridate::ymd(DATE), # Convert date format
           year=lubridate::year(date)) %>% # Extract year from date
    filter(year>=2010) %>% # Filter data from 2010 onwards
    mutate(state=str_to_title(gsub("-", " ", state)),
           st = state.abb[match(state,state.name)]) # Convert state names to state abbreviation 

# Check date range of processed data
range(df1$date)

# Separate drought severity levels into two datasets for plotting
# df1a - positive values
df1a = df1 %>% select(state, st, date, 3:7) %>%
    pivot_longer(!1:3)

# df1b - negative values
df1b = df1 %>% select(state, st, date, 9:13) %>%
    pivot_longer(!1:3) %>%
    mutate(value=value*-1) 

# Remove states without data in geofacet grid
mygrid <- as_tibble(us_state_grid1) %>%
  filter(!code %in% c("DC", "HI", "AK"))

# Plot
df1a %>% ggplot() +
    geom_col(data=df1a, aes(x=date, y=value, fill=name)) +
    scale_fill_brewer("", palette = "OrRd", labels=c("Abnormally dry","Moderate drought","Severe drought","Extreme drought","Exceptional drought")) +
    guides(fill = guide_legend(nrow=1, reverse=TRUE,label.position = "bottom", keywidth=8, vjust=-5)) +
    ggnewscale::new_scale_fill() +
    geom_col(data=df1b, aes(x=date, y=value, fill=name)) +
    scale_x_date(date_labels = "'%y") +
    facet_geo(~st, grid=mygrid, label = "") +
    cowplot::theme_minimal_grid(9.5) +
    scale_fill_brewer("", palette = "Blues", labels=c("Abnormally wet","Moderate wet","Severe wet","Extreme wet","Exceptional wet")) +
    guides(fill = guide_legend(nrow=1, reverse=FALSE,label.position = "bottom", keywidth=8, vjust=-5)) +
    theme(text=element_text(color="black", family="Roboto"),
          panel.grid = element_blank(),
          legend.position = "bottom",
          legend.box="horizontal",   
          legend.justification = "center",
          legend.box.margin = margin(0, 0, 0, 0, unit="cm"),
          legend.text = element_text(size= rel(1.3)), 
          axis.text.y=element_blank(),
          axis.text.x = element_blank(),
          axis.title=element_blank(),
          axis.ticks.y=element_blank(),
          axis.ticks.x = element_blank(),
          plot.title=element_text(family="Oswald", size=rel(3.5), hjust=0.5),
          plot.subtitle = element_text(family="Oswald", size=rel(2), hjust=0.5, margin=margin(b=6)),
          strip.text = element_text(size=rel(1.5)),
          plot.caption=element_text(family="Roboto Condensed", size=rel(1.3)),
          plot.margin=margin(.5,.5,.5,.5, unit="cm"),
          plot.background=element_rect(fill="white", color=NA)) +
    labs(title="U.S. Drought Conditions",
         subtitle="from January 2010 to April 2022", 
         caption="Data: National Integrated Drought Information System | #TidyTuesday Week 24")       

# Save plot
ggsave("TidyTuesday_2022_Week24.png", width = 20, height = 12, dpi = 600)
