#18-06-2022
#TidyTuesday - Week 24 US Drought

# Libraries
library(tidyverse)
library(geofacet)
library(ggnewscale)
library(showtext)
library(sysfonts)
showtext_opts(dpi = 320)
showtext_auto(enable = TRUE)

# Font
font_add_google("Roboto", "roboto")
font_add_google("Roboto Condensed", "roboto condensed")
font_add_google("Oswald", "oswald")

# Data
drought <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-14/drought.csv')
drought_fips <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-14/drought-fips.csv')

# Data Processing
df1 = drought %>%
    mutate(DATE=str_remove(DATE,"d_"),
           date=lubridate::ymd(DATE),
           year=lubridate::year(date)) %>%
    filter(year>=2010) %>%
    mutate(state=str_to_title(gsub("-", " ", state)),
           st = state.abb[match(state,state.name)])

range(df1$date)

df1a = df1 %>% select(state, st, date, 3:7) %>%
    pivot_longer(!1:3)

df1b = df1 %>% select(state, st, date, 9:13) %>%
    pivot_longer(!1:3) %>%
    mutate(value=value*-1) 

#Remove Statates Without Data in Geogrid
mygrid = us_state_grid1 %>% filter(!code %in% c("DC","HI","AK"))

#Main Plot
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
    theme(text=element_text(color="black", family="roboto"),
          panel.grid = element_blank(),
          legend.position = "bottom",
          #legend.box="vertical",   #legend in two rows
          #legend.margin=margin(),
          legend.text = element_text(size= rel(.4)), 
          axis.text.y=element_blank(),
          axis.text.x = element_text(color="white", size=rel(.3)),
          axis.title=element_blank(),
          axis.ticks.y=element_blank(),
          axis.ticks.x = element_line(color="white", size=.2),
          plot.title=element_text(family="oswald", size=rel(1.2), hjust=0.5),
          plot.subtitle = element_text(family="oswald", hjust=0.5, margin=margin(b=6)),
          strip.text = element_text(size=rel(.5)),
          legend.justification = "center",
          plot.caption=element_text(family="roboto condensed", size=rel(.5)),
          plot.margin=margin(.5,.5,.5,.5, unit="cm"),
          plot.background=element_rect(fill="white", color=NA)) +
    labs(title="U.S. Drought Conditions",
         subtitle="from January 2010 to April 2022", 
         caption="Data: National Integrated Drought Information System | #TidyTuesday week 24")         
