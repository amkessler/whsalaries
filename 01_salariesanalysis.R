
library(tidyverse)
library(lubridate)
library(janitor)
library(glue)
library(plotly)
library(DT)
library(googlesheets)
library(kableExtra)
library(leaflet)
# library(ggmap)
library(RColorBrewer)
library(htmlwidgets)
library(htmltools)
library(tigris)
options(tigris_class = "sf")


#import
salaries <- read_csv("whsalaries_2017_2018.csv")

salaries <- salaries %>% 
  clean_names() %>% 
  mutate(
    name = str_to_upper(name),
    last = str_to_upper(last),
    firstandmiddle = str_to_upper(firstandmiddle),
    first = str_to_upper(first),
    status = str_to_upper(status),
    paybasis = str_to_upper(paybasis),
    positiontitle = str_to_upper(positiontitle)
    )

salaries$salary <- parse_number(salaries$salary)
