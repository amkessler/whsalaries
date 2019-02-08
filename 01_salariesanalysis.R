
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


#import and format columns
salaries <- read_csv("whsalaries_2017_2018.csv")

salaries <- salaries %>% 
  clean_names() %>% 
  mutate(
    name = str_to_upper(name),
    last = str_to_upper(last),
    first_m = str_to_upper(first_m),
    first = str_to_upper(first),
    status = str_to_upper(status),
    paybasis = str_to_upper(paybasis),
    positiontitle = str_to_upper(positiontitle)
  )

salaries$salary <- parse_number(salaries$salary)



#### looking for those who are in both years' of data ####

yr2017 <- salaries %>% 
  filter(year == "2017")

yr2018 <- salaries %>% 
  filter(year == "2018")

joined <- inner_join(yr2017, yr2018, by = c("name" = "name"))

names(joined)

#removed duplicate columns
joined2 <- joined %>% 
  select(
    name,
    salary2017 = salary.x,
    salary2018 = salary.y,
    title2017 = positiontitle.x,
    title2018 = positiontitle.y
    )

raises <- joined2 %>% 
  mutate(
    difference = (salary2018 - salary2017),
    pct_change = (difference/salary2017)*100
  ) %>% 
  select(
    name,
    salary2017,
    salary2018,
    difference,
    pct_change,
    title2017,
    title2018
  )

write_csv(raises, "raises.csv")
