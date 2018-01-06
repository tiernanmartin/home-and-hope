# Setup ----

library(tidyverse)
library(sf)
library(mapview)
library(snakecase)
library(AmesHousing)
library(rpart)
library(rattle)
library(skimr)

root <- rprojroot::is_rstudio_project
root_file <- root$make_fix_file()
options(httr_oob_default=TRUE) 
htmltools::tagList(rmarkdown::html_dependency_font_awesome())
Sys.setlocale("LC_CTYPE", "Chinese")

# Access Data ----

a <- make_ames() %>% 
  rename_all(to_screaming_snake_case) %>% 
  mutate(geometry = st_sfc(map2(LONGITUDE, LATITUDE, ~ st_point(c(.x,.y))))) %>% 
  st_sf %>% 
  st_set_crs(4326)

# View Data ----

mapview(a)
