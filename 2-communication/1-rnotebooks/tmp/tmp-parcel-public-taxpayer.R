# Setup ----

library(tidyverse)
library(stringr)
library(sf)
library(googledrive)
library(miscgis)
library(forcats)
library(leaflet)
library(mapview)
library(miscgis)
library(snakecase)
library(magrittr)
library(rprojroot)
library(knitr) 
library(lubridate)
library(RColorBrewer) 

root <- rprojroot::is_rstudio_project
root_file <- root$make_fix_file()
options(httr_oob_default=TRUE) 
htmltools::tagList(rmarkdown::html_dependency_font_awesome())

# Load data ----

# Public parcels from KC GIS

#NOTE: change this to the Google Drive process

pub_fp <- root_file("1-data/2-external/public_parcels/")

pub <- st_read(dsn = pub_fp)

names <- 
  pub %>% 
  st_drop_geometry() %>% 
  transmute(KCTP_NAME = as.character(KCTP_NAME)) %>% 
  group_by(KCTP_NAME) %>% 
  tally %>% 
  arrange(desc(n))

# KC Real prop extract

realprop <- RSocrata::read.socrata("https://data.kingcounty.gov/resource/mmfz-e8xr.csv",
                                   email = "tiernan@futurewise.org",
                                   password = "SAwu@T191")

r <- 
  realprop %>% 
  rename_all(to_screaming_snake_case) 

housing_authorities <- 
  r %>% 
  filter(str_detect(TAXPAYERNAME,"AUTHORITY")) %>% 
  group_by(TAXPAYERNAME) %>% 
  tally %>% 
  arrange(desc(n))
