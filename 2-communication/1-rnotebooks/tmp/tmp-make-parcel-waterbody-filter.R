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
library(scales)
library(lwgeom)
library(tictoc)
library(rbenchmark)

root <- rprojroot::is_rstudio_project
root_file <- root$make_fix_file()
options(httr_oob_default=TRUE) 
htmltools::tagList(rmarkdown::html_dependency_font_awesome())

# Access Data ----

# FILTERED PARCELS

p_id <- as_id("1wiBedRBk8Ygx7jZGctdH0kdLe-4jlQr4")

p <- drive_read(p_id, TRUE, read_fun = st_read, stringsAsFactors = FALSE) 

p %<>% 
  st_set_crs(2926)

p_nest <- st_nest_sf(p)


# WATERBODIES

w_id <- as_id("1OF2Z0sNWBmdDdZ4lPgUoolVELd7HCyWL")

w <- drive_read(w_id, TRUE, read_fun, read_sf, stringsAsFactors = FALSE)

w_union_nest <- w %>%
  st_nest_sf %>% 
  st_set_crs(4326) %>% 
  st_transform(2926) %>% 
  st_union()

# Filters ----





# Upload to Drive ----

