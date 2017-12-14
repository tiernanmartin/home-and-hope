# Setup ----

library(tidyverse) 
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
library(htmltools)

root <- rprojroot::is_rstudio_project
root_file <- root$make_fix_file()
options(httr_oob_default=TRUE) 
htmltools::tagList(rmarkdown::html_dependency_font_awesome())

# Access Data ----

p_id <- as_id("")

p <- drive_read(p_id, TRUE, read_fun = read_sf, stringsAsFactors = FALSE) 

p %<>% 
  st_set_crs(2926) %>% 
  st_transform(4326)

p_nest <- st_nest_sf(p) 

url <- "http://blue.kingcounty.com/Assessor/eRealProperty/Dashboard.aspx?ParcelNbr="

p_sf <- 
  p_nest %>%  
  mutate(PROP_NAME = map_chr(data, "PROP_NAME"),
         PIN = map_chr(data, "PIN"),
         PIN = map_chr(PIN, ~ a(href = str_c(url,.x),target="_blank",.x) %>% as.character),
         WATER_OVERLAP_PCT = map_dbl(data, "WATER_OVERLAP_PCT"),
         WATER_OVERLAP_LGL = map_lgl(data, ~pull(.x, "WATER_OVERLAP_LGL") %>% as.logical),
         CURRENT_ZONING = map_chr(data, "CURRENT_ZONING"),
         CAT = map_chr(data, "CAT"),
         POPUP = str_c(PROP_NAME,PIN,CURRENT_ZONING,CAT,sep = br())
         ) %>% 
  select(-data,-POPUP) # take out POPUP b/c mapview does a good job 

# View Data ----

# All parcels included in this subset
mapview(p_sf)

# Parcels with some waterbody overlap
p_sf %>% filter(WATER_OVERLAP_LGL) %>% mapview()
