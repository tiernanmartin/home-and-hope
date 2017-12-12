# Setup ----

library(jsonlite)
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
library(downloader)
library(esri2sf)

root <- rprojroot::is_rstudio_project
root_file <- root$make_fix_file()
options(httr_oob_default=TRUE) 
htmltools::tagList(rmarkdown::html_dependency_font_awesome())

# Access Data ----

# WATERBODIES

base_url <- "https://fortress.wa.gov/ecy/ecyprodgislb/arcgis/rest/services/NHD/NHD_Cache/MapServer/"

suffixes <- as.character(0:4)

layers <- c("NHD Coastline",
            "NHD Rivers",
            "NHD Waterbody",
            "NHD Area")

urls <- base_url %>% 
  str_c(suffixes, sep = "") %>% 
  set_names(layers)

w <- esri2sf(urls["NHD Waterbody"])

w %<>% 
  rename_if(not_sfc,to_screaming_snake_case) %>% 
  rename(geometry = geoms) %>% 
  st_as_sf

# KING COUNTY

kc_url <- "https://opendata.arcgis.com/datasets/19b7f1e85a0f4c9ebfcc2830bd1d783e_121.geojson"

kc <- read_sf(kc_url)

kc %<>%
  rename_if(not_sfc,to_screaming_snake_case)

# Filters ----

list(w,kc) %>% map(st_crs)

w_kc <- w %>% 
  mutate(KC_LGL = map_lgl(geoms, ~st_intersects_any(.x,kc))) %>% 
  filter(KC_LGL)
  

# Upload to Drive ----

w_kc_ready <- w_kc %>% 
  mutate_if(is_logical,as.character)

w_kc_fp <- root_file("./1-data/3-interim/waterbodies-kc.gpkg")

drive_folder_id <- as_id("0B5Pp4V6eCkhrZ3NHOEE0Sl9FbWc")

st_write(w_kc_ready, w_kc_fp, layer = "waterbodies_kc", driver = "GPKG", layer_options = "OVERWRITE=TRUE")

drive_upload(media = w_kc_fp, path = drive_folder_id)

# drive_update(file = w_kc_fp,media = as_id("1OF2Z0sNWBmdDdZ4lPgUoolVELd7HCyWL"))
