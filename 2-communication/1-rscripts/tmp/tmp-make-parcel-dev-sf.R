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

# parcel filtered by: uga, zoning, waterbody overlap

p_id <- as_id("1jrEAX7ogq1RdNU-hfrC-tntF66b6NcKg")

p <- drive_read(p_id, TRUE, read_fun = read_sf, stringsAsFactors = FALSE) 

p %<>% 
  st_set_crs(2926) %>% 
  st_transform(4326)


# Create: developable sf ----

# NOTE: This is a placeholder.
#       It currently only makes use of WATER_OVERLAP_PCT to calculate
#       the DEVELOPABLE_PCT, but it could be extended to account for
#       things like steep slopes or environmental hazards.

p_dev <- p %>% 
  mutate(DEVELOPABLE_PCT = 1 - WATER_OVERLAP_PCT)

# Write + Upload Data ----

p_dev_sf_fp <- "./1-data/3-interim/p_dev_sf.gpkg"

p_dev_sf_ready <- p_dev %>% 
  unnest() %>% 
  select(-matches("1$")) %>%   # drop duplicate columns
  mutate_if(is_logical,as.character)

drive_folder <- as_id("0B5Pp4V6eCkhrZ3NHOEE0Sl9FbWc")

st_write(p_dev_sf_ready,dsn = p_dev_sf_fp, driver = "GPKG",layer_options = c("OVERWRITE=YES"))

drive_upload(p_dev_sf_fp, path = drive_folder, name = "p_dev_sf.gpkg")

drive_update(as_id("1O3QRkE7SmW3AxlWSdecARBW7MQ1TmHnH"), p_dev_sf_fp)
