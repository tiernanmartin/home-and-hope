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

<<<<<<< HEAD
# Ecology: http://www.ecy.wa.gov/services/gis/data/data.htm
# ArcGIS: https://www.arcgis.com/home/item.html?id=b5a20ceaa6114e28b688d4236b417b2b
=======
w_id <- as_id("1OF2Z0sNWBmdDdZ4lPgUoolVELd7HCyWL")

w <- drive_read(w_id, TRUE, read_fun, read_sf, stringsAsFactors = FALSE)
>>>>>>> 55993ec8864f544addec89a7db70e909aaa7c4d7

w_union <- w %>% 
  st_set_crs(4326) %>% 
  st_union() %>% 
  st_transform(2926) %>% 
  st_make_valid() 

# Filters ----

st_intersect_area <- function(x, y, crs){ 
  
  x_sfc <- x %>% 
    st_sfc %>% 
    st_set_crs(crs)
  
  area_x <- x_sfc %>% st_area() %>% as.double()
  
  area_xy <- st_intersection(x_sfc, y) %>% st_area %>% as.double()
  
  if(is_empty(area_xy)){return(as.double(0))}
  
  overlap_pct <- area_xy %>% 
    divide_by(area_x) %>% 
    as.double() %>% 
    round(2)
  
  return(overlap_pct)
}

# WARNING: long-running process (~40 minutes)

# p_overlap <- p_nest %>% 
#   mutate(WATER_OVERLAP_PCT = map_dbl(geometry, st_intersect_area, y = w_union, crs = 2926)) %>% 
#   mutate(WATER_OVERLAP_LGL = WATER_OVERLAP_PCT > 0) %>% 
#   unnest()

p_water_overlap_0_50 <- p_overlap %>% 
  filter(WATER_OVERLAP_PCT <= .5)

# Upload to Drive ----

# Parcels with pct overlap column

p_overlap_fp <- root_file("./1-data/3-interim/p-water-overlap-sf.gpkg")

drive_folder_id <- as_id("0B5Pp4V6eCkhrZ3NHOEE0Sl9FbWc")

p_overlap_ready <- p_overlap %>% 
  mutate_if(is_logical, as.character)
  
st_write(obj = p_overlap_ready, dsn = p_overlap_fp, driver = "GPKG", layer = "p_water_overlap",layer_options = "OVERWRITE=TRUE")

drive_upload(media = p_overlap_fp, path = drive_folder_id)

# drive_update(file = p_overlap_ready, media = as_id("1Trvievehg4DCA2WuYLQ6aBRljMkl_bCW"))

# Parcels with 50% or less water overlap

p_water_overlap_0_50_fp <- root_file("./1-data/3-interim/p-water-overlap-0-50-sf.gpkg")

p_water_overlap_0_50_ready <- p_water_overlap_0_50 %>% 
  mutate_if(is_logical, as.character)

st_write(obj = p_water_overlap_0_50_ready, dsn = p_water_overlap_0_50_fp, driver = "GPKG", layer = "p_water_overlap_0_50_ready",layer_options = "OVERWRITE=TRUE")

drive_upload(media = p_water_overlap_0_50_fp, path = drive_folder_id)

# drive_update(file = p_water_overlap_0_50_ready, media = as_id("1jrEAX7ogq1RdNU-hfrC-tntF66b6NcKg"))
