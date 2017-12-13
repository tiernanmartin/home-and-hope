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

w_union <- w %>% 
  st_set_crs(4326) %>% 
  st_union() %>% 
  st_transform(2926) %>% 
  st_make_valid() 

# Filters ----

sample_pins <- c("7666200311","0468000050","9528100790","0225049061","4315701200","342403900109","2524039040")

p_sample <- p_nest %>% 
  mutate(PIN = map_chr(data,"PIN")) %>% 
  filter(PIN %in% sample_pins)

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

tmp <- p_sample %>% 
  mutate(WATER_OVERLAP_PCT = map_dbl(geometry, st_intersect_area, y = w_union, crs = 2926)) %>% 
  st_transform(4326)


tic()

p_overlap_fp <- root_file("./1-data/3-interim/p-no-water-sf.gpkg")

p_overlap <- p_nest %>% 
  mutate(WATER_OVERLAP_PCT = map_dbl(geometry, st_intersect_area, y = w_union, crs = 2926))

st_write(obj = p_overlap, dsn = p_overlap_fp, driver = "GPKG", layer_options = "OVERWRITE=TRUE")

toc()
# Upload to Drive ----

