library(sf)
library(osmdata)
library(tidyverse)
library(miscgis)
library(mapview)
library(visdat)
library(snakecase)
library(skimr)


q <- opq(bbox = "King County, Washington") %>% 
  add_osm_feature(key = "public_transport", value = "",value_exact = FALSE)

transit_pts <- q %>% 
  osmdata_sf() %>% 
  pluck("osm_points") %>% 
  rename_all(to_screaming_snake_case)

transit_pts %>% 
  st_drop_geometry() %>% 
  vis_dat()

transit_pts %>% 
  st_drop_geometry() %>% 
  select(OSM_ID, NAME,BUS, TRAIN, TRAM, TROLLEYBUS, FERRY, PUBLIC_TRANSPORT, SOURCE) %>% 
  skim

transit_pts %>% 
  st_drop_geometry() %>% 
  count(PUBLIC_TRANSPORT, sort = TRUE)

trns_pts <- transmute(transit_pts,
                      OSM_ID = as.character(OSM_ID))

ready <- 
  transit_pts %>% 
  st_drop_geometry() %>% 
  select(OSM_ID ,NAME,BUS, TRAIN, TRAM, TROLLEYBUS, FERRY, PUBLIC_TRANSPORT, SOURCE) %>% 
  gather(TRANSIT_TYPE, VALUE, -OSM_ID, -NAME,-PUBLIC_TRANSPORT, -SOURCE) %>% 
  filter(!is.na(VALUE)) %>%  
  mutate_if(is.factor, as.character) %>% 
  left_join(trns_pts, by = "OSM_ID") %>% 
  st_sf
  

mapview(ready, zcol = "TRANSIT_TYPE", legend = TRUE)
