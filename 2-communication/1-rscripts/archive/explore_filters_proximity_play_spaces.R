loadd(inventory_suitable)
loadd(play_spaces_osm)


glimpse(inventory_suitable)

inventory_suitable %>% 
  st_drop_geometry %>% 
  count(FILTER_PROXIMITY_PLAY_SPACE, sort = TRUE)

p_ready <- inventory_suitable %>% 
  st_set_geometry("geom_pt") %>%  
  transmute(PIN,PROPERTY_NAME,OWNER_NAME,FILTER_PROXIMITY_PLAY_SPACE, PLAY_SPACE_TYPE_EIGHTH, PLAY_SPACE_TYPE_QTR,HELPERS_URL_PARCEL_VIEWER) 

spaces <- st_transform(play_spaces_osm, 2926)

mapview(list(spaces, p_ready ),  
        zcol = list(NULL, "FILTER_PROXIMITY_PLAY_SPACE"),
        col.regions = list("green",NULL),
        color = list("green",NULL),
        legend = list(TRUE, TRUE))

mapview(spaces, color = "green", col.regions = "green") +
  mapview(p_ready, zcol = "FILTER_PROXIMITY_PLAY_SPACE", legend = TRUE)
