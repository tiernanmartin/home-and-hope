# COMMAND: MAKE_FILTERS_PROXIMITY_PLAY_SPACE ----
make_filters_proximity_play_space <- function(...){
   
  loadd(parcel_sf_ready)
  loadd(play_spaces_osm)
  
  p_pt <- parcel_sf_ready %>% 
    st_set_geometry("geom_pt") %>% 
    st_transform(2926) %>% 
    transmute(PIN)
  
  buffer_dist_eighth <- set_units(1/8, "mile")
  buffer_dist_qtr <- set_units(1/4, "mile") 
  
  
  ps_buff <- play_spaces_osm
  
  ps_buff$geom_eighth_mi_buff <- st_buffer(st_geometry(ps_buff), buffer_dist_eighth)
  
  ps_buff$geom_qtr_mi_buff <- st_buffer(st_geometry(ps_buff), buffer_dist_qtr)
  
  
  
  append_eighth <- function(x) str_c(x, "EIGHTH",  sep = "_")
  
  append_qtr <- function(x) str_c(x,"QTR",  sep = "_")
  
  
  ps_buff_eighth <- ps_buff %>% 
    st_set_geometry("geom_eighth_mi_buff") %>% 
    select_if(not_sfc) %>% 
    rename_all(append_eighth)
  
  ps_buff_qtr <- ps_buff %>% 
    st_set_geometry("geom_qtr_mi_buff") %>% 
    select_if(not_sfc) %>% 
    rename_all(append_qtr)
  
 
    # ~ 1 min. operation
  p_ps_eighth <- st_join(p_pt, ps_buff_eighth)
  
  
  # ~ 1 min. operation
  p_ps_qtr <- st_join(p_pt, ps_buff_qtr)
  

    # ~ 12 min. operation
  
  p_prox_play_eighth <- p_ps_eighth %>% 
    st_drop_geometry() %>%    
    group_by(PIN) %>%  
    nest %>% 
    mutate(FILTER_PROXIMITY_PLAY_SPACE_EIGHTH = map_lgl(data, ~ !all(map_lgl(.x$PLAY_SPACE_OSM_ID_EIGHTH,is.na))),
           PLAY_SPACE_TYPE_EIGHTH = map_chr(data, ~ str_count_factor(.x$PLAY_SPACE_TYPE_EIGHTH))) %>% 
    transmute(PIN,
              FILTER_PROXIMITY_PLAY_SPACE_EIGHTH,
              PLAY_SPACE_TYPE_EIGHTH = if_else(FILTER_PROXIMITY_PLAY_SPACE_EIGHTH,PLAY_SPACE_TYPE_EIGHTH, NA_character_)) 

  # ~ 8 min. operation
  p_prox_play_qtr <- p_ps_qtr %>% 
    st_drop_geometry() %>%    
    group_by(PIN) %>%  
    nest %>% 
    mutate(FILTER_PROXIMITY_PLAY_SPACE_QTR = map_lgl(data, ~ !all(map_lgl(.x$PLAY_SPACE_OSM_ID_QTR,is.na))),
           PLAY_SPACE_TYPES_QTR = map_chr(data, ~ str_count_factor(.x$PLAY_SPACE_TYPE_QTR))) %>% 
    transmute(PIN,
              FILTER_PROXIMITY_PLAY_SPACE_QTR,
              PLAY_SPACE_TYPES_QTR = if_else(FILTER_PROXIMITY_PLAY_SPACE_QTR,PLAY_SPACE_TYPES_QTR, NA_character_)) 
  
  p_prox_play <- full_join(p_prox_play_eighth, p_prox_play_qtr, by = "PIN") %>% 
    mutate(FILTER_PROXIMITY_PLAY_SPACE = case_when(
      FILTER_PROXIMITY_PLAY_SPACE_EIGHTH ~ "1/8 mile",
      FILTER_PROXIMITY_PLAY_SPACE_QTR ~ "1/4 mile", 
      TRUE ~ "Greater than 1/4 mile"
    ))
  
  
  filters_proximity_play_space <- p_prox_play
  
  return(filters_proximity_play_space)
   
}




