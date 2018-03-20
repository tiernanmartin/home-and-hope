# COMMAND: MAKE_FILTERS_PROXIMITY_TRANSIT ----
make_filters_proximity_transit <- function(...){
   
  p_pt <- parcel_sf_ready %>% 
    st_set_geometry("geom_pt") %>% 
    st_transform(2926) %>% 
    transmute(PIN)
  
  buffer_dist_qtr <- set_units(1/4, "mile")
  buffer_dist_half <- set_units(1/2, "mile")
  
  ts_buff <- transit_stops_osm
  
  ts_buff$geom_qtr_mi_buff <- st_buffer(st_geometry(ts_buff), buffer_dist_qtr)
  
  ts_buff$geom_half_mi_buff <- st_buffer(st_geometry(ts_buff), buffer_dist_half)
  
  append_qtr <- function(x) str_c(x,"QTR",  sep = "_")
  
  append_half <- function(x) str_c(x, "HALF",  sep = "_")
  
  ts_buff_qtr <- ts_buff %>% 
    st_set_geometry("geom_qtr_mi_buff") %>% 
    select_if(not_sfc) %>% 
    rename_all(append_qtr)
  
  ts_buff_half <- ts_buff %>% 
    st_set_geometry("geom_half_mi_buff") %>% 
    select_if(not_sfc) %>% 
    rename_all(append_half)

  
  # ~ 1 min. operation
  p_ts_qtr <- st_join(p_pt, ts_buff_qtr)
  
  # ~ 8 min. operation
  p_ts_half <- st_join(p_pt, ts_buff_half)
  
  # ~ 8 min. operation
  p_prox_trans_qtr <- p_ts_qtr %>% 
    st_drop_geometry() %>%    
    group_by(PIN) %>%  
    nest %>% 
    mutate(FILTER_PROXIMITY_TRANSIT_QTR = map_lgl(data, ~ !all(map_lgl(.x$TRANSIT_STOP_OSM_ID_QTR,is.na))),
           TRANSIT_STOP_TYPES_QTR = map_chr(data, ~ str_count_factor(.x$TRANSIT_STOP_TYPE_QTR))) %>% 
    transmute(PIN,
              FILTER_PROXIMITY_TRANSIT_QTR,
              TRANSIT_STOP_TYPES_QTR = if_else(FILTER_PROXIMITY_TRANSIT_QTR,TRANSIT_STOP_TYPES_QTR, NA_character_)) 
  
  # ~ 13 min. operation
  p_prox_trans_half <- p_ts_half %>% 
    st_drop_geometry() %>%    
    group_by(PIN) %>%  
    nest %>% 
    mutate(FILTER_PROXIMITY_TRANSIT_HALF = map_lgl(data, ~ !all(map_lgl(.x$TRANSIT_STOP_OSM_ID_HALF,is.na))),
           TRANSIT_STOP_TYPES_HALF = map_chr(data, ~ str_count_factor(.x$TRANSIT_STOP_TYPE_HALF))) %>% 
    transmute(PIN,
              FILTER_PROXIMITY_TRANSIT_HALF,
              TRANSIT_STOP_TYPES_HALF = if_else(FILTER_PROXIMITY_TRANSIT_HALF,TRANSIT_STOP_TYPES_HALF, NA_character_)) 
  
  p_prox_trans <- full_join(p_prox_trans_qtr,p_prox_trans_half, by = "PIN") %>% 
    mutate(FILTER_PROXIMITY_TRANSIT = case_when(
      FILTER_PROXIMITY_TRANSIT_QTR ~ "1/4 mile",
      FILTER_PROXIMITY_TRANSIT_HALF ~ "1/2 mile",
      TRUE ~ "Greater than 1/2 mile"
    ))
  
  
  filters_proximity_transit <- p_prox_trans
  
  return(filters_proximity_transit)
   
}




