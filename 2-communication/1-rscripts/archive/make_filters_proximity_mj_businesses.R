# COMMAND: MAKE_FILTERS_PROXIMITY_MARIJUANA ----
make_filters_proximity_marijuana <- function(...){ 
   p_prox_mj <-  parcel_sf_ready %>% 
    st_buffer(dist = set_units(1000, "ft")) %>% 
    transmute(PIN,
              FILTER_PROX_MJ = st_intersects_any(.,mj_businesses)) %>% 
    st_drop_geometry()
  
  filters_proximity_marijuana <- p_prox_mj
  
  return(filters_proximity_marijuana)
   
}