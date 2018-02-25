# COMMAND: MAKE_FILTERS_ZCTA ----
make_filters_zcta <- function(parcel_ready, zcta){
  
  p <- parcel_ready %>% select(PIN)
  
  zcta_subdivide <- st_subdivide(zcta, 100) %>% 
    st_collection_extract() %>% 
    transmute(ZCTA = ZCTA5CE10)
   
  p$ZCTA <- st_over(p$geom_pt,zcta_subdivide, "ZCTA") 
  
  p_ready <- st_drop_geometry(p) 
  
  filters_zcta <- p_ready
  
  return(filters_zcta)
   
}

