make_suitability_parcel_area_ratio <- function(parcel_sf_ready){
  
  # ~ 34 min. operation 
  
  p_area_ratio <- parcel_sf_ready %>%  
    st_set_geometry("geometry") %>% 
    transmute(PIN,
             SUIT_PARCEL_AREA_RATIO = st_area_ratio(.)) 
  
  return(p_area_ratio) 
}
