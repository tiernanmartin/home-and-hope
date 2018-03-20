make_filters_school_district <- function(...){
  
  p_pt <- parcel_sf_ready %>% 
    st_set_geometry("geom_pt") %>% 
    st_transform(2926) %>% 
    select(PIN)
  
  schl <- school_districts %>% 
    transmute(SCHOOL_DISTRICT = NAME) %>% 
    st_transform(2926) %>% 
    st_subdivide(max_vertices = 100) %>% 
    st_collection_extract() 
  
  p_schl <- p_pt
  
  p_schl$FILTER_SCHOOL_DISTRICT <- st_over(p_schl, schl, "SCHOOL_DISTRICT") 
  
  p_schl_ready <- st_drop_geometry(p_zng)
  
  school_district <- p_schl_ready
  
  return(school_district)
  
}