make_utilization <- function(...){
  
  loadd(suitability)
  loadd(utilization_present)
  loadd(utilization_potential)
  loadd(utilization_criteria)
  
  util_join <- suitability %>% 
    st_drop_geometry() %>% 
    select(PIN) %>% 
    left_join(utilization_present, by = "PIN") %>% 
    left_join(utilization_potential, by = "PIN") 
  
  util_ready <-  util_join %>%  
    mutate(UTILIZATION_RATIO = UTIL_PRESENT/UTIL_POTENTIAL_UTILIZATION_SQFT,
           UTIL_UNDER_UTILIZED_GENTLE_LGL = if_else(UTILIZATION_RATIO < utilization_criteria["ratio_gentle"],TRUE,FALSE,NA),
           UTIL_UNDER_UTILIZED_MODERATE_LGL = if_else(UTILIZATION_RATIO < utilization_criteria["ratio_moderate"],TRUE,FALSE,NA),
           UTIL_UNDER_UTILIZED_AGGR_LGL = if_else(UTILIZATION_RATIO < utilization_criteria["ratio_aggressive"],TRUE,FALSE,NA),
           UTILIZATION_GENTLE = if_else(!UTIL_UNDER_UTILIZED_GENTLE_LGL, "fully-utilized", "under-utilized", LOT_SIZE_TYPE),
           UTILIZATION_MODERATE = if_else(!UTIL_UNDER_UTILIZED_MODERATE_LGL, "fully-utilized", "under-utilized", LOT_SIZE_TYPE), 
           UTILIZATION_AGGR = if_else(!UTIL_UNDER_UTILIZED_AGGR_LGL, "fully-utilized", "under-utilized", LOT_SIZE_TYPE))
  
  utilization <- util_ready
  
  return(utilization)
  
}