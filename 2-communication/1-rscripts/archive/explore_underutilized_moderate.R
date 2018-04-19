library(mapview)

loadd(inventory_suitable, dd)

tt <- dd %>% filter(FIELD_TAG_TOOLTIP) %>% pull(FIELD_NAME_DEV)

inv_poly <- inventory_suitable %>% 
  st_set_geometry("geometry") %>% 
  filter(UTILIZATION_MODERATE %!in% "fully-utilized") %>% 
  select_at(vars(tt, "UTILIZATION_MODERATE")) 

mapview(inv_poly, zcol = "UTILIZATION_MODERATE", legend = TRUE)
