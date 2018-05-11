library(mapview)

loadd(inventory, inventory_suitable)

inv <- st_drop_geometry(inventory)

suit <- st_drop_geometry(inventory_suitable)

# FIX THIS - which column should it be?
suit %>% 
  count(OWNER_NAME, sort = TRUE) %>% 
  filter(str_detect(OWNER_NAME, "AUTHORITY")) %>% 
  print(n=Inf)
