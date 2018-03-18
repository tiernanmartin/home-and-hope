library(mapview)

loadd(inventory_suitable)

tmp <- inventory_suitable %>% 
  st_set_geometry("geometry") %>% 
  select(-geom_pt) %>% 
  transmute(PIN,
            PROPERTY_NAME,
            OWNER_NAME,
            OWNER_CATEGORY,
            TYPE = factor(case_when(OWNER_CATEGORY %in% c("non-profit","uncategorized") ~ OWNER_CATEGORY, 
                                    TRUE ~ "public")),
            PRESENT_USE,
            HELPERS_URL_PARCEL_VIEWER)


mapview(tmp, zcol = "TYPE", legend = TRUE)
