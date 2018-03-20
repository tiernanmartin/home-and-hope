library(mapview)

loadd(inventory_suitable)

loadd(suitability_criteria)

tmp <- inventory_suitable %>% 
  st_set_geometry("geometry") %>% 
  select(-geom_pt) %>% 
  transmute(PIN,
            PROPERTY_NAME,
            OWNER_NAME,
            OWNER_CATEGORY,
            TYPE = factor(case_when(OWNER_CATEGORY %in% c("non-profit","uncategorized") ~ OWNER_CATEGORY, 
                                    TRUE ~ "public")),
            SUIT_PARCEL_AREA,
            SUIT_PARCEL_AREA_RATIO,
            PRESENT_USE,
            UTIL_PRESENT,
            UTIL_POTENTIAL_UTILIZATION_SQFT,
            UTIL_DEVELOPABLE_LOT_COVERAGE_PCT = fct_lump(factor(UTIL_DEVELOPABLE_LOT_COVERAGE_PCT),n = 2), 
            UTILIZATION_MODERATE,
            HELPERS_URL_PARCEL_VIEWER)


tmp2 <- tmp %>% filter(SUIT_PARCEL_AREA <= set_units(10, acre))

tmp3 <- tmp %>% filter(SUIT_PARCEL_AREA <= set_units(.125, acre))


qplot(data = tmp, x = as.double(SUIT_PARCEL_AREA))

qplot(data = tmp2, x = as.double(SUIT_PARCEL_AREA))

qplot(data = tmp3, x = as.double(SUIT_PARCEL_AREA))

mapview(tmp, zcol = "UTILIZATION_MODERATE", legend = TRUE)

inventory_suitable %>% 
  st_set_geometry("geometry") %>% 
  transmute(PIN,FILTER_PROXIMITY_TRANSIT = as.character(FILTER_PROXIMITY_TRANSIT), TRANSIT_STOP_TYPES, HELPERS_URL_PARCEL_VIEWER) %>% 
  mapview(zcol = "FILTER_PROXIMITY_TRANSIT", legend = TRUE)
