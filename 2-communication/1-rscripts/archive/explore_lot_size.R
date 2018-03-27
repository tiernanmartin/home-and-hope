library(mapview)

loadd(inventory)
loadd(inventory_suitable)
loadd(parcel_sf_ready)

# COMPARE SUIT_LOT_SIZE COUNTS ----
# (Inventory vs Inventory Suitable)

list(inventory, 
     inventory_suitable) %>% 
  map(st_drop_geometry) %>% 
  map(~ select_if(.x, not_sfc)) %>% 
  bind_rows(.id = "id") %>% 
  transmute(ID = factor(if_else(id == 1, "FULL", "SUITABLE")),
            SUIT_LOT_SIZE) %>% 
  tabyl(SUIT_LOT_SIZE, ID ) %>%
  adorn_totals("row") %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 0) %>%
  adorn_ns() %>%
  adorn_title("combined")

# VIEW MAP OF PARCELS BY LOT SIZE TYPES ----

inv <- inventory %>% 
  st_drop_geometry() %>% 
  transmute(PIN,
            OWNER_NAME, 
            PROPERTY_NAME,
            SUITABLE_LGL,
            SUITABLE_NO_LOTSIZE = SUITABLE_OWNER_LGL & SUITABLE_WATER_OVERLAP_LGL & SUITABLE_WITHIN_UGA_LGL & SUITABLE_ZONING_CONSOL_20_LGL & SUITABLE_PRESENT_USE_LGL & SUITABLE_PARCEL_AREA_RATIO_LGL,
            SUIT_PARCEL_AREA = as.integer(SUIT_PARCEL_AREA),
            SUIT_LOT_SIZE,
            SUIT_LOT_SIZE_WITH_OUTLIERS = if_else(SUITABLE_LGL, "suitable size", as.character(SUIT_LOT_SIZE)) %>% factor %>% fct_rev,
            HELPERS_URL_PARCEL_VIEWER
  ) %>% 
  filter(SUITABLE_NO_LOTSIZE) %>% 
  mutate_if(is.logical,as.factor) %>% 
  inner_join(parcel_sf_ready, by = "PIN") %>% 
  st_sf %>% 
  st_set_geometry("geometry") %>% 
  select(-geom_pt)

inv %>% 
  st_drop_geometry() %>% 
  count(SUIT_LOT_SIZE_WITH_OUTLIERS)

# # A tibble: 3 x 2
#   SUIT_LOT_SIZE_WITH_OUTLIERS     n
#   <fct>                       <int>
# 1 over-sized (undevelopable)    173
# 2 suitable size               12185
# 3 under-sized                  2322

gg1 <- ggplot(data = st_drop_geometry(inv), aes(x = SUIT_PARCEL_AREA))
gg1 <- gg1 + geom_histogram(position = "dodge")
gg1 <- gg1 + facet_wrap(~ SUIT_LOT_SIZE_WITH_OUTLIERS, nrow = 1, scales = "free")
gg1 <- gg1 + theme_bw()
gg1


mapview(inv, zcol = "SUIT_LOT_SIZE_WITH_OUTLIERS", legend = TRUE)
