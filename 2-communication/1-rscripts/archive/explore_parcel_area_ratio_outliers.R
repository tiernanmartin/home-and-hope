inv <- inventory %>% 
  st_drop_geometry() %>% 
  transmute(PIN, 
            SUITABLE_LGL,
            SUITABLE = if_else(SUITABLE_LGL, "suitable", "not") %>% factor() %>% fct_rev,
            NEW_SUITABLE_LGL = SUITABLE_OWNER_LGL & SUITABLE_WATER_OVERLAP_LGL & SUITABLE_WITHIN_UGA_LGL & SUITABLE_ZONING_CONSOL_20_LGL & SUITABLE_PRESENT_USE_LGL,
         PROPERTY_NAME, 
         OWNER_CATEGORY, 
         ZONING = SUIT_ZONING_CONSOL_20,
         ZONING_TYPE = case_when(
           ZONING %in% sf ~ "single family",
           ZONING %in% mf ~ "multi-family",
           TRUE ~ "other"
         ) %>% factor(levels = c("single family", "multi-family", "other")),
         SUIT_PARCEL_AREA = as.double(SUIT_PARCEL_AREA),  
         SUIT_PARCEL_AREA_OUTLIER = is_outlier(sqrt(SUIT_PARCEL_AREA)),
         SUIT_PARCEL_AREA_RATIO,
         SUIT_PARCEL_AREA_RATIO_OUTLIER = is_outlier(SUIT_PARCEL_AREA_RATIO),
         AREA_CITY_BLOCK = if_else(SUIT_PARCEL_AREA >= .75,"greater than a block","less than a block","less than a block") %>% factor() %>% fct_rev(),
         AREA_CITY_BLOCK_DETAIL = cut(SUIT_PARCEL_AREA, 
                                      breaks=c(-Inf, 0.1, 0.378, .75, 1.5, Inf ), 
                                      labels=c("under-sized","quarter-block","half-block", "whole-block", "over-sized")),
         BLDG_LGL = if_else(BLDG_NBR>1,TRUE,FALSE,FALSE),
         BDLG_FCT = BLDG_NBR %>% 
           factor(ordered = TRUE) %>% 
           fct_explicit_na(0) %>% 
           fct_lump(n = 4,  other_level = "more than 3") %>% 
           fct_relevel(c("0","1","2","3","more than 3")))


inv_ratio <- inv %>% 
  transmute(PIN, 
            SUIT_PARCEL_AREA_RATIO,
            SUIT_PARCEL_AREA_RATIO_OUTLIER,
            SUIT_PARCEL_AREA_RATIO_OUTLIER_FCT = if_else(SUIT_PARCEL_AREA_RATIO_OUTLIER, "OUTLIER", "NOT OUTLIER") %>% factor %>% fct_rev)


gg1 <- ggplot(data = inv_ratio, aes(x = SUIT_PARCEL_AREA_RATIO, fill = SUIT_PARCEL_AREA_RATIO_OUTLIER_FCT))
gg1 <- gg1 + geom_histogram(position = "dodge")
gg1 <- gg1 + theme_bw()
gg1

gg2 <- ggplot(data = inv_ratio, aes(x = SUIT_PARCEL_AREA_RATIO))
gg2 <- gg2 + geom_histogram()
gg2 <- gg2 + facet_wrap( ~SUIT_PARCEL_AREA_RATIO_OUTLIER_FCT ,nrow = 1, scales = "free")
gg2 <- gg2 + theme_bw()
gg2