tbl <- read_csv(here("1-data/4-ready/inventory_suitable_table.csv"))

tbl_ready <- tbl %>% 
  mutate(HOUSING_AUTHORITY_LGL = str_detect(OWNER_NAME, "HOUSING AUTHORITY")) %>% 
  mutate(OWNER_NAME_CONSOLIDATED = case_when(
    HOUSING_AUTHORITY_LGL & str_detect(OWNER_NAME_CONSOLIDATED, "SEATTLE") ~ "SEATTLE HOUSING AUTHORITY",
    HOUSING_AUTHORITY_LGL & str_detect(OWNER_NAME_CONSOLIDATED, "C.O.S") ~ "SEATTLE HOUSING AUTHORITY",
    HOUSING_AUTHORITY_LGL & str_detect(OWNER_NAME_CONSOLIDATED, "KING COUNTY") ~ "KING COUNTY AUTHORITY",
    HOUSING_AUTHORITY_LGL & str_detect(OWNER_NAME_CONSOLIDATED, "K C") ~ "KING COUNTY AUTHORITY",
    HOUSING_AUTHORITY_LGL & str_detect(OWNER_NAME_CONSOLIDATED, "RENTON") ~ "RENTON AUTHORITY",
    TRUE ~ OWNER_NAME_CONSOLIDATED
  )) %>% 
  mutate(OWNER_CATEGORY = if_else(HOUSING_AUTHORITY_LGL, "housing authority",OWNER_CATEGORY),
         FILTER_OWNER_TYPE = if_else(HOUSING_AUTHORITY_LGL, "housing authority",FILTER_OWNER_TYPE),
         FILTER_PUBLIC_OWNER = if_else(HOUSING_AUTHORITY_LGL,OWNER_NAME_CONSOLIDATED,FILTER_PUBLIC_OWNER)) %>% 
  select(-HOUSING_AUTHORITY_LGL)

tbl_ready %>% count(FILTER_OWNER_TYPE, sort = TRUE)


write_csv(tbl_ready, here("1-data/4-ready/site-inventory-20180605/inventory_suitable_table.csv"))

write_xlsx(tbl_ready, here("1-data/4-ready/site-inventory-20180605/inventory_suitable_table.xlsx"))
