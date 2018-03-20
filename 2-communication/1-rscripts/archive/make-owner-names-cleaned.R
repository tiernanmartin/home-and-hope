make_owner_names_cleaned <- function(parcel_ready, ...){
  
  loadd(parcel_ready)
  loadd(owner_public_categories)
  loadd(owner_nonprofit_categories)
  loadd(owner_exempt_categories) 
  

  # BIND OWNER CATEGORIES AND JOIN TO PARCEL_READY ----    
   p <- parcel_ready %>% 
    st_drop_geometry() %>% 
    select(PIN)
  
  owner <- reduce(list(...), bind_rows) %>% 
    right_join(p, by = "PIN") %>% 
    mutate(OWNER_CATEGORY = if_else(is.na(OWNER_CATEGORY),"uncategorized",OWNER_CATEGORY))
  
  # CREAT CLEAN NAMES ----
  
  city_names_clean <- parcel_ready %>% 
    pluck("DISTRICT_NAME") %>% 
    unique() %>% 
    discard(is.na) %>% 
    c("TACOMA", "VASHON ISLAND") %>% 
    toupper %>% 
    str_replace_all("KING COUNTY", "UNINCORPORATED AREA") %>% 
    {tibble(OWNER_NAME_CLEAN = .)}
  
  city_names_clean_long <- city_names_clean %>% 
    mutate(OWNER_NAME_CLEAN_FULL = OWNER_NAME_CLEAN) %>% 
    unnest_tokens(TOKEN, OWNER_NAME_CLEAN, token = "ngrams", n = 1, to_lower = FALSE) %>% 
    select(TOKEN, OWNER_NAME_CLEAN_FULL) %>% 
    mutate(OWNER_NAME_CLEAN_FULL = if_else(OWNER_NAME_CLEAN_FULL %in% c("KING COUNTY", "UNINCORPORATED AREA"),
                                           OWNER_NAME_CLEAN_FULL,
                                           str_c("CITY OF ",OWNER_NAME_CLEAN_FULL)))
  
  tribe_names_clean <- tibble(TOKEN = c("MUCKLESHOOT",
                                        "SNOQUALMIE",
                                        "COWLITZ"),
                              OWNER_NAME_CLEAN_FULL = c("MUCKLESHOOT INDIAN TRIBE",
                                                        "SNOQUALMIE INDIAN TRIBE",
                                                        "COWLITZ INDIAN TRIBE"))
  
  # CREAT ANTI_JOIN TABLES ----   
  new_owner_antijoin_names <- tibble(TOKEN = c("CITY", "OF"))
  
  tribe_owner_antijoin_names <- tibble(TOKEN = c("INDIAN", "TRIBE"))
  
  # CREAT JOIN TABLES ----   
  
  clean_name_tbl <- 
    tribble(
      ~ OWNER_CATEGORY, ~OWNER_NAME_CLEAN,
      "uncategorized", NA_character_,
      "city", NA_character_,
      "county", "KING COUNTY",
      "non-profit", NA_character_,
      "school district", NA_character_,
      "federal", "US GOVERNMENT",
      "state", "WASHINGTON STATE",
      "special purpose district", NA_character_,
      "homeowners association", NA_character_,
      "port", "PORT OF SEATTLE",
      "tribal", NA_character_, 
      "regional transit authority", "SOUND TRANSIT"
    ) 
  
  city_names_join_tbl <- 
    owner %>%  
    mutate(OWNER_NAME_TOKEN = OWNER_NAME) %>% 
    left_join(clean_name_tbl, by = "OWNER_CATEGORY") %>%  
    filter(OWNER_CATEGORY %in% "city") %>% 
    unnest_tokens(TOKEN, OWNER_NAME_TOKEN, token = "ngrams", n = 1, to_lower = FALSE) %>% 
    anti_join(new_owner_antijoin_names ) %>% 
    stringdist_left_join(city_names_clean_long, by = c(TOKEN = "TOKEN"),  max_dist = 1, distance_col = "DIST") %>% 
    group_by(PIN) %>% 
    summarise(OWNER_NAME = first(OWNER_NAME),
              OWNER_NAME_CLEAN = first_not_na(OWNER_NAME_CLEAN_FULL)) %>%  
    select(-OWNER_NAME) 
  
  tribe_names_join_tbl <- 
    owner %>% 
    mutate(OWNER_NAME_TOKEN = OWNER_NAME) %>% 
    left_join(clean_name_tbl, by = "OWNER_CATEGORY") %>%
    filter(OWNER_CATEGORY %in% "tribal") %>%
    unnest_tokens(TOKEN, OWNER_NAME_TOKEN, token = "ngrams", n = 1, to_lower = FALSE) %>%
    anti_join(tribe_owner_antijoin_names) %>%
    stringdist_left_join(tribe_names_clean, by = c(TOKEN = "TOKEN"),  max_dist = 1, distance_col = "DIST") %>%
    group_by(PIN) %>%
    summarise(OWNER_NAME = first(OWNER_NAME),
              OWNER_NAME_CLEAN = first_not_na(OWNER_NAME_CLEAN_FULL)) %>%
    select(-OWNER_NAME)
  
  # JOIN THE TABLES ----   
  
  owner_names_consolidated <- 
    owner %>%  
    left_join(clean_name_tbl, by = "OWNER_CATEGORY") %>% 
    left_join(city_names_join_tbl, by = "PIN") %>% 
    left_join(tribe_names_join_tbl, by = "PIN") %>% 
    group_by(PIN) %>% 
    summarize(OWNER_CATEGORY = first(OWNER_CATEGORY),
              OWNER_NAME = first(OWNER_NAME),
              OWNER_NAME_CLEAN = first_not_na(c(OWNER_NAME_CLEAN.x, OWNER_NAME_CLEAN.y))) %>% 
    transmute(PIN,
              OWNER_NAME,
              OWNER_NAME_CONSOLIDATED = if_else(is.na(OWNER_NAME_CLEAN), OWNER_NAME,OWNER_NAME_CLEAN),
              OWNER_CATEGORY)
  
  # RETURN ----   
  
  owner_ready <- owner_names_consolidated
  
  return(owner_ready)
  
  
}