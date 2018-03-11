make_owner_exempt_categories <- function(parcel_ready, suitability_tax_exempt, owner_public_categories, owner_nonprofit_categories, owner_antijoin_names, owner_name_category_key, owner_name_recode_key){
  
  
  names <- suitability_tax_exempt %>%  
    filter(SUIT_OWNER_OTHER_EXEMPT) %>% 
    inner_join(parcel_ready, by = "PIN") %>% 
    st_drop_geometry() %>% 
    transmute(PIN,
              OWNER_NAME = str_trim(toupper(TAXPAYER_NAME)))
  
 # CLEAN/RECODE NAMES ----
  
  names_cleaned <- names %>% 
    unnest_tokens(ORIG, OWNER_NAME, token = "ngrams", n = 1, to_lower = FALSE) %>%  
    left_join(owner_name_recode_key, by = "ORIG") %>% 
    mutate(OWNER_NAME = if_else(is.na(NEW),ORIG,NEW)) %>% 
    group_by(PIN) %>% 
    summarise(OWNER_NAME_CLEAN = str_c(OWNER_NAME, collapse = " ")) %>% 
    full_join(names, by = "PIN") %>%  
    select(PIN,OWNER_NAME,OWNER_NAME_CLEAN)
  
  names_trimmed <- names_cleaned %>% 
    unnest_tokens(ORIG, OWNER_NAME_CLEAN, token = "ngrams", n = 1, to_lower = FALSE) %>% 
    anti_join(owner_antijoin_names, by = c("ORIG" = "word")) %>%   
    group_by(PIN) %>% 
    summarise(OWNER_NAME_TRIM = str_c(ORIG, collapse = " ")) %>% 
    full_join(names_cleaned, by = "PIN") %>%  
    select(PIN,OWNER_NAME,OWNER_NAME_CLEAN,OWNER_NAME_TRIM)
  
  view_names_trimmed_cnt <- function(){
    names_trimmed_cnt <- names_trimmed %>% 
      count(OWNER_NAME_TRIM, sort = TRUE)
    
    return(names_trimmed_cnt)
  }
  
  view_names_trimmed_cnt()
  
  # CREATE NGRAMS ----
  
  names_ngrams_1 <- names_trimmed %>%  
    unnest_tokens(EXEMPT_NGRAM_1, OWNER_NAME_TRIM, token = "ngrams", n = 1, to_lower = FALSE) %>% 
    select(-OWNER_NAME)
  
  names_ngrams_1_cnt <- names_ngrams_1 %>% 
    count(EXEMPT_NGRAM_1, sort = TRUE)
  
  names_ngrams_2 <- names_trimmed %>%  
    unnest_tokens(EXEMPT_NGRAM_2, OWNER_NAME_TRIM, token = "ngrams", n = 2, to_lower = FALSE) %>% 
    separate(EXEMPT_NGRAM_2, c("EXEMPT_NGRAM_2_A", "EXEMPT_NGRAM_2_B"),sep = " ") %>% 
    select(-OWNER_NAME)
  
  names_ngrams_2_cnt <- names_ngrams_2 %>% 
    count(EXEMPT_NGRAM_2_A,EXEMPT_NGRAM_2_B, sort = TRUE)
  
  names_ngrams_3 <- names_trimmed %>%  
    unnest_tokens(EXEMPT_NGRAM_3, OWNER_NAME_TRIM, token = "ngrams", n = 3, to_lower = FALSE) %>% 
    separate(EXEMPT_NGRAM_3, c("EXEMPT_NGRAM_3_A", "EXEMPT_NGRAM_3_B", "EXEMPT_NGRAM_3_C"),sep = " ") %>% 
    select(-OWNER_NAME)
  
  names_ngrams_3_cnt <- names_ngrams_3 %>% 
    count(EXEMPT_NGRAM_3_A,EXEMPT_NGRAM_3_B,EXEMPT_NGRAM_3_C, sort = TRUE)
  
  names_ngrams <- 
    list(names_cleaned,names_ngrams_1,names_ngrams_2,names_ngrams_3) %>% 
    reduce(left_join, by = c("PIN","OWNER_NAME_CLEAN"))
  
  
  # VIEW NGRAMS NETWORK ----

make_bigram_graph <- function(){
  bigram_graph <- names_ngrams_2 %>% 
  count(EXEMPT_NGRAM_2_A,EXEMPT_NGRAM_2_B, sort = TRUE) %>% 
  filter(n > 20) %>% 
  graph_from_data_frame
  
  set.seed(2016)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()
}

# make_bigram_graph()
  
  
  

# CREATE NGRAM CATEGORY KEYS ----

cat_ngram_1 <- owner_name_category_key %>% 
  select(CATEGORY,EXEMPT_NGRAM_1) %>% 
  drop_na

cat_ngram_2 <- owner_name_category_key %>% 
  select(CATEGORY,matches("EXEMPT_NGRAM_2")) %>% 
  drop_na

cat_ngram_3 <- owner_name_category_key %>% 
  select(CATEGORY,matches("EXEMPT_NGRAM_3")) %>% 
  drop_na


# PUBLIC AND NONPROFIT CATEGORIES ----

pub_join <- owner_public_categories %>% 
  group_by(OWNER_NAME) %>% 
  summarise(PUBLIC = first(OWNER_CATEGORY)) 

np_join <- owner_nonprofit_categories %>% 
  group_by(OWNER_NAME) %>% 
  summarise(NONPROFIT = first(OWNER_CATEGORY))

# JOIN CATEGORIES TABLE ----


ngram_1_categorized <- names_ngrams %>% 
  left_join(cat_ngram_1, by = "EXEMPT_NGRAM_1") %>% 
  group_by(PIN,OWNER_NAME_CLEAN) %>% 
  summarise(CATEGORY = first_not_na(CATEGORY)) %>% 
  select(-OWNER_NAME_CLEAN) %>% 
  drop_na

ngram_2_categorized <- names_ngrams %>% 
  left_join(cat_ngram_2, c("EXEMPT_NGRAM_2_A", "EXEMPT_NGRAM_2_B")) %>% 
  group_by(PIN,OWNER_NAME_CLEAN) %>% 
  summarise(CATEGORY = first_not_na(CATEGORY)) %>% 
  select(-OWNER_NAME_CLEAN) %>% 
  drop_na

ngram_3_categorized <- names_ngrams %>% 
  left_join(cat_ngram_3, c("EXEMPT_NGRAM_3_A", "EXEMPT_NGRAM_3_B", "EXEMPT_NGRAM_3_C")) %>% 
  group_by(PIN,OWNER_NAME_CLEAN) %>% 
  summarise(CATEGORY = first_not_na(CATEGORY)) %>% 
  select(-OWNER_NAME_CLEAN) %>% 
  drop_na

names_categorized <- 
  list(names_trimmed, 
       ngram_1_categorized,
       ngram_2_categorized,
       ngram_3_categorized) %>% 
  reduce(left_join, by = "PIN") %>% 
  group_by(PIN) %>% 
  summarise(OWNER_NAME = first(OWNER_NAME),
            OWNER_NAME_CLEAN = first(OWNER_NAME_CLEAN),
            OWNER_NAME_TRIM = first(OWNER_NAME_TRIM),
            OWNER_CATEGORY = first_not_na(c(CATEGORY.x,CATEGORY.y,CATEGORY))
  ) %>%
  transmute(PIN,
            OWNER_NAME = OWNER_NAME_CLEAN,
            OWNER_CATEGORY = OWNER_CATEGORY) %>%
  left_join(pub_join, by = "OWNER_NAME") %>% 
  left_join(np_join, by = "OWNER_NAME") %>% 
  group_by(PIN) %>% 
  summarise(OWNER_NAME = first(OWNER_NAME),
            OWNER_CATEGORY = first_not_na(c(OWNER_CATEGORY,PUBLIC,NONPROFIT))) %>% 
  ungroup



# VIEW CATEGORIES ----

names_categorized %>% count(OWNER_CATEGORY, sort = TRUE)

names_categorized %>% 
  filter(is.na(OWNER_CATEGORY)) %>% 
  count(OWNER_NAME, sort = TRUE) %>% 
  filter(n>10) %>% 
  print(n=Inf)

# RETURN----

return(names_categorized)


}
