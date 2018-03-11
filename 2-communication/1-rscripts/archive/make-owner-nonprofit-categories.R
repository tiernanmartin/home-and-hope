make_owner_nonprofit_categories <- function(parcel_ready, suitability_tax_exempt, owner_antijoin_names, owner_name_category_key, owner_name_recode_key){
  
  
  names <- suitability_tax_exempt %>%  
    filter(SUIT_OWNER_NONPROFIT) %>% 
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
  
  # view_names_trimmed_cnt()
  
  # CREATE NGRAMS ----
  
  names_ngrams_1 <- names_trimmed %>%  
    unnest_tokens(NP_NGRAM_1, OWNER_NAME_TRIM, token = "ngrams", n = 1, to_lower = FALSE) %>% 
    select(-OWNER_NAME)
  
  names_ngrams_1_cnt <- names_ngrams_1 %>% 
    count(NP_NGRAM_1, sort = TRUE)
  
  names_ngrams_2 <- names_trimmed %>%  
    unnest_tokens(NP_NGRAM_2, OWNER_NAME_TRIM, token = "ngrams", n = 2, to_lower = FALSE) %>% 
    separate(NP_NGRAM_2, c("NP_NGRAM_2_A", "NP_NGRAM_2_B"),sep = " ") %>% 
    select(-OWNER_NAME)
  
  names_ngrams_2_cnt <- names_ngrams_2 %>% 
    count(NP_NGRAM_2_A,NP_NGRAM_2_B, sort = TRUE)
  
  names_ngrams_3 <- names_trimmed %>%  
    unnest_tokens(NP_NGRAM_3, OWNER_NAME_TRIM, token = "ngrams", n = 3, to_lower = FALSE) %>% 
    separate(NP_NGRAM_3, c("NP_NGRAM_3_A", "NP_NGRAM_3_B", "NP_NGRAM_3_C"),sep = " ") %>% 
    select(-OWNER_NAME)
  
  names_ngrams_3_cnt <- names_ngrams_3 %>% 
    count(NP_NGRAM_3_A,NP_NGRAM_3_B,NP_NGRAM_3_C, sort = TRUE)
  
  names_ngrams <- 
    list(names_cleaned,names_ngrams_1,names_ngrams_2,names_ngrams_3) %>% 
    reduce(left_join, by = c("PIN","OWNER_NAME_CLEAN"))
  
  
  # VIEW NGRAMS NETWORK ----

make_bigram_graph <- function(){
  bigram_graph <- names_ngrams_2 %>% 
  count(NGRAM_2_A,NGRAM_2_B, sort = TRUE) %>% 
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
  select(CATEGORY,NP_NGRAM_1) %>% 
  drop_na

cat_ngram_2 <- owner_name_category_key %>% 
  select(CATEGORY,matches("NP_NGRAM_2")) %>% 
  drop_na

cat_ngram_3 <- owner_name_category_key %>% 
  select(CATEGORY,matches("NP_NGRAM_3")) %>% 
  drop_na


# JOIN CATEGORIES TABLE ----


ngram_1_categorized <- names_ngrams %>% 
  left_join(cat_ngram_1, by = "NP_NGRAM_1") %>% 
  group_by(PIN,OWNER_NAME_CLEAN) %>% 
  summarise(CATEGORY = first_not_na(CATEGORY)) %>% 
  select(-OWNER_NAME_CLEAN) %>% 
  drop_na

ngram_2_categorized <- names_ngrams %>% 
  left_join(cat_ngram_2, c("NP_NGRAM_2_A", "NP_NGRAM_2_B")) %>% 
  group_by(PIN,OWNER_NAME_CLEAN) %>% 
  summarise(CATEGORY = first_not_na(CATEGORY)) %>% 
  select(-OWNER_NAME_CLEAN) %>% 
  drop_na

ngram_3_categorized <- names_ngrams %>% 
  left_join(cat_ngram_3, c("NP_NGRAM_3_A", "NP_NGRAM_3_B", "NP_NGRAM_3_C")) %>% 
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
            OWNER_CATEGORY = if_else(is.na(OWNER_CATEGORY), "non-profit",OWNER_CATEGORY,OWNER_CATEGORY))



# VIEW CATEGORIES ----

# names_categorized %>% count(OWNER_CATEGORY, sort = TRUE)

# names_categorized %>% filter(is.na(OWNER_CATEGORY)) %>% count(OWNER_NAME, sort = TRUE) %>% print(n=Inf)

# RETURN----

return(names_categorized)


}
