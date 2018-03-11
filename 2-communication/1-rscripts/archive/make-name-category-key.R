make_owner_name_category_key<- function(){
  
  categories_gs <- gs_key("1cYNIpQpDJTZWi46S_9bZ6rjgRu8JWes1BxOeoJJD2tg")
  
  cat_ngram_list <- gs_read_all(categories_gs) 
  
  cat_ngram_list$CATEGORIES <- NULL
  
  cat_ngram_long <- cat_ngram_list %>% 
    map_dfr(~ gather(.x, NGRAM_TYPE, WORD, -CATEGORY)) 
  
  cat_ngram_wide <- cat_ngram_long %>%  
    group_by(NGRAM_TYPE) %>% 
    mutate(row = row_number()) %>% 
    spread(NGRAM_TYPE,WORD) %>% 
    arrange(row) %>% 
    select(-row) 
  
  owner_name_category_key <- cat_ngram_wide
  
  return(owner_name_category_key)
  
  
}