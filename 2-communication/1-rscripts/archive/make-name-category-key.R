make_name_category_key <- function(){
  
  pub_categories_gs <- gs_key("1cYNIpQpDJTZWi46S_9bZ6rjgRu8JWes1BxOeoJJD2tg")
  
  cat_ngram_1 <- gs_read(ss = pub_categories_gs,ws = "NGRAM_1")
  
  cat_ngram_2 <- gs_read(ss = pub_categories_gs,ws = "NGRAM_2")
  
  cat_ngram_3 <- gs_read(ss = pub_categories_gs,ws = "NGRAM_3")
  
  cat_ngram_1_long <- cat_ngram_1 %>% 
    gather(NGRAM_TYPE, WORD, -CATEGORY)
  
  cat_ngram_2_long <- cat_ngram_2 %>% 
    gather(NGRAM_TYPE, WORD, -CATEGORY)
  
  cat_ngram_3_long <- cat_ngram_3 %>% 
    gather(NGRAM_TYPE, WORD, -CATEGORY)
  
  cat_ngram_long <- list(cat_ngram_1_long,cat_ngram_2_long,cat_ngram_3_long) %>% 
    bind_rows() %>% 
    select(NGRAM_TYPE, WORD, CATEGORY)
  
  cat_ngram_wide <- cat_ngram_long %>%  
    group_by(NGRAM_TYPE) %>% 
    mutate(row = row_number()) %>% 
    spread(NGRAM_TYPE,WORD) %>% 
    arrange(row) %>% 
    select(-row)
  
  name_category_key <- cat_ngram_wide
  
  return(cat_ngram_wide)
   
  
}