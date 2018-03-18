
# COMMAND: MAKE_NAME_RECODE_KEY ----
make_name_recode_key <- function(...){
  
  trigger <- list(...)
  
  recode_key_fp <- here("1-data/1-raw/name_recode_key.rda")
  
  recode_key_gs <- gs_key("1aInQqXPK3tqrXKd80PXPugR8G7Nysz46tirCTAdKn6s")
  
  recode_key <- gs_read(recode_key_gs) %>% 
    replace_na(list(NEW = ""))
  
  write_rds(recode_key, recode_key_fp)
  
  return(recode_key)
  
  
}