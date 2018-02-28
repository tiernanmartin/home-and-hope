make_filters_eligibility_dda <- function(filters_zcta){
  
  elig_dda_fp <- here("1-data/2-external/DDA2018M.PDF")
  
  elig_dda_dr_id <- as_id("1KbD_gAHy_0DTTxHZgTbL96VOviTwdNRZ")
  
  make_or_read2(fp = elig_dda_fp,
                  dr_id = elig_dda_dr_id,
                  skip_get_expr = TRUE,
                  get_expr = function(fp){
                    # SOURCE:  https://www.huduser.gov/portal/Datasets/qct/DDA2018M.PDF
                  },
                  make_expr = function(fp, dr_id){
                    
                    drive_download(dr_id, path = fp)
                  
                  },
                  read_expr = function(fp){
                    message(glue("* Note: This file is not actually read but it does exists here: '{fp}'.")) 
                    })
  
  list_pages <- extract_tables(elig_dda_fp, method = "data.frame")
  
  tbl_pages <- list_pages %>% 
    map(~ .x %>% t %>% as_tibble) %>% 
    reduce(bind_cols) %>% 
    mutate_all(funs(empty_as_na))
  
  zcta_dda <- tbl_pages %>% 
    slice(3:14) %>% 
    gather(OLD_COL,ZCTA) %>% 
    drop_na() %>% 
    transmute(ZCTA = str_replace(ZCTA,"\\*",""),
              FILTER_ELIGIBILITY_DDA = TRUE) 
  
  elig_dda <- filters_zcta %>% 
    left_join(zcta_dda, by = "ZCTA") %>% 
    transmute(PIN,
              FILTER_ELIGIBILITY_DDA = if_else(is.na(FILTER_ELIGIBILITY_DDA),FALSE,FILTER_ELIGIBILITY_DDA))
  
  filters_eligibility_dda <- elig_dda
  
  return(filters_eligibility_dda)
}
  