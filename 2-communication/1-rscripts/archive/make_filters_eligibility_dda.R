make_filters_eligibility_dda <- function(filters_ztca){
  
  elig_dda_fp <- here("1-data/2-external/DDA2018M.PDF")
  
  elig_dda_dr_id <- as_id("1KbD_gAHy_0DTTxHZgTbL96VOviTwdNRZ")
  
  make_or_read2(fp = elig_dda_fp,
                  dr_id = elig_dda_dr_id,
                  skip_get_expr = FALSE,
                  get_expr = function(fp){
                    # SOURCE:  https://www.huduser.gov/portal/Datasets/qct/DDA2018M.PDF
                  },
                  make_expr = function(fp, dr_id){
                    
                    drive_download(dr_id, path = fp)
                  
                  },
                  read_expr = function(fp){fp})
  
  elig_dda <- elig_dda_load %>% 
    transmute(CENSUS_TRACT = as.character(FIPS),
              FILTER_ELIGIBILITY_dda = TRUE)
    
  
  p_ready_eligibility_dda <- filters_census_tract %>% 
    st_drop_geometry() %>% 
    select(PIN, CENSUS_TRACT) %>% 
    left_join(elig_dda, by = "CENSUS_TRACT") %>% 
    transmute(PIN,
              FILTER_ELIGIBILITY_dda = if_else(is.na(FILTER_ELIGIBILITY_dda),FALSE,FILTER_ELIGIBILITY_dda))
  
  filters_eligibility_dda <- p_ready_eligibility_dda
  
  return(filters_eligibility_dda)
  