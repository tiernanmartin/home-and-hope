make_filters_eligibility_qct <- function(filters_census_tract){
  
  elig_qtc_fp <- here("1-data/2-external/QCT2018.DBF")
  
  elig_qtc_dr_id <- as_id("1JU0MQKta1mQurT89DJtk8nFvYcgxP4qk")
  
  elig_qtc_load <- 
    make_or_read2(fp = elig_qtc_fp,
                  dr_id = elig_qtc_dr_id,
                  skip_get_expr = FALSE,
                  get_expr = function(fp){
                    # SOURCE:  https://www.huduser.gov/portal/datasets/qct/QCT2018dbf.zip
                  },
                  make_expr = function(fp, dr_id){
                    
                    target_name <- "QCT2018.DBF"
                    
                    dir_path <- here("1-data/2-external/")
                    
                    drive_read_zip(dr_id = dr_id,
                                   .tempdir = FALSE,
                                   dir_path = dir_path,
                                   read_fun = foreign::read.dbf,
                                   target_name = target_name)
                  },
                  read_expr = function(fp){foreign::read.dbf(fp)})
  
  elig_qtc <- elig_qtc_load %>% 
    transmute(CENSUS_TRACT = as.character(FIPS),
              FILTER_ELIGIBILITY_QCT = TRUE)
    
  
  p_ready_eligibility_qct <- filters_census_tract %>% 
    st_drop_geometry() %>% 
    select(PIN, CENSUS_TRACT) %>% 
    left_join(elig_qtc, by = "CENSUS_TRACT") %>% 
    transmute(PIN,
              FILTER_ELIGIBILITY_QCT = if_else(is.na(FILTER_ELIGIBILITY_QCT),FALSE,FILTER_ELIGIBILITY_QCT))
  
  filters_eligibility_qct <- p_ready_eligibility_qct
  
  return(filters_eligibility_qct)
  
}