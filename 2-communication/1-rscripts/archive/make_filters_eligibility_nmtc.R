make_filters_eligibility_nmtc <- function(filters_census_tract){
  
  elig_nmtc_fp <- here("1-data/2-external/NMTC-2011-2015-LIC-Nov2-2017-4pm.xlsx")
  
  elig_nmtc_dr_id <- as_id("1gM7oRPZTEDM4N7Xhcmih8R4WFTxLj8rK")
  
  elig_nmtc_load <- 
    make_or_read2(fp = elig_nmtc_fp,
                  dr_id = elig_nmtc_dr_id,
                  skip_get_expr = FALSE,
                  get_expr = function(fp){
                    # SOURCE: https://www.cdfifund.gov/Documents/NMTC%202011-2015%20LIC%20Nov2-2017-4pm.xlsx
                  },
                  make_expr = function(fp, dr_id){
                    drive_read(dr_id = dr_id,  path = fp, read_fun = read_xlsx,.tempfile = FALSE)
                  },
                  read_expr = function(fp){read_xlsx(fp)})
  
  elig_nmtc <- elig_nmtc_load %>% 
    clean_names() %>% 
    rename_all(to_screaming_snake_case) %>% 
    transmute(CENSUS_TRACT = X_2010_CENSUS_TRACT_NUMBER_FIPS_CODE_GEOID,
              NMTC = DOES_CENSUS_TRACT_QUALIFY_FOR_NMTC_LOW_INCOME_COMMUNITY_LIC_ON_POVERTY_OR_INCOME_CRITERIA,
              FILTER_ELIGIBILITY_NMTC = if_else(NMTC %in% "Yes",TRUE,FALSE)
              ) 
  
  
   p_ready_eligibility_nmtc <- filters_census_tract %>% 
    st_drop_geometry() %>% 
    select(PIN, CENSUS_TRACT) %>% 
    left_join(elig_nmtc, by = "CENSUS_TRACT") %>% 
    select(PIN,
           FILTER_ELIGIBILITY_NMTC)
  
  filters_eligibility_nmtc <- p_ready_eligibility_nmtc
  
  return(filters_eligibility_nmtc)
   
}