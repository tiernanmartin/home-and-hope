# COMMAND: MAKE_ENV_RESTRICT ----

make_env_restrict <- function(){ 
  
  env_fp <- here("./1-data/2-external/EXTR_EnvironmentalRestriction_V.csv")
  
  env_dr_id <- as_id("1OAXpWcO22Un1PGEHho4J3cMnMNurhVYu")
  
  env_load <-  
    make_or_read2(fp = env_fp,
                  dr_id = env_dr_id,
                  skip_get_expr = TRUE,
                  get_expr = function(fp){ # Source: http://aqua.kingcounty.gov/extranet/assessor/Environmental%20Restriction.zip
                  },
                  make_expr = function(fp, dr_id){
                    zip_dir <- here("1-data/2-external")
                    
                    target_name <- "EXTR_EnvironmentalRestriction_V"
                    
                    drive_read_zip(
                      dr_id = dr_id,
                      .tempdir = FALSE,
                      dir_path = zip_dir,
                      read_fun = read_csv,
                      target_name = target_name 
                    ) 
                    
                  },
                  read_expr = function(fp){ read_csv(env_fp)})
  
  env <- env_load %>% 
    rename_all(to_screaming_snake_case) 
     
  env_restrict <- env
  
  return(env_restrict)
  
}


