# COMMAND: MAKE_SCHOOL_DISTRICTS ----
make_school_districts <- function(){
  
  school_dist_fp <- here("1-data/2-external/schdst")
  
  school_dist_dr_id <- as_id("11K9yTC7C1PnKAtTIPqLsKxZE6qF6p-QO")
  
  school_dist_load <- 
    make_or_read2(fp = school_dist_fp,
                  dr_id = school_dist_dr_id,
                  skip_get_expr = FALSE,
                  get_expr = function(fp){
                    
                    # SOURCE: ftp://ftp.kingcounty.gov/gis-web/GISData/schdst_SHP.zip
                      
                  },
                  make_expr = function(fp, dr_id){
                    zip_dir <- here("1-data/2-external")
                    
                    target_name <- "schdst"
                    
                    drive_read_zip(dr_id = dr_id,
                                   dir_path = zip_dir,
                                   read_fun = st_read,
                                   target_name = target_name,
                                   .tempdir = FALSE, 
                                   stringsAsFactors = FALSE)
                  },
                  read_expr = function(fp){read_sf(fp,stringsAsFactors = FALSE)})
  
  
  school_districts <- rename_if(school_dist_load, not_sfc, to_screaming_snake_case)
  
  return(school_districts)
  
}