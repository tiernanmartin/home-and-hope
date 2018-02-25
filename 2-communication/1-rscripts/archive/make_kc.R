# COMMAND: MAKE_KING_COUNTY ----
make_kc <- function(){
  
  kc_fp <- here("1-data/2-external/kc.gpkg")
  
  kc_dr_id <- as_id("1dcxPI9uV8G6yQYIGLyfGQXhrOBC9bJW_")
  
  kc_load <- 
    make_or_read2(fp = kc_fp,
                  dr_id = kc_dr_id,
                  skip_get_expr = FALSE,
                  get_expr = function(fp){
                    
                    kc_id <- 53
                    
                    kc <- kc_id %>% 
                      counties() %>% 
                      filter(NAME %in% "King")
                    
                    st_write(kc, fp, driver = "GPKG")
                  
                    drive_folder <- as_id("0B5Pp4V6eCkhrdlJ3MXVaNW16T0U")
                  
                    drive_upload(fp, drive_folder)
                      
                  },
                  make_expr = function(fp, dr_id){
                    drive_read(dr_id = dr_id,.tempfile = FALSE,path = fp,read_fun = read_sf,stringsAsFactors = FALSE)
                  },
                  read_expr = function(fp){read_sf(fp,stringsAsFactors = FALSE)})
  
  kc <- kc_load %>% st_transform(2926)
  
  return(kc)
  
}
