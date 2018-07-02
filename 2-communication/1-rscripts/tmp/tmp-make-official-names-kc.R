# MAKE_OFFICIAL_NAMES_KC ----

make_official_names_kc <- function(...){
  
  names_kc_fp <-  here("1-data/2-external/kc-bureaucracy.csv")
  
  names_kc_dr_id <- as_id("1Zvg0ewPjjAgIlTIChWopP-hUspI1gXv2")
  
  names_kc_load <- make_or_read2(fp = names_kc_fp,dr_id = names_kc_dr_id, skip_get_expr = TRUE,
                                 get_expr = function(fp){
                                   github_url <- "https://github.com/tiernanmartin/datasets/raw/master/king-county-bureaucracy/data/kc-bureaucracy.csv"
                                 
                                   dat <- read_csv(github_url)
                                   
                                   write_fp <- here("1-data/2-external/kc-bureaucracy.csv")
                                   
                                   write_csv(dat, write_fp)
                                   
                                   drive_folder <- as_id("0B5Pp4V6eCkhrdlJ3MXVaNW16T0U")
                                   
                                   drive_upload(media = write_fp, drive_folder)
                                   
                                   },
                                 make_expr = function(fp, dr_id){
                                   drive_read(dr_id = dr_id,
                                              .tempfile = FALSE,
                                              path = fp,
                                              read_fun = read_csv)
                                 },
                                 read_expr = function(fp){read_csv(fp)})
  
  official_names_kc <- names_kc_load
  
  return(official_names_kc)
  
}