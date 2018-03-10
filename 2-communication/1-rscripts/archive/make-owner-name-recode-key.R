make_owner_name_recode_key <- function(){

  recode_key_fp <- here("1-data/1-raw/owner_name_recode_key.rda")
  
  recode_key_dr_id <- as_id("1sIiBnyYfwazVXD_1ktBVPR_rmEejD6c0")
  
  recode_key_load <- 
    make_or_read2(fp = recode_key_fp, dr_id = recode_key_dr_id, skip_get_expr = FALSE,
                  get_expr = function(fp){
                    
                      tp_name_recode <- 
                      tribble(
                        ~ ORIG, ~ NEW,
                        "-"," ",
                        "(|)","",
                        "CTR", "CENTER",
                        "DIST", "DISTRICT",
                        "DIS", "DISTRICT",
                        "CTY", "CITY",
                        "SVCS", "SERVICES",
                        "SVSC", "SERVICES",
                        "WTR", "WATER",
                        "AUTH", "AUTHORITY",
                        "KC", "KING COUNTY",
                        "WA", "WASHINGTON",
                        "WASH", "WASHINGTON",
                        "WS", "WASHINGTON STATE",
                        "WASHINGOTN", "WASHINGTON",
                        "WADNR", "WASHINGTON DNR",
                        "SCH", "SCHOOL",
                        "SCHL", "SCHOOL",
                        "SD", "SCHOOL DISTRICT",
                        "DI", "DISTRICT"
                      )  
                      
                      write_rds(tp_name_recode, fp)
                      
                      drive_folder <- as_id("0B5Pp4V6eCkhrb1lDdlNaOFY4V0U")
                      
                      drive_upload(fp, drive_folder) 
                  },
                  make_expr = function(fp, dr_id){
                    drive_read(dr_id = dr_id,
                               .tempfile = FALSE,
                               path = fp,
                               read_fun = read_rds)
                  },
                  read_expr = function(fp){read_rds(fp)})
  
  owner_name_recode_key <- recode_key_load
  
  return(owner_name_recode_key)
   
  
}