# COMMAND: MAKE_BUS_STOPS_METRO ----

make_bus_stops_metro <- function(){
  
  bs_fp <- here("1-data/2-external/transitstop/transitstop.shp")
  
  bs_dr_id <- as_id("1yB5EYVPdC4ephNwRLWOtH9UK5PPH8cNc")
  
  bs_load <- 
    make_or_read2(fp = bs_fp, dr_id = bs_dr_id, skip_get_expr = FALSE,
                  get_expr = function(fp){
                    # SOURCE: ftp://ftp.kingcounty.gov/gis-web/GISData/transitstop_SHP.zip
                  },
                  make_expr = function(fp, dr_id){
                    zip_dir <- here("1-data/2-external/")
                    
                    target_name <- "transitstop" 
                    
                    drive_read_zip(dr_id = dr_id,
                                   .tempdir = FALSE,
                                   dir_path = zip_dir,
                                   read_fun = read_sf,
                                   target_name = target_name)
                  },
                  read_expr = function(fp){read_sf(fp)})
  
  stop_status <- tribble(
    ~ STOP_STATUS,    ~ STOP_STATUS_DESC,
            "ACT",            	"Active",
            "CLO",	"Permanently Closed",
            "INA",	"Temporary Inactive",
            "PLN",	"Plan"
    )
  
  bs <- bs_load %>% 
    st_transform(2926) %>% 
    left_join(stop_status, by = c(STOP_STATU = "STOP_STATUS")) %>% 
    transmute(STOP_ID = as.character(STOP_ID),
              TRANSIT_TYPE = "bus",
              TRANSIT_PROVIDER_NAME = "KING COUNTY METRO",
              STOP_STATUS = STOP_STATUS_DESC) %>% 
    filter(STOP_STATUS %in% c("Active", "Temporary Inactive", "Plan"))
  
  bus_stops_metro <- bs
  
  return(bus_stops_metro)
}