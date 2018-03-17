make_trigger_dd_google_drive <- function(){
  
  dr_id <- as_id("1EAjo_iL_wibBQUqZ9hvE1My6by4ip57b-dWB8mzmhN0")
  
  mod_time <- dr_id %>% 
    drive_get %>% 
    mutate(modified = lubridate::as_datetime(map_chr(drive_resource, "modifiedTime"))) %>% 
    pull
  
  return(mod_time)
}