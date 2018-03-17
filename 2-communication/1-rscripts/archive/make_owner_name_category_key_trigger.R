make_trigger_owner_name_category_key <- function(){
  
  dr_id <- as_id("1cYNIpQpDJTZWi46S_9bZ6rjgRu8JWes1BxOeoJJD2tg")
  
  mod_time <- dr_id %>% 
    drive_get %>% 
    mutate(modified = lubridate::as_datetime(map_chr(drive_resource, "modifiedTime"))) %>% 
    pull
  
  return(mod_time)
}