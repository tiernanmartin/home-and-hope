# COMMAND: MAKE_TRANSIT_STOPS_OSM ----

make_transit_stops_osm <- function(){
  
  ts_fp <- here("1-data/2-external/transit-stops-osm.gpkg")
  
  ts_dr_id <- as_id("1kcEMuGu4QdKpva61Lf5CN-ZfWCk59leA")
  
  ts_load <- 
    make_or_read2(fp = ts_fp, dr_id = ts_dr_id, skip_get_expr = FALSE,
                  get_expr = function(fp){
                    
                    q <- opq(bbox = "King County, Washington") %>% 
                      add_osm_feature(key = "public_transport", value = "",value_exact = FALSE)
                    
                    transit_pts <- q %>% 
                      osmdata_sf() %>% 
                      pluck("osm_points") %>% 
                      rename_all(to_screaming_snake_case)
                    
                    st_write(transit_pts, fp)
                    
                    drive_folder <- as_id("0B5Pp4V6eCkhrdlJ3MXVaNW16T0U")
                    
                    drive_upload(fp, drive_folder)
                    
                  },
                  make_expr = function(fp, dr_id){
                    drive_read(dr_id = dr_id,.tempfile = FALSE,path = fp,read_fun = read_sf)
                  },
                  read_expr = function(fp){read_sf(fp)}
                    )
  
  ts_pts <- ts_load %>% 
    transmute( OSM_ID = as.character(OSM_ID)) %>% 
    st_transform(2926)
  
  ts_ready <- ts_load %>% 
    st_drop_geometry() %>% 
    select(OSM_ID ,NAME,BUS, TRAIN, STEETCAR = TRAM, FERRY, PUBLIC_TRANSPORT, SOURCE) %>% 
    gather(TRANSIT_TYPE, VALUE, -OSM_ID, -NAME,-PUBLIC_TRANSPORT, -SOURCE) %>% 
    filter(!is.na(VALUE)) %>%  
    transmute(TRANSIT_STOP_OSM_ID = OSM_ID,
              TRANSIT_STOP_NAME = NAME,
              TRANSIT_STOP_TYPE = TRANSIT_TYPE,
              TRANSIT_STOP_SOURCE = SOURCE) %>% 
    mutate_if(is.factor, as.character) %>% 
    left_join(ts_pts, by = c(TRANSIT_STOP_OSM_ID = "OSM_ID")) %>% 
    st_sf
  
  view_ts_ready <- function(){mapview(ts_ready, zcol = "TRANSIT_STOP_TYPE", legend = TRUE)}
  
  transit_stops_osm <- ts_ready
  
  return(transit_stops_osm)
  
  
}