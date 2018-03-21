# COMMAND: MAKE_PLACE_SPACES_OSM ----

make_play_spaces_osm <- function(){
  
  ps_fp <- here("1-data/2-external/play-spaces-osm.gpkg")
  
  ps_dr_id <- as_id("1lGOap459M_cXNxnYgTNH8H6dtA-FfZbe")
  
  ps_load <- 
    make_or_read2(fp = ps_fp, dr_id = ps_dr_id, skip_get_expr = FALSE,
                  get_expr = function(fp){
                    
                    q <- opq(bbox = "King County, Washington") %>% 
                      add_osm_feature(key = "leisure", value = "park",value_exact = FALSE) 
                    
                    
                    
                    osm_list <-  list("osm_polygons",
                                      "osm_multipolygons") %>% 
                      map(~ pluck(osmdata_sf(q),.x)) 
                    
                    common_cols <- osm_list %>% 
                      map(names) %>% 
                      reduce(intersect) 
                    
                    play_spaces <- osm_list %>% 
                      map(~ select_at(.x, .vars = vars(common_cols))) %>% 
                      reduce(rbind)%>%  
                      st_cast("MULTIPOLYGON") %>% 
                      as_tibble %>% 
                      st_sf %>% 
                      st_transform(2926) %>% 
                      rename_if(not_sfc, to_screaming_snake_case) %>% 
                      mutate_if(is.factor, as.character) %>% 
                      select(PLAY_SPACE_OSM_ID = OSM_ID,
                             PLAY_SPACE_NAME = NAME,
                             PLAY_SPACE_TYPE = LEISURE) 
                    
                    st_write(play_spaces, fp)
                    
                    drive_folder <- as_id("0B5Pp4V6eCkhrdlJ3MXVaNW16T0U")
                    
                    drive_upload(fp, drive_folder)
                    
                  },
                  make_expr = function(fp, dr_id){
                    drive_read(dr_id = dr_id,.tempfile = FALSE,path = fp,read_fun = read_sf)
                  },
                  read_expr = function(fp){read_sf(fp)}
    )
  
  play_spaces <- ps_load %>%   
                      filter(!is.na(PLAY_SPACE_TYPE)) %>%  
                      filter(PLAY_SPACE_TYPE %!in% "dog_park")
  
  
  play_spaces %>%
    st_drop_geometry() %>%
    arrange(PLAY_SPACE_NAME) %>%
    vis_dat()

  play_spaces %>%
    st_drop_geometry() %>%
    count(PLAY_SPACE_TYPE, sort = TRUE)
  
  view_ps_ready <- function(){mapview(ps_ready, zcol = "TRANSIT_STOP_TYPE", legend = TRUE)}
  
  play_spaces_osm <- play_spaces
  
  return(play_spaces_osm)
  
  
}