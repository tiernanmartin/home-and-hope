# COMMAND: MAKE_ZCTA ----
make_zcta <- function(king_county){
  
  zcta_fp <- here("1-data/2-external/zcta.gpkg")
  
  zcta_dr_id <- as_id("1QyJT9tIHGuMrRIx-i9DCQK4kroIY_X-7")
  
  zcta_load <- 
    make_or_read2(fp = zcta_fp,
                  dr_id = zcta_dr_id,
                  skip_get_expr = FALSE,
                  get_expr = function(fp){
                    
                    zcta <- zctas()
                    
                    st_write(zcta, fp, driver = "GPKG")
                    
                    zip_path <- here("1-data/2-external/zcta.zip")
                    
                    zip_pithy(zip_path, fp)
                    
                    drive_folder <- as_id("0B5Pp4V6eCkhrdlJ3MXVaNW16T0U")
                  
                    drive_upload(zip_path, drive_folder)
                      
                  },
                  make_expr = function(fp, dr_id){
                    zip_dir <- here("1-data/2-external")
                    
                    target_name <- "zcta.gpkg"
                    
                    drive_read_zip(dr_id = dr_id,
                                   dir_path = zip_dir,
                                   read_fun = st_read,
                                   target_name = target_name,
                                   .tempdir = FALSE, 
                                   stringsAsFactors = FALSE)
                  },
                  read_expr = function(fp){read_sf(fp,stringsAsFactors = FALSE)})
  
  buff_dist <- 5280*2 # 2 miles in ft
  max_vertices <- 256
  
  kc_buff <-  king_county %>% 
    st_buffer(buff_dist) %>% 
    st_subdivide(max_vertices) %>% 
    st_collection_extract()
  
  zcta_2926 <- st_transform(zcta_load, 2926) 
  
  zcta_2926$geom_pt <- st_centroid(st_geometry(zcta_2926))
  
  zcta_2926$ZCTA_WITHIN_KC <- st_intersects_any(zcta_2926$geom_pt,kc_buff)
  
  zcta <- filter(zcta_2926, ZCTA_WITHIN_KC) 
  
  return(zcta)
  
}
