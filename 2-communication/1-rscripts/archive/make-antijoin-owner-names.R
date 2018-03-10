make_antijoin_owner_names <- function(){
  
  anti_fp <- here("1-data/2-external/stop-words.csv")
  
  anti_dr_id <- as_id("17n2h9np6OjHyD9cZ0tHSRbBubpbN4ckX")
  
  anti_load <- 
    make_or_read2(fp = anti_fp, dr_id = anti_dr_id, skip_get_expr = FALSE,
                  get_expr = function(fp){
                    data(stop_words)
                    
                    write_csv(stop_words, fp)
                    
                    drive_folder <- as_id("0B5Pp4V6eCkhrdlJ3MXVaNW16T0U")
                    
                    drive_upload(fp, drive_folder)
                  },
                  make_expr = function(fp, dr_id){
                    drive_read(dr_id = dr_id,
                               .tempfile = FALSE,
                               path = fp,
                               read_fun = read_csv)
                  },
                  read_expr = function(fp){read_csv(fp)})
  
  numbers_0_500 <- tibble(word = as.character(0:500))
  
  ok_words <- c("STATES")
  
  anti <- transmute(anti_load, word = toupper(word)) %>% 
    bind_rows(numbers_0_500) %>% 
    filter(word %!in% ok_words)
  
  antijoin_owner_names <- anti

  return(antijoin_owner_names)
    
  
}