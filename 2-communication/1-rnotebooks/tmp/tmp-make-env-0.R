pub_fp <- root_file("./1-data/3-interim/public_parcels_20170727.rds")

pub <- 
  make_or_read(pub_fp,
               {
                 dr_id <- as_id("0B5Pp4V6eCkhrSUhXdHNtdjBfa28")
                 
                 drive_read(dr_id = dr_id,
                            path = pub_fp,
                            read_fun = read_rds,
                            .tempfile = FALSE)
               },
               {read_rds(pub_fp)})

env_fp <- root_file("./1-data/3-interim/environmental_restrictions.rds")

env <- 
  make_or_read(env_fp,
               {
                 dr_id <- as_id("1DOoMKnTO1f-8jWxley4yolwT0VC48Dqr")
                 
                 drive_read(dr_id = dr_id,
                            .tempfile = FALSE,
                            path = env_fp,
                            read_fun = read_csv)
                 
               },
               {read_csv(env_fp)}) %>% 
  rename_all(to_screaming_snake_case)

env_ready <- 
  env %>% 
  rename_all(to_screaming_snake_case) %>% 
  mutate(PIN = str_c(MAJOR,MINOR)) %>% 
  select(PIN,everything())

env_test <- 
  semi_join(env_ready, pub, by = "PIN")

