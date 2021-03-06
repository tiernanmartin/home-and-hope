# Setup ----

library(tidyverse) 
library(sf)
library(googledrive)
library(miscgis)
library(forcats)
library(leaflet)
library(mapview)
library(miscgis)
library(snakecase)
library(magrittr)
library(rprojroot)
library(knitr) 
library(lubridate)
library(scales)
library(lwgeom)
library(tictoc)
library(rbenchmark)
library(htmltools)

root <- rprojroot::is_rstudio_project
root_file <- root$make_fix_file()
options(httr_oob_default=TRUE) 
htmltools::tagList(rmarkdown::html_dependency_font_awesome())

# Access Data ----

# Apartment Buildings

apt_fp <- "./1-data/2-external/EXTR_AptComplex.csv"


apt_load <- apt_fp %>% 
  make_or_read({
    zip_dir <- "./1-data/2-external"
    
    target_name <- "EXTR_AptComplex.csv"
    
    dr_id <- as_id("11kkudStD4TuoiRqMie-Y4_8tZQJmLPBw")
    
    drive_read_zip(
      dr_id = dr_id,
      .tempdir = FALSE,
      dir_path = zip_dir,
      read_fun = read_csv,
      target_name = target_name
    ) 
  },
  {
    read_csv(apt_fp)
  }) 


apt <- apt_load %>% 
  rename_all(to_screaming_snake_case) %>% 
  mutate(PIN = str_c(MAJOR,MINOR,sep = ""))

rm(apt_load)
gc(verbose = FALSE)


# Commercial Buildings

comm_fp <- "./1-data/2-external/EXTR_CommBldg.csv"


comm_load <- comm_fp %>% 
  make_or_read({
    zip_dir <- "./1-data/2-external"
    
    target_name <- "EXTR_CommBldg.csv"
    
    dr_id <- as_id("1VT_plwHQve51ldIg3chFUpcTSMD_ZyrN")
    
    drive_read_zip(
      dr_id = dr_id,
      .tempdir = FALSE,
      dir_path = zip_dir,
      read_fun = read_csv,
      target_name = target_name
    ) 
  },
  {
    read_csv(comm_fp)
  }) 


comm <- comm_load %>% 
  rename_all(to_screaming_snake_case) %>% 
  mutate(PIN = str_c(MAJOR,MINOR,sep = ""))

rm(comm_load)
gc(verbose = FALSE)

# Condos

condo_fp <- "./1-data/2-external/EXTR_CondoComplex.csv"


condo_load <- condo_fp %>% 
  make_or_read({
    zip_dir <- "./1-data/2-external"
    
    target_name <- "EXTR_CondoComplex.csv"
    
    dr_id <- as_id("1avYhRKzHijnc-YZQDGICqWrQNhQoTwnB")
    
    drive_read_zip(
      dr_id = dr_id,
      .tempdir = FALSE,
      dir_path = zip_dir,
      read_fun = read_csv,
      target_name = target_name
    ) 
  },
  {
    read_csv(condo_fp)
  }) 


condo <- condo_load %>% 
  rename_all(to_screaming_snake_case) 

rm(condo_load)
gc(verbose = FALSE)


# Residential

res_fp <- "./1-data/2-external/EXTR_ResBldg.csv"


res_load <- res_fp %>% 
  make_or_read({
    zip_dir <- "./1-data/2-external"
    
    target_name <- "EXTR_ResBldg.csv"
    
    dr_id <- as_id("10rz6hc4lEAaaU-0Jcv0iCiVMFBVTc-en")
    
    drive_read_zip(
      dr_id = dr_id,
      .tempdir = FALSE,
      dir_path = zip_dir,
      read_fun = read_csv,
      target_name = target_name
    ) 
  },
  {
    read_csv(res_fp)
  }) 


res <- res_load %>% 
  rename_all(to_screaming_snake_case) %>% 
  mutate(PIN = str_c(MAJOR,MINOR,sep = ""))

rm(res_load)
gc(verbose = FALSE)

# Create: bldg ----

# Create an empty tibble with all of the desired columns and no rows
# This will be the join seed

bldg_empty <- tibble(PIN = as.character(""), 
                     BLDG_NBR = as.integer(""),
                     BLDG_NET_SQ_FT= as.integer(""),
                     NBR_LIVING_UNITS = as.integer(""),
                     NBR_BLDGS = as.integer(""),
                     BLDG_CAT = as.character("")) %>% 
  slice(0)

comm_join <- comm %>% 
  transmute(PIN,
            BLDG_NBR,
            BLDG_NET_SQ_FT,
            NBR_BLDGS,
            BLDG_CAT = "commercial")

res_join <- res %>% 
  transmute(PIN,
            BLDG_NBR,
            BLDG_NET_SQ_FT = SQ_FT_TOT_LIVING,
            NBR_LIVING_UNITS,
            BLDG_CAT = "residential")

apt_join <- apt %>% 
  transmute(PIN,
            NBR_BLDGS,
            NBR_LIVING_UNITS = NBR_UNITS,
            BLDG_CAT = "apartment")

condo_join <- condo %>% 
  transmute(PIN = MAJOR,
            NBR_LIVING_UNITS = NBR_UNITS,
            BLDG_CAT = "condo")

# Join all bulding objects to the empty tibble
bldg_all <- bldg_empty %>% 
  full_join(comm_join) %>% 
  full_join(res_join) %>% 
  full_join(apt_join) %>% 
  full_join(condo_join) 


bldg_all_sum <- bldg_all %>%  
  mutate(NBR_BLDGS = if_else(BLDG_CAT %in% c("residential", "condo"),as.integer(1),NBR_BLDGS)) %>% 
  mutate(CAT_LGL = TRUE,
         COL_NAME = str_c("TYPE",toupper(BLDG_CAT),"LGL", sep = "_")) %>% 
  spread(COL_NAME, CAT_LGL) %>% 
  mutate_at(vars(TYPE_APARTMENT_LGL:TYPE_RESIDENTIAL_LGL), ~ if_else(is.na(.),FALSE,.)) %>% 
  group_by(PIN) %>%  
  summarise(NBR_BLDGS = max(n()),  
            BLDG_NET_SQ_FT = sum(BLDG_NET_SQ_FT, na.rm = TRUE),
            NBR_LIVING_UNITS = sum(NBR_LIVING_UNITS, na.rm = TRUE),
            TYPE_APARTMENT_LGL = any(TYPE_APARTMENT_LGL),
            TYPE_COMMERCIAL_LGL = any(TYPE_COMMERCIAL_LGL),
            TYPE_CONDO_LGL = any(TYPE_CONDO_LGL),
            TYPE_RESIDENTIAL_LGL = any(TYPE_RESIDENTIAL_LGL))

bldg <- bldg_all_sum
  
# Write + Upload Data ----

bldg_fp <- "./1-data/3-interim/bldg.csv"

bldg_ready <- bldg

drive_folder <- as_id("0B5Pp4V6eCkhrZ3NHOEE0Sl9FbWc")

write_csv(bldg_ready,bldg_fp)

drive_upload(bldg_fp, path = drive_folder, name = "bldg.csv")

drive_update(as_id("1qsfN1k8UyJOZHFYD_5HSw7nyeS_lzn2_"), p_util_fp)
