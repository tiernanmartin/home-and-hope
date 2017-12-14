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

# Clean, Join, Filter ----




# Write + Upload Data ----
