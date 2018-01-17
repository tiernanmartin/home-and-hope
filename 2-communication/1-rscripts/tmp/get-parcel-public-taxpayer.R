# Setup ----
 
library(sf)
library(googledrive)
library(miscgis) 
library(snakecase)
library(magrittr)
library(rprojroot)
library(RSocrata) 

library(tidyverse)

root <- rprojroot::is_rstudio_project
root_file <- root$make_fix_file()
options(httr_oob_default=TRUE)  

# Load data ----

# This data is from a special dataset created by the King County 
# Assessor office (or maybe the GIS Dept?) to facilitate
# the analysis of public land

pub_fp <- root_file("1-data/2-external/kc-public-parcels.csv")

soda_api_endpoint <- "https://data.kingcounty.gov/resource/pdft-6nx2.json"

pub_load <- RSocrata::read.socrata(url = soda_api_endpoint,email = "FAKE@FAKE_EMAIL.COM",password = "FAKE_PASSWORD")

pub <- pub_load %>%  
  as_tibble %>% 
  select(major,
         minor,
         pin,
         TaxpayerName = taxpayer_na,
         districtName = district_na,
         PropName = prop_name,
         LandGeneralUse = land_genera,
         LandCurrentZoning = land_curren,
         LandPresentUse = land_presen,
         sq_ft_lot,
         land_issues,
         eRealPropertyLink = e_real_prope,
         bus_buff_750,
         bus_buf_qtr_m
         ) %>% 
  rename_all(to_screaming_snake_case)

write_csv(pub, pub_fp)

drive_folder <- as_id("0B5Pp4V6eCkhrdlJ3MXVaNW16T0U")

# drive_upload(pub_fp, drive_folder)

drive_update(as_id("1ERHvVa9K6F-lk1L8X47Oy1sdw_s2t9kv"), pub)
  
