# Setup ----

library(tidyverse)
library(stringr)
library(sf)
library(googledrive)
library(miscgis)
library(forcats)
library(leaflet)
library(mapview)
library(miscgis)
library(htmltools)
library(snakecase)

options(httr_oob_default=TRUE) 

# Access Data ----

# Parcel (tabular)

p_fp <- "./1-data/2-external/EXTR_Parcel_20171013.csv"

maybe_make(p_fp,{
  dr_id <- "0B5Pp4V6eCkhrUjVsYWNfNmRnNWc"
  
  d <- drive_get(id = dr_id)
  
  tmp <- tempfile()
  
  d %>% drive_download(path = tmp)
  
  unzip_dir <- "./1-data/2-external/"
  
  unzip(zipfile = tmp,exdir = unzip_dir)
})

p <- 
  read_csv(p_fp) %>% 
  rename_all(to_screaming_snake_case)


# Real Property Account

acct_fp <- "./1-data/2-external/EXTR_RPAcct_NoName.csv"

maybe_make(acct_fp,{
  dr_id <- "0B5Pp4V6eCkhrOVBJdW1ZN21jTDA"
  
  d <- drive_get(id = dr_id)
  
  tmp <- tempfile()
  
  d %>% drive_download(path = tmp)
  
  unzip_dir <- "./1-data/2-external/"
  
  unzip(zipfile = tmp,exdir = unzip_dir)
})

acct <- 
  read_csv(acct_fp) %>% 
  rename_all(to_screaming_snake_case)

# Parcel (spatial)

p_sf_fp <- "./1-data/2-external/parcel"

maybe_make(p_sf_fp,{
  dr_id <- "0B5Pp4V6eCkhrRnM4bHFmWTBTQnM"
  
  d <- drive_get(id = dr_id)
  
  tmp <- tempfile()
  
  d %>% drive_download(path = tmp)
  
  unzip_dir <- "./1-data/2-external/"
  
  unzip(zipfile = tmp,exdir = unzip_dir)
})

p_sf <- 
  st_read(p_sf_fp, layer = "parcel") %>% 
  rename_if(not_sfc, to_screaming_snake_case)



# Clean, Join, Filter ----


p_ready <- 
  p %>% 
  mutate(PIN = str_c(MAJOR, MINOR)) %>% 
  select(PIN,
         PROP_NAME,
         PROP_TYPE,
         DISTRICT_NAME,
         CURRENT_ZONING,
         PRESENT_USE,
         SQ_FT_LOT,
         ACCESS,
         TOPOGRAPHY,
         PCNT_UNUSABLE,
         CONTAMINATION,
         HISTORIC_SITE,
         EASEMENTS,
         SEISMIC_HAZARD,
         LANDSLIDE_HAZARD,
         STEEP_SLOPE_HAZARD 
         )
  

acct_frmt <- 
  acct %>% 
  mutate(PIN = str_c(MAJOR,MINOR)) %>% 
  select(PIN, 
         ACCT_NBR,
         BILL_YR,
         TAX_VAL_REASON,
         APPR_LAND_VAL,
         APPR_IMPS_VAL,
         TAXABLE_LAND_VAL,
         TAXABLE_IMPS_VAL)

acct_ready <- 
  acct_frmt %>% 
  miscgis::subset_duplicated("PIN") %>% 
  group_by(PIN) %>% 
  drop_na %>% 
  ungroup %>% 
  bind_rows(subset_duplicated(acct_frmt,"PIN",notin = TRUE)) %>% 
  arrange(PIN)

tax_0 <- 
  list(acct_ready,p_ready,p_sf) %>% 
  reduce(.f = inner_join, by = "PIN") %>% 
  filter(TAXABLE_LAND_VAL == 0 & TAXABLE_IMPS_VAL == 0) %>% 
  st_as_sf()

# Write Data ----

write_csv(tax_0, "./parcel-tax0-20171013.csv")


# Access Data ----

# Parcel

fp <- "./1-data/2-external/EXTR_Parcel_20171013.csv"

maybe_make(fp,{
  dr_id <- "0B5Pp4V6eCkhrUjVsYWNfNmRnNWc"
  
  d <- drive_get(id = dr_id)
  
  tmp <- tempfile()
  
  d %>% drive_download(path = tmp)
  
  unzip_dir <- "./1-data/2-external/"
  
  unzip(zipfile = tmp,exdir = unzip_dir)
})


# Parcel Tax Info

fp <- "./1-data/2-external/EXTR_Parcel_20171013.csv"

maybe_make(fp,{
  dr_id <- "0B5Pp4V6eCkhrcFNYeXNSWUxJRkE"
  
  d <- drive_get(id = dr_id)
  
  tmp <- tempfile()
  
  d %>% drive_download(path = tmp)
  
  unzip_dir <- "./1-data/2-external/"
  
  unzip(zipfile = tmp,exdir = unzip_dir)
})


# Load Data ----

