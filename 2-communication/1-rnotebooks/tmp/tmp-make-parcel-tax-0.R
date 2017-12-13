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


p_load <- 
  make_or_read(p_fp,
               {
                 zip_dir <- "./1-data/2-external"
                 
                 target_name <- "EXTR_Parcel_20171013.csv"
                 
                 dr_id <- as_id("0B5Pp4V6eCkhraF9jOTl3bURiMkU")
                 
                 drive_read_zip(
                   dr_id = dr_id,
                   .tempdir = FALSE,
                   dir_path = zip_dir,
                   read_fun = read_csv,
                   target_name = target_name
                 ) 
               },
               {
                 read_csv(p_fp)
               }) 


p <- 
  p_load %>% 
  rename_all(to_screaming_snake_case) 

rm(p_load)
gc(verbose = FALSE)



# Real Property Account

acct_fp <- "./1-data/2-external/EXTR_RPAcctl_20171013.csv"

acct_load <- 
  make_or_read(acct_fp,
               {
                 dr_id <- "0B5Pp4V6eCkhreGt2djZCTnFUeFU"
                 
                 zip_dir <- "./1-data/2-external"
                 
                 target_name <- "EXTR_RPAcctl_20171013.csv"
                 
                 drive_read_zip(dr_id = dr_id,
                                .tempdir = FALSE,
                                dir_path = zip_dir,
                                read_fun = read_csv,
                                target_name = target_name)
                 
               },
               {read_csv(acct_fp)}) 

tr_fp <- "./1-data/3-interim/kc-tax-reason.rds"

tax_reason <- 
  make_or_read(tr_fp,
               {
                 dr_id <- as_id("0B5Pp4V6eCkhrQzdjY0hKVUhOR3M")
                 
                 drive_read(dr_id = dr_id,
                            .tempfile = FALSE,
                            path = tr_fp,
                            read_fun = read_rds)
                 
               },
               {read_rds(tr_fp)})

ts_fp <- "./1-data/3-interim/kc-tax-status.rds"

tax_status <- 
  make_or_read(ts_fp,
               {
                 dr_id <- as_id("0B5Pp4V6eCkhrWFlYLWM3RC1pRDA")
                 
                 drive_read(dr_id = dr_id,
                            .tempfile = FALSE,
                            path = ts_fp,
                            read_fun = read_rds)
                 
               },
               {read_rds(ts_fp)})

acct <- 
  acct_load %>% 
  rename_all(to_screaming_snake_case) 

rm(acct_load)
gc(verbose = FALSE)



# Parcel (spatial)

p_sf_fp <- "./1-data/2-external/parcel"

p_sf_load <- 
  make_or_read(p_sf_fp,
               {
                 dr_id <- "0B5Pp4V6eCkhrRnM4bHFmWTBTQnM"
                 
                 zip_dir <- "./1-data/2-external"
                 
                 target_name <- "parcel"
                 
                 drive_read_zip(dr_id = dr_id,
                                dir_path = zip_dir,
                                read_fun = st_read,
                                target_name = target_name,
                                .tempdir = FALSE,
                                layer = "parcel",
                                stringsAsFactors = FALSE)
               },
               {st_read(p_sf_fp, layer = "parcel", stringsAsFactors = FALSE)})

p_sf <-
  p_sf_load %>%
  rename_if(not_sfc, to_screaming_snake_case)

rm(p_sf_load)
gc(verbose = FALSE)

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
  mutate(PIN = str_c(MAJOR,MINOR),
         TAX_STATUS = tax_status[TAX_STAT],
         TAX_REASON = tax_reason[TAX_VAL_REASON]) %>% 
  select(PIN, 
         ACCT_NBR,
         BILL_YR,
         TAX_STATUS,
         TAX_REASON,
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

p_sf_ready <- 
  select(p_sf, PIN)

tax_0_sf <- 
  list(acct_ready,p_ready,p_sf_ready) %>% 
  reduce(.f = inner_join, by = "PIN") %>% 
  filter(TAX_STATUS %!in% "Taxable") %>% 
  st_as_sf()



# Write + Upload Data ----

tax_0_fp <- "./1-data/3-interim/tax_0_sf_20171013.gpkg"

st_write(tax_0_sf,dsn = tax_0_fp, driver = "GPKG")

drive_folder <- as_id("0B5Pp4V6eCkhrZ3NHOEE0Sl9FbWc")

drive_upload(tax_0_fp, path = drive_folder)