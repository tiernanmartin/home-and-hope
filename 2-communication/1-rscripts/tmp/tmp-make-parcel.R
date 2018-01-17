# Setup ----

library(sf)
library(googledrive) 
library(snakecase)
library(miscgis)

library(tidyverse)

root <- rprojroot::is_rstudio_project
root_file <- root$make_fix_file()
options(httr_oob_default=TRUE) 
htmltools::tagList(rmarkdown::html_dependency_font_awesome())

# Access Data ----

# KC Lookup

lu_fp <- root_file("1-data/2-external/EXTR_LookUp.csv")

lu_dr_id <- as_id("1-L42pHb7lySqonanSwBbXSH9OZrKHp2A")

lu_load <- lu_fp %>% 
  make_or_read2(dr_id = lu_dr_id,
                get_expr = {
                  # Source: http://aqua.kingcounty.gov/extranet/assessor/Lookup.zip
                },
                make_expr = {
                  drive_read(dr_id = lu_dr_id,
                             .tempfile = FALSE,
                             path = lu_fp,
                             read_fun = read_csv)
                },
                read_expr = {read_csv(lu_fp)}
                )
  
lu <- rename_all(lu_load, to_screaming_snake_case) 

pu <-  lu %>%  
  filter(LU_TYPE == 102) %>% 
  select(PRESENTUSE = LU_ITEM,
         PRESENTUSE_DESC = LU_DESCRIPTION)


# Real Property Account

acct_fp <- root_file("1-data/2-external/kc_real_prop_acct_extract.rds")

acct_dr_id <- as_id("19f4AUMAEshnDNJqGjVsurFthbKGcKYvh")

acct_load <- acct_fp %>% 
  make_or_read2(dr_id = acct_dr_id,
                get_expr = {
                  realprop <- read.socrata("https://data.kingcounty.gov/resource/mmfz-e8xr.csv",
                                           email = "FAKE_NAME@FAKE_EMAIL.COM",
                                           password = "FAKE_PASSWORD" # CHANGE TO REAL BEFORE RUNNING
                  )
                  
                  r <- realprop %>% 
                    rename_all(to_screaming_snake_case) 
                  
                  r_fp <- root_file("./1-data/2-external/kc_real_prop_acct_extract.rds")
                  
                  drive_folder_id <- as_id("0B5Pp4V6eCkhrdlJ3MXVaNW16T0U")
                  
                  write_rds(r,r_fp, compress = "gz")
                  
                  drive_upload(media = r_fp, path = drive_folder_id)
                  
                  # drive_update(as_id("19f4AUMAEshnDNJqGjVsurFthbKGcKYvh"), r_fp)
                },
                make_expr = {
                  drive_read(dr_id = acct_dr_id,path = acct_fp,.tempfile = FALSE,read_fun = read_rds)
                },
                read_expr = {read_rds(acct_fp)})


# Parcel 

p_fp <- root_file("./1-data/2-external/EXTR_Parcel_20171013.csv")

p_dr_id <- as_id("0B5Pp4V6eCkhraF9jOTl3bURiMkU")


p_load <- p_fp %>% 
  make_or_read2(dr_id = p_dr_id,
                get_expr = { # Source: ftp://ftp.kingcounty.gov/gis-web/GISData/public_parcels_SHP.zip}
                },
                make_expr = {
                  zip_dir <- "./1-data/2-external"
                 
                 target_name <- "EXTR_Parcel_20171013.csv"
                 
                 drive_read_zip(
                   dr_id = dr_id,
                   .tempdir = FALSE,
                   dir_path = zip_dir,
                   read_fun = read_csv,
                   target_name = target_name
                 ) 
                 
                },
                read_expr = { read_csv(p_fp)})

p <- rename_all(p_load, to_screaming_snake_case) 

rm(p_load)
gc(verbose = FALSE)

# Tax Reason

tr_fp <- root_file("./1-data/3-interim/kc-tax-reason.rds")

tr_dr_id <- as_id("0B5Pp4V6eCkhrQzdjY0hKVUhOR3M")

tax_reason <- tr_fp %>% 
  make_or_read2(dr_id = tr_dr_id,
                get_expr = {
                  # source: 
                },
                make_expr = {
                  drive_read(dr_id = dr_id,
                            .tempfile = FALSE,
                            path = tr_fp,
                            read_fun = read_rds)},
                read_expr =  {read_rds(tr_fp)})

ts_fp <- "./1-data/3-interim/kc-tax-status.rds"

# Tax Status

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