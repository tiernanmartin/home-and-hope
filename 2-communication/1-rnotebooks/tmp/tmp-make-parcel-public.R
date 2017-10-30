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

root <- rprojroot::is_rstudio_project
root_file <- root$make_fix_file()
options(httr_oob_default=TRUE) 
htmltools::tagList(rmarkdown::html_dependency_font_awesome())

# Access Data ----

pub_fp <- root_file("./1-data/2-external/public_parcels_shp_20170727")

pub_load <- 
  make_or_read(pub_fp,
               {
                 dr_id <- as_id("0B5Pp4V6eCkhrZ2tfS3d6YzZ0aXM")
                 
                 zip_dir <- root_file("./1-data/2-external")
                 
                 target_name <- "public_parcels_shp_20170727"
                 
                 drive_read_zip(dr_id = dr_id,
                                .tempdir = FALSE,
                                dir_path = zip_dir,
                                read_fun = st_read,
                                target_name = target_name,
                                layer = "public_parcels", 
                                stringsAsFactors = FALSE)
                 
               },
               {st_read(pub_fp, layer = "public_parcels", stringsAsFactors = FALSE)})

pub <-
  pub_load %>% 
  st_drop_geometry() %>% 
  rename_all(to_screaming_snake_case)

rm(pub_load)
gc(verbose = FALSE)

# Clean Data ----

pub_ready <- 
  pub %>% 
  select(PIN,
         TYPE,
         PRESENTUSE,
         LOT_SQFT = LOTSQFT,
         APPR_LND_VAL = APPRLNDVAL,
         APPR_IMPR_VAL = APPR_IMPR,
         KCTP_NAME,
         JURIS,
         ADDR_FULL,
         ZIP_5,
         POSTAL_CTY_NAME = POSTALCTYN,
         LAT,
         LON) %>% 
  mutate(DATE = "20170727")

# Upload the data to Drive ----

tmp <- tempfile()

write_rds(pub_ready, tmp)

drive_folder <- as_id("0B5Pp4V6eCkhrZ3NHOEE0Sl9FbWc")

drive_upload(tmp, path = drive_folder,name = "public_parcels_20170727.rds")
