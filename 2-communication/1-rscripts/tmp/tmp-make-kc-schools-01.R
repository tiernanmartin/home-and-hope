# Setup ----

library(tidyverse)
library(sf)
library(googledrive)
library(miscgis)

options(httr_oob_default=TRUE) 

# Access Data ----

d <- st_read("https://opendata.arcgis.com/datasets/365b0f50792c4bf6b06fc134d0494231_107.geojson")

# Clean Data ----


# Upload the data to Drive ----

folder <- drive_get("~/Futurewise/Enterprise/1-data/2-external")

drive_upload_obj(x = d,
                 filename = "kc-schools.rds",
                 write_fun = write_rds, 
                 folder = folder)