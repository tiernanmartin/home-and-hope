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

options(httr_oob_default=TRUE) 

# Access Data ----

# New function: maybe_make

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

