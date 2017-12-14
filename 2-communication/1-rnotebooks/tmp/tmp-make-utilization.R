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


# Clean, Join, Filter ----




# Write + Upload Data ----
