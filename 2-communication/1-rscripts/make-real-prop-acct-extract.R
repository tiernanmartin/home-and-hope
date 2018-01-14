# Setup ----

library(tidyverse) 
library(sf)
library(googledrive)
library(miscgis) 
library(miscgis)
library(snakecase)
library(magrittr)
library(rprojroot) 
library(RSocrata) 


root <- rprojroot::is_rstudio_project
root_file <- root$make_fix_file()
options(httr_oob_default=TRUE) 
htmltools::tagList(rmarkdown::html_dependency_font_awesome())

# Load data ----

# KC Real prop extract

realprop <- read.socrata("https://data.kingcounty.gov/resource/mmfz-e8xr.csv",
                                   email = "tiernan@futurewise.org",
                                   password = "FAKE_PASSWORD" # CHANGE TO REAL BEFORE RUNNING
                         )

r <- realprop %>% 
  rename_all(to_screaming_snake_case) 

r_fp <- root_file("./1-data/2-external/kc_real_prop_acct_extract.rds")

drive_folder_id <- as_id("0B5Pp4V6eCkhrdlJ3MXVaNW16T0U")

write_rds(r,r_fp, compress = "gz")

drive_upload(media = r_fp, path = drive_folder_id)

# drive_update(as_id("19f4AUMAEshnDNJqGjVsurFthbKGcKYvh"), r_fp)
