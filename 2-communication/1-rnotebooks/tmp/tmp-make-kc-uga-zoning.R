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
library(snakecase)
library(magrittr)
library(rprojroot)
library(knitr) 
library(lubridate)
library(RColorBrewer)

root <- rprojroot::is_rstudio_project
root_file <- root$make_fix_file()
options(httr_oob_default=TRUE) 
htmltools::tagList(rmarkdown::html_dependency_font_awesome())

# Access Data ----

# URBAN GROWTH AREA

uga_fp <- "./1-data/2-external/uga"

uga_sf <- 
  make_or_read(uga_fp,
               {
                 
                 dr_id <- as_id("0B5Pp4V6eCkhrZGQ0Q0h5aHNUVW8")
                 
                 zip_dir <- root_file("./1-data/2-external")
                 
                 target_name <- "uga"
                 
                 drive_read_zip(dr_id = dr_id,
                                .tempdir = FALSE,
                                dir_path = zip_dir,
                                read_fun = st_read,
                                target_name = target_name,
                                layer = "uga", 
                                stringsAsFactors = FALSE)
                 
               },
               {
                 st_read(uga_fp, layer = "uga", stringsAsFactors = FALSE)
               })

# ZONING

zng_fp <- "./1-data/2-external/zoning_kc_consol_20"

zng_load <- 
  make_or_read(zng_fp,
               {
                 
                 dr_id <- as_id("0B5Pp4V6eCkhrOTUwT29WQl9STVk")
                 
                 zip_dir <- root_file("./1-data/2-external")
                 
                 target_name <- "zoning_kc_consol_20"
                 
                 drive_read_zip(dr_id = dr_id,
                                .tempdir = FALSE,
                                dir_path = zip_dir,
                                read_fun = st_read,
                                target_name = target_name,
                                layer = "zoning_kc_consol_20", 
                                stringsAsFactors = FALSE)
                 
               },
               {
                 st_read(zng_fp, layer = "zoning_kc_consol_20", stringsAsFactors = FALSE)
               })


# Clean, Join, Filter ----

zoning_sf <- 
  zng_load %>% 
  st_transform(4326) %>% 
  select(CAT = CONSOL20) %>% 
  filter(!duplicated(CAT)) %>%  
  mutate(
    CAT_FCT = factor(CAT),
    POT_SUITABLE_LGL = case_when(
      CAT %in% c("Central Business District", 
                 "General Commercial", 
                 "General Mixed Use", 
                 "Historic District", 
                 "Mixed Use Commercial/Residential", 
                 "Mobile Home Park", 
                 "Multi-Family Residential",
                 "Public Use/Institutional",
                 "Single-Family Residential") ~ TRUE,
      TRUE ~ FALSE
    ),
    DEV_ASSUMPTION = case_when(
      !POT_SUITABLE_LGL ~ "none",
      CAT %in% c("General Mixed Use",
                 "Mixed Use Commercial/Residential") ~ "six story mixed use",
      CAT %in% "Multi-Family Residential" ~ "six story affordable",
      CAT %in% "Central Business District" ~ "high rise construction, mixed-income, ~150 affordable apartments", 
      CAT %in% "Single-Family Residential" ~ "one unit per 5000 SF of lot size",
      TRUE ~ "no assumption for this class"
    ),
    DEV_ASSUMPTION_FCT = factor(case_when(
      DEV_ASSUMPTION %in% "six story affordable" ~ "6 story res",
      DEV_ASSUMPTION %in% "six story mixed use" ~ "6 story mixed",
      str_detect(DEV_ASSUMPTION, "high\\srise") ~ "150 units in mixed income highrise",
      str_detect(DEV_ASSUMPTION, "5000") ~ "1 unit/5000sf",
      str_detect(DEV_ASSUMPTION, "assumption") ~ "no assumption",
      TRUE ~ "none"
    ),
    levels = c("6 story res", "6 story mixed", "150 units in mixed income highrise","1 unit/5000sf","no assumption","none"), ordered = TRUE)
  ) %>% 
  arrange(DEV_ASSUMPTION_FCT)



# Check out the map ----

pal <- 
  zoning_sf %>% 
  st_drop_geometry() %>% 
  filter(POT_SUITABLE_LGL) %>% 
  transmute(CAT_FCT = factor(CAT)) %>% 
  pull %>% 
  {colorFactor("Set1", domain = .)}
  

# zoning_sf %>%  
#   filter(POT_SUITABLE_LGL) %>% 
#   mutate(CAT_FCT = factor(CAT)) %>%  
#   leaflet() %>% 
#   addTiles() %>% 
#   addPolygons(fillColor = ~pal(CAT_FCT)) 
  



# Write + Upload Data ----

zoning_sf_fp <- "./1-data/3-interim/zoning_sf.gpkg"

st_write(zoning_sf,dsn = zoning_sf_fp, driver = "GPKG",layer_options = c("OVERWRITE=YES"))

drive_folder <- as_id("0B5Pp4V6eCkhrZ3NHOEE0Sl9FbWc")

drive_upload(zoning_sf_fp, path = drive_folder, name = "zoning-sf.gpkg")

# drive_update(as_id("0B5Pp4V6eCkhrby11Qzd2NEV5Wnc"), zoning_sf_fp)
