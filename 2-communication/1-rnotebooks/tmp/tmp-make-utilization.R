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

# Access Data: bldg ----

# See: tmp-make-bldg.R

bldg_fp <- "./1-data/3-interim/bldg.csv"

bldg <- bldg_fp %>% 
  make_or_read({
    dr_id <- as_id("1qsfN1k8UyJOZHFYD_5HSw7nyeS_lzn2_")
    
    drive_read(dr_id,.tempfile = FALSE,path = bldg_fp,read_fun = read_csv)
  },
               {read_csv(bldg_fp)})
  
# Create: p_KCTP_NAME NOTE: this should be taken care of in a previous script ----

r_fp <- root_file("./1-data/2-external/kc_real_prop_acct_extract.rds")

r_load <- r_fp %>% 
  make_or_read({NULL}, #fill this in later
               {read_rds(r_fp)})

r <- r_load %>% 
  transmute(PIN = str_c(MAJOR,MINOR, sep = ""),
            KCTP_NAME = TAXPAYERNAME) %>% 
  filter(!duplicated(PIN))



# Create: p_util ----

# parcel filtered by: uga, zoning, waterbody overlap
# parcel with: DEVELOPABLE_PCT

p_id <- as_id("1O3QRkE7SmW3AxlWSdecARBW7MQ1TmHnH")

p <- drive_read(p_id, TRUE, read_fun = read_sf, stringsAsFactors = FALSE) 

p %<>% 
  st_set_crs(2926) %>% 
  st_transform(4326)

url <- "http://blue.kingcounty.com/Assessor/eRealProperty/Dashboard.aspx?ParcelNbr="

p_nest <- p %>% 
  st_nest_sf() %>% 
  mutate(PROP_NAME = map_chr(data, "PROP_NAME"),
         PIN = map_chr(data, "PIN"),
         DEVELOPABLE_PCT = map_dbl(data, "DEVELOPABLE_PCT"),
         WATER_OVERLAP_PCT = map_dbl(data, "WATER_OVERLAP_PCT"),
         WATER_OVERLAP_LGL = map_lgl(data, ~pull(.x, "WATER_OVERLAP_LGL") %>% as.logical),
         CURRENT_ZONING = map_chr(data, "CURRENT_ZONING"),
         CAT = map_chr(data, "CAT"),
         PRESENT_USE = map_chr(data, "PRESENT_USE"),
         SQ_FT_LOT = map_int(data, "SQ_FT_LOT"), 
         DEV_ASSUMPTION = map_chr(data, "DEV_ASSUMPTION"),
         ACCESS = map_int(data,"ACCESS"),
         TOPOGRAPHY = map_int(data, "TOPOGRAPHY"),
         RESTRICTIVE_SZ_SHAPE = map_int(data,"RESTRICTIVE_SZ_SHAPE"),
         PCNT_UNUSABLE = map_int(data, "PCNT_UNUSABLE"),
         CONTAMINATION = map_int(data, "CONTAMINATION"),
         STEEP_SLOPE_HAZARD = map_chr(data, "STEEP_SLOPE_HAZARD")
         )

city_block_sqft <- as.integer(66000)

# the lot size types and the parameters that determine them
# note: in order for this object to be successfully joined to the parcel
#       object, the column names must be identical

lot_size_types <- 
  crossing(LOT_SIZE = c("less than 1/8 block", "1/4 block", "greater than 1/4 block"),
           RESTRICTIVE_SZ_SHAPE = unique(p_nest$RESTRICTIVE_SZ_SHAPE),
           DEV_ASSUMPTION = unique(p_nest$DEV_ASSUMPTION)
  ) %>% 
  arrange(RESTRICTIVE_SZ_SHAPE) %>% 
  mutate(LOT_SIZE_TYPE = case_when(is.na(DEV_ASSUMPTION) ~ NA_character_,
                                   str_detect(DEV_ASSUMPTION,"^no") ~ "not developable",
                                   RESTRICTIVE_SZ_SHAPE == as.integer(1) ~ "restrictive shape",
                                   str_detect(DEV_ASSUMPTION,"^one") ~ "single family",
                                   str_detect(LOT_SIZE,"less") ~ "small",
                                   str_detect(LOT_SIZE,"^1") ~ "medium",
                                   TRUE ~ "large"))

# Parameters of the development assumptions
# note: in order for this object to be successfully joined to the parcel
#       object (with lot size types), the column names must be identical

dev_params <- tibble::tribble(
        ~LOT_SIZE_TYPE, ~LOT_COVERAGE, ~N_STORIES,
                    NA,            NA,         NA,
     "not developable",            NA,         NA,
   "restrictive shape",            NA,         NA,
       "single family",          0.75,          2,
               "small",          0.75,          3,
              "medium",          0.75,          7,
               "large",           0.5,          7
  ) 

p_join <- p_nest %>% 
  left_join(bldg, by = "PIN") %>% 
  left_join(r, by = "PIN") %>% 
  select(-data,everything(),data)

p_util <- p_join %>% 
  mutate(LOT_SIZE = case_when(
    SQ_FT_LOT < city_block_sqft/8 ~  "less than 1/8 block",
    between(SQ_FT_LOT,city_block_sqft/8,city_block_sqft/4) ~ "1/4 block",
    SQ_FT_LOT > city_block_sqft/4 ~  "greater than 1/4 block",
    TRUE ~ NA_character_)
  ) %>%  
  left_join(lot_size_types) %>% 
  left_join(dev_params) %>% 
  mutate(
    DEVELOPABLE_LGL = if_else(LOT_SIZE_TYPE %in% c("single family","small","medium","large"),
                              TRUE,
                              FALSE,
                              missing = FALSE)
         ) %>% 
  mutate(
    MAX_UTILIZATION_SF = if_else(DEVELOPABLE_LGL,
                                      as.integer(round(LOT_COVERAGE*(DEVELOPABLE_PCT * SQ_FT_LOT * N_STORIES))),
                                      NA_integer_)) %>% 
  mutate(
    UNDER_UTILIZED_LGL = case_when(
      BLDG_NET_SQ_FT >= MAX_UTILIZATION_SF ~ FALSE,
      BLDG_NET_SQ_FT < MAX_UTILIZATION_SF ~ TRUE,
      TRUE ~ NA)
  ) %>% 
  select(PROP_NAME,
         KCTP_NAME,
         PIN,
         UNDER_UTILIZED_LGL,
         BLDG_NET_SQ_FT,
         NBR_BLDGS,
         MAX_UTILIZATION_SF,
         LOT_SIZE,
         LOT_SIZE_TYPE,
         everything())

# Write + Upload Data ----

p_util_fp <- "./1-data/3-interim/p_utilization.gpkg"

p_util_ready <- p_util %>% 
  unnest() %>% 
  select(-matches("1$")) %>%   # drop duplicate columns
  mutate_if(is_logical,as.character)

drive_folder <- as_id("0B5Pp4V6eCkhrZ3NHOEE0Sl9FbWc")

st_write(p_util_ready,dsn = p_util_fp, driver = "GPKG",layer_options = c("OVERWRITE=YES"))

drive_upload(p_util_fp, path = drive_folder, name = "p_utilization.gpkg")

drive_update(as_id("15B9sduv0IBIA2giXnGR8sVigus73qvcc"), p_util_fp)
