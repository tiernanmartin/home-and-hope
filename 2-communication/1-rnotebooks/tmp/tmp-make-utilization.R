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

# Apartment Buildings

apt_fp <- "./1-data/2-external/EXTR_AptComplex.csv"


apt_load <- apt_fp %>% 
  make_or_read({
    zip_dir <- "./1-data/2-external"
    
    target_name <- "EXTR_AptComplex.csv"
    
    dr_id <- as_id("11kkudStD4TuoiRqMie-Y4_8tZQJmLPBw")
    
    drive_read_zip(
      dr_id = dr_id,
      .tempdir = FALSE,
      dir_path = zip_dir,
      read_fun = read_csv,
      target_name = target_name
    ) 
  },
  {
    read_csv(apt_fp)
  }) 


apt <- apt_load %>% 
  rename_all(to_screaming_snake_case) %>% 
  mutate(PIN = str_c(MAJOR,MINOR,sep = ""))

rm(apt_load)
gc(verbose = FALSE)


# Commercial Buildings

comm_fp <- "./1-data/2-external/EXTR_CommBldg.csv"


comm_load <- comm_fp %>% 
  make_or_read({
    zip_dir <- "./1-data/2-external"
    
    target_name <- "EXTR_CommBldg.csv"
    
    dr_id <- as_id("1VT_plwHQve51ldIg3chFUpcTSMD_ZyrN")
    
    drive_read_zip(
      dr_id = dr_id,
      .tempdir = FALSE,
      dir_path = zip_dir,
      read_fun = read_csv,
      target_name = target_name
    ) 
  },
  {
    read_csv(comm_fp)
  }) 


comm <- comm_load %>% 
  rename_all(to_screaming_snake_case) %>% 
  mutate(PIN = str_c(MAJOR,MINOR,sep = ""))

rm(comm_load)
gc(verbose = FALSE)

# Condos

condo_fp <- "./1-data/2-external/EXTR_CondoComplex.csv"


condo_load <- condo_fp %>% 
  make_or_read({
    zip_dir <- "./1-data/2-external"
    
    target_name <- "EXTR_CondoComplex.csv"
    
    dr_id <- as_id("1avYhRKzHijnc-YZQDGICqWrQNhQoTwnB")
    
    drive_read_zip(
      dr_id = dr_id,
      .tempdir = FALSE,
      dir_path = zip_dir,
      read_fun = read_csv,
      target_name = target_name
    ) 
  },
  {
    read_csv(condo_fp)
  }) 


condo <- condo_load %>% 
  rename_all(to_screaming_snake_case) 

rm(condo_load)
gc(verbose = FALSE)


# Residential

res_fp <- "./1-data/2-external/EXTR_ResBldg.csv"


res_load <- res_fp %>% 
  make_or_read({
    zip_dir <- "./1-data/2-external"
    
    target_name <- "EXTR_ResBldg.csv"
    
    dr_id <- as_id("10rz6hc4lEAaaU-0Jcv0iCiVMFBVTc-en")
    
    drive_read_zip(
      dr_id = dr_id,
      .tempdir = FALSE,
      dir_path = zip_dir,
      read_fun = read_csv,
      target_name = target_name
    ) 
  },
  {
    read_csv(res_fp)
  }) 


res <- res_load %>% 
  rename_all(to_screaming_snake_case) %>% 
  mutate(PIN = str_c(MAJOR,MINOR,sep = ""))

rm(res_load)
gc(verbose = FALSE)

# Create: bldg ----

bldg_empty <- tibble(PIN = as.character(""), 
                     BLDG_NBR = as.integer(""),
                     BLDG_NET_SQ_FT= as.integer(""),
                     NBR_LIVING_UNITS = as.integer(""),
                     NBR_BLDGS = as.integer(""),
                     BLDG_CAT = as.character("")) %>% 
  slice(0)

comm_join <- comm %>% 
  transmute(PIN,
            BLDG_NBR,
            BLDG_NET_SQ_FT,
            NBR_BLDGS,
            BLDG_CAT = "commercial")

res_join <- res %>% 
  transmute(PIN,
            BLDG_NBR,
            NBR_LIVING_UNITS,
            BLDG_CAT = "residential")

apt_join <- apt %>% 
  transmute(PIN,
            NBR_BLDGS,
            NBR_LIVING_UNITS = NBR_UNITS,
            BLDG_CAT = "apartment")

condo_join <- condo %>% 
  transmute(PIN = MAJOR,
            NBR_LIVING_UNITS = NBR_UNITS,
            BLDG_CAT = "condo")


bldg_all <- bldg_empty %>% 
  full_join(comm_join) %>% 
  full_join(res_join) %>% 
  full_join(apt_join) %>% 
  full_join(condo_join) 


bldg_all_sum <- bldg_all %>% 
  mutate(NBR_BLDGS = if_else(BLDG_CAT %in% c("residential", "condo"),as.integer(1),NBR_BLDGS)) %>% 
  mutate(CAT_LGL = TRUE,
         COL_NAME = str_c("TYPE",toupper(BLDG_CAT),"LGL", sep = "_")) %>% 
  spread(COL_NAME, CAT_LGL) %>% 
  mutate_at(vars(TYPE_APARTMENT_LGL:TYPE_RESIDENTIAL_LGL), ~ if_else(is.na(.),FALSE,.)) %>% 
  group_by(PIN) %>% 
  summarise(NBR_BLDGS = as.integer(max(NBR_BLDGS)),  # note: sum() may be a better approach
            BLDG_NET_SQ_FT = sum(BLDG_NET_SQ_FT, na.rm = TRUE),
            NBR_LIVING_UNITS = sum(NBR_LIVING_UNITS, na.rm = TRUE),
            TYPE_APARTMENT_LGL = any(TYPE_APARTMENT_LGL),
            TYPE_COMMERCIAL_LGL = any(TYPE_COMMERCIAL_LGL),
            TYPE_CONDO_LGL = any(TYPE_CONDO_LGL),
            TYPE_RESIDENTIAL_LGL = any(TYPE_RESIDENTIAL_LGL))

bldg <- bldg_all_sum
  
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

p_id <- as_id("1jrEAX7ogq1RdNU-hfrC-tntF66b6NcKg")

p <- drive_read(p_id, TRUE, read_fun = read_sf, stringsAsFactors = FALSE) 

p %<>% 
  st_set_crs(2926) %>% 
  st_transform(4326)

url <- "http://blue.kingcounty.com/Assessor/eRealProperty/Dashboard.aspx?ParcelNbr="

p_nest <- p %>% 
  st_nest_sf() %>% 
  mutate(PROP_NAME = map_chr(data, "PROP_NAME"),
         PIN = map_chr(data, "PIN"),
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
  mutate(DEVELOPABLE_SF = 1 - WATER_OVERLAP_PCT) %>%   # this should probably be done in the parcel make script
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
                                      as.integer(round(LOT_COVERAGE*(DEVELOPABLE_SF * SQ_FT_LOT * N_STORIES))),
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
