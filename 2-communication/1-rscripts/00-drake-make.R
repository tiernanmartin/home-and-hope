# SETUP ----
library(drake)
library(tigris)
library(sf)
library(lwgeom)
library(googledrive)
library(miscgis)  
library(snakecase)
library(magrittr)
library(rprojroot) 
library(RSocrata)
library(glue)
library(fuzzyjoin)
library(tidyverse) 


root <- rprojroot::is_rstudio_project
root_file <- root$make_fix_file()
options(httr_oob_default=TRUE,
        tigris_class = "sf") 
htmltools::tagList(rmarkdown::html_dependency_font_awesome())

# MAKE PLANS ----

lookup_plan <- drake_plan( 
  lu = make_lu(),
  prop_type = make_prop_type(),
  tax_status = make_tax_status(),
  tax_reason = make_tax_reason()
)

parcel_plan <- drake_plan(
  pub_parcel = make_pub_parcel(),
  acct = make_acct(),
  parcel_df = make_parcel_df(),
  parcel_sf_poly = make_parcel_sf_poly(),
  parcel_sf = make_parcel_sf(parcel_sf_poly),
  parcel_ready = make_parcel_ready(lu, prop_type, tax_status, tax_reason, pub_parcel, acct, parcel_df, parcel_sf)
)

miscellaneous_plan <- drake_plan(
  waterbodies = make_waterbodies(),
  uga = make_uga(),
  zoning = make_zoning(),
  census_tracts = make_census_tracts()
)

development_assumptions_plan <- drake_plan(
  city_block_sqft = make_city_block_sqft(),
  lot_types = make_lot_types(city_block_sqft),
  development_assumptions_zoning = make_development_assumptions_zoning(),
  development_assumptions_lot = make_development_assumptions_lot(lot_types, development_assumptions_zoning)
)

suitability_criteria_plan <- drake_plan(
  criteria_tax_exempt = make_criteria_tax_exempt(),
  criteria_max_water_overlap_pct = make_criteria_max_water_overlap_pct(),
  criteria_within_uga = make_criteria_within_uga(),
  criteria_developable_zoning = make_criteria_developable_zoning(development_assumptions_zoning),
  criteria_undevelopable_present_use = make_criteria_undevelopable_present_use(),
  suitability_criteria = make_suitability_criteria(criteria_tax_exempt, criteria_max_water_overlap_pct, criteria_within_uga, criteria_developable_zoning, criteria_undevelopable_present_use)
)

suitability_plan <- drake_plan(
  suitability_tax_exempt = make_suitability_tax_exempt(parcel_ready),
  suitability_water_overlap = make_suitability_water_overlap(parcel_ready, waterbodies),
  suitability_within_uga = make_suitability_within_uga(parcel_ready, uga),
  suitability_developable_zoning = make_suitability_developable_zoning(parcel_ready, zoning),
  suitability_present_use = make_suitability_present_use(parcel_ready),
  suitability = make_suitability(parcel_ready, suitability_criteria, suitability_tax_exempt, suitability_water_overlap, suitability_within_uga, suitability_developable_zoning, suitability_present_use)
)

building_plan <- drake_plan(
  building_residential = make_building_residential(),
  building_apartment = make_building_apartment(),
  building_condo = make_building_condo(),
  building_commercial = make_building_commercial(),
  building = make_building(building_residential, building_apartment, building_condo, building_commercial)
)

utilization_criteria_plan <-  drake_plan(
  criteria_lot_size = make_criteria_lot_size(city_block_sqft, lot_types)
)

utilization_plan <- drake_plan(
  utilization_present = make_utilization_present(parcel_ready, building),
  utilization_lot_size = make_utilization_lot_size(parcel_ready, criteria_lot_size),
  utilization_potential = make_utilization_potential(suitability, development_assumptions_lot, utilization_lot_size),
  utilization = make_utilization(suitability, utilization_present, utilization_potential)
)

filter_plan <- drake_plan(
  filters_census_tract = make_filters_census_tract(parcel_ready, census_tracts),
  filters = make_filters(parcel_ready, filters_census_tract)
)

inventory_plan <- drake_plan(
  inventory = make_inventory(filters, suitability,  utilization),
  inventory_suitable = make_inventory_suitable(inventory)
)

project_plan <- rbind(
  lookup_plan,
  parcel_plan,
  miscellaneous_plan,
  development_assumptions_plan,
  suitability_criteria_plan,
  suitability_plan,
  building_plan,
  utilization_criteria_plan,
  utilization_plan,
  filter_plan,
  inventory_plan)   


# FUNCTION: WITHIN_RANGE ----
`%within_range%` <- function(x,range){ 
  between(x,min(range),max(range))
          
          } 

# FUNCTION: LESSER_OF ----
lesser_of <- function(x,y){
  x <- as.double(x)
  y <- as.double(y)
  
  if_else(x<= y, x,y, missing = x)
  
  } 

# FUNCTION: MAKE_PARSE_LU_STRING ----
parse_lu_string <- function(string, col_sep, row_sep, join_name, long_name){ 
  str_split(string, pattern = row_sep) %>% 
    flatten() %>% 
    keep(~ str_detect(.x,"")) %>%  
    str_replace_all("\\\n","") %>% 
    map_chr(c) %>% 
    map_df(~.x %>% str_split(pattern = col_sep) %>% as.data.frame %>% t %>% as_data_frame ) %>%  
    set_names(c(join_name,long_name))
}


# FUNCTION: ST_INTERSECTS_ANY ----
st_intersects_any <- function(x, y) {
  sapply(st_intersects(x, y), function(z) length(z)> 0)
}
# FUNCTION: ST_INTERSECT_AREA ----
st_intersect_area <- function(x, y){ 
  
  x_sfc <- x %>% 
    st_geometry %>% 
    st_transform(st_crs(y)) 
  
  area_x <- x_sfc %>% st_area() %>% as.double()
  
  area_xy <- st_intersection(x_sfc, y) %>% st_area %>% as.double()
  
  if(is_empty(area_xy)){return(as.double(0))}
  
  overlap_pct <- area_xy %>% 
    magrittr::divide_by(area_x) %>% 
    as.double() %>% 
    round(2)
  
  return(overlap_pct)
}

# FUNCTION: ST_OVER ----

st_over <- function(x,y,col){
  idx <- sapply(st_intersects(x,y), function(z) if (length(z)==0) NA_integer_ else z[1])
  
  y[idx,col][[1]]
}

# COMMAND: MAKE_LU ----

make_lu <- function(){
  
  lu_fp <- root_file("1-data/2-external/EXTR_LookUp.csv")
  
  lu_dr_id <- as_id("1-L42pHb7lySqonanSwBbXSH9OZrKHp2A")
  
  lu_load <- 
    make_or_read2(fp = lu_fp,
                  dr_id = lu_dr_id,
                  skip_get_expr = TRUE,
                  get_expr = function(fp){# Source: http://aqua.kingcounty.gov/extranet/assessor/Lookup.zip
                    },
                  make_expr = function(fp, dr_id){
                    drive_read(dr_id = dr_id,.tempfile = FALSE,path = fp, read_fun = read_csv)
                  },
                  read_expr = function(fp){read_csv(fp)})
  
  lu <- rename_all(lu_load, to_screaming_snake_case)
}

# COMMAND: MAKE_TAX_STATUS ----

make_tax_status <- function(){
  
  tax_s_fp <- root_file("1-data/1-raw/tax_status.txt")
  
  tax_s_dr_id <- as_id("1xY6l2FRF2a-6Ugk2_qzPo3aA7a35hw8a")
  
  tax_s_load <- 
    make_or_read2(fp = tax_s_fp,
                  dr_id = tax_s_dr_id,
                  skip_get_expr = FALSE,
                  get_expr = function(fp){
                    
                    string <- "T = Taxable; X = Exempt; O = Operating"
                    
                    writeLines(string, fp)
                    
                    drive_folder <- as_id("0B5Pp4V6eCkhrb1lDdlNaOFY4V0U")
                    
                    drive_upload(media = fp, path = drive_folder)
                    
                  },
                  make_expr = function(fp, dr_id){  
                    drive_download(file = dr_id, path = fp) 
                    read_lines(fp)
                  },
                  read_expr = function(fp){read_lines(fp)})
  
  tax_status <- 
    tax_s_load %>% 
    str_c(collapse = "\n") %>% 
    parse_lu_string(col_sep = "\\s=\\s",row_sep = ";\\s",join_name = "TAX_STATUS","TAX_STATUS_DESC")
  
}

# COMMAND: MAKE_TAX_REASON ----

make_tax_reason <- function(){
   
  tax_r_fp <- root_file("1-data/1-raw/tax_reason.txt")
  
  tax_r_dr_id <- as_id("1S9YyHFwTDYrMnM0WGZEHiFrgEUpAfqWL")
  
  tax_reason_load <-  
    make_or_read2(fp = tax_r_fp,
                  dr_id = tax_r_dr_id,
                  skip_get_expr = FALSE,
                  get_expr = function(fp){
                    
                    string <- "
FS = senior citizen exemption
EX = exempt
OP = operating
NP = non profit exemption
CU = open space exemption
HI = home improvement exemption
HP = historic property exemption
MX = more than one reason applies
"
                    writeLines(string, fp)
                    
                    drive_folder <- as_id("0B5Pp4V6eCkhrb1lDdlNaOFY4V0U")
                    
                    drive_upload(media = fp, path = drive_folder) 
                  },
                  make_expr = function(fp, dr_id){  
                    drive_download(file = dr_id, path = fp) 
                    read_lines(tax_r_fp)
                  },
                  read_expr = function(fp){read_lines(fp)})
  
  tax_reason <- 
    tax_reason_load %>% 
    str_c(collapse = "\n") %>% 
    parse_lu_string(col_sep = "\\s=\\s", row_sep = "\n",join_name = "TAX_REASON","TAX_REASON_DESC")
} 


# COMMAND: MAKE_PROP_TYPE ----

make_prop_type <- function(){
  
  pt_fp <- root_file("1-data/1-raw/prop_type.txt")
  
  pt_dr_id <- as_id("1nJp_t4hvf1sy9flwmKLnWKnZvdLQs2XN")
  
  pt_load <- 
    make_or_read2(fp = pt_fp,
                  dr_id = pt_dr_id,
                  skip_get_expr = FALSE,
                  get_expr = function(fp){
                    
                    string <- "
C = Commercial
K = Condominium
M = Coal & Mineral Rights
N = Mining
R = Residential
T = Timber
U = Undivided Interest
X = Exempt
"
                    writeLines(string, fp)
                    
                    drive_folder <- as_id("0B5Pp4V6eCkhrb1lDdlNaOFY4V0U")
                    
                    drive_upload(media = fp, path = drive_folder) 
                    
                  },
                  make_expr = function(fp, dr_id){ 
                    drive_download(file = dr_id, path = fp) 
                    read_lines(fp)
                  },
                  read_expr = function(fp){read_lines(fp)})
  
  prop_type <- pt_load %>% 
    str_c(collapse = "\n") %>% 
    parse_lu_string(col_sep = "\\s=\\s", row_sep = "\n",join_name = "PROP_TYPE","PROP_TYPE_DESC")
  
}


# COMMAND: MAKE_PUB_PARCEL ----

make_pub_parcel <- function(){ 
  pub_fp <-  root_file("1-data/2-external/kc-public-parcels.csv")
  
  pub_dr_id <- as_id("1ERHvVa9K6F-lk1L8X47Oy1sdw_s2t9kv")
  
  pub_load <- 
    make_or_read2(fp = pub_fp,
                  dr_id = pub_dr_id,
                  skip_get_expr = TRUE,
                  get_expr = function(fp){
                     soda_api_endpoint <- "https://data.kingcounty.gov/resource/pdft-6nx2.json"

                    pub_load <- RSocrata::read.socrata(url = soda_api_endpoint,email = "FAKE@FAKE_EMAIL.COM",password = "FAKE_PASSWORD") # CHANGE THIS TO RE-DOWNLOAD

                    pub <- pub_load %>%
                      as_tibble %>%
                      select(major,
                             minor,
                             pin,
                             TaxpayerName = taxpayer_na,
                             districtName = district_na,
                             PropName = prop_name,
                             LandGeneralUse = land_genera,
                             LandCurrentZoning = land_curren,
                             LandPresentUse = land_presen,
                             sq_ft_lot,
                             land_issues,
                             eRealPropertyLink = e_real_prope,
                             bus_buff_750,
                             bus_buf_qtr_m
                      ) %>%
                      rename_all(to_screaming_snake_case)

                    write_csv(pub, fp)

                    drive_folder <- as_id("0B5Pp4V6eCkhrdlJ3MXVaNW16T0U")

                    drive_upload(fp, drive_folder)
                    },
                  make_expr = function(fp, dr_id){ 
                    drive_read(dr_id = dr_id,.tempfile = FALSE,path = fp,read_fun = read_csv)
                  },
                  read_expr = function(fp){read_csv(fp)})
  
  pub_parcel <- rename_all(pub_load, to_screaming_snake_case) 
  
  rm(pub_load)
  gc(verbose = FALSE)
  
  return(pub_parcel)
}

# COMMAND: MAKE_ACCT ----

make_acct <- function(){ 
  
  acct_fp <- root_file("./1-data/2-external/kc_real_prop_acct_extract.rds")
  
  acct_dr_id <- as_id("19f4AUMAEshnDNJqGjVsurFthbKGcKYvh")
  
  acct <- 
    make_or_read2(fp = acct_fp,
                  dr_id = acct_dr_id,
                  skip_get_expr = TRUE,
                  get_expr = function(fp){
                    realprop <- read.socrata("https://data.kingcounty.gov/resource/mmfz-e8xr.csv",
                                             email = "FAKE_NAME@FAKE_EMAIL.COM",
                                             password = "FAKE_PASSWORD" # CHANGE TO REAL BEFORE RUNNING
                    )
                    
                    r <- realprop %>% 
                      rename_all(to_screaming_snake_case)  
                    
                    drive_folder_id <- as_id("0B5Pp4V6eCkhrdlJ3MXVaNW16T0U")
                    
                    write_rds(r, fp, compress = "gz")
                    
                    drive_upload(media = fp, path = drive_folder_id)
                    
                  },
                  make_expr = function(fp, dr_id){
                    drive_read(dr_id = dr_id,path = fp,.tempfile = FALSE,read_fun = read_rds)
                  },
                  read_expr = function(fp){read_rds(fp)})
  
}


# COMMAND: MAKE_PARCEL_DF ----
make_parcel_df <- function(){
  
  p_fp <- root_file("./1-data/2-external/EXTR_Parcel_20171013.csv")
  
  p_dr_id <- as_id("0B5Pp4V6eCkhraF9jOTl3bURiMkU")
  
  p_load <-  
    make_or_read2(fp = p_fp,
                  dr_id = p_dr_id,
                  skip_get_expr = TRUE,
                  get_expr = function(fp){ # Source: http://aqua.kingcounty.gov/extranet/assessor/Parcel.zip
                  },
                  make_expr = function(fp, dr_id){
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
                  read_expr = function(fp){ read_csv(p_fp)})
  
  parcel_df <- rename_all(p_load, to_screaming_snake_case) 
  
  rm(p_load)
  gc(verbose = FALSE)
  
  return(parcel_df)
}

# COMMAND: MAKE_PARCEL_SF_POLY ----
 
make_parcel_sf_poly <- function(){
  
  p_sf_poly_fp <- "./1-data/2-external/parcel"
  
  p_sf_poly_dr_id <- as_id("0B5Pp4V6eCkhrRnM4bHFmWTBTQnM")
  
  p_sf_poly_load <- 
    make_or_read2(fp = p_sf_poly_fp, 
                  dr_id = p_sf_poly_dr_id, 
                  skip_get_expr = TRUE,
                  get_expr = function(fp){
                    # SOURCE: ftp://ftp.kingcounty.gov/gis-web/GISData/parcel_SHP.zip
                  },
                  make_expr = function(fp,dr_id){
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
                  read_expr = function(fp){st_read(fp, layer = "parcel", stringsAsFactors = FALSE)}
    )
  
  parcel_sf_poly <-
    p_sf_poly_load %>%
    rename_if(not_sfc, to_screaming_snake_case)
  
  rm(p_sf_poly_load)
  gc(verbose = FALSE)
  
  return(parcel_sf_poly)
}

# COMMAND: MAKE_PARCEL_SF ----
 
make_parcel_sf <- function(parcel_sf_poly){
  
  p_sf_fp <- root_file("1-data/3-interim/kc-parcel-geoms-sf.rds")
  
  p_sf_dr_id <- as_id("1vuc_LDus5BsxRJOX7PjhF62O9ZCezk6v")
  
  p_sf <- 
    make_or_read2(fp = p_sf_fp, 
                  dr_id = p_sf_dr_id, 
                  skip_get_expr = TRUE,
                  get_expr = function(fp){
                    
                    p_sf_2926 <- st_transform(parcel_sf_poly, 2926)
                    
                    p_sf_2926$geom_pt <- st_centroid(st_geometry(p_sf_2926))
                    
                    p_sf_ready <- p_sf_2926 %>% 
                      mutate(MAJOR = str_pad(MAJOR, 6, "left", "0"),
                             MINOR = str_pad(MINOR, 4, "left", "0")) %>% 
                      transmute(PIN = str_c(MAJOR,MINOR),
                                geom_pt) %>% 
                      drop_na() %>% 
                      st_transform(4326) %>% 
                      st_set_geometry("geometry") %>% 
                      st_sf
                    
                    write_rds(p_sf_ready, fp)  # save as rds to keep second geometry
                    
                    zip_fp <- root_file("1-data/3-interim/kc-parcel-geoms-sf.zip")
                    
                    zip_pithy(zip_fp, fp)
                    
                    drive_folder <- as_id("0B5Pp4V6eCkhrZ3NHOEE0Sl9FbWc")
                    
                    drive_upload(media = zip_fp,path = drive_folder)
                    
                    # drive_update(file = as_id("1vuc_LDus5BsxRJOX7PjhF62O9ZCezk6v"),media = zip_fp)
                    
                    
                  },
                  make_expr = function(fp,dr_id){
                    zip_dir <- "./1-data/2-external"
                    
                    target_name <- "kc-parcel-geoms-sf"
                    
                    p_sf <- drive_read_zip(dr_id = dr_id,
                                   dir_path = zip_dir,
                                   read_fun = read_rds,
                                   target_name = target_name,
                                   .tempdir = FALSE,
                                   layer = "parcel",
                                   stringsAsFactors = FALSE) %>% 
                      as_tibble %>% 
                      st_sf()
                    
                  },
                  read_expr = function(fp){read_rds(fp) %>%  as_tibble %>%  st_sf()}
    )
  
  return(p_sf)
}

# COMMAND: MAKE_PARCEL_READY ----

make_parcel_ready <- function(lu, prop_type, tax_status,tax_reason, pub_parcel, acct, parcel_df, parcel_sf){
   
  # MAKE P_SF_READY

  p_sf_ready <-  parcel_sf %>% 
    miscgis::subset_duplicated("PIN") %>% 
    group_by(PIN) %>%  # remove any duplicates PIN records with NAs
    slice(1) %>%  # take the first record and discard the rest
    ungroup %>% 
    rbind(subset_duplicated(parcel_sf,"PIN",notin = TRUE)) %>% 
    st_set_geometry("geometry")
    
    
  # MAKE P_READY
  
  present_use <- lu %>% 
    filter(LU_TYPE == 102) %>% 
    select(PRESENT_USE = LU_ITEM,
           PRESENT_USE_DESC = LU_DESCRIPTION)
  
  pub_parcel_ready <- pub_parcel %>% 
    transmute(PIN,
              ASSESSOR_PUB_LIST_LGL = TRUE)
  
  p_ready <- parcel_df %>% 
   mutate(MAJOR = str_pad(string = MAJOR,width = 6,side = "left",pad = "0"),
           MINOR = str_pad(string = MINOR,width = 4,side = "left",pad = "0"),
           PIN = str_c(MAJOR,MINOR)) %>% 
    left_join(prop_type, by = "PROP_TYPE") %>% 
    left_join(present_use, by = "PRESENT_USE") %>% 
    left_join(pub_parcel_ready, by = "PIN") %>% 
    mutate(ASSESSOR_PUB_LIST_LGL = if_else(is.na(ASSESSOR_PUB_LIST_LGL),FALSE,ASSESSOR_PUB_LIST_LGL)) %>% 
    select(PIN,
           PROP_NAME,
           PROP_TYPE = PROP_TYPE_DESC, 
           ASSESSOR_PUB_LIST_LGL,
           DISTRICT_NAME,
           CURRENT_ZONING,
           PRESENT_USE = PRESENT_USE_DESC,
           SQ_FT_LOT,
           ACCESS,
           TOPOGRAPHY,
           RESTRICTIVE_SZ_SHAPE,
           PCNT_UNUSABLE,
           CONTAMINATION,
           HISTORIC_SITE:OTHER_PROBLEMS 
    )
     
  # MAKE ACCT_READY
  
  acct_frmt <- acct %>% 
    mutate(MAJOR = str_pad(string = MAJOR,width = 6,side = "left",pad = "0"),
           MINOR = str_pad(string = MINOR,width = 4,side = "left",pad = "0"),
           PIN = str_c(MAJOR,MINOR)) %>% 
    rename(TAX_STATUS = TAXSTAT,
           TAX_REASON = TAXVALREASON) %>% 
    left_join(tax_status, by = "TAX_STATUS") %>% 
    left_join(tax_reason, by = "TAX_REASON") %>%  
    select(PIN, 
           TAXPAYER_NAME = TAXPAYERNAME,
           BILL_YR = BILLYR,
           TAX_STATUS = TAX_STATUS_DESC,
           TAX_REASON = TAX_REASON_DESC,
           APPR_IMPS_VAL = APPRIMPSVAL,
           APPR_LAND_VAL = APPRLANDVAL, 
           TAXABLE_IMPS_VAL = TAXABLEIMPSVAL,
           TAXABLE_LAND_VAL = TAXABLELANDVAL
    )
  
  acct_ready <- acct_frmt %>% 
    miscgis::subset_duplicated("PIN") %>% 
    group_by(PIN) %>% 
    drop_na %>% # remove any duplicates PIN records with NAs
    slice(1) %>%  # take the first record and discard the rest
    ungroup %>% 
    bind_rows(subset_duplicated(acct_frmt,"PIN",notin = TRUE)) %>% 
    arrange(PIN)
  
  # MAKE PARCEL_READY
  
  obj_list <- list(acct_ready,p_ready,p_sf_ready)
  
  parcel_ready <- obj_list %>% 
    reduce(.f = inner_join, by = "PIN") %>% 
    st_as_sf()

  return(parcel_ready)
}

# COMMAND: MAKE_WATERBODIES ----

make_waterbodies <- function(){
  
  w_fp <- root_file("./1-data/2-external/waterbodies-kc.gpkg")
  
  w_dr_id <- as_id("1OF2Z0sNWBmdDdZ4lPgUoolVELd7HCyWL")
  
  w_load <- 
    make_or_read2(fp = w_fp,
                  dr_id = w_dr_id,
                  skip_get_expr = TRUE,
                  get_expr = function(fp){
                    # SOURCE 1: ArcGIS: https://www.arcgis.com/home/item.html?id=b5a20ceaa6114e28b688d4236b417b2b
                    # SOURCE 2: Ecology: http://www.ecy.wa.gov/services/gis/data/data.htm
                  },
                  make_expr = function(fp, dr_id){drive_read(dr_id = dr_id, .tempfile = FALSE, path = fp, read_fun = read_sf)},
                  read_expr = function(fp){read_sf(fp, stringsAsFactors = TRUE)})
  
  waterbodies <- st_transform(w_load,2926) 

  return(waterbodies)
}

# COMMAND: MAKE_UGA ----

make_uga <- function(){
  
  uga_fp <- root_file("1-data/2-external/uga")
  
  uga_dr_id <- as_id("0B5Pp4V6eCkhrZGQ0Q0h5aHNUVW8")
  
  uga_load <- 
    make_or_read2(fp = uga_fp,
                  dr_id = uga_dr_id,
                  skip_get_expr = TRUE,
                  get_expr = function(fp){
                    # SOURCE: ftp://ftp.kingcounty.gov/gis-web/GISData/urban_growth_SHP.zip
                    # METADATA: http://www5.kingcounty.gov/sdc/Metadata.aspx?Layer=urban_growth
                  },
                  make_expr = function(fp, dr_id){
                    
                    zip_dir <- root_file("1-data/2-external/uga")
                    
                    target_name <- "uga"
                    
                    drive_read_zip(dr_id = dr_id,
                                   dir_path = zip_dir,
                                   read_fun = st_read,
                                   target_name = target_name,
                                   .tempdir = FALSE,
                                   layer = "uga",
                                   stringsAsFactors = FALSE)
                  },
                  read_expr = function(fp){read_sf(fp, stringsAsFactors = FALSE)})
  
  uga <- rename_if(uga_load, not_sfc,to_screaming_snake_case)
  
  return(uga)
}

# COMMAND: MAKE_ZONING ----

make_zoning <- function(){
  
  zoning_fp <- root_file("1-data/2-external/zoning_kc_consol_20")
  
  zoning_dr_id <- as_id("0B5Pp4V6eCkhrOTUwT29WQl9STVk")
  
  zoning_load <- 
    make_or_read2(fp = zoning_fp,
                  dr_id = zoning_dr_id,
                  skip_get_expr = TRUE,
                  get_expr = function(fp){
                    # SOURCE: ftp://ftp.kingcounty.gov/gis-web/GISData/zoning_kc_consol_20_SHP.zip
                    # METADATA: http://www5.kingcounty.gov/sdc/Metadata.aspx?Layer=zoning_kc_consol_20
                  },
                  make_expr = function(fp, dr_id){
                    
                    zip_dir <- root_file("1-data/2-external/")
                    
                    target_name <- "zoning_kc_consol_20"
                    
                    drive_read_zip(dr_id = dr_id,
                                   dir_path = zip_dir,
                                   read_fun = st_read,
                                   target_name = target_name,
                                   .tempdir = FALSE,
                                   layer = "zoning_kc_consol_20",
                                   stringsAsFactors = FALSE)
                  },
                  read_expr = function(fp){read_sf(fp, stringsAsFactors = FALSE)})
  
  zoning <- rename_if(zoning_load, not_sfc,to_screaming_snake_case)
  
  return(zoning)
}

# COMMAND: MAKE_CENSUS_TRACTS ----
make_census_tracts <- function(){
  
  tr_fp <- root_file("1-data/2-external/kc_census_tracts.gpkg")
  
  tr_dr_id <- as_id("18KYUqhoAgB65HY82mlPGYCN4TcNaRu7J")
  
  tr_load <- 
    make_or_read2(fp = tr_fp,
                  dr_id = tr_dr_id,
                  skip_get_expr = FALSE,
                  get_expr = function(fp){
                    
                    tr <- tracts(state = 53, county = "King")
                    
                    st_write(tr, fp, driver = "GPKG")
                    
                    drive_folder <- as_id("0B5Pp4V6eCkhrdlJ3MXVaNW16T0U")
                  
                    drive_upload(fp, drive_folder)
                      
                  },
                  make_expr = function(fp, dr_id){
                    
                    drive_read(dr_id = dr_id,.tempfile = FALSE,path = fp,read_fun = read_sf,stringsAsFactors = FALSE)
                  },
                  read_expr = function(fp){read_sf(fp,stringsAsFactors = FALSE)})
  
  census_tracts <- tr_load %>% st_transform(2926)
  
  return(census_tracts)
  
}

# COMMAND: MAKE_CITY_BLOCK_SQFT ----

make_city_block_sqft <- function(){as.integer(66000)} 


# COMMAND: MAKE_LOT_TYPES ----

make_lot_types <- function(city_block_sqft){
  lot_types <- tribble(
                ~LOT_SIZE_DESC,
         "less than 1/8 block",
                   "1/4 block",
      "greater than 1/4 block"
     )

  
  return(lot_types)
}

# COMMAND: MAKE_DEVELOPMENT_ASSUMPTIONS_ZONING ----

make_development_assumptions_zoning <- function(){
  
  dev_assumptions_tbl <- tribble(
                                 ~CONSOL_20, ~DEVELOPABLE_LGL,                                             ~DEVELOPMENT_ASSUMPTION,
                "Central Business District",             TRUE,  "high rise construction, mixed-income, ~150 affordable apartments",
                       "General Commercial",             TRUE,                                               "six story mixed use",
                        "General Mixed Use",             TRUE,                                               "six story mixed use",
                        "Historic District",             TRUE,                                      "no assumption for this class",
         "Mixed Use Commercial/Residential",             TRUE,                                               "six story mixed use",
                         "Mobile Home Park",             TRUE,                                      "no assumption for this class",
                 "Multi-Family Residential",             TRUE,                                              "six story affordable",
                 "Public Use/Institutional",             TRUE,                                      "no assumption for this class",
                "Single-Family Residential",             TRUE,                                  "one unit per 5000 SF of lot size",
                             "Undesignated",             TRUE,                                      "no assumption for this class",
                                         NA,             TRUE,                                      "no assumption for this class",
                      "Agriculture-Related",            FALSE,                                      "no assumption for this class",
      "Aviation and Transportation-Related",            FALSE,                                      "no assumption for this class",
                                   "Forest",            FALSE,                                      "no assumption for this class",
                 "Industrial/Manufacturing",            FALSE,                                      "no assumption for this class",
                 "Mineral Resource-Related",            FALSE,                                      "no assumption for this class",
              "Mixed Use Commercial/Office",            FALSE,                                      "no assumption for this class",
                     "Office/Business Park",            FALSE,                                      "no assumption for this class",
        "Park/Golf Course/Trail/Open Space",            FALSE,                                      "no assumption for this class",
                               "Rural Area",            FALSE,                                      "no assumption for this class",
                  "Sensitive/Critical Area",            FALSE,                                      "no assumption for this class"
     )
  
  development_assumptions_zoning <- dev_assumptions_tbl
  
  return(development_assumptions_zoning)

}

# COMMAND: MAKE_DEVELOPMENT_ASSUMPTIONS_LOT ----

make_development_assumptions_lot <- function(lot_types, development_assumptions_zoning){
  
  
  
  lot_dev_params <- tribble(
                                ~LOT_SIZE_TYPE, ~LOT_COVERAGE_PCT, ~ LOT_STORIES_NBR,
                                            NA,            NA,              NA,
                               "undevelopable",            NA,              NA,
      "potentially developable; no assumption",            NA,              NA,
                               "single family",          0.75,               2,
                                       "small",          0.75,               3,
                                      "medium",          0.75,               7,
                                       "large",           0.5,               7
     )
  
  lot_dev_assumptions <- 
    crossing(LOT_SIZE_DESC = lot_types$LOT_SIZE_DESC, 
             CONSOL_20 = unique(development_assumptions_zoning$CONSOL_20) ) %>%  
    left_join(development_assumptions_zoning, by = "CONSOL_20") %>%   
    mutate(LOT_SIZE_TYPE = case_when(is.na(DEVELOPMENT_ASSUMPTION) ~ NA_character_,
                                     !DEVELOPABLE_LGL ~ "undevelopable zoning",
                                     str_detect(DEVELOPMENT_ASSUMPTION,"^no") ~ "potentially developable; no assumption", 
                                     str_detect(DEVELOPMENT_ASSUMPTION,"^one") ~ "single family",
                                     str_detect(LOT_SIZE_DESC,"less") ~ "small",
                                     str_detect(LOT_SIZE_DESC,"^1") ~ "medium",
                                     TRUE ~ "large")) %>% 
    left_join(lot_dev_params, by = "LOT_SIZE_TYPE") %>% 
    select(CONSOL_20,
           DEVELOPABLE_LGL,
           DEVELOPMENT_ASSUMPTION,
           LOT_SIZE_DESC, 
           LOT_SIZE_TYPE,
           LOT_COVERAGE_PCT,
           LOT_STORIES_NBR) 

  
  development_assumptions_lot <- lot_dev_assumptions
  
  return(development_assumptions_lot)

}

# COMMAND: MAKE_CRITERIA_TAX_EXEMPT ----

make_criteria_tax_exempt <- function(){
  
  crit_tax_e <- list("tax_exempt" = TRUE)
  
  criteria_tax_exempt <- crit_tax_e
  
  return(criteria_tax_exempt)
  
}

# COMMAND: MAKE_CRITERIA_MAX_WATER_OVERLAP_PCT----

make_criteria_max_water_overlap_pct <- function(){
  
  crit_wtr_overlap <- list("water_overlap" = 0.5)
  
  criteria_max_water_overlap_pct <- crit_wtr_overlap
  
  return(criteria_max_water_overlap_pct)
}


# COMMAND: MAKE_CRITERIA_WITHIN_UGA ----

make_criteria_within_uga <- function(){
  
  crit_within_uga <- list("within_uga" = TRUE)
  
  criteria_within_uga <- crit_within_uga
  
  return(criteria_within_uga)
}

# COMMAND: MAKE_CRITERIA_DEVELOPABLE_ZONING ----

make_criteria_developable_zoning <- function(development_assumptions_zoning){
  
  dz <- development_assumptions_zoning %>% 
    filter(DEVELOPABLE_LGL) %>% 
    pull(CONSOL_20)
  
  crit_dz <- list("developable_zoning" = dz)
  
  criteria_developable_zoning <-  crit_dz
  
  return(criteria_developable_zoning)
  
}

# COMMAND: MAKE_CRITERIA_UNDEVELOPABLE_PRESENT_USE ----

make_criteria_undevelopable_present_use <- function(){
  
   list_uses <- function(){
                      parcel_ready %>% 
                        st_drop_geometry() %>% 
                        count(PRESENT_USE, sort = TRUE) %>% 
                        print(n = Inf)
                    }
   
   undev_presentuse <- c(
     "Park, Public(Zoo/Arbor)",
     "Mortuary/Cemetery/Crematory",
     "Open Space Tmbr Land/Greenbelt",
     "Open Space(Curr Use-RCW 84.34)",
     "Mining/Quarry/Ore Processing",
     "Farm",
     "Reserve/Wilderness Area",
     "Open Space(Agric-RCW 84.34)",
     "Forest Land(Desig-RCW 84.33)",
     "Forest Land(Class-RCW 84.33)",
     "Tideland, 1st Class",
     "Tideland, 2nd Class"
   )
   
  crit_undev_presentuse <- list( "undevelopable_presentuse" = undev_presentuse) 
  
  criteria_undevelopable_presentuse <-  crit_undev_presentuse
  
  return(criteria_undevelopable_presentuse)
  
}

# COMMAND: MAKE_SUITABILITY_CRITERIA ----

make_suitability_criteria <- function(criteria_tax_exempt, criteria_max_water_overlap_pct, criteria_within_uga, criteria_developable_zoning, criteria_undevelopable_present_use){
  suitability_criteria <- c(
    criteria_tax_exempt,
    criteria_max_water_overlap_pct ,
    criteria_within_uga,
    criteria_developable_zoning ,
    criteria_undevelopable_present_use 
  )

}

# COMMAND: MAKE_SUITABILITY_TAX_EXEMPT ----

make_suitability_tax_exempt <- function(parcel_ready){
  
  tax_exempt <- parcel_ready %>%  
    st_drop_geometry() %>% 
    mutate(SUIT_OWNER_PUBLIC = if_else(ASSESSOR_PUB_LIST_LGL,TRUE,FALSE,FALSE),
           SUIT_OWNER_NONPROFIT = if_else(TAX_REASON %in% "non profit exemption",TRUE,FALSE,FALSE),
           SUIT_OWNER_TAX_E = SUIT_OWNER_PUBLIC | SUIT_OWNER_NONPROFIT) %>% 
    select(PIN,
           SUIT_OWNER_PUBLIC,
           SUIT_OWNER_NONPROFIT,
           SUIT_OWNER_TAX_E) 
  
  return(tax_exempt)
}
# COMMAND: MAKE_SUITABILITY_WATER_OVERLAP ----

make_suitability_water_overlap <- function(parcel_ready, waterbodies){
  # Convert to EPSG 2926 
  p_ready_poly <- parcel_ready %>%  
    select(PIN) %>% 
    st_transform(2926)
  
  # Filter waterbodies to include only those larger than 1/2 sq km
  
  min_sq_km <- 0.5
  
  tolerance <- 5
  
  wtr <- st_transform(waterbodies, 2926) %>%  
    filter( AREA_SQ_KM > min_sq_km) %>% 
    st_simplify(preserveTopology = TRUE, dTolerance = tolerance)
  
  p_water <- p_ready_poly 
  
  # ~ 3 min. operation
  
  p_water$SUIT_WATER_OVERLAP_LGL <- st_intersects_any(x = p_water,y = wtr)  
  
  intersect_idx <- which(p_water$SUIT_WATER_OVERLAP_LGL)
  
  p_water$SUIT_WATER_OVERLAP_PCT <- as.double(0)
  
  wtr_union <- st_union(wtr)
  
  # ~ 2 min. operation
  
  p_water[intersect_idx,"SUIT_WATER_OVERLAP_PCT"] <- st_intersect_area(x = p_water[intersect_idx,],
                                                                       y = wtr_union)
  
  
  p_water_ready <- p_water %>% 
    st_drop_geometry() %>% 
    select(PIN, 
           SUIT_WATER_OVERLAP_LGL,
           SUIT_WATER_OVERLAP_PCT)
  
  water_overlap <- p_water_ready
  
  return(water_overlap)
  
}


# COMMAND: MAKE_SUITABILITY_WITHIN_UGA ----

make_suitability_within_uga <- function(parcel_ready, uga){
  
  p_ready_pt <- parcel_ready %>% 
    st_set_geometry("geom_pt") %>% 
    st_transform(2926)
  
  uga_2926 <- st_transform(uga, 2926)
  
  uga_subdivide <- st_subdivide(uga_2926, 100) %>% 
    st_collection_extract()
  
  # ~ 20 min. operation
  p_ready_pt$SUIT_WITHIN_UGA <- st_intersects_any(p_ready_pt,uga_subdivide)
  
  p_ready_within_uga <- p_ready_pt %>% 
    st_drop_geometry() %>% 
    select(PIN, SUIT_WITHIN_UGA)
  
  return(p_ready_within_uga)
  
}

# COMMAND: MAKE_SUITABILITY_DEVELOPABLE_ZONING ----

make_suitability_developable_zoning <- function(parcel_ready, zoning){
  
  p_pt <- parcel_ready %>% 
    st_set_geometry("geom_pt") %>% 
    st_transform(2926) %>% 
    select(PIN)
  
  zng <- zoning %>% st_transform(2926) %>% 
    st_subdivide(max_vertices = 100) %>% 
    st_collection_extract() 
  
  p_zng <- p_pt
  
  p_zng$SUIT_ZONING_CONSOL_20 <- st_over(p_zng, zng, "CONSOL_20") 
  
  p_dz_ready <- st_drop_geometry(p_zng)
  
  developable_zoning <- p_dz_ready
  
  return(developable_zoning)
  
}

# COMMAND: MAKE_SUITABILITY_PRESENT_USE ----

make_suitability_present_use <- function(parcel_ready){
  
  pres_use <- parcel_ready %>%  
    st_drop_geometry() %>% 
    transmute(PIN,
              SUIT_PRESENT_USE = PRESENT_USE)
  
  
  present_use <- pres_use
  
  return(present_use)
}

# COMMAND: MAKE_SUITABILITY ----
make_suitability <- function(parcel_ready, suitability_criteria, suitability_tax_exempt, suitability_water_overlap, suitability_within_uga, suitability_developable_zoning, suitability_present_use){
  
  suitability <- list(parcel_ready, 
                      suitability_tax_exempt, 
                      suitability_water_overlap, 
                      suitability_within_uga, 
                      suitability_developable_zoning, 
                      suitability_present_use) %>% 
    reduce(left_join, by = "PIN") %>% 
    mutate(
      SUITABLE_OWNER_LGL = if_else(SUIT_OWNER_TAX_E == suitability_criteria[["tax_exempt"]],TRUE,FALSE,FALSE),
      SUITABLE_WATER_OVERLAP_LGL = if_else(SUIT_WATER_OVERLAP_PCT <= suitability_criteria[["water_overlap"]],TRUE,FALSE,FALSE),
      SUITABLE_WITHIN_UGA_LGL = if_else(SUIT_WITHIN_UGA == suitability_criteria[["within_uga"]],TRUE,FALSE,FALSE),
      SUITABLE_ZONING_CONSOL_20_LGL = if_else(SUIT_ZONING_CONSOL_20 %in% suitability_criteria[["developable_zoning"]],TRUE,FALSE,FALSE) ,
      SUITABLE_PRESENT_USE_LGL = if_else(! SUIT_PRESENT_USE %in% suitability_criteria[["undevelopable_presentuse"]],TRUE,FALSE,FALSE),
      SUITABLE_LGL = SUITABLE_OWNER_LGL & SUITABLE_WATER_OVERLAP_LGL & SUITABLE_WITHIN_UGA_LGL & SUITABLE_ZONING_CONSOL_20_LGL & SUITABLE_PRESENT_USE_LGL
    ) 
  
  return(suitability)
  
}
# COMMAND: MAKE_BUILDING_RESIDENTIAL ----

make_building_residential <- function(){ 
  
  bldg_res_fp <- root_file(res_fp <- "1-data/2-external/EXTR_ResBldg.csv")
  
  bldg_res_dr_id <- as_id("10rz6hc4lEAaaU-0Jcv0iCiVMFBVTc-en")
  
  bldg_res_load <- 
    make_or_read2(fp = bldg_res_fp,
                  dr_id = bldg_res_dr_id,
                  skip_get_expr = TRUE,
                  get_expr = function(fp){
                    # SOURCE: url here
                  },
                  make_expr = function(fp, dr_id){
                    
                    zip_dir <- root_file("1-data/2-external")
                    
                    target_name <- "EXTR_ResBldg.csv" 
                    
                    drive_read_zip(
                      dr_id = dr_id,
                      .tempdir = FALSE,
                      dir_path = zip_dir,
                      read_fun = read_csv,
                      target_name = target_name
                    ) 
                    
                  },
                  read_expr = function(fp){read_csv(fp)})
  
  bldg_res <- bldg_res_load %>% 
    rename_all(to_screaming_snake_case) %>% 
    mutate(PIN = str_c(MAJOR,MINOR,sep = ""))
  
  return(bldg_res)
  
}

# COMMAND: MAKE_BUILDING_APARTMENT ----

make_building_apartment <- function(){ 
  
  bldg_apt_fp <- root_file(apt_fp <- "1-data/2-external/EXTR_AptComplex.csv")
  
  bldg_apt_dr_id <- as_id("11kkudStD4TuoiRqMie-Y4_8tZQJmLPBw")
  
  bldg_apt_load <- 
    make_or_read2(fp = bldg_apt_fp,
                  dr_id = bldg_apt_dr_id,
                  skip_get_expr = TRUE,
                  get_expr = function(fp){
                    # SOURCE: url here
                  },
                  make_expr = function(fp, dr_id){
                    
                    zip_dir <- root_file("1-data/2-external")
                    
                    target_name <- "EXTR_AptComplex.csv" 
                    
                    drive_read_zip(
                      dr_id = dr_id,
                      .tempdir = FALSE,
                      dir_path = zip_dir,
                      read_fun = read_csv,
                      target_name = target_name
                    ) 
                    
                  },
                  read_expr = function(fp){read_csv(fp)})
  
  building_apartment <- bldg_apt_load %>% 
    rename_all(to_screaming_snake_case) %>% 
    mutate(PIN = str_c(MAJOR,MINOR,sep = ""))
  
  return(building_apartment)
  
}

# COMMAND: MAKE_BUILDING_CONDO ----

make_building_condo <- function(){ 
  
  bldg_condo_fp <- root_file(condo_fp <- "1-data/2-external/EXTR_CondoComplex.csv")
  
  bldg_condo_dr_id <- as_id("1avYhRKzHijnc-YZQDGICqWrQNhQoTwnB")
  
  bldg_condo_load <- 
    make_or_read2(fp = bldg_condo_fp,
                  dr_id = bldg_condo_dr_id,
                  skip_get_expr = TRUE,
                  get_expr = function(fp){
                    # SOURCE: url here
                  },
                  make_expr = function(fp, dr_id){
                    
                    zip_dir <- root_file("1-data/2-external")
                    
                    target_name <- "EXTR_CondoComplex.csv" 
                    
                    drive_read_zip(
                      dr_id = dr_id,
                      .tempdir = FALSE,
                      dir_path = zip_dir,
                      read_fun = read_csv,
                      target_name = target_name
                    ) 
                    
                  },
                  read_expr = function(fp){read_csv(fp)})
  
  building_condo <- bldg_condo_load %>% 
    rename_all(to_screaming_snake_case)
  
  return(building_condo)
  
}

# COMMAND: MAKE_BUILDING_COMMERCIAL ----

make_building_commercial <- function(){ 
  
  bldg_comm_fp <- root_file(condo_fp <- "1-data/2-external/EXTR_CommBldg.csv")
  
  bldg_comm_dr_id <- as_id("1VT_plwHQve51ldIg3chFUpcTSMD_ZyrN")
  
  bldg_comm_load <- 
    make_or_read2(fp = bldg_comm_fp,
                  dr_id = bldg_comm_dr_id,
                  skip_get_expr = TRUE,
                  get_expr = function(fp){
                    # SOURCE: url here
                  },
                  make_expr = function(fp, dr_id){
                    
                    zip_dir <- root_file("1-data/2-external")
                    
                    target_name <- "EXTR_CommBldg.csv" 
                    
                    drive_read_zip(
                      dr_id = dr_id,
                      .tempdir = FALSE,
                      dir_path = zip_dir,
                      read_fun = read_csv,
                      target_name = target_name
                    ) 
                    
                  },
                  read_expr = function(fp){read_csv(fp)})
  
  building_commercial <- bldg_comm_load %>% 
    rename_all(to_screaming_snake_case) %>% 
    mutate(PIN = str_c(MAJOR,MINOR,sep = ""))
  
  return(building_commercial)
  
}

# COMMAND: MAKE_BUILDING ----

make_building <- function(building_residential, building_apartment, building_condo, building_commercial){
  
  bldg_empty <- tibble(PIN = as.character(""), 
                       BLDG_NBR = as.integer(""),
                       BLDG_NET_SQ_FT= as.integer(""),
                       NBR_LIVING_UNITS = as.integer(""),
                       NBR_BLDGS = as.integer(""),
                       BLDG_CAT = as.character("")) %>% 
    slice(0)
  
  comm_join <- building_commercial %>% 
    transmute(PIN,
              BLDG_NBR,
              BLDG_NET_SQ_FT,
              NBR_BLDGS,
              BLDG_CAT = "commercial")
  
  res_join <- building_residential %>% 
    transmute(PIN,
              BLDG_NBR,
              BLDG_NET_SQ_FT = SQ_FT_TOT_LIVING,
              NBR_LIVING_UNITS,
              BLDG_CAT = "residential")
  
  apt_join <- building_apartment %>% 
    transmute(PIN,
              NBR_BLDGS,
              NBR_LIVING_UNITS = NBR_UNITS,
              BLDG_CAT = "apartment")
  
  condo_join <- building_condo %>% 
    transmute(PIN = MAJOR,
              NBR_LIVING_UNITS = NBR_UNITS,
              BLDG_CAT = "condo")
  
  # Join all bulding objects to the empty tibble
  bldg_all <- list(bldg_empty, comm_join, res_join, apt_join, condo_join) %>% 
    reduce(full_join) 
  
  # ~ 2 min. operation
  
  bldg_all_sum <- bldg_all %>%  
    mutate(NBR_BLDGS = if_else(BLDG_CAT %in% c("residential", "condo"),as.integer(1),NBR_BLDGS)) %>% 
    mutate(CAT_LGL = TRUE,
           COL_NAME = str_c("TYPE",toupper(BLDG_CAT),"LGL", sep = "_")) %>% 
    spread(COL_NAME, CAT_LGL) %>% 
    mutate_at(vars(TYPE_APARTMENT_LGL:TYPE_RESIDENTIAL_LGL), ~ if_else(is.na(.),FALSE,.)) %>% 
    group_by(PIN) %>%  
    summarise(BLDG_NBR = max(n()),  
              BLDG_NET_SQ_FT = sum(BLDG_NET_SQ_FT, na.rm = TRUE),
              BLDG_LIVING_UNITS = sum(NBR_LIVING_UNITS, na.rm = TRUE),
              BLDG_TYPE_APARTMENT_LGL = any(TYPE_APARTMENT_LGL),
              BLDG_TYPE_COMMERCIAL_LGL = any(TYPE_COMMERCIAL_LGL),
              BLDG_TYPE_CONDO_LGL = any(TYPE_CONDO_LGL),
              BLDG_TYPE_RESIDENTIAL_LGL = any(TYPE_RESIDENTIAL_LGL))
  
  
  building <- bldg_all_sum
}

# COMMAND: MAKE_CRITERIA_LOT_SIZE ----

make_criteria_lot_size <- function(city_block_sqft, lot_types){
  eight_block <- city_block_sqft/8
  
  quarter_block <- city_block_sqft/4
  
  crit_lot_size <- list("breaks" = c(-Inf,eight_block,quarter_block, Inf),
                      "labels" =  lot_types$LOT_SIZE_DESC)
  criteria_lot_size <- crit_lot_size
 
   return(criteria_lot_size)
}

# COMMAND: MAKE_UTILIZATION_CRITERIA ----

make_utilization_criteria <- function(criteria_underutilized){
  utilization_criteria <- c(criteria_lot_size)
  
  return(utilization_criteria)
}

# COMMAND: MAKE_UTILILIZATION_PRESENT ----

make_utilization_present <- function(parcel_ready, building){
  
  util_present <- parcel_ready %>% 
    st_drop_geometry() %>% 
    select(PIN) %>% 
    left_join(building, by = "PIN") %>% 
    mutate(UTIL_PRESENT = if_else(is.na(BLDG_NET_SQ_FT),0,as.double(BLDG_NET_SQ_FT),missing = 0))
  
  utilization_present <- util_present
  
  return(utilization_present)
}


# COMMAND: MAKE_UTILIZATION_LOT_SIZE----
make_utilization_lot_size <- function(parcel_ready, criteria_lot_size){
  
  util_ls <- parcel_ready %>% 
    st_drop_geometry() %>% 
    transmute(PIN,
              LOT_SIZE_DESC = as.character(cut(SQ_FT_LOT,
                               breaks = criteria_lot_size[["breaks"]],
                               labels = criteria_lot_size[["labels"]])))
  
  utilization_lot_size <- util_ls
  return(utilization_lot_size)
  
}

# COMMAND: MAKE_UTILILIZATION_POTENTIAL ---- 

make_utilization_potential <- function(suitability, development_assumptions_lot, utilization_lot_size){
  
  suit_trim <- suitability %>% 
    st_drop_geometry() %>% 
    transmute(PIN, 
           LOT_SQ_FT = SQ_FT_LOT,
           UTIL_LOT_DEVELOPABLE_PCT = round(1-SUIT_WATER_OVERLAP_PCT,2),
           CONSOL_20 = SUIT_ZONING_CONSOL_20
           )
  
  util_potential <- suit_trim %>% 
  left_join(utilization_lot_size, by = "PIN") %>% 
    left_join(development_assumptions_lot, by = c("CONSOL_20", "LOT_SIZE_DESC")) %>% 
    rename(UTIL_DEVELOPABLE_LGL = DEVELOPABLE_LGL,
           UTIL_DEVELOPMENT_ASSUMPTION = DEVELOPMENT_ASSUMPTION) %>% 
    mutate(UTIL_DEVELOPABLE_ESTIMATE_LGL = if_else(LOT_SIZE_TYPE %in% c("single family","small","medium","large"),
                              TRUE,
                              FALSE,
                              missing = FALSE)) %>% 
    mutate(UTIL_DEVELOPABLE_LOT_COVERAGE_PCT = lesser_of(LOT_COVERAGE_PCT, UTIL_LOT_DEVELOPABLE_PCT) ) %>% # take the lesser of lot coverage estimate and water overlap 
    mutate(UTIL_POTENTIAL_UTILIZATION_SQFT = round(LOT_SQ_FT * UTIL_DEVELOPABLE_LOT_COVERAGE_PCT * LOT_STORIES_NBR)) %>%  # this is where the math happens!
    select(PIN,
           starts_with("LOT"),
           starts_with("UTIL"))
  
  utilization_potential <- util_potential
  
  return(utilization_potential) 
}

# COMMAND: MAKE_UTILILIZATION ----

make_utilization <- function(suitability, utilization_present, utilization_potential){
  
  util <- suitability %>% 
    st_drop_geometry() %>% 
    select(PIN) %>% 
    left_join(utilization_present, by = "PIN") %>% 
    left_join(utilization_potential, by = "PIN") %>% 
    mutate(UTIL_UNDER_UTILIZED_LGL = if_else(UTIL_PRESENT < UTIL_POTENTIAL_UTILIZATION_SQFT,TRUE,FALSE,NA)) %>% 
    mutate(UTILIZATION = case_when( 
      !UTIL_UNDER_UTILIZED_LGL ~ "fully-utilized",
      UTIL_UNDER_UTILIZED_LGL ~ "under-utilized", 
      TRUE ~ LOT_SIZE_TYPE 
    ))
  
  utilization <- util
  return(utilization)
  
}

# COMMAND: MAKE_FILTERS_TRACT ----
make_filters_census_tract <- function(parcel_ready, census_tracts){
  
  p_ready_pt <- parcel_ready %>% 
    st_set_geometry("geom_pt") %>% 
    st_transform(2926)
  
  tr_subdivide <- st_subdivide(census_tracts, 100) %>% 
    st_collection_extract() 
   
  p_ready_pt$CENSUS_TRACT <- st_over(p_ready_pt,tr_subdivide, "GEOID") 
  
  p_ready_tr <- p_ready_pt %>% 
    st_drop_geometry() %>% 
    select(PIN, CENSUS_TRACT)
  
  filters_census_tract <- p_ready_tr
  
  return(filters_census_tract)
   
}

# COMMAND: MAKE_FILTERS ----
make_filters <- function(parcel_ready, filters_census_tract){
  
  filter_list <- list(filters_census_tract) %>% 
    reduce(left_join, by = "PIN")
  
  filters <- parcel_ready %>% 
    st_drop_geometry() %>% 
    select(PIN) %>% 
    left_join(filter_list, by = "PIN") 
  
  return(filters)
    
  
}

# COMMAND: MAKE_INVENTORY ----

make_inventory <- function(filters, suitability,  utilization){
  
  inv <- list(filters, suitability, utilization) %>% 
    reduce(left_join, by = "PIN")
  
  inventory <- inv
  
  return(inventory)
  
}

# COMMAND: MAKE_INVENTORY_SUITABLE ----

make_inventory_suitable <- function(inventory){
  
  inv_suit <- filter(inventory, SUITABLE_LGL)
  
  inventory_suitable <- inv_suit
  
  return(inventory_suitable)
  
}

# RUN PROJECT PLAN ----
make(project_plan)
