# SETUP ----
library(drake)
library(sf)
library(lwgeom)
library(googledrive)
library(miscgis)  
library(snakecase)
library(magrittr)
library(rprojroot) 
library(RSocrata)
library(glue)
library(tidyverse) 


root <- rprojroot::is_rstudio_project
root_file <- root$make_fix_file()
options(httr_oob_default=TRUE) 
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
  parcel_ready = make_parcel_ready(lu, prop_type, tax_status, tax_reason, acct, parcel_df, parcel_sf)
)

uga_plan <- drake_plan(uga = make_uga()) # TEMPORARY - REMOVE THIS PLAN

miscellaneous_plan <- drake_plan(
  waterbodies = make_waterbodies(),
  uga = make_uga(),
  zoning = make_zoning()
)

suitability_plan <- drake_plan(
  tax_e = make_tax_e(parcel_ready, pub_parcel),
  water_coverage = make_water_coverage(parcel_ready, waterbodies),
  within_uga = make_within_uga(parcel_ready, uga),
  developable_zoning = make_zoning(parcel_ready, zoning),
  present_use = make_present_use(parcel_ready),
  parcel_suitability = make_suitability(parcel_sf, tax_e, water_coverage, uga, zoning, present_use)
)

utilization_plan <- drake_plan(
  util_present = make_util_present(),
  util_potential = make_util_potential(),
  parcel_utilization = make_parcel_utilization(parcel_sf, util_present, util_potential)
)


project_plan <- rbind(
  lookup_plan,
  parcel_plan,
  suitability_plan,
  utilization_plan)    # rbind all plans together 


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
st_intersects_any <- function(x,y){
  st_intersects(x,y) %>% 
    map_lgl(~ length(.x)>0)
}
# FUNCTION: ST_INTERSECT_AREA ----
st_intersect_area <- function(x, y, crs){ 
  
  x_sfc <- x %>% 
    st_sfc %>% 
    st_set_crs(crs)
  
  area_x <- x_sfc %>% st_area() %>% as.double()
  
  area_xy <- st_intersection(x_sfc, y) %>% st_area %>% as.double()
  
  if(is_empty(area_xy)){return(as.double(0))}
  
  overlap_pct <- area_xy %>% 
    divide_by(area_x) %>% 
    as.double() %>% 
    round(2)
  
  return(overlap_pct)
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

make_parcel_ready <- function(lu, prop_type, tax_status,tax_reason, acct, parcel_df, parcel_sf){
  
  # MAKE P_SF_READY

  p_sf_ready <- parcel_sf
    
  # MAKE P_READY
  
  present_use <- lu %>% 
    filter(LU_TYPE == 102) %>% 
    select(PRESENT_USE = LU_ITEM,
           PRESENT_USE_DESC = LU_DESCRIPTION)
  
  p_ready <- parcel_df %>% 
   mutate(MAJOR = str_pad(string = MAJOR,width = 6,side = "left",pad = "0"),
           MINOR = str_pad(string = MINOR,width = 4,side = "left",pad = "0"),
           PIN = str_c(MAJOR,MINOR)) %>% 
    left_join(prop_type, by = "PROP_TYPE") %>% 
    left_join(present_use, by = "PRESENT_USE") %>% 
    select(PIN,
           PROP_NAME,
           PROP_TYPE = PROP_TYPE_DESC, 
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
    drop_na %>% 
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
  
  waterbodies <- w_load %>% 
    st_set_crs(4326) %>% 
    st_union() %>% 
    st_transform(2926) %>% 
    st_make_valid() 

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
                  get_expr = function(fp){},
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

# COMMAND: MAKE_TAX_E ----

make_tax_e <- function(parcel_ready, pub_parcel){
  
  pub_join <- pub_parcel %>% 
    select(PIN) %>% 
    mutate(CRIT_SUIT_OWNER_PUBLIC = TRUE)
  
  tax_e <- parcel_ready %>% 
    left_join(pub_join, by = "PIN") %>% 
    mutate(CRIT_SUIT_OWNER_PUBLIC = if_else(CRIT_SUIT_OWNER_PUBLIC,TRUE,FALSE,FALSE),
           CRIT_SUIT_OWNER_NONPROFIT = if_else(TAX_REASON %in% "non profit exemption",TRUE,FALSE,FALSE),
           CRIT_SUIT_OWNER_TAX_E = CRIT_SUIT_OWNER_PUBLIC | CRIT_SUIT_OWNER_NONPROFIT) %>% 
    select(PIN,
           CRIT_SUIT_OWNER_PUBLIC,
           CRIT_SUIT_OWNER_NONPROFIT,
           CRIT_SUIT_OWNER_TAX_E) %>% 
    st_set_geometry(NULL)
  
  return(tax_e)
}

# COMMAND: MAKE_WATER_COVERAGE ----

make_water_coverage <- function(parcel_ready, waterbodies){
  
  wc_fp <- root_file("")
  
  wc_dr_id <- as_id("")
  
  wc_load <- 
    make_or_read2(fp = wc_fp,
                  dr_id = wc_dr_id,
                  skip_get_expr = FALSE,
                  get_expr = function(fp){
                    
                    # actual_process <- function(){
                    #   
                    #   # NOTE: This the is the actual operation, but it's long-running,
                    #   #       so I use a shortcut for the first iteration of the project.
                    #   
                    #   water_coverage <- parcel_ready %>% 
                    #   slice(1:1000) %>% 
                    #   select(PIN) %>% 
                    #   mutate(WATER_OVERLAP_PCT = map_dbl(geometry, st_intersect_area, y = waterbodies, crs = 2926)) %>%
                    #   mutate(WATER_OVERLAP_LGL = WATER_OVERLAP_PCT > 0) %>% 
                    #   st_set_geometry(NULL)
                    # 
                    # drive_folder <- as_id("0B5Pp4V6eCkhrZ3NHOEE0Sl9FbWc") 
                    # 
                    # write_csv(water_coverage, fp) #CHANGE TO fp
                    # 
                    # drive_upload(fp, drive_folder) 
                    # }
                    
                    
                  },
                  make_expr = function(fp, dr_id){
                    
                    drive_read(dr_id = as_id("1jrEAX7ogq1RdNU-hfrC-tntF66b6NcKg"),
                               .tempfile = FALSE,
                               path = root_file("./1-data/3-interim/p-water-overlap-0-50-sf.gpkg"),
                               read_fun = read_sf)
                    
                  },
                  read_expr = function(fp){read_sf(root_file("./1-data/3-interim/p-water-overlap-0-50-sf.gpkg"))})
  
  wc_join <- wc_load %>% 
    transmute(PIN, 
              CRIT_SUIT_WATER_OVERLAP_PCT = WATER_OVERLAP_PCT,
              CRIT_SUIT_WATER_OVERLAP_LGL = as.logical(WATER_OVERLAP_LGL)) %>%  
    st_set_geometry(NULL)
  
  water_coverage <- parcel_ready %>% 
    left_join(wc_join, by = "PIN") %>% 
    mutate(CRIT_SUIT_WATER_OVERLAP_PCT = if_else(is.na(CRIT_SUIT_WATER_OVERLAP_PCT),0,CRIT_SUIT_WATER_OVERLAP_PCT),
           CRIT_SUIT_WATER_OVERLAP_LGL = if_else(CRIT_SUIT_WATER_OVERLAP_LGL,TRUE,FALSE,FALSE)) %>% 
    select(PIN,
           CRIT_SUIT_WATER_OVERLAP_PCT,
           CRIT_SUIT_WATER_OVERLAP_LGL) %>% 
    st_set_geometry(NULL)
  
  return(water_coverage)
  
}


# COMMAND: MAKE_WITHIN_UGA ----

make_within_uga <- function(parcel_ready, uga){
  
  w_uga_fp <- root_file("./1-data/3-interim/parcel_within_uga.csv")
  
  w_uga_dr_id <- as_id("")
  
  w_uga_load <- 
    make_or_read2(fp = w_uga_fp,
                  dr_id = w_uga_dr_id,
                  skip_get_expr = FALSE,
                  get_expr = function(fp){
                    
                    
                    # actual_process <- function(){
                    # 
                    # # NOTE: This the is the actual operation, but it's long-running,
                    # #       so I use a shortcut for the first iteration of the project.
                    # uga_subdivide <- st_subdivide(uga, 100)
                    # 
                    # p_centroid <- parcel_ready %>%  
                    #   mutate(geom_centroid = st_sfc(map(geometry, st_centroid, of_largest_polygon = TRUE))) 
                    #   
                    # st_geometry(p_centroid) <- st_sfc(p_centroid$geom_centroid)
                    # 
                    # p_uga_sf <- 
                    #   tibble(UGA_LGL = st_intersects_any(st_transform(p_centroid, 2926), st_transform(uga_subdivide, 2926))) %>% 
                    #   bind_cols(p_centroid) 
                    # # more may be needed here (it's super slow)
                    # }
                    
                    
                  },
                  make_expr = function(fp, dr_id){
                    drive_read(dr_id = as_id("1zbTXAE2OapKtxJduXL2Oltn9OqSxaakP"),
                               .tempfile = FALSE,
                               path = root_file("1-data/3-interim/pub-uga-zng-sf.rds"),
                               read_fun = read_rds)
                  },
                  read_expr = function(fp){read_rds(root_file("1-data/3-interim/pub-uga-zng-sf.rds"))})
    
  w_uga_join <- w_uga_load %>% 
    select(PIN) %>% 
    mutate(CRIT_SUIT_WITHIN_UGA = TRUE) %>% 
    st_drop_geometry()
  
  within_uga <- parcel_ready %>% 
    left_join(w_uga_join, by = "PIN") %>% 
    mutate(CRIT_SUIT_WITHIN_UGA = if_else(CRIT_SUIT_WITHIN_UGA,TRUE,FALSE,FALSE)) %>% 
    st_drop_geometry()
  
}

# COMMAND: MAKE_DEVELOPABLE_ZONING ----

# COMMAND: MAKE_PRESENT_USE ----

# COMMAND: MAKE_PARCEL_SUITABILITY ----

# COMMAND: MAKE_UTIL_PRESENT ----

# COMMAND: MAKE_UTIL_POTENTIAL ---- 

# COMMAND: MAKE_PARCEL_UTILIZATION----
# RUN PROJECT PLAN ----
# make(project_plan)
