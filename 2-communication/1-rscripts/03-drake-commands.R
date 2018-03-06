# COMMAND: MAKE_PARCEL_METADATA_TABLE ----
make_parcel_metadata_table <- function(){
  
  p_meta_tbl_fp <- here("1-data/3-interim/KC_Parcel_Metadata_Table.csv")
  
  p_meta_tbl_dr_id <- as_id("19UL-kxImyu6u5QIoi25qqHxTuX8RCXvYot9T0KoaWyM")
  
  
  p_meta_tbl_load <- 
    make_or_read2(fp = p_meta_tbl_fp, dr_id = p_meta_tbl_dr_id, skip_get_expr = FALSE,
                  get_expr = function(fp){  #SOURCE: http://info.kingcounty.gov/assessor/datadownload/desc/Parcel.doc
                    },
                  make_expr = function(fp, dr_id){
                    drive_read(dr_id = dr_id,.tempfile = FALSE,path = fp,read_fun = read_csv)
                  },
                  read_expr = function(fp){read_csv(fp)})
  
  p_meta_tbl <- p_meta_tbl_load %>% 
    transmute(FIELD_NAME = to_screaming_snake_case(FIELD_NAME),
              LU_TYPE = LOOKUP)
  
  parcel_metadata_table <- p_meta_tbl
  
  return(parcel_metadata_table)
  
  
}


# COMMAND: MAKE_LU ----

make_lu <- function(){
  
  lu_fp <- here("1-data/2-external/EXTR_LookUp.csv")
  
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
  
  tax_s_fp <- here("1-data/1-raw/tax_status.txt")
  
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
   
  tax_r_fp <- here("1-data/1-raw/tax_reason.txt")
  
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
  
  pt_fp <- here("1-data/1-raw/prop_type.txt")
  
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

# COMMAND: MAKE_PRESENT_USE_RECODE ----

make_present_use_recode <- function(){
  
  pu_fp <- here("1-data/1-raw/present_use_recode.csv")
  
  pu_dr_id <- as_id("1FYNs7oioWbjHUS4An4CLSmNOhN5xA7Oa")
  
  pu_load <- 
    make_or_read2(fp = pu_fp,
                  dr_id = pu_dr_id,
                  skip_get_expr = FALSE,
                  get_expr = function(fp){
                    
                    present_use_recode <- 
                      tribble(
                        ~ ORIG, ~ NEW,
                        "Srvc","Service",
                        "Ctr","Center",
                        "Bldg","Building",
                        "Nghbrhood","Neighborhood",
                        "Prop ","Property ",
                        "Soc ", "Social ",
                        "Greenhse", "Greenhouse",
                        "Nrsry", "Nursery",
                        "Hort", "Horticulture",
                        "Relig", "Religious",
                        "Res ", "Residential ",
                        "Gen ", "General ",
                        "Warehse", "Warehouse",
                        "Billbrd", "Billboard",
                        "Fac\\)", "Facility\\)",
                        "Curr", "Current",
                        "Tmbr", "Timber",
                        "Dev ", "Development "
                      ) 
                    
                    write_csv(present_use_recode, fp)
                    
                    drive_folder <- as_id("0B5Pp4V6eCkhrb1lDdlNaOFY4V0U")
                    
                    drive_upload(media = fp, path = drive_folder) 
                    
                  },
                  make_expr = function(fp, dr_id){ 
                    drive_download(file = dr_id, path = fp) 
                    read_csv(fp)
                  },
                  read_expr = function(fp){read_csv(fp)})
  
  present_use_recode <- pu_load 
  
  return(present_use_recode)
  
}

# COMMAND: MAKE_PARCEL_LOOKUP ----

make_parcel_lookup <- function(parcel_metadata_table, lu, present_use_recode){
  
  p_lu_orig <- parcel_metadata_table %>% 
    left_join(lu, by = "LU_TYPE") %>% 
    drop_na
  
  p_lu_token <- p_lu_orig %>% 
    mutate(id = row_number()) %>% 
    unnest_tokens(ORIG, LU_DESCRIPTION, to_lower = FALSE) 
  
  p_lu_recoded <- p_lu_token %>% 
    left_join(present_use_recode, by = "ORIG") %>% 
    mutate(LU_DESCRIPTION = if_else(is.na(NEW),ORIG,NEW)) %>% 
    select(id,FIELD_NAME,LU_ITEM, LU_DESCRIPTION) %>%
    nest(-id) %>% 
    mutate(FIELD_NAME = map_chr(data, ~.x %>% pull("FIELD_NAME") %>% first),
           LU_ITEM = map_int(data, ~.x %>% pull("LU_ITEM") %>% first),
           LU_DESCRIPTION = map_chr(data, ~.x %>% pull("LU_DESCRIPTION") %>% str_c(collapse = " "))) %>% 
    select(FIELD_NAME,
           LU_ITEM,
           LU_DESCRIPTION)
  
  parcel_lookup <- p_lu_recoded
  
  return(parcel_lookup)
  
  
}

# COMMAND: MAKE_PUB_PARCEL ----

make_pub_parcel <- function(){ 
  pub_fp <-  here("1-data/2-external/kc-public-parcels.csv")
  
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
  
  acct_fp <- here("./1-data/2-external/kc_real_prop_acct_extract.rds")
  
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
  
  return(acct)
  
}



# COMMAND: MAKE_PARCEL_ADDR ----
make_parcel_addr <- function(){
  
  p_fp <- here("./1-data/2-external/address/address.shp")
  
  p_dr_id <- as_id("1MD5ctFCUNsC0Fw599YUFV5UaPArHxvFL")
  
  p_load <-  
    make_or_read2(fp = p_fp,
                  dr_id = p_dr_id,
                  skip_get_expr = TRUE,
                  get_expr = function(fp){ # Source: ftp://ftp.kingcounty.gov/gis-web/GISData/city_SHP.zip
                  },
                  make_expr = function(fp, dr_id){
                    zip_dir <- "./1-data/2-external"
                    
                    target_name <- "address"
                    
                    drive_read_zip(
                      dr_id = dr_id,
                      .tempdir = FALSE,
                      dir_path = zip_dir,
                      read_fun = read_sf,
                      target_name = target_name,
                      stringsAsFactors = FALSE
                    ) 
                    
                  },
                  read_expr = function(fp){ read_sf(p_fp,stringsAsFactors = FALSE)})
  
  parcel_addr <- p_load %>% 
    rename_if(not_sfc, to_screaming_snake_case) %>% 
    st_drop_geometry 
    
  
  return(parcel_addr)
}

# COMMAND: MAKE_PARCEL_DF ----
make_parcel_df <- function(){
  
  p_fp <- here("./1-data/2-external/EXTR_Parcel_20171013.csv")
  
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
  
  p_sf_2926 <- st_transform(parcel_sf_poly, 2926)
  
  p_sf_2926$geom_pt <- st_centroid(st_geometry(p_sf_2926)) 
  
  p_sf_ready <- p_sf_2926 %>% 
    mutate(MAJOR = str_pad(MAJOR, 6, "left", "0"),
           MINOR = str_pad(MINOR, 4, "left", "0")) %>% 
    transmute(PIN = str_c(MAJOR,MINOR),
              geom_pt) %>% 
    drop_na() %>% 
    st_set_geometry("geometry") %>% 
    st_transform(2926) %>% 
    st_sf
  
  parcel_sf <- p_sf_ready
  
  return(parcel_sf)
}

# COMMAND: MAKE_PARCEL_READY ----

make_parcel_ready <- function(parcel_lookup, prop_type, tax_status, tax_reason, present_use_recode, pub_parcel, acct, parcel_addr, parcel_df, parcel_sf_poly, parcel_sf){
   
  
  # MAKE P_SF_READY

  p_sf_ready <-  parcel_sf %>%
    miscgis::subset_duplicated("PIN") %>%
    group_by(PIN) %>%
    slice(1) %>%
    ungroup %>%
    rbind(subset_duplicated(parcel_sf,"PIN",notin = TRUE)) %>%
    st_set_geometry("geometry")
  
  

  # MAKE P_ADDR_READY
  p_addr_ready <- parcel_addr %>%
    transmute(PIN,
              ADDRESS_FULL = ADDR_FULL,
              CITY_NAME = CTYNAME,
              CITY_NAME_POSTAL = POSTALCTYN,
              ZIPCODE = factor(ZIP_5),
              PRIMARY_ADDR_LGL = as.logical(PRIM_ADDR)) %>%
    mutate(CITY_NAME = factor(case_when(
                all(is.na(CITY_NAME),is.na(CITY_NAME_POSTAL)) ~ NA_character_,
                is.na(CITY_NAME) ~ CITY_NAME_POSTAL,
                TRUE ~ CITY_NAME
              ))) %>%
  group_by(PIN) %>%
    arrange(desc(PRIMARY_ADDR_LGL)) %>%
    slice(1) %>%
    ungroup

 
  
  # MAKE P_READY 
  
  recode_cols <- unique(parcel_lookup$FIELD_NAME) %>% keep(~.x %in% colnames(parcel_df))
  
  fine_cols <- colnames(parcel_df) %>% discard(~.x %in% recode_cols)
  
  
  parcel_recode_cols <- 
    parcel_df %>%  
    select_at(vars(recode_cols)) %>% 
    gather(FIELD_NAME, LU_ITEM) %>% 
    left_join(parcel_lookup, by = c("FIELD_NAME", "LU_ITEM")) %>% 
    select(FIELD_NAME, LU_DESCRIPTION) %>% 
    group_by(FIELD_NAME) %>% 
    mutate(ROW = 1:n()) %>% 
    spread(FIELD_NAME,LU_DESCRIPTION) %>% 
    select(-ROW)
  
  parcel_df_recoded <- 
    parcel_df %>% 
    select_at(vars(fine_cols)) %>% 
    bind_cols(parcel_recode_cols) %>% 
    mutate_if(is_logical_yn, recode_logical_yn) %>% 
    mutate_if(is_logical_01, recode_logical_01) %>% 
    mutate_if(is_logical_yesno, recode_logical_yesno)
 

  pub_parcel_ready <- pub_parcel %>%
    transmute(PIN,
              ASSESSOR_PUB_LIST_LGL = TRUE)

  p_ready <- parcel_df_recoded %>%
   mutate(MAJOR = str_pad(string = MAJOR,width = 6,side = "left",pad = "0"),
           MINOR = str_pad(string = MINOR,width = 4,side = "left",pad = "0"),
           PIN = str_c(MAJOR,MINOR)) %>%
    left_join(prop_type, by = "PROP_TYPE") %>%
    left_join(pub_parcel_ready, by = "PIN") %>%
    mutate(ASSESSOR_PUB_LIST_LGL = if_else(is.na(ASSESSOR_PUB_LIST_LGL),FALSE,ASSESSOR_PUB_LIST_LGL)) %>%  
    select(PIN,
           PROP_NAME,
           PROP_TYPE = PROP_TYPE_DESC,
           ASSESSOR_PUB_LIST_LGL,
           DISTRICT_NAME,
           CURRENT_ZONING,
           PRESENT_USE,
           SQ_FT_LOT,
           ACCESS,
           TOPOGRAPHY,
           RESTRICTIVE_SZ_SHAPE,
           PCNT_UNUSABLE,
           CONTAMINATION,
           HISTORIC_SITE,
           CURRENT_USE_DESIGNATION,
           NATIVE_GROWTH_PROT_ESMT,
           EASEMENTS,
           OTHER_DESIGNATION,
           DEED_RESTRICTIONS,
           DEVELOPMENT_RIGHTS_PURCH,
           COAL_MINE_HAZARD,
           CRITICAL_DRAINAGE,
           EROSION_HAZARD,
           LANDFILL_BUFFER,
           HUNDRED_YR_FLOOD_PLAIN,
           SEISMIC_HAZARD,
           LANDSLIDE_HAZARD,
           STEEP_SLOPE_HAZARD,
           STREAM,
           WETLAND,
           SPECIES_OF_CONCERN,
           SENSITIVE_AREA_TRACT,
           WATER_PROBLEMS,
           TRANSP_CONCURRENCY,
           OTHER_PROBLEMS 
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

  obj_list <- list(acct_ready,p_addr_ready,p_ready,p_sf_ready)

  parcel_ready <- obj_list %>%
    reduce(.f = inner_join, by = "PIN") %>%
    st_as_sf()

  return(parcel_ready)
}

# COMMAND: MAKE_WATERBODIES ----

make_waterbodies <- function(){
  
  w_fp <- here("./1-data/2-external/waterbodies-kc.gpkg")
  
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
  
  uga_fp <- here("1-data/2-external/uga")
  
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
                    
                    zip_dir <- here("1-data/2-external/uga")
                    
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
  
  zoning_fp <- here("1-data/2-external/zoning_kc_consol_20")
  
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
                    
                    zip_dir <- here("1-data/2-external/")
                    
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
  
  tr_fp <- here("1-data/2-external/kc_census_tracts.gpkg")
  
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

# COMMAND: MAKE_KING_COUNTY ----
make_king_county <- function(){
  
  kc_fp <- here("1-data/2-external/kc.gpkg")
  
  kc_dr_id <- as_id("1dcxPI9uV8G6yQYIGLyfGQXhrOBC9bJW_")
  
  kc_load <- 
    make_or_read2(fp = kc_fp,
                  dr_id = kc_dr_id,
                  skip_get_expr = FALSE,
                  get_expr = function(fp){
                    
                    kc_id <- 53
                    
                    kc <- kc_id %>% 
                      counties() %>% 
                      filter(NAME %in% "King")
                    
                    st_write(kc, fp, driver = "GPKG")
                  
                    drive_folder <- as_id("0B5Pp4V6eCkhrdlJ3MXVaNW16T0U")
                  
                    drive_upload(fp, drive_folder)
                      
                  },
                  make_expr = function(fp, dr_id){
                    drive_read(dr_id = dr_id,.tempfile = FALSE,path = fp,read_fun = read_sf,stringsAsFactors = FALSE)
                  },
                  read_expr = function(fp){read_sf(fp,stringsAsFactors = FALSE)})
  
  king_county <- kc_load %>% st_transform(2926)
  
  return(king_county)
  
}
# COMMAND: MAKE_ZCTA ----
make_zcta <- function(king_county){
  
  zcta_fp <- here("1-data/2-external/zcta.gpkg")
  
  zcta_dr_id <- as_id("1QyJT9tIHGuMrRIx-i9DCQK4kroIY_X-7")
  
  zcta_load <- 
    make_or_read2(fp = zcta_fp,
                  dr_id = zcta_dr_id,
                  skip_get_expr = FALSE,
                  get_expr = function(fp){
                    
                    zcta <- zctas()
                    
                    st_write(zcta, fp, driver = "GPKG")
                    
                    zip_path <- here("1-data/2-external/zcta.zip")
                    
                    zip_pithy(zip_path, fp)
                    
                    drive_folder <- as_id("0B5Pp4V6eCkhrdlJ3MXVaNW16T0U")
                  
                    drive_upload(zip_path, drive_folder)
                      
                  },
                  make_expr = function(fp, dr_id){
                    zip_dir <- here("1-data/2-external")
                    
                    target_name <- "zcta.gpkg"
                    
                    drive_read_zip(dr_id = dr_id,
                                   dir_path = zip_dir,
                                   read_fun = st_read,
                                   target_name = target_name,
                                   .tempdir = FALSE, 
                                   stringsAsFactors = FALSE)
                  },
                  read_expr = function(fp){read_sf(fp,stringsAsFactors = FALSE)})
  
  buff_dist <- 5280*2 # 2 miles in ft
  max_vertices <- 256
  
  kc_buff <-  king_county %>% 
    st_buffer(buff_dist) %>% 
    st_subdivide(max_vertices) %>% 
    st_collection_extract()
  
  zcta_2926 <- st_transform(zcta_load, 2926) 
  
  zcta_2926$geom_pt <- st_centroid(st_geometry(zcta_2926))
  
  zcta_2926$ZCTA_WITHIN_KC <- st_intersects_any(zcta_2926$geom_pt,kc_buff)
  
  zcta <- filter(zcta_2926, ZCTA_WITHIN_KC) 
  
  return(zcta)
  
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
    dplyr::filter(DEVELOPABLE_LGL) %>% 
    pull(CONSOL_20)
  
  crit_dz <- list("developable_zoning" = dz)
  
  criteria_developable_zoning <-  crit_dz
  
  return(criteria_developable_zoning)
  
}

# COMMAND: MAKE_CRITERIA_UNDEVELOPABLE_PRESENT_USE ----

make_criteria_undevelopable_present_use <- function(){
  
   # list_uses <- function(){
   #                    parcel_ready %>% 
   #                      st_drop_geometry() %>% 
   #                      count(PRESENT_USE, sort = TRUE) %>% 
   #                      print(n = Inf)
   #                  }
   
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
     "Tideland, 2nd Class",
     "Air Terminal and Hangers",
     "Terminal(Marine/Comm Fish)",
     "River/Creek/Stream",
     "Art Gallery/Museum/Soc Srvc"
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
    dplyr::filter( AREA_SQ_KM > min_sq_km) %>% 
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
    ) %>% 
    st_sf
  
  return(suitability)
  
}
# COMMAND: MAKE_BUILDING_RESIDENTIAL ----

make_building_residential <- function(){ 
  
  bldg_res_fp <- here(res_fp <- "1-data/2-external/EXTR_ResBldg.csv")
  
  bldg_res_dr_id <- as_id("10rz6hc4lEAaaU-0Jcv0iCiVMFBVTc-en")
  
  bldg_res_load <- 
    make_or_read2(fp = bldg_res_fp,
                  dr_id = bldg_res_dr_id,
                  skip_get_expr = TRUE,
                  get_expr = function(fp){
                    # SOURCE: url here
                  },
                  make_expr = function(fp, dr_id){
                    
                    zip_dir <- here("1-data/2-external")
                    
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
  
  bldg_apt_fp <- here(apt_fp <- "1-data/2-external/EXTR_AptComplex.csv")
  
  bldg_apt_dr_id <- as_id("11kkudStD4TuoiRqMie-Y4_8tZQJmLPBw")
  
  bldg_apt_load <- 
    make_or_read2(fp = bldg_apt_fp,
                  dr_id = bldg_apt_dr_id,
                  skip_get_expr = TRUE,
                  get_expr = function(fp){
                    # SOURCE: url here
                  },
                  make_expr = function(fp, dr_id){
                    
                    zip_dir <- here("1-data/2-external")
                    
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
  
  bldg_condo_fp <- here(condo_fp <- "1-data/2-external/EXTR_CondoComplex.csv")
  
  bldg_condo_dr_id <- as_id("1avYhRKzHijnc-YZQDGICqWrQNhQoTwnB")
  
  bldg_condo_load <- 
    make_or_read2(fp = bldg_condo_fp,
                  dr_id = bldg_condo_dr_id,
                  skip_get_expr = TRUE,
                  get_expr = function(fp){
                    # SOURCE: url here
                  },
                  make_expr = function(fp, dr_id){
                    
                    zip_dir <- here("1-data/2-external")
                    
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
  
  bldg_comm_fp <- here(condo_fp <- "1-data/2-external/EXTR_CommBldg.csv")
  
  bldg_comm_dr_id <- as_id("1VT_plwHQve51ldIg3chFUpcTSMD_ZyrN")
  
  bldg_comm_load <- 
    make_or_read2(fp = bldg_comm_fp,
                  dr_id = bldg_comm_dr_id,
                  skip_get_expr = TRUE,
                  get_expr = function(fp){
                    # SOURCE: url here
                  },
                  make_expr = function(fp, dr_id){
                    
                    zip_dir <- here("1-data/2-external")
                    
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

# COMMAND: MAKE_FILTERS_ZCTA ----
make_filters_zcta <- function(parcel_ready, zcta){
  
  p <- parcel_ready %>% select(PIN)
  
  zcta_subdivide <- st_subdivide(zcta, 100) %>% 
    st_collection_extract() %>% 
    transmute(ZCTA = ZCTA5CE10)
   
  p$ZCTA <- st_over(p$geom_pt,zcta_subdivide, "ZCTA") 
  
  p_ready <- st_drop_geometry(p) 
  
  filters_zcta <- p_ready
  
  return(filters_zcta)
   
}

# COMMAND: MAKE_FILTERS_PUBLIC_OWNER ----
make_filters_public_owner <- function(parcel_ready){
  
  # THIS IS DUMMY DATA + SHOULD BE REPLACED
  
  p_ready_po <- parcel_ready %>% 
    st_drop_geometry() %>% 
    transmute(PIN, 
              FILTER_PUBLIC_OWNER = str_trim(TAXPAYER_NAME))
  
  filters_public_owner <- p_ready_po
  
  return(filters_public_owner)
   
}

# COMMAND: MAKE_FILTERS_SURPLUS_STATUS ----
make_filters_surplus_status <- function(parcel_ready){
  
  # THIS IS DUMMY DATA + SHOULD BE REPLACED
  
  p_ready_ss <- parcel_ready %>% 
    st_drop_geometry() %>% 
    transmute(PIN, 
              FILTER_SURPLUS_STATUS_LGL = sample(c(TRUE,FALSE),n(),replace = TRUE))
  
  filters_surplus_status <- p_ready_ss
  
  return(filters_surplus_status)
   
}

# COMMAND: MAKE_FILTERS_PROXIMITY_MARIJUANA ----
make_filters_proximity_marijuana <- function(parcel_ready){
  
  # THIS IS DUMMY DATA + SHOULD BE REPLACED
  
  
  p_ready_prox_mj <- parcel_ready %>% 
    st_drop_geometry() %>% 
    transmute(PIN, 
              FILTER_PROXIMITY_MARIJUANA = spatial_dummy(n(), mean = 25, sd = 10)
              )
  
  filters_proximity_marijuana <- p_ready_prox_mj
  
  return(filters_proximity_marijuana)
   
}

# COMMAND: MAKE_FILTERS_PROXIMITY_PRESCHOOL ----
make_filters_proximity_preschool <- function(parcel_ready){
  
  # THIS IS DUMMY DATA + SHOULD BE REPLACED
  
  
  p_ready_prox_ps <- parcel_ready %>% 
    st_drop_geometry() %>% 
    transmute(PIN, 
              FILTER_PROXIMITY_PRESCHOOL = spatial_dummy(n(), mean = 10, sd = 10)
              )
  
  filters_proximity_preschool <- p_ready_prox_ps
  
  return(filters_proximity_preschool)
   
}

# COMMAND: MAKE_FILTERS_PROXIMITY_OPEN_SPACE ----
make_filters_proximity_open_space <- function(parcel_ready){
  
  # THIS IS DUMMY DATA + SHOULD BE REPLACED  
  
   p_ready_prox_os<- parcel_ready %>% 
    st_drop_geometry() %>% 
    transmute(PIN, 
              FILTER_PROXIMITY_OPEN_SPACE = spatial_dummy(n(), mean = 5, sd = 5)
              )
  
  filters_proximity_open_space <- p_ready_prox_os
  
  return(filters_proximity_open_space)
   
}

# COMMAND: MAKE_FILTERS_POTENTIAL_UNITS ----
make_filters_potential_units <- function(parcel_ready){
  
  # THIS IS DUMMY DATA + SHOULD BE REPLACED  
  
   p_ready_pu<- parcel_ready %>% 
    st_drop_geometry() %>% 
    transmute(PIN, 
              FILTER_POTENTIAL_UNITS = integer_dummy(n(),10,5)
              )
  
  filters_potential_units <- p_ready_pu
  
  return(filters_potential_units)
   
}

# COMMAND: MAKE_FILTERS_LEG_DISTRICT ----
make_filters_leg_district <- function(parcel_ready){
  
  # THIS IS DUMMY DATA + SHOULD BE REPLACED  
  
  leg_districts <- c(34,11,37,43,46,36,32,33,45,41,30,1,47,5,31,48,39) %>% 
    map_chr(~str_c("District ",.x))
  
   p_ready_ld<- parcel_ready %>% 
    st_drop_geometry() %>% 
    transmute(PIN, 
              FILTER_LEG_DISTRICT = sample(leg_districts,n(), replace = TRUE)
              )
  
  filters_leg_district <- p_ready_ld
  
  return(filters_leg_district)
   
}

# COMMAND: MAKE_FILTERS_SCHOOL_DISTRICT ----
make_filters_school_district <- function(parcel_ready){
  
  # THIS IS DUMMY DATA + SHOULD BE REPLACED  
  
  school_districts <- c(
"Seattle", "Federal Way", "Enumclaw", "Mercer Island", "Highline", "Vashon Island", "Renton", "Skykomish", "Bellevue", "Tukwila", "Riverview", "Auburn", "Tahoma", "Snoqualmie Valley", "Issaquah", "Shoreline", "Lake Washington", "Kent", "Northshore", "Fife"
)

  
   p_ready_sd<- parcel_ready %>% 
    st_drop_geometry() %>% 
    transmute(PIN, 
              FILTER_SCHOOL_DISTRICT = sample(school_districts,n(), replace = TRUE)
              )
  
  filters_school_district <- p_ready_sd
  
  return(filters_school_district)
   
}

# COMMAND: MAKE_FILTERS_HISTORIC ----
make_filters_historic <- function(parcel_ready){
  
  # THIS IS DUMMY DATA + SHOULD BE REPLACED  
 
   p_ready_hist<- parcel_ready %>% 
    st_drop_geometry() %>% 
    transmute(PIN, 
              FILTER_HISTORIC_LGL = sample(c(TRUE,FALSE),n(), replace = TRUE)
              )
  
  filters_historic <- p_ready_hist
  
  return(filters_historic)
   
}

# COMMAND: MAKE_FILTERS_AFFORD_EXPIR_DATE ----
make_filters_afford_expir_date <- function(parcel_ready){
  
  # THIS IS DUMMY DATA + SHOULD BE REPLACED  
 
  sample_dates <- c("20181231","20191231","20201231") %>% 
    map(ymd) %>% 
    map_dbl(pluck,1) %>% 
    as_date
  
   p_ready_afford_expir_date <- parcel_ready %>% 
    st_drop_geometry() %>% 
    transmute(PIN, 
              FILTER_AFFORD_EXPIR_DATE = sample(sample_dates,n(), replace = TRUE)
              )
  
  filters_afford_expir_date <- p_ready_afford_expir_date
  
  return(filters_afford_expir_date)
   
}

# COMMAND: MAKE_FILTERS_ELIGIBILITY_NMTC ----
make_filters_eligibility_nmtc <- function(filters_census_tract){
  
  elig_nmtc_fp <- here("1-data/2-external/NMTC-2011-2015-LIC-Nov2-2017-4pm.xlsx")
  
  elig_nmtc_dr_id <- as_id("1gM7oRPZTEDM4N7Xhcmih8R4WFTxLj8rK")
  
  elig_nmtc_load <- 
    make_or_read2(fp = elig_nmtc_fp,
                  dr_id = elig_nmtc_dr_id,
                  skip_get_expr = FALSE,
                  get_expr = function(fp){
                    # SOURCE: https://www.cdfifund.gov/Documents/NMTC%202011-2015%20LIC%20Nov2-2017-4pm.xlsx
                  },
                  make_expr = function(fp, dr_id){
                    drive_read(dr_id = dr_id,  path = fp, read_fun = read_xlsx,.tempfile = FALSE)
                  },
                  read_expr = function(fp){read_xlsx(fp)})
  
  elig_nmtc <- elig_nmtc_load %>% 
    clean_names() %>% 
    rename_all(to_screaming_snake_case) %>% 
    transmute(CENSUS_TRACT = X_2010_CENSUS_TRACT_NUMBER_FIPS_CODE_GEOID,
              NMTC = DOES_CENSUS_TRACT_QUALIFY_FOR_NMTC_LOW_INCOME_COMMUNITY_LIC_ON_POVERTY_OR_INCOME_CRITERIA,
              FILTER_ELIGIBILITY_NMTC = if_else(NMTC %in% "Yes",TRUE,FALSE)
              ) 
  
  
   p_ready_eligibility_nmtc <- filters_census_tract %>% 
    st_drop_geometry() %>% 
    select(PIN, CENSUS_TRACT) %>% 
    left_join(elig_nmtc, by = "CENSUS_TRACT") %>% 
    select(PIN,
           FILTER_ELIGIBILITY_NMTC)
  
  filters_eligibility_nmtc <- p_ready_eligibility_nmtc
  
  return(filters_eligibility_nmtc)
   
}

# COMMAND: MAKE_FILTERS_ELIGIBILITY_DDA ----
make_filters_eligibility_dda <- function(filters_zcta){
  
  elig_dda_fp <- here("1-data/2-external/DDA2018M.PDF")
  
  elig_dda_dr_id <- as_id("1KbD_gAHy_0DTTxHZgTbL96VOviTwdNRZ")
  
  make_or_read2(fp = elig_dda_fp,
                  dr_id = elig_dda_dr_id,
                  skip_get_expr = TRUE,
                  get_expr = function(fp){
                    # SOURCE:  https://www.huduser.gov/portal/Datasets/qct/DDA2018M.PDF
                  },
                  make_expr = function(fp, dr_id){
                    
                    drive_download(dr_id, path = fp)
                  
                  },
                  read_expr = function(fp){
                    message(glue("* Note: This file is not actually read but it does exists here: '{fp}'.")) 
                    })
  
  list_pages <- extract_tables(elig_dda_fp, method = "data.frame")
  
  tbl_pages <- list_pages %>% 
    map(~ .x %>% t %>% as_tibble) %>% 
    reduce(bind_cols) %>% 
    mutate_all(funs(empty_as_na))
  
  zcta_dda <- tbl_pages %>% 
    slice(3:14) %>% 
    gather(OLD_COL,ZCTA) %>% 
    drop_na() %>% 
    transmute(ZCTA = str_replace(ZCTA,"\\*",""),
              FILTER_ELIGIBILITY_DDA = TRUE) 
  
  elig_dda <- filters_zcta %>% 
    left_join(zcta_dda, by = "ZCTA") %>% 
    transmute(PIN,
              FILTER_ELIGIBILITY_DDA = if_else(is.na(FILTER_ELIGIBILITY_DDA),FALSE,FILTER_ELIGIBILITY_DDA))
  
  filters_eligibility_dda <- elig_dda
  
  return(filters_eligibility_dda)
}
  

# COMMAND: MAKE_FILTERS_ELIGIBILITY_QTC ----
make_filters_eligibility_qct <- function(filters_census_tract){
  
  elig_qtc_fp <- here("1-data/2-external/QCT2018.DBF")
  
  elig_qtc_dr_id <- as_id("1JU0MQKta1mQurT89DJtk8nFvYcgxP4qk")
  
  elig_qtc_load <- 
    make_or_read2(fp = elig_qtc_fp,
                  dr_id = elig_qtc_dr_id,
                  skip_get_expr = FALSE,
                  get_expr = function(fp){
                    # SOURCE:  https://www.huduser.gov/portal/datasets/qct/QCT2018dbf.zip
                  },
                  make_expr = function(fp, dr_id){
                    
                    target_name <- "QCT2018.DBF"
                    
                    dir_path <- here("1-data/2-external/")
                    
                    drive_read_zip(dr_id = dr_id,
                                   .tempdir = FALSE,
                                   dir_path = dir_path,
                                   read_fun = foreign::read.dbf,
                                   target_name = target_name)
                  },
                  read_expr = function(fp){foreign::read.dbf(fp)})
  
  elig_qtc <- elig_qtc_load %>% 
    transmute(CENSUS_TRACT = as.character(FIPS),
              FILTER_ELIGIBILITY_QCT = TRUE)
    
  
  p_ready_eligibility_qct <- filters_census_tract %>% 
    st_drop_geometry() %>% 
    select(PIN, CENSUS_TRACT) %>% 
    left_join(elig_qtc, by = "CENSUS_TRACT") %>% 
    transmute(PIN,
              FILTER_ELIGIBILITY_QCT = if_else(is.na(FILTER_ELIGIBILITY_QCT),FALSE,FILTER_ELIGIBILITY_QCT))
  
  filters_eligibility_qct <- p_ready_eligibility_qct
  
  return(filters_eligibility_qct)
  
}

# COMMAND: MAKE_FILTERS ----
make_filters <- function(parcel_ready, ...){
  
  filter_list <- list(...) %>% 
    reduce(left_join, by = "PIN")
  
  filters <- parcel_ready %>% 
    st_drop_geometry() %>% 
    select(PIN) %>% 
    left_join(filter_list, by = "PIN") 
  
  return(filters)
    
  
}


# COMMAND: MAKE_HELPERS_URL_PARCEL_VIEWER ----
make_helpers_url_parcel_viewer <- function(parcel_ready){
  
  url <- "http://blue.kingcounty.com/Assessor/eRealProperty/Detail.aspx?ParcelNbr="
  
  url_pv <- parcel_ready %>% 
    st_drop_geometry() %>% 
    transmute(PIN,
              HELPERS_URL_PARCEL_VIEWER = str_c(url,PIN,sep = ""))
  
 helpers_url_parcel_viewer <- url_pv
 
 return(helpers_url_parcel_viewer)
  
  
}



# COMMAND: MAKE_HELPERS_OPP360_XWALK ----
make_helpers_opp360_xwalk <- function(){
  opp360_xwalk_fp <- "./1-data/2-external/PolicyMap_FIPS_URL_Crosswalk.xlsx"
  
  opp360_xwalk_dr_id <- as_id("1H-9j4zFpR72bcf971V_dDI1ZD9I2rAKI")
  
  opp360_xwalk_load <- 
    make_or_read2(fp = opp360_xwalk_fp, 
                  dr_id = opp360_xwalk_dr_id, 
                  skip_get_expr = TRUE,
                  get_expr = function(fp){
                    # SOURCE: KIS Team (email from Zach Patton on Feb. 13, 2018)
                  },
                  make_expr = function(fp,dr_id){ 
                    drive_read(dr_id = dr_id,.tempfile = FALSE,path = fp,read_fun = read_xlsx)
                  },
                  read_expr = function(fp){read_xlsx(fp)}
    )
  
  opp360_xwalk <-
    opp360_xwalk_load %>%
    rename_all(to_screaming_snake_case) 
  
  return(opp360_xwalk)
  
}


# COMMAND: MAKE_HELPERS_URL_OPP360 ----
make_helpers_url_opp360 <- function(filters_census_tract, helpers_opp360_xwalk){ 
  
  url_opp360 <- filters_census_tract %>%  
    left_join(helpers_opp360_xwalk, by = c("CENSUS_TRACT" = "FIPS_TEXT")) %>% 
    transmute(PIN, 
           HELPERS_URL_OPP360 = URL,
           HELPERS_PID_OPP360 = as.character(PID)) 
  
 helpers_url_opp360 <- url_opp360
 
 return(helpers_url_opp360)
  
  
}

# COMMAND: MAKE_HELPERS ----
make_helpers <- function(...){
  
  helpers <- reduce(list(...), left_join, by = "PIN")
  
  return(helpers)
}
# COMMAND: MAKE_INVENTORY ----

make_inventory <- function(filters, helpers, suitability,  utilization){
  
  inv <- list(suitability, filters, helpers, utilization) %>% 
    reduce(left_join, by = "PIN") %>%  
    st_sf
  
  inventory <- inv
  
  return(inventory)
  
}

# COMMAND: MAKE_INVENTORY_SUITABLE ----

make_inventory_suitable <- function(inventory){
  
  inv_suit <- dplyr::filter(inventory, SUITABLE_LGL)
  
  inventory_suitable <- inv_suit
  
  return(inventory_suitable)
  
}


# COMMAND: MAKE_INVENTORY_SUITABLE_POLY ----

make_inventory_suitable_poly <- function(inventory_suitable){
  
  inventory_suitable_poly <- inventory_suitable %>% 
    select(PIN,geometry)
  
  return(inventory_suitable_poly)
}

# COMMAND: MAKE_INVENTORY_SUITABLE_POINT ----

make_inventory_suitable_point <- function(inventory_suitable){
  
  inventory_suitable_point <- inventory_suitable %>% 
    st_set_geometry("geom_pt") %>% 
    select(PIN, geom_pt)
  
  return(inventory_suitable_point)
}


# COMMAND: MAKE_DD_FIELD_NAME_DEV ----

make_dd_field_name_dev <- function(inventory){
  name_dev <- inventory %>% 
    st_drop_geometry() %>% 
    select_if(not_sfc) %>% 
    names() %>% 
    {tibble("FIELD_NAME_DEV" = .)}
  
  dd_field_name_dev <- name_dev
  
  return(dd_field_name_dev)
}

# COMMAND: MAKE_DD_FIELD_NAME_USER ----

make_dd_field_name_user <- function(){ 
  
  dd_field_name_user <- 
    tibble::tribble(
                            ~FIELD_NAME_DEV,                                         ~FIELD_NAME_USER,
                                      "PIN",                           "Parcel Identification Number",
                             "CENSUS_TRACT",                                           "Census Tract",
                      "FILTER_PUBLIC_OWNER",                                   "Name of Public Owner",
                "FILTER_SURPLUS_STATUS_LGL",                                         "Surplus Status",
               "FILTER_PROXIMITY_MARIJUANA",                      "Proximity to Marijunan Businesses",
               "FILTER_PROXIMITY_PRESCHOOL",                                 "Proximity to Preschool",
              "FILTER_PROXIMITY_OPEN_SPACE",                                "Proximity to Open Space",
                   "FILTER_SCHOOL_DISTRICT",                                        "School District",
                      "FILTER_HISTORIC_LGL",                          "Historic Property or Landmark",
                 "FILTER_AFFORD_EXPIR_DATE",                              "Affordability Expir. Date",
                  "FILTER_ELIGIBILITY_NMTC",                              "Funding Eligibility: NMTC",
                   "FILTER_ELIGIBILITY_DDA",                               "Funding Eligibility: DDA",
                   "FILTER_ELIGIBILITY_QCT",                               "Funding Eligibility: CTC",
                "HELPERS_URL_PARCEL_VIEWER",                               "Link to KC Parcel Viewer",
                       "HELPERS_URL_OPP360",                                "Link to Opportunity 360",
                            "TAXPAYER_NAME",                                          "Taxpayer Name",
                                  "BILL_YR",                                          "Tax Bill Year",
                               "TAX_STATUS",                                             "Tax Status",
                               "TAX_REASON",                                   "Tax Status Reason(s)",
                            "APPR_IMPS_VAL",                        "Tax Appraised Improvement Value",
                            "APPR_LAND_VAL",                               "Tax Appraised Land Value",
                         "TAXABLE_IMPS_VAL",                          "Tax Taxable Improvement Value",
                         "TAXABLE_LAND_VAL",                                 "Tax Taxable Land Value",
                             "ADDRESS_FULL",                                         "Street Address",
                                "CITY_NAME",                                              "City Name",
                         "CITY_NAME_POSTAL",                                     "City Name (Postal)",
                                  "ZIPCODE",                                                "Zipcode",
                         "PRIMARY_ADDR_LGL",                   "T/F: Primary Address for the Parcel?",
                                "PROP_NAME",                                          "Property Name",
                                "PROP_TYPE",                                          "Property Type",
                    "ASSESSOR_PUB_LIST_LGL",                   "T/F: Assessor Public-Ownership List?",
                            "DISTRICT_NAME",                                          "District Name",
                           "CURRENT_ZONING",                                         "Current Zoning",
                              "PRESENT_USE",                                            "Present Use",
                                "SQ_FT_LOT",                                        "Lot Size (SqFt)",
                                   "ACCESS",                                "Lot Accessibility Level",
                               "TOPOGRAPHY",                                       "T/F: Topography?",
                     "RESTRICTIVE_SZ_SHAPE",                           "T/F: Restrictive Size/Shape?",
                            "PCNT_UNUSABLE",                                   "Lot Percent Unusable",
                            "CONTAMINATION",                                     "T/F: Contaminated?",
                            "HISTORIC_SITE",                                    "T/F: Historic Site?",
                  "CURRENT_USE_DESIGNATION",                                "Current Use Designation",
                  "NATIVE_GROWTH_PROT_ESMT",                 "T/F: Native Growth Protection Estimate",
                                "EASEMENTS",                                        "T/F :Easements?",
                        "OTHER_DESIGNATION",                               "T/F: Other Designations?",
                        "DEED_RESTRICTIONS",                                "T/F: Deed Restrictions?",
                 "DEVELOPMENT_RIGHTS_PURCH",                     "T/F: Development Rights Purchased?",
                         "COAL_MINE_HAZARD",                                 "T/F: Coal Mine Hazard?",
                        "CRITICAL_DRAINAGE",                                "T/F: Critical Drainage?",
                           "EROSION_HAZARD",                                   "T/F: Erosion Hazard?",
                          "LANDFILL_BUFFER",                           "T/F: Within Landfill Buffer?",
                   "HUNDRED_YR_FLOOD_PLAIN",                      "T/F: Within 100 Year Flood Plain?",
                           "SEISMIC_HAZARD",                                   "T/F: Seismic Hazard?",
                         "LANDSLIDE_HAZARD",                                 "T/F: Landslide Hazard?",
                       "STEEP_SLOPE_HAZARD",                               "T/F: Steep Slope Hazard?",
                                   "STREAM",                                           "T/F: Stream?",
                                  "WETLAND",                                          "T/F: Wetland?",
                       "SPECIES_OF_CONCERN",                               "T/F: Species of Concern?",
                     "SENSITIVE_AREA_TRACT",                             "T/F: Sensitive Area Tract?",
                           "WATER_PROBLEMS",                                   "T/F: Water Problems?",
                       "TRANSP_CONCURRENCY",                       "T/F: Transportation Concurrency?",
                           "OTHER_PROBLEMS",                                   "T/F: Other Problems?",
                        "SUIT_OWNER_PUBLIC",                                 "T/F: Public Ownership?",
                     "SUIT_OWNER_NONPROFIT",                             "T/F: Non-Profit Ownership?",
                         "SUIT_OWNER_TAX_E",                             "T/F: Tax-Exempt Ownership?",
                   "SUIT_WATER_OVERLAP_LGL",                         "T/F: Overlaped by Waterbodies?",
                   "SUIT_WATER_OVERLAP_PCT",                "T/F: Percent Overlapped by Waterbodies?",
                          "SUIT_WITHIN_UGA",                     "T/F: Within the Urban Growth Area?",
                    "SUIT_ZONING_CONSOL_20",                    "Zoning (KC Consolidated Categories)",
                         "SUIT_PRESENT_USE",                                            "Present Use",
                       "SUITABLE_OWNER_LGL",                               "T/F: Suitable Ownership?",
               "SUITABLE_WATER_OVERLAP_LGL",                           "T/F: Suitable Water Overlap?",
                  "SUITABLE_WITHIN_UGA_LGL",              "T/F: Suitable Urban Growth Area Distance?",
            "SUITABLE_ZONING_CONSOL_20_LGL",                                  "T/F: Suitable Zoning?",
                 "SUITABLE_PRESENT_USE_LGL",                             "T/F: Suitable Present Use?",
                             "SUITABLE_LGL",                                    "T/F: Suitable Site?",
                                 "BLDG_NBR",                                    "Number of Buildings",
                           "BLDG_NET_SQ_FT",                           "Net Area (SqFt) of Buildings",
                        "BLDG_LIVING_UNITS",                             "Net Number of Living Units",
                  "BLDG_TYPE_APARTMENT_LGL",                       "T/F: Apartment Building On-Site?",
                 "BLDG_TYPE_COMMERCIAL_LGL",                      "T/F: Commercial Building On-Site?",
                      "BLDG_TYPE_CONDO_LGL",                           "T/F: Condo Building On-Site?",
                "BLDG_TYPE_RESIDENTIAL_LGL",                     "T/F: Residential Building On-Site?",
                             "UTIL_PRESENT",                             "Present Utilization (Sqft)",
                                "LOT_SQ_FT",                                        "Lot Size (SqFt)",
                            "LOT_SIZE_DESC",                                   "Lot Size Description",
                            "LOT_SIZE_TYPE",                             "Lot Size Devevlopment Type",
                         "LOT_COVERAGE_PCT",                          "Percent Developable (Assumed)",
                          "LOT_STORIES_NBR",                "Number of Developable Stories (Assumed)",
                 "UTIL_LOT_DEVELOPABLE_PCT",      "Percent Developable (Assumed) and Not Under Water",
                     "UTIL_DEVELOPABLE_LGL",  "T/F: Developable Zoning (KC Consolidated Categories)?",
              "UTIL_DEVELOPMENT_ASSUMPTION",                         "Development Scenario (Assumed)",
            "UTIL_DEVELOPABLE_ESTIMATE_LGL",            "T/F: In Zoning With A Development Scenario?",
        "UTIL_DEVELOPABLE_LOT_COVERAGE_PCT",                          "Percent Developable (Assumed)",
          "UTIL_POTENTIAL_UTILIZATION_SQFT",                           "Potential Utilization (Sqft)",
                  "UTIL_UNDER_UTILIZED_LGL",    "T/F: Is Potential Utilization Greater than Present?",
                              "UTILIZATION",                                   "Utilization Category"
       )

return(dd_field_name_user)
}

# COMMAND: MAKE_DD_FIELD_TAGS ----

make_dd_field_tags <- function(){ 

 
  
  dd_field_tags <- 
     tibble::tribble(
                             ~FIELD_NAME_DEV, ~FIELD_TAG_SENSITIVE, ~FIELD_TAG_FILTER, ~FIELD_TAG_TOOLTIP, ~FIELD_TAG_TABLE, ~FIELD_TAG_DUMMY,
                                       "PIN",                FALSE,             FALSE,               TRUE,             TRUE,            FALSE,
                              "CENSUS_TRACT",                FALSE,              TRUE,              FALSE,             TRUE,            FALSE,
                       "FILTER_PUBLIC_OWNER",                FALSE,              TRUE,              FALSE,            FALSE,             TRUE,
                 "FILTER_SURPLUS_STATUS_LGL",                FALSE,              TRUE,              FALSE,            FALSE,             TRUE,
                "FILTER_PROXIMITY_MARIJUANA",                FALSE,              TRUE,              FALSE,            FALSE,             TRUE,
                "FILTER_PROXIMITY_PRESCHOOL",                FALSE,              TRUE,              FALSE,            FALSE,             TRUE,
               "FILTER_PROXIMITY_OPEN_SPACE",                FALSE,              TRUE,              FALSE,            FALSE,             TRUE,
                    "FILTER_SCHOOL_DISTRICT",                FALSE,              TRUE,              FALSE,            FALSE,             TRUE,
                       "FILTER_HISTORIC_LGL",                FALSE,              TRUE,              FALSE,            FALSE,             TRUE,
                  "FILTER_AFFORD_EXPIR_DATE",                FALSE,              TRUE,              FALSE,            FALSE,             TRUE,
                   "FILTER_ELIGIBILITY_NMTC",                FALSE,              TRUE,              FALSE,            FALSE,             FALSE,
                    "FILTER_ELIGIBILITY_DDA",                FALSE,              TRUE,              FALSE,            FALSE,             FALSE,
                    "FILTER_ELIGIBILITY_QCT",                FALSE,              TRUE,              FALSE,            FALSE,             FALSE,
                 "HELPERS_URL_PARCEL_VIEWER",                FALSE,             FALSE,               TRUE,             TRUE,            FALSE,
                        "HELPERS_URL_OPP360",                FALSE,             FALSE,               TRUE,             TRUE,            FALSE,
                             "TAXPAYER_NAME",                 TRUE,              TRUE,               TRUE,             TRUE,            FALSE,
                                   "BILL_YR",                FALSE,             FALSE,              FALSE,             TRUE,            FALSE,
                                "TAX_STATUS",                FALSE,             FALSE,              FALSE,             TRUE,            FALSE,
                                "TAX_REASON",                FALSE,             FALSE,              FALSE,             TRUE,            FALSE,
                             "APPR_IMPS_VAL",                 TRUE,             FALSE,              FALSE,             TRUE,            FALSE,
                             "APPR_LAND_VAL",                 TRUE,             FALSE,              FALSE,             TRUE,            FALSE,
                          "TAXABLE_IMPS_VAL",                 TRUE,             FALSE,              FALSE,             TRUE,            FALSE,
                          "TAXABLE_LAND_VAL",                 TRUE,             FALSE,              FALSE,             TRUE,            FALSE,
                              "ADDRESS_FULL",                FALSE,             FALSE,               TRUE,             TRUE,            FALSE,
                                 "CITY_NAME",                FALSE,              TRUE,               TRUE,             TRUE,            FALSE,
                          "CITY_NAME_POSTAL",                FALSE,             FALSE,              FALSE,             TRUE,            FALSE,
                                   "ZIPCODE",                FALSE,              TRUE,               TRUE,             TRUE,            FALSE,
                          "PRIMARY_ADDR_LGL",                FALSE,             FALSE,              FALSE,             TRUE,            FALSE,
                                 "PROP_NAME",                 TRUE,             FALSE,              FALSE,             TRUE,            FALSE,
                                 "PROP_TYPE",                FALSE,             FALSE,              FALSE,             TRUE,            FALSE,
                     "ASSESSOR_PUB_LIST_LGL",                FALSE,             FALSE,              FALSE,             TRUE,            FALSE,
                             "DISTRICT_NAME",                FALSE,              TRUE,              FALSE,             TRUE,            FALSE,
                            "CURRENT_ZONING",                FALSE,             FALSE,               TRUE,             TRUE,            FALSE,
                               "PRESENT_USE",                FALSE,             FALSE,               TRUE,             TRUE,            FALSE,
                                 "SQ_FT_LOT",                FALSE,             FALSE,               TRUE,             TRUE,            FALSE,
                                    "ACCESS",                FALSE,             FALSE,              FALSE,             TRUE,            FALSE,
                                "TOPOGRAPHY",                FALSE,             FALSE,              FALSE,             TRUE,            FALSE,
                      "RESTRICTIVE_SZ_SHAPE",                FALSE,             FALSE,              FALSE,             TRUE,            FALSE,
                             "PCNT_UNUSABLE",                FALSE,             FALSE,              FALSE,             TRUE,            FALSE,
                             "CONTAMINATION",                FALSE,             FALSE,              FALSE,             TRUE,            FALSE,
                             "HISTORIC_SITE",                FALSE,             FALSE,              FALSE,             TRUE,            FALSE,
                   "CURRENT_USE_DESIGNATION",                FALSE,             FALSE,              FALSE,             TRUE,            FALSE,
                   "NATIVE_GROWTH_PROT_ESMT",                FALSE,             FALSE,              FALSE,             TRUE,            FALSE,
                                 "EASEMENTS",                FALSE,             FALSE,              FALSE,             TRUE,            FALSE,
                         "OTHER_DESIGNATION",                FALSE,             FALSE,              FALSE,             TRUE,            FALSE,
                         "DEED_RESTRICTIONS",                FALSE,             FALSE,              FALSE,             TRUE,            FALSE,
                  "DEVELOPMENT_RIGHTS_PURCH",                FALSE,             FALSE,              FALSE,             TRUE,            FALSE,
                          "COAL_MINE_HAZARD",                FALSE,             FALSE,              FALSE,             TRUE,            FALSE,
                         "CRITICAL_DRAINAGE",                FALSE,             FALSE,              FALSE,             TRUE,            FALSE,
                            "EROSION_HAZARD",                FALSE,             FALSE,              FALSE,             TRUE,            FALSE,
                           "LANDFILL_BUFFER",                FALSE,             FALSE,              FALSE,             TRUE,            FALSE,
                    "HUNDRED_YR_FLOOD_PLAIN",                FALSE,             FALSE,              FALSE,             TRUE,            FALSE,
                            "SEISMIC_HAZARD",                FALSE,             FALSE,              FALSE,             TRUE,            FALSE,
                          "LANDSLIDE_HAZARD",                FALSE,             FALSE,              FALSE,             TRUE,            FALSE,
                        "STEEP_SLOPE_HAZARD",                FALSE,             FALSE,              FALSE,             TRUE,            FALSE,
                                    "STREAM",                FALSE,             FALSE,              FALSE,             TRUE,            FALSE,
                                   "WETLAND",                FALSE,             FALSE,              FALSE,             TRUE,            FALSE,
                        "SPECIES_OF_CONCERN",                FALSE,             FALSE,              FALSE,             TRUE,            FALSE,
                      "SENSITIVE_AREA_TRACT",                FALSE,             FALSE,              FALSE,             TRUE,            FALSE,
                            "WATER_PROBLEMS",                FALSE,             FALSE,              FALSE,             TRUE,            FALSE,
                        "TRANSP_CONCURRENCY",                FALSE,             FALSE,              FALSE,             TRUE,            FALSE,
                            "OTHER_PROBLEMS",                FALSE,             FALSE,              FALSE,             TRUE,            FALSE,
                         "SUIT_OWNER_PUBLIC",                FALSE,              TRUE,              FALSE,            FALSE,            FALSE,
                      "SUIT_OWNER_NONPROFIT",                FALSE,              TRUE,              FALSE,            FALSE,            FALSE,
                          "SUIT_OWNER_TAX_E",                FALSE,             FALSE,              FALSE,            FALSE,            FALSE,
                    "SUIT_WATER_OVERLAP_LGL",                FALSE,             FALSE,              FALSE,            FALSE,            FALSE,
                    "SUIT_WATER_OVERLAP_PCT",                FALSE,             FALSE,              FALSE,            FALSE,            FALSE,
                           "SUIT_WITHIN_UGA",                FALSE,             FALSE,              FALSE,            FALSE,            FALSE,
                     "SUIT_ZONING_CONSOL_20",                FALSE,              TRUE,              FALSE,            FALSE,            FALSE,
                          "SUIT_PRESENT_USE",                FALSE,             FALSE,              FALSE,            FALSE,            FALSE,
                        "SUITABLE_OWNER_LGL",                FALSE,             FALSE,              FALSE,            FALSE,            FALSE,
                "SUITABLE_WATER_OVERLAP_LGL",                FALSE,             FALSE,              FALSE,            FALSE,            FALSE,
                   "SUITABLE_WITHIN_UGA_LGL",                FALSE,             FALSE,              FALSE,            FALSE,            FALSE,
             "SUITABLE_ZONING_CONSOL_20_LGL",                FALSE,             FALSE,              FALSE,            FALSE,            FALSE,
                  "SUITABLE_PRESENT_USE_LGL",                FALSE,             FALSE,              FALSE,            FALSE,            FALSE,
                              "SUITABLE_LGL",                FALSE,             FALSE,              FALSE,             TRUE,            FALSE,
                                  "BLDG_NBR",                FALSE,             FALSE,              FALSE,             TRUE,            FALSE,
                            "BLDG_NET_SQ_FT",                FALSE,             FALSE,              FALSE,            FALSE,            FALSE,
                         "BLDG_LIVING_UNITS",                FALSE,             FALSE,              FALSE,            FALSE,            FALSE,
                   "BLDG_TYPE_APARTMENT_LGL",                FALSE,             FALSE,              FALSE,            FALSE,            FALSE,
                  "BLDG_TYPE_COMMERCIAL_LGL",                FALSE,             FALSE,              FALSE,            FALSE,            FALSE,
                       "BLDG_TYPE_CONDO_LGL",                FALSE,             FALSE,              FALSE,            FALSE,            FALSE,
                 "BLDG_TYPE_RESIDENTIAL_LGL",                FALSE,             FALSE,              FALSE,            FALSE,            FALSE,
                              "UTIL_PRESENT",                FALSE,             FALSE,              FALSE,            FALSE,            FALSE,
                                 "LOT_SQ_FT",                FALSE,             FALSE,              FALSE,            FALSE,            FALSE,
                             "LOT_SIZE_DESC",                FALSE,             FALSE,              FALSE,            FALSE,            FALSE,
                             "LOT_SIZE_TYPE",                FALSE,             FALSE,              FALSE,            FALSE,            FALSE,
                          "LOT_COVERAGE_PCT",                FALSE,             FALSE,              FALSE,            FALSE,            FALSE,
                           "LOT_STORIES_NBR",                FALSE,             FALSE,              FALSE,            FALSE,            FALSE,
                  "UTIL_LOT_DEVELOPABLE_PCT",                FALSE,             FALSE,              FALSE,            FALSE,            FALSE,
                      "UTIL_DEVELOPABLE_LGL",                FALSE,             FALSE,              FALSE,            FALSE,            FALSE,
               "UTIL_DEVELOPMENT_ASSUMPTION",                FALSE,             FALSE,              FALSE,            FALSE,            FALSE,
             "UTIL_DEVELOPABLE_ESTIMATE_LGL",                FALSE,             FALSE,              FALSE,            FALSE,            FALSE,
         "UTIL_DEVELOPABLE_LOT_COVERAGE_PCT",                FALSE,             FALSE,              FALSE,            FALSE,            FALSE,
           "UTIL_POTENTIAL_UTILIZATION_SQFT",                FALSE,             FALSE,              FALSE,            FALSE,            FALSE,
                   "UTIL_UNDER_UTILIZED_LGL",                FALSE,             FALSE,              FALSE,            FALSE,            FALSE,
                               "UTILIZATION",                FALSE,              TRUE,              FALSE,             TRUE,            FALSE
        )

    
return(dd_field_tags)
}

# COMMAND: MAKE_DD_FIELD_FORMAT ----

make_dd_field_format <- function(inventory){
  
  dd_field_format <- inventory %>%
    st_drop_geometry() %>% 
    select_if(not_sfc) %>% 
    map_chr(class) %>% 
    {tibble("FIELD_NAME_DEV" = names(.),
            "FIELD_FORMAT" = .)}
  
  return(dd_field_format)
  
  
  }

# COMMAND: MAKE_DD_FIELD_DESCRIPTION ----

make_dd_field_description <- function(dd_field_name_dev){
  dd_field_description <-
    dd_field_name_dev %>% 
    mutate(FIELD_DESCRIPTION = "[Descriptions will be added once the field list is complete.]")
  
  return(dd_field_description)
  
  }

# COMMAND: MAKE_DD_DATA_SOURCE ----

make_dd_data_source <- function(){
  
  dd_data_source <- 
tibble::tribble(
                       ~FIELD_NAME_DEV,                             ~DATA_SOURCE,
                                 "PIN",  "King County Department of Assessments",
                        "CENSUS_TRACT",                              "US Census",
                 "FILTER_PUBLIC_OWNER",  "King County Department of Assessments",
           "FILTER_SURPLUS_STATUS_LGL",                                  "Other",
          "FILTER_PROXIMITY_MARIJUANA",                                  "Other",
          "FILTER_PROXIMITY_PRESCHOOL",                                  "Other",
         "FILTER_PROXIMITY_OPEN_SPACE",                                  "Other",
              "FILTER_SCHOOL_DISTRICT",                                  "Other",
                 "FILTER_HISTORIC_LGL",                                  "Other",
            "FILTER_AFFORD_EXPIR_DATE",                                  "Other",
             "FILTER_ELIGIBILITY_NMTC",                                  "HUD",
              "FILTER_ELIGIBILITY_DDA",                                  "HUD",
              "FILTER_ELIGIBILITY_QCT",                                  "HUD",
           "HELPERS_URL_PARCEL_VIEWER",  "King County Department of Assessments",
                  "HELPERS_URL_OPP360",          "Enterprise Community Partners",
                       "TAXPAYER_NAME",  "King County Department of Assessments",
                             "BILL_YR",  "King County Department of Assessments",
                          "TAX_STATUS",  "King County Department of Assessments",
                          "TAX_REASON",  "King County Department of Assessments",
                       "APPR_IMPS_VAL",  "King County Department of Assessments",
                       "APPR_LAND_VAL",  "King County Department of Assessments",
                    "TAXABLE_IMPS_VAL",  "King County Department of Assessments",
                    "TAXABLE_LAND_VAL",  "King County Department of Assessments",
                        "ADDRESS_FULL",  "King County Department of Assessments",
                           "CITY_NAME",  "King County Department of Assessments",
                    "CITY_NAME_POSTAL",  "King County Department of Assessments",
                             "ZIPCODE",  "King County Department of Assessments",
                    "PRIMARY_ADDR_LGL",  "King County Department of Assessments",
                           "PROP_NAME",  "King County Department of Assessments",
                           "PROP_TYPE",  "King County Department of Assessments",
               "ASSESSOR_PUB_LIST_LGL",  "King County Department of Assessments",
                       "DISTRICT_NAME",  "King County Department of Assessments",
                      "CURRENT_ZONING",  "King County Department of Assessments",
                         "PRESENT_USE",  "King County Department of Assessments",
                           "SQ_FT_LOT",  "King County Department of Assessments",
                              "ACCESS",  "King County Department of Assessments",
                          "TOPOGRAPHY",  "King County Department of Assessments",
                "RESTRICTIVE_SZ_SHAPE",  "King County Department of Assessments",
                       "PCNT_UNUSABLE",  "King County Department of Assessments",
                       "CONTAMINATION",  "King County Department of Assessments",
                       "HISTORIC_SITE",  "King County Department of Assessments",
             "CURRENT_USE_DESIGNATION",  "King County Department of Assessments",
             "NATIVE_GROWTH_PROT_ESMT",  "King County Department of Assessments",
                           "EASEMENTS",  "King County Department of Assessments",
                   "OTHER_DESIGNATION",  "King County Department of Assessments",
                   "DEED_RESTRICTIONS",  "King County Department of Assessments",
            "DEVELOPMENT_RIGHTS_PURCH",  "King County Department of Assessments",
                    "COAL_MINE_HAZARD",  "King County Department of Assessments",
                   "CRITICAL_DRAINAGE",  "King County Department of Assessments",
                      "EROSION_HAZARD",  "King County Department of Assessments",
                     "LANDFILL_BUFFER",  "King County Department of Assessments",
              "HUNDRED_YR_FLOOD_PLAIN",  "King County Department of Assessments",
                      "SEISMIC_HAZARD",  "King County Department of Assessments",
                    "LANDSLIDE_HAZARD",  "King County Department of Assessments",
                  "STEEP_SLOPE_HAZARD",  "King County Department of Assessments",
                              "STREAM",  "King County Department of Assessments",
                             "WETLAND",  "King County Department of Assessments",
                  "SPECIES_OF_CONCERN",  "King County Department of Assessments",
                "SENSITIVE_AREA_TRACT",  "King County Department of Assessments",
                      "WATER_PROBLEMS",  "King County Department of Assessments",
                  "TRANSP_CONCURRENCY",  "King County Department of Assessments",
                      "OTHER_PROBLEMS",  "King County Department of Assessments",
                   "SUIT_OWNER_PUBLIC",  "King County Department of Assessments",
                "SUIT_OWNER_NONPROFIT",  "King County Department of Assessments",
                    "SUIT_OWNER_TAX_E",  "King County Department of Assessments",
              "SUIT_WATER_OVERLAP_LGL",                        "King County GIS",
              "SUIT_WATER_OVERLAP_PCT",                        "King County GIS",
                     "SUIT_WITHIN_UGA",                        "King County GIS",
               "SUIT_ZONING_CONSOL_20",                        "King County GIS",
                    "SUIT_PRESENT_USE",  "King County Department of Assessments",
                  "SUITABLE_OWNER_LGL",  "King County Department of Assessments",
          "SUITABLE_WATER_OVERLAP_LGL",                        "King County GIS",
             "SUITABLE_WITHIN_UGA_LGL",                        "King County GIS",
       "SUITABLE_ZONING_CONSOL_20_LGL",                        "King County GIS",
            "SUITABLE_PRESENT_USE_LGL",  "King County Department of Assessments",
                        "SUITABLE_LGL",                       "Multiple Sources",
                            "BLDG_NBR",  "King County Department of Assessments",
                      "BLDG_NET_SQ_FT",  "King County Department of Assessments",
                   "BLDG_LIVING_UNITS",  "King County Department of Assessments",
             "BLDG_TYPE_APARTMENT_LGL",  "King County Department of Assessments",
            "BLDG_TYPE_COMMERCIAL_LGL",  "King County Department of Assessments",
                 "BLDG_TYPE_CONDO_LGL",  "King County Department of Assessments",
           "BLDG_TYPE_RESIDENTIAL_LGL",  "King County Department of Assessments",
                        "UTIL_PRESENT",  "King County Department of Assessments",
                           "LOT_SQ_FT",  "King County Department of Assessments",
                       "LOT_SIZE_DESC",  "King County Department of Assessments",
                       "LOT_SIZE_TYPE",  "King County Department of Assessments",
                    "LOT_COVERAGE_PCT",  "King County Department of Assessments",
                     "LOT_STORIES_NBR",  "King County Department of Assessments",
            "UTIL_LOT_DEVELOPABLE_PCT",  "King County Department of Assessments",
                "UTIL_DEVELOPABLE_LGL",  "King County Department of Assessments",
         "UTIL_DEVELOPMENT_ASSUMPTION",  "King County Department of Assessments",
       "UTIL_DEVELOPABLE_ESTIMATE_LGL",  "King County Department of Assessments",
   "UTIL_DEVELOPABLE_LOT_COVERAGE_PCT",  "King County Department of Assessments",
     "UTIL_POTENTIAL_UTILIZATION_SQFT",  "King County Department of Assessments",
             "UTIL_UNDER_UTILIZED_LGL",  "King County Department of Assessments",
                         "UTILIZATION",  "King County Department of Assessments"
  )


return(dd_data_source)
  }

# COMMAND: MAKE_DD_DICTIONARY_VERSION ----

make_dd_dictionary_version <- function(dd_field_name_dev, version_string){
  
  date_string <- format(Sys.Date(), format="%B %d, %Y") 
  
  dd_dictionary_version <-
    dd_field_name_dev %>% 
    mutate(DICTIONARY_VERSION = str_c(version_string, date_string, sep = " - "))
  
  return(dd_dictionary_version)
}

# COMMAND: MAKE_DD_GOOGLE_DRIVE ----

make_dd_google_drive <- function(...){
  dd_gd <- reduce(list(...), left_join, by = "FIELD_NAME_DEV")
  
  sheet_key <- as_id("1EAjo_iL_wibBQUqZ9hvE1My6by4ip57b-dWB8mzmhN0")
  
  dd_ss <- gs_key(sheet_key, lookup = NULL, visibility = NULL, verbose = TRUE)

  gs_edit_cells(dd_ss, 
              ws = 1, 
              input = dd_gd, 
              anchor = "A1", 
              byrow = FALSE,
              col_names = TRUE, 
              trim = FALSE, 
              verbose = TRUE)
  
  return(dd_ss)
}

# COMMAND: MAKE_DD ----

make_dd <- function(dd_google_drive){
  
  dd <- gs_read(dd_google_drive, ws = "STATIC_VERSION")
  
  return(dd)
  
}



# COMMAND: WRITE_INVENTORY_RDA ----

write_inventory_rda <- function(x, path){
  
  inventory_table <- x %>% 
    st_drop_geometry() %>% 
    select_if(not_sfc)  
  
  save(inventory_table,file = path)

}

# COMMAND: WRITE_INVENTORY_CSV----
write_inventory_csv <- function(x, path){
  
  inventory_table <- x %>% 
    st_drop_geometry() %>% 
    select_if(not_sfc)   
  
  write_csv(inventory_table, path)

}


# COMMAND: WRITE_INVENTORY_XLSX ----
write_inventory_xlsx <- function(x, path){
  
  inventory_table <- x %>% 
    st_drop_geometry() %>% 
    select_if(not_sfc)  
  
  write_xlsx(inventory_table, path)

}


# COMMAND: WRITE_INVENTORY_GEOJSON ----

write_inventory_geojson <- function(obj, dsn){
  
  obj_4326 <- st_transform(obj, 4326)
  
  st_write(obj_4326, dsn, driver = "GeoJSON",delete_dsn = TRUE)
  
}

# COMMAND: WRITE_INVENTORY_SHP ----

write_inventory_shp <- function(obj, dsn){
  
  obj_2926 <- st_transform(obj, 2926)
  
  st_write(obj_2926, dsn, driver = "ESRI Shapefile",delete_dsn = TRUE)
  
}
