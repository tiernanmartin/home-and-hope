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
                        "Dev ", "Development ",
                        "Agric", "Agriculture",
                        "Desig", "Designated",
                        "Comm", "Commercial"
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

# COMMAND: MAKE_NAME_RECODE_KEY ----
make_name_recode_key <- function(...){
  
  trigger <- list(...)
  
  recode_key_fp <- here("1-data/1-raw/name_recode_key.rda")
  
  recode_key_gs <- gs_key("1aInQqXPK3tqrXKd80PXPugR8G7Nysz46tirCTAdKn6s")
  
  recode_key <- gs_read(recode_key_gs) %>% 
    replace_na(list(NEW = ""))
  
  write_rds(recode_key, recode_key_fp)
  
  return(recode_key)
  
  
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

# COMMAND: MAKE_ENV_RESTRICTIONS ----

make_env_restrictions <- function(){ 
  
  env_fp <- here("./1-data/2-external/EXTR_EnvironmentalRestriction_V.csv")
  
  env_dr_id <- as_id("1OAXpWcO22Un1PGEHho4J3cMnMNurhVYu")
  
  env_load <-  
    make_or_read2(fp = env_fp,
                  dr_id = env_dr_id,
                  skip_get_expr = TRUE,
                  get_expr = function(fp){ # Source: http://aqua.kingcounty.gov/extranet/assessor/Environmental%20Restriction.zip
                  },
                  make_expr = function(fp, dr_id){
                    zip_dir <- here("1-data/2-external")
                    
                    target_name <- "EXTR_EnvironmentalRestriction_V.csv"
                    
                    drive_read_zip(
                      dr_id = dr_id,
                      .tempdir = FALSE,
                      dir_path = zip_dir,
                      read_fun = read_csv,
                      target_name = target_name 
                    ) 
                    
                  },
                  read_expr = function(fp){ read_csv(env_fp)})
  
  env <- env_load %>% 
    rename_all(to_screaming_snake_case) 
     
  env_restrictions <- env
  
  return(env_restrictions)
  
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
    transmute(PIN = make_pin(MAJOR, MINOR),
              geom_pt) %>% 
    drop_na() %>% 
    st_set_geometry("geometry") %>% 
    st_transform(2926) %>% 
    st_sf
  
  parcel_sf <- p_sf_ready
  
  return(parcel_sf)
}


# COMMAND: MAKE_PARCEL_SF_READY ----

make_parcel_sf_ready <- function(parcel_sf){
   
  
  # MAKE P_SF_READY

  p_sf_ready <-  parcel_sf %>%
    miscgis::subset_duplicated("PIN") %>%
    group_by(PIN) %>%
    slice(1) %>%
    ungroup %>%
    rbind(subset_duplicated(parcel_sf,"PIN",notin = TRUE)) %>%
    st_set_geometry("geometry")
  
  parcel_sf_ready <- p_sf_ready
  
  return(parcel_sf_ready)

  }  

# COMMAND: MAKE_PARCEL_ADDR_READY ----

make_parcel_addr_ready <- function(parcel_addr){

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
    mutate_if(is.factor, as.character) %>% 
  group_by(PIN) %>%
    arrange(desc(PRIMARY_ADDR_LGL)) %>%
    slice(1) %>%
    ungroup

  parcel_addr_ready <- p_addr_ready
  
  return(parcel_addr_ready)
  
}

# COMMAND: MAKE_PARCEL_DF_READY ----

make_parcel_df_ready <- function(parcel_lookup, prop_type, name_recode_key, pub_parcel, parcel_df){
  
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
  
  
  parcel_df_recoded <- parcel_df %>% 
    select_at(vars(fine_cols)) %>% 
    bind_cols(parcel_recode_cols) %>% 
    mutate_if(is_logical_yn, recode_logical_yn) %>% 
    mutate_if(is_logical_01, recode_logical_01) %>%
    mutate_if(is_logical_yesno, recode_logical_yesno) %>% 
    mutate(PIN = make_pin(MAJOR, MINOR))
  
  parcel_df_recoded_clean_names <- parcel_df_recoded %>% 
    transmute(PIN,
              PROPERTY_NAME = if_else(is.na(PROP_NAME),NA_character_, str_clean_upper(PROP_NAME))) %>%  
    unnest_tokens(ORIG, PROPERTY_NAME, token = "ngrams", n = 1, to_lower = FALSE) %>% 
    mutate(ORIG = if_else(str_detect(ORIG,"NA"),"", ORIG)) %>% 
    left_join(name_recode_key, by = "ORIG") %>% 
    mutate(PROPERTY_NAME = if_else(is.na(NEW),ORIG,NEW)) %>% 
    group_by(PIN) %>% 
    summarise(PROPERTY_NAME = str_squish(str_c(PROPERTY_NAME, collapse = " ")))  %>% 
    mutate(PROPERTY_NAME = if_else(PROPERTY_NAME %in% "",NA_character_,PROPERTY_NAME)) %>% 
    right_join(parcel_df_recoded, by = "PIN")
  

  pub_parcel_ready <- pub_parcel %>%
    transmute(PIN,
              ASSESSOR_PUB_LIST_LGL = TRUE)

  p_df_ready <- parcel_df_recoded_clean_names %>%
   mutate(PIN = make_pin(MAJOR, MINOR)) %>%
    left_join(prop_type, by = "PROP_TYPE") %>%
    left_join(pub_parcel_ready, by = "PIN") %>%
    mutate(ASSESSOR_PUB_LIST_LGL = if_else(is.na(ASSESSOR_PUB_LIST_LGL),FALSE,ASSESSOR_PUB_LIST_LGL)) %>%  
    transmute(PIN,
           PROPERTY_NAME,
           PROP_TYPE = PROP_TYPE_DESC,
           ASSESSOR_PUB_LIST_LGL, 
           MUNICIPALITY = if_else(toupper(DISTRICT_NAME) == "KING COUNTY", "UNINCORPORATED KING COUNTY", toupper(DISTRICT_NAME)),
           CURRENT_ZONING,
           PRESENT_USE,
           HBU_AS_IF_VACANT,
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


  parcel_df_ready <- p_df_ready
  
  return(parcel_df_ready)
}
  
# COMMAND: MAKE_PARCEL_ACCT_READY ----

make_parcel_acct_ready <- function(acct, tax_status, tax_reason){

  # MAKE ACCT_READY

  acct_frmt <- acct %>%
    mutate(PIN = make_pin(MAJOR, MINOR)) %>%
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

  p_acct_ready <- acct_frmt %>%
    miscgis::subset_duplicated("PIN") %>%
    group_by(PIN) %>%
    drop_na %>% # remove any duplicates PIN records with NAs
    slice(1) %>%  # take the first record and discard the rest
    ungroup %>%
    bind_rows(subset_duplicated(acct_frmt,"PIN",notin = TRUE)) %>%
    arrange(PIN)


  parcel_acct_ready <- p_acct_ready
  
  return(parcel_acct_ready)
}

# COMMAND: MAKE_PARCEL_ENV_READY ----

make_parcel_env_ready <- function(env_restrictions){
  
  env_frmt <- env_restrictions %>% 
    rename_all(to_screaming_snake_case) %>%   
    filter(!is.na(TYPE)) %>% 
    transmute(PIN = make_pin(MAJOR, MINOR),
              TYPE = to_parsed_case(TYPE) %>% toupper %>% str_replace_all("_"," "), 
              PCNT_AFFECTED,
              PCT = if_else(PCNT_AFFECTED>0, str_c(PCNT_AFFECTED,"%"),""),
              RESTRICTION = str_c(TYPE, " (",PCT,")") %>% str_remove_all("\\s\\(\\)"))
  
  env_res <- env_frmt %>% 
    group_by(PIN) %>%  
    nest() %>% 
    transmute(PIN,
              ENV_RESTRICTIONS = map_chr(data, ~ pluck(.x,"RESTRICTION") %>% str_c(collapse = ", "))) 
  
  parcel_env_ready <- env_res
  
  return(parcel_env_ready)
  
}

# COMMAND: MAKE_PARCEL_READY ----

make_parcel_ready <- function(parcel_addr_ready, parcel_env_ready, ...){

  # MAKE PARCEL_READY

  obj_list <- list(...)

  parcel_ready <- obj_list %>%
    reduce(.f = inner_join, by = "PIN") %>%
    left_join(parcel_addr_ready, by = "PIN") %>% 
    left_join(parcel_env_ready, by = "PIN") %>% 
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
# COMMAND: MAKE_SCHOOL_DISTRICTS ----
make_school_districts <- function(){
  
  school_dist_fp <- here("1-data/2-external/schdst")
  
  school_dist_dr_id <- as_id("11K9yTC7C1PnKAtTIPqLsKxZE6qF6p-QO")
  
  school_dist_load <- 
    make_or_read2(fp = school_dist_fp,
                  dr_id = school_dist_dr_id,
                  skip_get_expr = FALSE,
                  get_expr = function(fp){
                    
                    # SOURCE: ftp://ftp.kingcounty.gov/gis-web/GISData/schdst_SHP.zip
                      
                  },
                  make_expr = function(fp, dr_id){
                    zip_dir <- here("1-data/2-external")
                    
                    target_name <- "schdst"
                    
                    drive_read_zip(dr_id = dr_id,
                                   dir_path = zip_dir,
                                   read_fun = st_read,
                                   target_name = target_name,
                                   .tempdir = FALSE, 
                                   stringsAsFactors = FALSE)
                  },
                  read_expr = function(fp){read_sf(fp,stringsAsFactors = FALSE)})
  
  
  school_districts <- rename_if(school_dist_load, not_sfc, to_screaming_snake_case)
  
  return(school_districts)
  
}

# COMMAND: MAKE_LEG_DISTRICTS ----
make_leg_districts <- function(){
  
  leg_dist_fp <- here("1-data/2-external/legdst")
  
  leg_dist_dr_id <- as_id("17S93IPka-lzv6nX7deDx08EmRHLulm_Q")
  
  leg_dist_load <- 
    make_or_read2(fp = leg_dist_fp,
                  dr_id = leg_dist_dr_id,
                  skip_get_expr = FALSE,
                  get_expr = function(fp){
                    
                    # SOURCE: ftp://ftp.kingcounty.gov/gis-web/GISData/legdist_SHP.zip
                      
                  },
                  make_expr = function(fp, dr_id){
                    zip_dir <- here("1-data/2-external")
                    
                    target_name <- "legdst"
                    
                    drive_read_zip(dr_id = dr_id,
                                   dir_path = zip_dir,
                                   read_fun = st_read,
                                   target_name = target_name,
                                   .tempdir = FALSE, 
                                   stringsAsFactors = FALSE)
                  },
                  read_expr = function(fp){read_sf(fp,stringsAsFactors = FALSE)})
  
  
  leg_districts <- rename_if(leg_dist_load, not_sfc, to_screaming_snake_case)
  
  return(leg_districts)
  
}

# COMMAND: MAKE_KC_COUNCIL_DISTRICTS ----
make_kc_council_districts <- function(){
  
  kcc_dist_fp <- here("1-data/2-external/kccdst")
  
  kcc_dist_dr_id <- as_id("17CoZ7cl__hZOproY6udM0wClT-4NPpJg")
  
  kcc_dist_load <- 
    make_or_read2(fp = kcc_dist_fp,
                  dr_id = kcc_dist_dr_id,
                  skip_get_expr = FALSE,
                  get_expr = function(fp){
                    
                    # SOURCE: ftp://ftp.kingcounty.gov/gis-web/GISData/legdist_SHP.zip
                      
                  },
                  make_expr = function(fp, dr_id){
                    zip_dir <- here("1-data/2-external")
                    
                    target_name <- "kccdst"
                    
                    drive_read_zip(dr_id = dr_id,
                                   dir_path = zip_dir,
                                   read_fun = st_read,
                                   target_name = target_name,
                                   .tempdir = FALSE, 
                                   stringsAsFactors = FALSE)
                  },
                  read_expr = function(fp){read_sf(fp,stringsAsFactors = FALSE)})
  
  
  kc_council_districts <- rename_if(kcc_dist_load, not_sfc, to_screaming_snake_case)
  
  return(kc_council_districts)
  
}


# COMMAND: MAKE_BUS_STOPS_METRO ----
make_bus_stops_metro <- function(){
  
  bs_fp <- here("1-data/2-external/transitstop/transitstop.shp")
  
  bs_dr_id <- as_id("1yB5EYVPdC4ephNwRLWOtH9UK5PPH8cNc")
  
  bs_load <- 
    make_or_read2(fp = bs_fp, dr_id = bs_dr_id, skip_get_expr = FALSE,
                  get_expr = function(fp){
                    # SOURCE: ftp://ftp.kingcounty.gov/gis-web/GISData/transitstop_SHP.zip
                  },
                  make_expr = function(fp, dr_id){
                    zip_dir <- here("1-data/2-external/")
                    
                    target_name <- "transitstop" 
                    
                    drive_read_zip(dr_id = dr_id,
                                   .tempdir = FALSE,
                                   dir_path = zip_dir,
                                   read_fun = read_sf,
                                   target_name = target_name)
                  },
                  read_expr = function(fp){read_sf(fp)})
  
  stop_status <- tribble(
    ~ STOP_STATUS,    ~ STOP_STATUS_DESC,
            "ACT",            	"Active",
            "CLO",	"Permanently Closed",
            "INA",	"Temporary Inactive",
            "PLN",	"Plan"
    )
  
  bs <- bs_load %>% 
    st_transform(2926) %>% 
    left_join(stop_status, by = c(STOP_STATU = "STOP_STATUS")) %>% 
    transmute(STOP_ID = as.character(STOP_ID),
              TRANSIT_TYPE = "bus",
              TRANSIT_PROVIDER_NAME = "KING COUNTY METRO",
              STOP_STATUS = STOP_STATUS_DESC) %>% 
    filter(STOP_STATUS %in% c("Active", "Temporary Inactive", "Plan"))
  
  bus_stops_metro <- bs
  
  return(bus_stops_metro)
}
# COMMAND: MAKE_TRANSIT_STOPS_OSM ----

make_transit_stops_osm <- function(){
  
  ts_fp <- here("1-data/2-external/transit-stops-osm.gpkg")
  
  ts_dr_id <- as_id("1kcEMuGu4QdKpva61Lf5CN-ZfWCk59leA")
  
  ts_load <- 
    make_or_read2(fp = ts_fp, dr_id = ts_dr_id, skip_get_expr = FALSE,
                  get_expr = function(fp){
                    
                    q <- opq(bbox = "King County, Washington") %>% 
                      add_osm_feature(key = "public_transport", value = "",value_exact = FALSE)
                    
                    transit_pts <- q %>% 
                      osmdata_sf() %>% 
                      pluck("osm_points") %>% 
                      rename_all(to_screaming_snake_case)
                    
                    st_write(transit_pts, fp)
                    
                    drive_folder <- as_id("0B5Pp4V6eCkhrdlJ3MXVaNW16T0U")
                    
                    drive_upload(fp, drive_folder)
                    
                  },
                  make_expr = function(fp, dr_id){
                    drive_read(dr_id = dr_id,.tempfile = FALSE,path = fp,read_fun = read_sf)
                  },
                  read_expr = function(fp){read_sf(fp)}
                    )
  
  ts_pts <- ts_load %>% 
    transmute( OSM_ID = as.character(OSM_ID)) %>% 
    st_transform(2926)
  
  ts_ready <- ts_load %>% 
    st_drop_geometry() %>% 
    select(OSM_ID ,NAME,BUS, TRAIN, STEETCAR = TRAM, FERRY, PUBLIC_TRANSPORT, SOURCE) %>% 
    gather(TRANSIT_TYPE, VALUE, -OSM_ID, -NAME,-PUBLIC_TRANSPORT, -SOURCE) %>% 
    filter(!is.na(VALUE)) %>%  
    transmute(TRANSIT_STOP_OSM_ID = OSM_ID,
              TRANSIT_STOP_NAME = NAME,
              TRANSIT_STOP_TYPE = TRANSIT_TYPE,
              TRANSIT_STOP_SOURCE = SOURCE) %>% 
    mutate_if(is.factor, as.character) %>% 
    left_join(ts_pts, by = c(TRANSIT_STOP_OSM_ID = "OSM_ID")) %>% 
    st_sf
  
  view_ts_ready <- function(){mapview(ts_ready, zcol = "TRANSIT_STOP_TYPE", legend = TRUE)}
  
  transit_stops_osm <- ts_ready
  
  return(transit_stops_osm)
  
  
}

# COMMAND: MAKE_PLACE_SPACES_OSM ----

make_play_spaces_osm <- function(){
  
  ps_fp <- here("1-data/2-external/play-spaces-osm.gpkg")
  
  ps_dr_id <- as_id("1lGOap459M_cXNxnYgTNH8H6dtA-FfZbe")
  
  ps_load <- 
    make_or_read2(fp = ps_fp, dr_id = ps_dr_id, skip_get_expr = FALSE,
                  get_expr = function(fp){
                    
                    q <- opq(bbox = "King County, Washington") %>% 
                      add_osm_feature(key = "leisure", value = "park",value_exact = FALSE) 
                    
                    
                    
                    osm_list <-  list("osm_polygons",
                                      "osm_multipolygons") %>% 
                      map(~ pluck(osmdata_sf(q),.x)) 
                    
                    common_cols <- osm_list %>% 
                      map(names) %>% 
                      reduce(intersect) 
                    
                    play_spaces <- osm_list %>% 
                      map(~ select_at(.x, .vars = vars(common_cols))) %>% 
                      reduce(rbind)%>%  
                      st_cast("MULTIPOLYGON") %>% 
                      as_tibble %>% 
                      st_sf %>% 
                      st_transform(2926) %>% 
                      rename_if(not_sfc, to_screaming_snake_case) %>% 
                      mutate_if(is.factor, as.character) %>% 
                      select(PLAY_SPACE_OSM_ID = OSM_ID,
                             PLAY_SPACE_NAME = NAME,
                             PLAY_SPACE_TYPE = LEISURE) 
                    
                    st_write(play_spaces, fp)
                    
                    drive_folder <- as_id("0B5Pp4V6eCkhrdlJ3MXVaNW16T0U")
                    
                    drive_upload(fp, drive_folder)
                    
                  },
                  make_expr = function(fp, dr_id){
                    drive_read(dr_id = dr_id,.tempfile = FALSE,path = fp,read_fun = read_sf)
                  },
                  read_expr = function(fp){read_sf(fp)}
    )
  
  play_spaces <- ps_load %>%   
                      filter(!is.na(PLAY_SPACE_TYPE)) %>%  
                      filter(PLAY_SPACE_TYPE %!in% "dog_park")
  
  play_spaces_osm <- play_spaces
  
  return(play_spaces_osm)
  
  
}

# COMMAND: MAKE_MJ_BUSINESSES ----

make_mj_businesses <- function(){
  
  mj_biz_fp <- here("1-data/2-external/MarijuanaApplicants.xls")
  
  mj_biz_dr_id <- as_id("1qrMPnRCq2giSyZbF3hdDMDEyljVd7d28")
  
  read_mj_biz <- function(sheet){
    make_or_read2(fp = mj_biz_fp,
                  dr_id = mj_biz_dr_id,
                  skip_get_expr = FALSE,
                  get_expr = function(fp){
                    
                    # SOURCE: https://lcb.wa.gov/sites/default/files/publications/Public_Records/2018/MarijuanaApplicants.xls
                      
                  },
                  make_expr = function(fp, dr_id){
                    
                    drive_read(dr_id = dr_id,.tempfile = FALSE,path = fp,read_fun = read_excel, sheet)
                  },
                  read_expr = function(fp){read_excel(fp, sheet)})
  }
    
  sheet_list <- list("Retailers 4-10-18", "Producers 4-10-18", "Processors 4-10-18", "Marijuana Transporter 4-10-18")
  
  mj_biz <- map_dfr(sheet_list, read_mj_biz) %>% 
    janitor::clean_names(case = "screaming_snake") %>%  
     transmute(TRADENAME,
              PRIV_DESC = PRIV_DESC,
              PRIVILEGE_STATUS = PRIVILEGE_STATUS,
              ZIP_CODE = str_extract(ZIP_CODE,"^.{0,5}"),
              COUNTY = COUNTY,
              ADDRESS_FULL = str_c( STREET_ADDRESS, CITY, STATE, ZIP_CODE, sep = ", "))
  
  # filter to King County then geocode

  geocode_fun <- function(address){
    geocode_url(address, 
                auth="standard_api", 
                privkey="AIzaSyAC19c3TtQwrSiQYKYDaf-9nDIqahirnD8",
                clean=TRUE, 
                add_date='today', 
                verbose=TRUE) 
  }
    
  mj_biz_geocoded <- mj_biz %>% 
    filter(COUNTY %in% "KING") %>%  
    filter(str_detect(PRIVILEGE_STATUS,"ACTIVE|PENDING") ) %>% 
    arrange(desc(as.integer(ZIP_CODE))) %>%  
    mutate(geocode_output = map(ADDRESS_FULL, geocode_fun)) %>% 
    unnest() %>% 
    rename_all(to_screaming_snake_case)
  
  mj_biz_sf <- mj_biz_geocoded %>% 
    filter(!is.na(LAT)) %>% 
    st_as_sf(coords = c("LNG", "LAT")) %>% 
    st_set_crs(4326) %>% 
    st_transform(2926)
  
 mj_businesses <- mj_biz_sf
  return(mj_businesses)
  
}

# COMMAND: MAKE_EL_FACILITIES ----

make_el_facilities <- function(){
  
  el_site_fp <- here("1-data/2-external/FutureWise EC Facility Data Request.xlsx")
  
  el_site_dr_id <- as_id("1h-VzyMczgy_py70vP-iCeyjac_BoTuYK")
  
  el_site_load <- 
    make_or_read2(fp = el_site_fp,
                  dr_id = el_site_dr_id,
                  skip_get_expr = FALSE,
                  get_expr = function(fp){
                    # SOURCE: partners at 3SI
                  },
                  make_expr = function(fp, dr_id){
                    drive_read(dr_id = dr_id,.tempfile = FALSE,path = fp,read_fun = read_excel, sheet = "Provider Data")
                  },
                  read_expr = function(fp){read_excel(fp, sheet = "Provider Data")})
  
   
  
  el_site_sf <- el_site_load %>%  
    clean_names(case = "screaming_snake") %>%  
    st_as_sf(coords = c("LONGITUDE", "LATITUDE")) %>% 
    st_set_crs(4326) %>% 
    st_transform(2926) 
  
 el_facilities <- el_site_sf

 return(el_facilities)
  
}

# COMMAND: MAKE_OTHER_SUITABILITY_CHARACTERISTICS ----
make_other_suitability_characteristics <- function(...){
  
  trigger <- list(...)
  
  other_suitability_characteristics_fp <- here("1-data/1-raw/other_suitability_characteristic.rda")
  
  other_gs <- gs_key("1a-xqAjyCI3XITm9BxfTdw6UNyoG5r2UyacNzE4N60QU")
  
  other <- gs_read(other_gs, sheet = "SUIT_OTHER", col_types = "ccc") 
  
  write_rds(other, other_suitability_characteristics_fp)
  
  other_suitability_characteristics <- other
  
  return(other_suitability_characteristics)
  
  
}

# COMMAND: MAKE_OWNER_ANTIJOIN_NAMES ----
make_owner_antijoin_names <- function(){
  
  anti_fp <- here("1-data/2-external/stop-words.csv")
  
  anti_dr_id <- as_id("17n2h9np6OjHyD9cZ0tHSRbBubpbN4ckX")
  
  anti_load <- 
    make_or_read2(fp = anti_fp, dr_id = anti_dr_id, skip_get_expr = FALSE,
                  get_expr = function(fp){
                    data(stop_words)
                    
                    write_csv(stop_words, fp)
                    
                    drive_folder <- as_id("0B5Pp4V6eCkhrdlJ3MXVaNW16T0U")
                    
                    drive_upload(fp, drive_folder)
                  },
                  make_expr = function(fp, dr_id){
                    drive_read(dr_id = dr_id,
                               .tempfile = FALSE,
                               path = fp,
                               read_fun = read_csv)
                  },
                  read_expr = function(fp){read_csv(fp)})
  
  numbers_0_500 <- tibble(word = as.character(0:500))
  
  ok_words <- c("STATE","STATES", "U", "S", "A", "US")
  
  anti <- transmute(anti_load, word = toupper(word)) %>% 
    bind_rows(numbers_0_500) %>% 
    filter(word %!in% ok_words)
  
  owner_antijoin_names <- anti

  return(owner_antijoin_names)
    
  
}

# COMMAND: MAKE_OWNER_NAME_CATEGORY_KEY ----
make_owner_name_category_key<- function(...){
  
  trigger <- list(...)
  
  owner_name_category_key_fp <- here("1-data/1-raw/owner_name_category_key.rda")
  
  categories_gs <- gs_key("1cYNIpQpDJTZWi46S_9bZ6rjgRu8JWes1BxOeoJJD2tg")
  
  cat_ngram_list <- gs_read_all(categories_gs,delay_length = 6) 
  
  cat_ngram_list$CATEGORIES <- NULL
  
  
  cat_ngram_long <- cat_ngram_list %>% 
    map_dfr(~ gather(.x, NGRAM_TYPE, WORD, -CATEGORY)) 
  
  cat_ngram_wide <- cat_ngram_long %>%  
    group_by(NGRAM_TYPE) %>% 
    mutate(row = row_number()) %>% 
    spread(NGRAM_TYPE,WORD) %>% 
    arrange(row) %>% 
    select(-row) 
  
  owner_name_category_key <- cat_ngram_wide
  
  write_rds(owner_name_category_key, owner_name_category_key_fp)
  
  return(owner_name_category_key)
  
  
}

# COMMAND: MAKE_OWNER_PUBLIC_CATEGORIES ----
make_owner_public_categories <- function(...){
  
  # loadd(parcel_ready) 
  # loadd(suitability_tax_exempt) 
  # loadd(owner_antijoin_names) 
  # loadd(owner_name_category_key) 
  # loadd(name_recode_key) 
  
  names <- suitability_tax_exempt %>%  
    filter(SUIT_OWNER_PUBLIC) %>% 
    inner_join(parcel_ready, by = "PIN") %>% 
    st_drop_geometry() %>% 
    transmute(PIN,
              OWNER_NAME = str_trim(toupper(TAXPAYER_NAME)))
  
  # CLEAN/RECODE NAMES ----
  
  names_cleaned <- names %>% 
    unnest_tokens(ORIG, OWNER_NAME, token = "ngrams", n = 1, to_lower = FALSE) %>%  
    left_join(name_recode_key, by = "ORIG") %>% 
    mutate(OWNER_NAME = if_else(is.na(NEW),ORIG,NEW)) %>% 
    group_by(PIN) %>% 
    summarise(OWNER_NAME_CLEAN = str_c(OWNER_NAME, collapse = " ")) %>% 
    full_join(names, by = "PIN") %>%  
    select(PIN,OWNER_NAME,OWNER_NAME_CLEAN)
  
  names_trimmed <- names_cleaned %>% 
    unnest_tokens(ORIG, OWNER_NAME_CLEAN, token = "ngrams", n = 1, to_lower = FALSE) %>% 
    anti_join(owner_antijoin_names, by = c("ORIG" = "word")) %>%   
    group_by(PIN) %>% 
    summarise(OWNER_NAME_TRIM = str_c(ORIG, collapse = " ")) %>% 
    full_join(names_cleaned, by = "PIN") %>%  
    select(PIN,OWNER_NAME,OWNER_NAME_CLEAN,OWNER_NAME_TRIM)
  
  view_names_trimmed_cnt <- function(){
    names_trimmed_cnt <- names_trimmed %>% 
      count(OWNER_NAME_TRIM, sort = TRUE)
  }
  
  # view_names_trimmed_cnt()
  
  # CREATE NGRAMS ----
  
  names_ngrams_1 <- names_trimmed %>%  
    unnest_tokens(PUBLIC_NGRAM_1, OWNER_NAME_TRIM, token = "ngrams", n = 1, to_lower = FALSE) %>% 
    select(-OWNER_NAME)
  
  names_ngrams_1_cnt <- names_ngrams_1 %>% 
    count(PUBLIC_NGRAM_1, sort = TRUE)
  
  names_ngrams_2 <- names_trimmed %>%  
    unnest_tokens(PUBLIC_NGRAM_2, OWNER_NAME_TRIM, token = "ngrams", n = 2, to_lower = FALSE) %>% 
    separate(PUBLIC_NGRAM_2, c("PUBLIC_NGRAM_2_A", "PUBLIC_NGRAM_2_B"),sep = " ") %>% 
    select(-OWNER_NAME)
  
  names_ngrams_2_cnt <- names_ngrams_2 %>% 
    count(PUBLIC_NGRAM_2_A,PUBLIC_NGRAM_2_B, sort = TRUE)
  
  names_ngrams_3 <- names_trimmed %>%  
    unnest_tokens(PUBLIC_NGRAM_3, OWNER_NAME_TRIM, token = "ngrams", n = 3, to_lower = FALSE) %>% 
    separate(PUBLIC_NGRAM_3, c("PUBLIC_NGRAM_3_A", "PUBLIC_NGRAM_3_B", "PUBLIC_NGRAM_3_C"),sep = " ") %>% 
    select(-OWNER_NAME)
  
  names_ngrams_3_cnt <- names_ngrams_3 %>% 
    count(PUBLIC_NGRAM_3_A,PUBLIC_NGRAM_3_B,PUBLIC_NGRAM_3_C, sort = TRUE)
  
  names_ngrams <- 
    list(names_cleaned,names_ngrams_1,names_ngrams_2,names_ngrams_3) %>% 
    reduce(left_join, by = c("PIN","OWNER_NAME_CLEAN"))
  
  
  # VIEW NGRAMS NETWORK ----

make_bigram_graph <- function(){
  bigram_graph <- names_ngrams_2 %>% 
  count(NGRAM_2_A,NGRAM_2_B, sort = TRUE) %>% 
  filter(n > 20) %>% 
  graph_from_data_frame
  
  set.seed(2016)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()
}

# make_bigram_graph()
  
  
  

# CREATE NGRAM CATEGORY KEYS ----

cat_ngram_1 <- owner_name_category_key %>% 
  select(CATEGORY,PUBLIC_NGRAM_1) %>% 
  drop_na

cat_ngram_2 <- owner_name_category_key %>% 
  select(CATEGORY,matches("PUBLIC_NGRAM_2")) %>% 
  drop_na

cat_ngram_3 <- owner_name_category_key %>% 
  select(CATEGORY,matches("PUBLIC_NGRAM_3")) %>% 
  drop_na


# JOIN CATEGORIES TABLE ----


ngram_1_categorized <- names_ngrams %>% 
  left_join(cat_ngram_1, by = "PUBLIC_NGRAM_1") %>% 
  group_by(PIN,OWNER_NAME_CLEAN) %>% 
  summarise(CATEGORY = first_not_na(CATEGORY)) %>% 
  select(-OWNER_NAME_CLEAN) %>% 
  drop_na

ngram_2_categorized <- names_ngrams %>% 
  left_join(cat_ngram_2, c("PUBLIC_NGRAM_2_A", "PUBLIC_NGRAM_2_B")) %>% 
  group_by(PIN,OWNER_NAME_CLEAN) %>% 
  summarise(CATEGORY = first_not_na(CATEGORY)) %>% 
  select(-OWNER_NAME_CLEAN) %>% 
  drop_na

ngram_3_categorized <- names_ngrams %>% 
  left_join(cat_ngram_3, c("PUBLIC_NGRAM_3_A", "PUBLIC_NGRAM_3_B", "PUBLIC_NGRAM_3_C")) %>% 
  group_by(PIN,OWNER_NAME_CLEAN) %>% 
  summarise(CATEGORY = first_not_na(CATEGORY)) %>% 
  select(-OWNER_NAME_CLEAN) %>% 
  drop_na

names_categorized <- 
  list(names_trimmed, 
       ngram_1_categorized,
       ngram_2_categorized,
       ngram_3_categorized) %>% 
  reduce(left_join, by = "PIN") %>% 
  group_by(PIN) %>% 
  summarise(OWNER_NAME = first(OWNER_NAME),
            OWNER_NAME_CLEAN = first(OWNER_NAME_CLEAN),
            OWNER_NAME_TRIM = first(OWNER_NAME_TRIM),
            OWNER_CATEGORY = first_not_na(c(CATEGORY.x,CATEGORY.y,CATEGORY))
  ) %>%
  transmute(PIN,
            OWNER_NAME = OWNER_NAME_CLEAN,
            OWNER_CATEGORY)



# VIEW CATEGORIES ----

# names_categorized %>% count(CATEGORY, sort = TRUE)

# names_categorized %>% filter(is.na(CATEGORY)) %>% count(OWNER_NAME_TRIM, sort = TRUE) %>% print(n=Inf)

# RETURN----

return(names_categorized)


}

# COMMAND: MAKE_OWNER_NONPROFIT_CATEGORIES ----
make_owner_nonprofit_categories <- function(parcel_ready, suitability_tax_exempt, owner_public_categories, owner_antijoin_names, owner_name_category_key, name_recode_key){
  
  
  names <- suitability_tax_exempt %>%  
    filter(SUIT_OWNER_NONPROFIT) %>% 
    inner_join(parcel_ready, by = "PIN") %>% 
    st_drop_geometry() %>% 
    transmute(PIN,
              OWNER_NAME = str_trim(toupper(TAXPAYER_NAME)))
  
 # CLEAN/RECODE NAMES ---- 
  
  names_cleaned_ngrams_1 <- names %>% 
    unnest_tokens(ORIG, OWNER_NAME, token = "ngrams", n = 1, to_lower = FALSE) %>%  
    left_join(name_recode_key, by = "ORIG") %>% 
    mutate(OWNER_NAME = if_else(is.na(NEW),ORIG,NEW)) %>% 
    group_by(PIN) %>% 
    summarise(OWNER_NAME_CLEAN = str_c(OWNER_NAME, collapse = " ")) %>% 
    full_join(names, by = "PIN") %>%  
    select(PIN,OWNER_NAME,OWNER_NAME_CLEAN)
  
  names_cleaned_ngrams_2 <- names_cleaned_ngrams_1 %>% 
    unnest_tokens(ORIG, OWNER_NAME_CLEAN, token = "ngrams", n = 2, to_lower = FALSE) %>%  
    left_join(name_recode_key, by = "ORIG") %>% 
    mutate(OWNER_NAME = if_else(is.na(NEW),ORIG,NEW)) %>%  
    arrange(PIN) %>% 
    separate(OWNER_NAME, c("WORD_1", "WORD_2"), sep = " ") %>%  
    gather(WORD, NAME_WORD, WORD_1:WORD_2) %>%  
    group_by(PIN) %>% 
    nest() %>% 
    group_by(PIN) %>% 
    summarise(OWNER_NAME_CLEAN = map_chr(data, ~ pluck(.x, "NAME_WORD") %>% unique %>% str_c(collapse = " "))) %>%
    full_join(names, by = "PIN") %>%  
    select(PIN,OWNER_NAME,OWNER_NAME_CLEAN)
  
  names_cleaned <- names_cleaned_ngrams_2
  
  names_trimmed <- names_cleaned %>% 
    unnest_tokens(ORIG, OWNER_NAME_CLEAN, token = "ngrams", n = 1, to_lower = FALSE) %>% 
    anti_join(owner_antijoin_names, by = c("ORIG" = "word")) %>%   
    group_by(PIN) %>% 
    summarise(OWNER_NAME_TRIM = str_c(ORIG, collapse = " ")) %>% 
    full_join(names_cleaned, by = "PIN") %>%  
    select(PIN,OWNER_NAME,OWNER_NAME_CLEAN,OWNER_NAME_TRIM)
  
  view_names_trimmed_cnt <- function(){
    names_trimmed_cnt <- names_trimmed %>% 
      count(OWNER_NAME_TRIM, sort = TRUE)
    
    return(names_trimmed_cnt)
  }
  
  # view_names_trimmed_cnt()
  
  # CREATE NGRAMS ----
  
  names_ngrams_1 <- names_trimmed %>%  
    unnest_tokens(NP_NGRAM_1, OWNER_NAME_TRIM, token = "ngrams", n = 1, to_lower = FALSE) %>% 
    select(-OWNER_NAME)
  
  names_ngrams_1_cnt <- names_ngrams_1 %>% 
    count(NP_NGRAM_1, sort = TRUE)
  
  names_ngrams_2 <- names_trimmed %>%  
    unnest_tokens(NP_NGRAM_2, OWNER_NAME_TRIM, token = "ngrams", n = 2, to_lower = FALSE) %>% 
    separate(NP_NGRAM_2, c("NP_NGRAM_2_A", "NP_NGRAM_2_B"),sep = " ") %>% 
    select(-OWNER_NAME)
  
  names_ngrams_2_cnt <- names_ngrams_2 %>% 
    count(NP_NGRAM_2_A,NP_NGRAM_2_B, sort = TRUE)
  
  names_ngrams_3 <- names_trimmed %>%  
    unnest_tokens(NP_NGRAM_3, OWNER_NAME_TRIM, token = "ngrams", n = 3, to_lower = FALSE) %>% 
    separate(NP_NGRAM_3, c("NP_NGRAM_3_A", "NP_NGRAM_3_B", "NP_NGRAM_3_C"),sep = " ") %>% 
    select(-OWNER_NAME)
  
  names_ngrams_3_cnt <- names_ngrams_3 %>% 
    count(NP_NGRAM_3_A,NP_NGRAM_3_B,NP_NGRAM_3_C, sort = TRUE)
  
  names_ngrams <- 
    list(names_cleaned,names_ngrams_1,names_ngrams_2,names_ngrams_3) %>% 
    reduce(left_join, by = c("PIN","OWNER_NAME_CLEAN"))
  
  
  # VIEW NGRAMS NETWORK ----

make_bigram_graph <- function(){
  bigram_graph <- names_ngrams_2 %>% 
  count(NGRAM_2_A,NGRAM_2_B, sort = TRUE) %>% 
  filter(n > 20) %>% 
  graph_from_data_frame
  
  set.seed(2016)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()
}

# make_bigram_graph()
  
  
  

# CREATE NGRAM CATEGORY KEYS ----

cat_ngram_1 <- owner_name_category_key %>% 
  select(CATEGORY,NP_NGRAM_1) %>% 
  drop_na

cat_ngram_2 <- owner_name_category_key %>% 
  select(CATEGORY,matches("NP_NGRAM_2")) %>% 
  drop_na

cat_ngram_3 <- owner_name_category_key %>% 
  select(CATEGORY,matches("NP_NGRAM_3")) %>% 
  drop_na



# PUBLIC AND NONPROFIT CATEGORIES ----

pub_join <- owner_public_categories %>% 
  group_by(OWNER_NAME) %>% 
  summarise(PUBLIC = first(OWNER_CATEGORY)) 


# JOIN CATEGORIES TABLE ----


ngram_1_categorized <- names_ngrams %>% 
  left_join(cat_ngram_1, by = "NP_NGRAM_1") %>% 
  group_by(PIN,OWNER_NAME_CLEAN) %>% 
  summarise(CATEGORY = first_not_na(CATEGORY)) %>% 
  select(-OWNER_NAME_CLEAN) %>% 
  drop_na

ngram_2_categorized <- names_ngrams %>% 
  left_join(cat_ngram_2, c("NP_NGRAM_2_A", "NP_NGRAM_2_B")) %>% 
  group_by(PIN,OWNER_NAME_CLEAN) %>% 
  summarise(CATEGORY = first_not_na(CATEGORY)) %>% 
  select(-OWNER_NAME_CLEAN) %>% 
  drop_na

ngram_3_categorized <- names_ngrams %>% 
  left_join(cat_ngram_3, c("NP_NGRAM_3_A", "NP_NGRAM_3_B", "NP_NGRAM_3_C")) %>% 
  group_by(PIN,OWNER_NAME_CLEAN) %>% 
  summarise(CATEGORY = first_not_na(CATEGORY)) %>% 
  select(-OWNER_NAME_CLEAN) %>% 
  drop_na

names_categorized <- 
  list(names_trimmed, 
       ngram_1_categorized,
       ngram_2_categorized,
       ngram_3_categorized) %>% 
  reduce(left_join, by = "PIN") %>% 
  group_by(PIN) %>% 
  summarise(OWNER_NAME = first(OWNER_NAME),
            OWNER_NAME_CLEAN = first(OWNER_NAME_CLEAN),
            OWNER_NAME_TRIM = first(OWNER_NAME_TRIM),
            OWNER_CATEGORY = first_not_na(c(CATEGORY.x,CATEGORY.y,CATEGORY))
  ) %>%
  transmute(PIN,
            OWNER_NAME = OWNER_NAME_CLEAN,
            OWNER_CATEGORY = if_else(is.na(OWNER_CATEGORY), "non-profit",OWNER_CATEGORY,OWNER_CATEGORY)) %>% 
  left_join(pub_join, by = "OWNER_NAME") %>% 
  group_by(PIN) %>% 
  summarise(OWNER_NAME = first(OWNER_NAME),
            OWNER_CATEGORY = first_not_na(c(PUBLIC, OWNER_CATEGORY))) %>% 
  ungroup



# VIEW CATEGORIES ----

# names_categorized %>% count(OWNER_CATEGORY, sort = TRUE)

# names_categorized %>% filter(is.na(OWNER_CATEGORY)) %>% count(OWNER_NAME, sort = TRUE) %>% print(n=Inf)

# RETURN----

return(names_categorized)


}


# COMMAND: MAKE_OWNER_EXEMPT_CATEGORIES ----
make_owner_exempt_categories <- function(parcel_ready, suitability_tax_exempt, owner_public_categories, owner_nonprofit_categories, owner_antijoin_names, owner_name_category_key, name_recode_key){
  
  
  names <- suitability_tax_exempt %>%  
    filter(SUIT_OWNER_OTHER_EXEMPT) %>% 
    inner_join(parcel_ready, by = "PIN") %>% 
    st_drop_geometry() %>% 
    transmute(PIN,
              OWNER_NAME = str_trim(toupper(TAXPAYER_NAME)))
  
 # CLEAN/RECODE NAMES ----
  
  names_cleaned <- names %>% 
    unnest_tokens(ORIG, OWNER_NAME, token = "ngrams", n = 1, to_lower = FALSE) %>%  
    left_join(name_recode_key, by = "ORIG") %>% 
    mutate(OWNER_NAME = if_else(is.na(NEW),ORIG,NEW)) %>% 
    group_by(PIN) %>% 
    summarise(OWNER_NAME_CLEAN = str_c(OWNER_NAME, collapse = " ")) %>% 
    full_join(names, by = "PIN") %>%  
    select(PIN,OWNER_NAME,OWNER_NAME_CLEAN)
  
  names_trimmed <- names_cleaned %>% 
    unnest_tokens(ORIG, OWNER_NAME_CLEAN, token = "ngrams", n = 1, to_lower = FALSE) %>% 
    anti_join(owner_antijoin_names, by = c("ORIG" = "word")) %>%   
    group_by(PIN) %>% 
    summarise(OWNER_NAME_TRIM = str_c(ORIG, collapse = " ")) %>% 
    full_join(names_cleaned, by = "PIN") %>%  
    select(PIN,OWNER_NAME,OWNER_NAME_CLEAN,OWNER_NAME_TRIM)
  
  view_names_trimmed_cnt <- function(){
    names_trimmed_cnt <- names_trimmed %>% 
      count(OWNER_NAME_TRIM, sort = TRUE)
    
    return(names_trimmed_cnt)
  }
  
  # view_names_trimmed_cnt()
  
  # CREATE NGRAMS ----
  
  names_ngrams_1 <- names_trimmed %>%  
    unnest_tokens(EXEMPT_NGRAM_1, OWNER_NAME_TRIM, token = "ngrams", n = 1, to_lower = FALSE) %>% 
    select(-OWNER_NAME)
  
  names_ngrams_1_cnt <- names_ngrams_1 %>% 
    count(EXEMPT_NGRAM_1, sort = TRUE)
  
  names_ngrams_2 <- names_trimmed %>%  
    unnest_tokens(EXEMPT_NGRAM_2, OWNER_NAME_TRIM, token = "ngrams", n = 2, to_lower = FALSE) %>% 
    separate(EXEMPT_NGRAM_2, c("EXEMPT_NGRAM_2_A", "EXEMPT_NGRAM_2_B"),sep = " ") %>% 
    select(-OWNER_NAME)
  
  names_ngrams_2_cnt <- names_ngrams_2 %>% 
    count(EXEMPT_NGRAM_2_A,EXEMPT_NGRAM_2_B, sort = TRUE)
  
  names_ngrams_3 <- names_trimmed %>%  
    unnest_tokens(EXEMPT_NGRAM_3, OWNER_NAME_TRIM, token = "ngrams", n = 3, to_lower = FALSE) %>% 
    separate(EXEMPT_NGRAM_3, c("EXEMPT_NGRAM_3_A", "EXEMPT_NGRAM_3_B", "EXEMPT_NGRAM_3_C"),sep = " ") %>% 
    select(-OWNER_NAME)
  
  names_ngrams_3_cnt <- names_ngrams_3 %>% 
    count(EXEMPT_NGRAM_3_A,EXEMPT_NGRAM_3_B,EXEMPT_NGRAM_3_C, sort = TRUE)
  
  names_ngrams <- 
    list(names_cleaned,names_ngrams_1,names_ngrams_2,names_ngrams_3) %>% 
    reduce(left_join, by = c("PIN","OWNER_NAME_CLEAN"))
  
  
  # VIEW NGRAMS NETWORK ----

make_bigram_graph <- function(){
  bigram_graph <- names_ngrams_2 %>% 
  count(EXEMPT_NGRAM_2_A,EXEMPT_NGRAM_2_B, sort = TRUE) %>% 
  filter(n > 20) %>% 
  graph_from_data_frame
  
  set.seed(2016)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()
}

# make_bigram_graph()
  
  
  

# CREATE NGRAM CATEGORY KEYS ----

cat_ngram_1 <- owner_name_category_key %>% 
  select(CATEGORY,EXEMPT_NGRAM_1) %>% 
  drop_na

cat_ngram_2 <- owner_name_category_key %>% 
  select(CATEGORY,matches("EXEMPT_NGRAM_2")) %>% 
  drop_na

cat_ngram_3 <- owner_name_category_key %>% 
  select(CATEGORY,matches("EXEMPT_NGRAM_3")) %>% 
  drop_na


# PUBLIC AND NONPROFIT CATEGORIES ----

pub_join <- owner_public_categories %>% 
  group_by(OWNER_NAME) %>% 
  summarise(PUBLIC = first(OWNER_CATEGORY)) 

np_join <- owner_nonprofit_categories %>% 
  group_by(OWNER_NAME) %>% 
  summarise(NONPROFIT = first(OWNER_CATEGORY))

# JOIN CATEGORIES TABLE ----


ngram_1_categorized <- names_ngrams %>% 
  left_join(cat_ngram_1, by = "EXEMPT_NGRAM_1") %>% 
  group_by(PIN,OWNER_NAME_CLEAN) %>% 
  summarise(CATEGORY = first_not_na(CATEGORY)) %>% 
  select(-OWNER_NAME_CLEAN) %>% 
  drop_na

ngram_2_categorized <- names_ngrams %>% 
  left_join(cat_ngram_2, c("EXEMPT_NGRAM_2_A", "EXEMPT_NGRAM_2_B")) %>% 
  group_by(PIN,OWNER_NAME_CLEAN) %>% 
  summarise(CATEGORY = first_not_na(CATEGORY)) %>% 
  select(-OWNER_NAME_CLEAN) %>% 
  drop_na

ngram_3_categorized <- names_ngrams %>% 
  left_join(cat_ngram_3, c("EXEMPT_NGRAM_3_A", "EXEMPT_NGRAM_3_B", "EXEMPT_NGRAM_3_C")) %>% 
  group_by(PIN,OWNER_NAME_CLEAN) %>% 
  summarise(CATEGORY = first_not_na(CATEGORY)) %>% 
  select(-OWNER_NAME_CLEAN) %>% 
  drop_na

names_categorized <- 
  list(names_trimmed, 
       ngram_1_categorized,
       ngram_2_categorized,
       ngram_3_categorized) %>% 
  reduce(left_join, by = "PIN") %>% 
  group_by(PIN) %>% 
  summarise(OWNER_NAME = first(OWNER_NAME),
            OWNER_NAME_CLEAN = first(OWNER_NAME_CLEAN),
            OWNER_NAME_TRIM = first(OWNER_NAME_TRIM),
            OWNER_CATEGORY = first_not_na(c(CATEGORY.x,CATEGORY.y,CATEGORY))
  ) %>%
  transmute(PIN,
            OWNER_NAME = OWNER_NAME_CLEAN,
            OWNER_CATEGORY = OWNER_CATEGORY) %>%
  left_join(pub_join, by = "OWNER_NAME") %>% 
  left_join(np_join, by = "OWNER_NAME") %>% 
  group_by(PIN) %>% 
  summarise(OWNER_NAME = first(OWNER_NAME),
            OWNER_CATEGORY = first_not_na(c(OWNER_CATEGORY,PUBLIC,NONPROFIT))) %>% 
  ungroup



# VIEW CATEGORIES ----

# names_categorized %>% count(OWNER_CATEGORY, sort = TRUE)
# 
# names_categorized %>%
#   filter(is.na(OWNER_CATEGORY)) %>%
#   count(OWNER_NAME, sort = TRUE) %>%
#   filter(n>10) %>%
#   print(n=Inf)

# RETURN----

return(names_categorized)


}


# COMMAND: MAKE_OWNER ----

make_owner <- function(parcel_ready, ...){
 # BIND OWNER CATEGORIES AND JOIN TO PARCEL_READY ----    
   p <- parcel_ready %>% 
    st_drop_geometry() %>% 
    select(PIN)
  
  owner <- reduce(list(...), bind_rows) %>% 
    right_join(p, by = "PIN") %>% 
    mutate(OWNER_CATEGORY = if_else(is.na(OWNER_CATEGORY),"uncategorized",OWNER_CATEGORY))
  
  # CREAT CLEAN NAMES ----
  
  city_names_clean <- parcel_ready %>% 
    pluck("MUNICIPALITY") %>%  
    unique() %>% 
    toupper() %>% 
    discard(is.na) %>% 
    c("TACOMA", "VASHON ISLAND") %>% 
    toupper %>% 
    str_replace_all("KING COUNTY", "UNINCORPORATED KING COUNTY") %>% 
    {tibble(OWNER_NAME_CLEAN = .)}
  
  city_names_clean_long <- city_names_clean %>% 
    mutate(OWNER_NAME_CLEAN_FULL = OWNER_NAME_CLEAN) %>% 
    unnest_tokens(TOKEN, OWNER_NAME_CLEAN, token = "ngrams", n = 1, to_lower = FALSE) %>% 
    select(TOKEN, OWNER_NAME_CLEAN_FULL) %>% 
    mutate(OWNER_NAME_CLEAN_FULL = if_else(OWNER_NAME_CLEAN_FULL %in% c("KING COUNTY", "UNINCORPORATED KING COUNTY"),
                                           OWNER_NAME_CLEAN_FULL,
                                           str_c("CITY OF ",OWNER_NAME_CLEAN_FULL)))
  
  tribe_names_clean <- tibble(TOKEN = c("MUCKLESHOOT",
                                        "SNOQUALMIE",
                                        "COWLITZ"),
                              OWNER_NAME_CLEAN_FULL = c("MUCKLESHOOT INDIAN TRIBE",
                                                        "SNOQUALMIE INDIAN TRIBE",
                                                        "COWLITZ INDIAN TRIBE"))
  
  nonprofit_names_clean <- owner_nonprofit_categories 
  
  nonprofit_names_trim <- owner_nonprofit_categories %>% 
    count(OWNER_NAME, sort = TRUE) %>% 
    filter(n > 3) %>% 
    drop_na() %>% 
    transmute(ID = row_number(),
              OWNER_NAME,
              TOKEN = OWNER_NAME) %>% 
    unnest_tokens(WORD, TOKEN, token = "ngrams", n = 1, to_lower = FALSE) %>% 
    anti_join(owner_antijoin_names, by = c("WORD" = "word")) %>% 
    group_by(ID) %>% 
    summarize(OWNER_NAME_CLEAN = first(OWNER_NAME),
              OWNER_NAME_TRIM = str_c(WORD, collapse = " ")) %>% 
    filter(!duplicated(OWNER_NAME_TRIM)) %>% 
    select(OWNER_NAME_CLEAN, OWNER_NAME_TRIM)
    
  
  
  # CREAT ANTI_JOIN TABLES ----   
  new_owner_antijoin_names <- tibble(TOKEN = c("CITY", "OF"))
  
  tribe_owner_antijoin_names <- tibble(TOKEN = c("INDIAN", "TRIBE"))
  
  # CREAT JOIN TABLES ----   
  
  clean_name_tbl <- 
    tribble(
      ~ OWNER_CATEGORY, ~OWNER_NAME_CLEAN,
      "uncategorized", NA_character_,
      "city", NA_character_,
      "county", "KING COUNTY",
      "non-profit", NA_character_,
      "school district", NA_character_,
      "federal", "US GOVERNMENT",
      "state", "WASHINGTON STATE",
      "special purpose district", NA_character_,
      "homeowners association", NA_character_,
      "port", "PORT OF SEATTLE",
      "tribal", NA_character_, 
      "regional transit authority", "SOUND TRANSIT"
    ) 
  
  city_names_join_tbl <- 
    owner %>%  
    mutate(OWNER_NAME_TOKEN = OWNER_NAME) %>% 
    left_join(clean_name_tbl, by = "OWNER_CATEGORY") %>%  
    filter(OWNER_CATEGORY %in% "city") %>% 
    unnest_tokens(TOKEN, OWNER_NAME_TOKEN, token = "ngrams", n = 1, to_lower = FALSE) %>% 
    anti_join(new_owner_antijoin_names ) %>% 
    stringdist_left_join(city_names_clean_long, by = c(TOKEN = "TOKEN"),  max_dist = 1, distance_col = "DIST") %>% 
    group_by(PIN) %>% 
    summarise(OWNER_NAME = first(OWNER_NAME),
              OWNER_NAME_CLEAN = first_not_na(OWNER_NAME_CLEAN_FULL)) %>%  
    select(-OWNER_NAME) 
  
  tribe_names_join_tbl <- 
    owner %>% 
    mutate(OWNER_NAME_TOKEN = OWNER_NAME) %>% 
    left_join(clean_name_tbl, by = "OWNER_CATEGORY") %>%
    filter(OWNER_CATEGORY %in% "tribal") %>%
    unnest_tokens(TOKEN, OWNER_NAME_TOKEN, token = "ngrams", n = 1, to_lower = FALSE) %>%
    anti_join(tribe_owner_antijoin_names) %>%
    stringdist_left_join(tribe_names_clean, by = c(TOKEN = "TOKEN"),  max_dist = 1, distance_col = "DIST") %>%
    group_by(PIN) %>%
    summarise(OWNER_NAME = first(OWNER_NAME),
              OWNER_NAME_CLEAN = first_not_na(OWNER_NAME_CLEAN_FULL)) %>%
    select(-OWNER_NAME)
  
  nonprofit_names_join_tbl <- 
    owner %>% 
    mutate(PIN,
           OWNER_NAME_TOKEN = OWNER_NAME) %>%  
    filter(OWNER_CATEGORY %in% "non-profit") %>%
    unnest_tokens(TOKEN, OWNER_NAME_TOKEN, token = "ngrams", n = 1, to_lower = FALSE) %>%
    anti_join(owner_antijoin_names, by = c("TOKEN" = "word")) %>% 
    group_by(PIN) %>% 
    summarize(OWNER_NAME = first(OWNER_NAME),
              OWNER_NAME_TRIM = str_c(TOKEN, collapse = " ")) %>% 
    stringdist_left_join(nonprofit_names_trim, by = c(OWNER_NAME_TRIM = "OWNER_NAME_TRIM"),  max_dist = 3, distance_col = "DIST") %>% 
    group_by(PIN) %>%
    summarise(OWNER_NAME_CLEAN = if_else(is.na(OWNER_NAME_CLEAN),OWNER_NAME,OWNER_NAME_CLEAN))  
  
  
  
  # JOIN THE TABLES ----   
  
  owner_names_consolidated <- 
    owner %>%  
    left_join(clean_name_tbl, by = "OWNER_CATEGORY") %>% 
    left_join(city_names_join_tbl, by = "PIN") %>% 
    left_join(tribe_names_join_tbl, by = "PIN") %>% 
    left_join(nonprofit_names_join_tbl, by = "PIN") %>%  
    gather(OLD_FIELD, OWNER_NAME_CLEAN, matches("OWNER_NAME_CLEAN")) %>% 
    group_by(PIN) %>% 
    summarize(OWNER_CATEGORY = first(OWNER_CATEGORY),
              OWNER_NAME = first(OWNER_NAME), 
              OWNER_NAME_CLEAN = first_not_na(OWNER_NAME_CLEAN)) %>% 
    transmute(PIN,
              OWNER_NAME,
              OWNER_NAME_CONSOLIDATED = if_else(is.na(OWNER_NAME_CLEAN), OWNER_NAME,OWNER_NAME_CLEAN),
              OWNER_CATEGORY)
  
  # RETURN ----   
  
  owner_ready <- owner_names_consolidated
  
  return(owner_ready)
}
# COMMAND: MAKE_CITY_BLOCK_SQFT   ----

make_city_block_sqft <- function(){
  city_block_sqft <- set_units(66000,ft^2)
  
  return(city_block_sqft)
  } 

# COMMAND: MAKE_CITY_BLOCK_ACRE   ----

make_city_block_acre <- function(...){
  
  city_block_acre <- set_units(city_block_sqft, acre)
  
  return(city_block_acre)
  
  } 


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


# COMMAND: MAKE_LOT_SIZE_BREAKS ----

make_lot_size_breaks <- function(...){
  
  lot_brks <- list(names = c("under-sized",
                             "quarter-block",
                             "half-block", 
                             "whole-block", 
                             "over-sized (developable)", 
                             "over-sized (undevelopable)"),
                    breaks = c( set_units(-Inf, acre),
                                set_units(0.1, acre), 
                                1/4 * city_block_acre, 
                                1/2 * city_block_acre, 
                                city_block_acre, 
                                set_units(27.5, acre) , 
                                set_units(Inf,acre) ))
  
  lot_size_breaks <- lot_brks
  
  return(lot_size_breaks)
  
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

# COMMAND: MAKE_CRITERIA_MAX_WATER_OVERLAP_PCT ----

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
     "Park Public Zoo Arbor",
     "Mortuary Cemetery Crematory",
     "Open Space Timber Land Greenbelt",
     "Open Space Current Use RCW 84.34",
     "Mining Quarry Ore Processing",
     "Farm",
     "Reserve Wilderness Area",
     "Open Space Agriculture RCW 84.34",
     "Forest Land Designated RCW 84.33",
     "Forest Land Class RCW 84.33",
     "Tideland 1st Class",
     "Tideland 2nd Class",
     "Air Terminal and Hangers",
     "Terminal Marine Commercial Fish",
     "River Creek Stream",
     "Art Gallery Museum Social Service",
     "Right of Way Utility Road",
     "Easement"
   )
   
  crit_undev_presentuse <- list( "undevelopable_presentuse" = undev_presentuse) 
  
  criteria_undevelopable_presentuse <-  crit_undev_presentuse
  
  return(criteria_undevelopable_presentuse)
  
}

# COMMAND: MAKE_CRITERIA_LOT_SIZE ----

make_criteria_lot_size <- function(...){ 
  
  # criteria_area <- list("area_max" = set_units(as.integer(40), acre),
  #                       "area_min" = set_units(as.double(1/8), acre))  # This is an educated-guess placeholder and may need to be adjusted 
  
  lot_sizes_discard <- c("under-sized", "over-sized (undevelopable)")
  
  criteria_lot_size <- lot_size_breaks %>% 
    pluck("names") %>% 
    discard(~ .x %in% lot_sizes_discard ) %>% 
    list %>% 
    set_names("lot_size")
  
  return(criteria_lot_size)
  
}

# COMMAND: MAKE_CRITERIA_AREA_RATIO ----

make_criteria_area_ratio <- function(){
  
  
  crit_ar <- list("area_ratio" = as.double(.2))  # This is an educated-guess placeholder and may need to be adjusted
  
  criteria_area_ratio <-  crit_ar
  
  return(criteria_area_ratio)
  
}


# COMMAND: MAKE_CRITERIA_STEEP_VACANT ----

make_criteria_steep_vacant <- function(){
  
  criteria_steep_vacant <- list("steep_vacant" = FALSE) 
  
  return(criteria_steep_vacant)
}

# COMMAND: MAKE_CRITERIA_OTHER ----

make_criteria_other <- function(){
  
  crit_other <- list("other" = FALSE)
  
  criteria_other <- crit_other
  
  return(criteria_other)
}

# COMMAND: MAKE_SUITABILITY_CRITERIA ----

make_suitability_criteria <- function(...){
  suitability_criteria <- list(...) %>% reduce(c)

}

# COMMAND: MAKE_SUITABILITY_TAX_EXEMPT ----

make_suitability_tax_exempt <- function(parcel_ready){
  
  tax_exempt <- parcel_ready %>%  
    st_drop_geometry() %>% 
    mutate(SUIT_OWNER_PUBLIC = if_else(ASSESSOR_PUB_LIST_LGL,TRUE,FALSE,FALSE),
           SUIT_OWNER_NONPROFIT = if_else(TAX_REASON %in% "non profit exemption",TRUE,FALSE,FALSE),
           SUIT_OWNER_OTHER_EXEMPT = if_else(TAX_REASON %in% "exempt" & !SUIT_OWNER_PUBLIC,TRUE,FALSE,FALSE),
           SUIT_OWNER_TAX_E = SUIT_OWNER_PUBLIC | SUIT_OWNER_NONPROFIT | SUIT_OWNER_OTHER_EXEMPT) %>% 
    select(PIN,
           SUIT_OWNER_PUBLIC,
           SUIT_OWNER_NONPROFIT,
           SUIT_OWNER_OTHER_EXEMPT,
           SUIT_OWNER_TAX_E) 
  
  return(tax_exempt)
}
# COMMAND: MAKE_SUITABILITY_WATER_OVERLAP ----

make_suitability_water_overlap <- function(parcel_sf_ready, waterbodies){
  # Convert to EPSG 2926 
  p_ready_poly <- parcel_sf_ready %>%  
    st_set_geometry("geometry") %>% 
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

make_suitability_within_uga <- function(parcel_sf_ready, uga){
  
  p_ready_pt <- parcel_sf_ready %>% 
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

make_suitability_developable_zoning <- function(...){
  
  p_pt <- parcel_sf_ready %>% 
    st_set_geometry("geom_pt") %>% 
    st_transform(2926) %>% 
    select(PIN)
  
  zng <- zoning %>% 
    st_transform(2926) %>% 
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


# COMMAND: MAKE_SUITABILITY_LOT_SIZE ----
make_suitability_lot_size <- function(...){
  
  # ~ 10 min. operation
  
  # select the largest polygon within the multipolygons
  p_area_poly <- parcel_sf_ready %>% 
    st_set_geometry("geometry") %>% 
    select_if(not_sfc) %>%  
    st_cast("POLYGON") %>% 
    mutate(SUIT_PARCEL_AREA = set_units(st_area(.), acre),
           SUIT_LOT_SIZE = cut(SUIT_PARCEL_AREA,
                               breaks = lot_size_breaks$breaks,
                               labels = lot_size_breaks$names)
           ) %>%
    group_by(PIN) %>% 
    arrange(desc(SUIT_PARCEL_AREA)) %>% 
    slice(1) %>% 
    ungroup %>% 
    st_drop_geometry() %>% 
    transmute(PIN, 
              SUIT_PARCEL_AREA,
              SUIT_LOT_SIZE)
  
  suitability_parcel_area <- p_area_poly
  
  
  return(suitability_parcel_area) 
}

# COMMAND: MAKE_SUITABILITY_PARCEL_AREA_RATIO ----
make_suitability_parcel_area_ratio <- function(parcel_sf_ready){
  
  # select the largest polygon within the multipolygons
  p_largest_polygons <- parcel_sf_ready %>% 
    st_set_geometry("geometry") %>% 
    select_if(not_sfc) %>%  
    st_cast("POLYGON") %>% 
    mutate(AREA_POLY = st_area(.)) %>%
    group_by(PIN) %>% 
    arrange(desc(AREA_POLY)) %>% 
    slice(1) %>% 
    ungroup 
  
  
  # ~ 34 min. operation 
  
  p_area_ratio <- p_largest_polygons %>%   
    transmute(PIN,
             SUIT_PARCEL_AREA_RATIO = st_area_ratio(.)) %>% 
    st_drop_geometry()
  
  return(p_area_ratio) 
}



# COMMAND: MAKE_SUITABILITY_STEEP_VACANT ----

make_suitability_steep_vacant <- function(...){
  
  p_ready_steep_vacant <- parcel_ready %>% 
    st_drop_geometry() %>% 
    transmute(PIN,
              SUIT_STEEP_VACANT = if_else(PRESENT_USE == "Vacant Single family" & TOPOGRAPHY, TRUE, FALSE, FALSE)
    )
  
  return(p_ready_steep_vacant)
  
}

# COMMAND: MAKE_SUITABILITY_OTHER ----

make_suitability_other <- function(...){
  
  p_ready_other <- parcel_sf_ready %>% 
    st_drop_geometry() %>% 
    left_join(other_suitability_characteristics, by = "PIN") %>% 
    transmute(PIN,
              SUIT_OTHER_TYPE = SUIT_OTHER,
              SUIT_OTHER = if_else(is.na(SUIT_OTHER),FALSE, TRUE, missing = FALSE),
              SUIT_OTHER_NOTE
              ) %>% 
    select(PIN,
           SUIT_OTHER,
           SUIT_OTHER_TYPE,
           SUIT_OTHER_NOTE) 
  
  return(p_ready_other)
  
}
# COMMAND: MAKE_SUITABILITY ----
make_suitability <- function(parcel_ready, suitability_criteria, ...){

  suitability <- list(...) %>% 
    reduce(left_join, by = "PIN") %>% 
    right_join(parcel_ready, by = "PIN") %>% 
    mutate(
      SUITABLE_OWNER_LGL = if_else(SUIT_OWNER_TAX_E == suitability_criteria[["tax_exempt"]],TRUE,FALSE,FALSE),
      SUITABLE_WATER_OVERLAP_LGL = if_else(SUIT_WATER_OVERLAP_PCT <= suitability_criteria[["water_overlap"]],TRUE,FALSE,FALSE),
      SUITABLE_WITHIN_UGA_LGL = if_else(SUIT_WITHIN_UGA == suitability_criteria[["within_uga"]],TRUE,FALSE,FALSE),
      SUITABLE_ZONING_CONSOL_20_LGL = if_else(SUIT_ZONING_CONSOL_20 %in% suitability_criteria[["developable_zoning"]],TRUE,FALSE,FALSE) ,
      SUITABLE_PRESENT_USE_LGL = if_else(! SUIT_PRESENT_USE %in% suitability_criteria[["undevelopable_presentuse"]],TRUE,FALSE,FALSE),
      SUITABLE_LOT_SIZE_LGL = if_else(SUIT_LOT_SIZE %in% suitability_criteria[["lot_size"]],TRUE,FALSE,FALSE),
      SUITABLE_PARCEL_AREA_RATIO_LGL = if_else(SUIT_PARCEL_AREA_RATIO >= suitability_criteria[["area_ratio"]],TRUE,FALSE,FALSE),
      SUITABLE_STEEP_VACANT_LGL = if_else(SUIT_STEEP_VACANT == suitability_criteria[["steep_vacant"]], TRUE, FALSE, FALSE),
      SUITABLE_OTHER_LGL = if_else(SUIT_OTHER == suitability_criteria[["other"]], TRUE, FALSE, FALSE),
      SUITABLE_LGL = SUITABLE_OWNER_LGL & SUITABLE_WATER_OVERLAP_LGL & SUITABLE_WITHIN_UGA_LGL & SUITABLE_ZONING_CONSOL_20_LGL & SUITABLE_PRESENT_USE_LGL & SUITABLE_LOT_SIZE_LGL & SUITABLE_PARCEL_AREA_RATIO_LGL & SUITABLE_STEEP_VACANT_LGL & SUITABLE_OTHER_LGL
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

make_building <- function(...){
  
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

make_util_criteria_lot_size <- function(city_block_sqft, lot_types){
  eight_block <- city_block_sqft/8
  
  quarter_block <- city_block_sqft/4
  
  crit_lot_size <- list("lot_size_breaks" = c(-Inf,eight_block,quarter_block, Inf),
                      "lot_size_labels" =  lot_types$LOT_SIZE_DESC)
  criteria_lot_size <- crit_lot_size
 
   return(criteria_lot_size)
}

# COMMAND: MAKE_CRITERIA_UTILIZATION_RATIO ----

make_util_criteria_utilization_ratio <- function(){
  
 crit_util_ratio <- list("ratio_gentle" = 1e-2,
                         "ratio_moderate" = 2/3,
                         "ratio_aggressive" = as.double(1)
 )
  
 criteria_utilization_ratio <- crit_util_ratio
 
 return(criteria_utilization_ratio)
  
  
}

# COMMAND: MAKE_UTILIZATION_CRITERIA ----

make_utilization_criteria <- function(...){
  utilization_criteria <- c(...)
  
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
make_utilization_lot_size <- function(...){
  
  util_ls <- parcel_ready %>% 
    st_drop_geometry() %>% 
    transmute(PIN,
              LOT_SIZE_DESC = as.character(cut(SQ_FT_LOT,
                               breaks = utilization_criteria[["lot_size_breaks"]],
                               labels = utilization_criteria[["lot_size_labels"]])))
  
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

make_utilization <- function(...){
   
  
  util_join <- suitability %>% 
    st_drop_geometry() %>% 
    select(PIN) %>% 
    left_join(utilization_present, by = "PIN") %>% 
    left_join(utilization_potential, by = "PIN") 
  
  util_ready <-  util_join %>%  
    mutate(UTILIZATION_RATIO = round(safe_divide(UTIL_PRESENT, UTIL_POTENTIAL_UTILIZATION_SQFT),2),
           UTIL_UNDER_UTILIZED_GENTLE_LGL = if_else(UTILIZATION_RATIO < utilization_criteria["ratio_gentle"],TRUE,FALSE,NA),
           UTIL_UNDER_UTILIZED_MODERATE_LGL = if_else(UTILIZATION_RATIO < utilization_criteria["ratio_moderate"],TRUE,FALSE,NA),
           UTIL_UNDER_UTILIZED_AGGR_LGL = if_else(UTILIZATION_RATIO < utilization_criteria["ratio_aggressive"],TRUE,FALSE,NA),
           UTILIZATION_GENTLE = if_else(!UTIL_UNDER_UTILIZED_GENTLE_LGL, "fully-utilized", "under-utilized", LOT_SIZE_TYPE),
           UTILIZATION_MODERATE = if_else(!UTIL_UNDER_UTILIZED_MODERATE_LGL, "fully-utilized", "under-utilized", LOT_SIZE_TYPE), 
           UTILIZATION_AGGR = if_else(!UTIL_UNDER_UTILIZED_AGGR_LGL, "fully-utilized", "under-utilized", LOT_SIZE_TYPE))
  
  utilization <- util_ready
  
  return(utilization)
  
}

# COMMAND: MAKE_FILTERS_TRACT ----
make_filters_census_tract <- function(parcel_sf_ready, census_tracts){
  
  p_ready_pt <- parcel_sf_ready %>% 
    st_set_geometry("geom_pt") %>% 
    st_transform(2926) %>% 
    transmute(PIN)
  
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
make_filters_zcta <- function(parcel_sf_ready, zcta){
  
  p <- parcel_sf_ready %>% 
    st_set_geometry("geom_pt") %>% 
    st_transform(2926) %>% 
    transmute(PIN)
  
  zcta_subdivide <- st_subdivide(zcta, 100) %>% 
    st_collection_extract() %>% 
    transmute(ZCTA = ZCTA5CE10)
   
  p$ZCTA <- st_over(p$geom_pt,zcta_subdivide, "ZCTA") 
  
  p_ready <- st_drop_geometry(p) 
  
  filters_zcta <- p_ready
  
  return(filters_zcta)
   
}

# COMMAND: MAKE_FILTERS_OWNER_TYPE ----

make_filters_owner_type <- function(parcel_ready, owner){
  
  p <- parcel_ready %>% 
    st_drop_geometry() %>% 
    select(PIN)
  
  p_own_cat <- p %>% 
    left_join(owner, by = "PIN") %>% 
    transmute(PIN,
              FILTER_OWNER_TYPE = if_else(OWNER_CATEGORY == "homeowners association", "non-profit", OWNER_CATEGORY) 
              )
  
  filters_owner_type <- p_own_cat
  
  return(filters_owner_type)
   
}

# COMMAND: MAKE_FILTERS_PUBLIC_OWNER ----
make_filters_public_owner <- function(...){ 
  
  p_ready_po <- owner %>%  
    transmute(PIN, 
              FILTER_PUBLIC_OWNER = if_else(OWNER_CATEGORY %in% c("non-profit","uncategorized", "homeowners association"),
                                            NA_character_,
                                            OWNER_NAME_CONSOLIDATED))
  
  filters_public_owner <- p_ready_po
  
  return(filters_public_owner)
   
}
 
# COMMAND: MAKE_FILTERS_PROXIMITY_TRANSIT ----
make_filters_proximity_transit <- function(...){
   
  p_pt <- parcel_sf_ready %>% 
    st_set_geometry("geom_pt") %>% 
    st_transform(2926) %>% 
    transmute(PIN)
  
  buffer_dist_qtr <- set_units(1/4, "mile")
  buffer_dist_half <- set_units(1/2, "mile")
  
  ts_buff <- transit_stops_osm
  
  ts_buff$geom_qtr_mi_buff <- st_buffer(st_geometry(ts_buff), buffer_dist_qtr)
  
  ts_buff$geom_half_mi_buff <- st_buffer(st_geometry(ts_buff), buffer_dist_half)
  
  append_qtr <- function(x) str_c(x,"QTR",  sep = "_")
  
  append_half <- function(x) str_c(x, "HALF",  sep = "_")
  
  ts_buff_qtr <- ts_buff %>% 
    st_set_geometry("geom_qtr_mi_buff") %>% 
    select_if(not_sfc) %>% 
    rename_all(append_qtr)
  
  ts_buff_half <- ts_buff %>% 
    st_set_geometry("geom_half_mi_buff") %>% 
    select_if(not_sfc) %>% 
    rename_all(append_half)

  
  # ~ 1 min. operation
  p_ts_qtr <- st_join(p_pt, ts_buff_qtr)
  
  # ~ 8 min. operation
  p_ts_half <- st_join(p_pt, ts_buff_half)
  
  # ~ 8 min. operation
  p_prox_trans_qtr <- p_ts_qtr %>% 
    st_drop_geometry() %>%    
    group_by(PIN) %>%  
    nest %>% 
    mutate(FILTER_PROXIMITY_TRANSIT_QTR = map_lgl(data, ~ !all(map_lgl(.x$TRANSIT_STOP_OSM_ID_QTR,is.na))),
           TRANSIT_STOP_TYPES_QTR = map_chr(data, ~ str_count_factor(.x$TRANSIT_STOP_TYPE_QTR))) %>% 
    transmute(PIN,
              FILTER_PROXIMITY_TRANSIT_QTR,
              TRANSIT_STOP_TYPES_QTR = if_else(FILTER_PROXIMITY_TRANSIT_QTR,TRANSIT_STOP_TYPES_QTR, NA_character_)) 
  
  # ~ 13 min. operation
  p_prox_trans_half <- p_ts_half %>% 
    st_drop_geometry() %>%    
    group_by(PIN) %>%  
    nest %>% 
    mutate(FILTER_PROXIMITY_TRANSIT_HALF = map_lgl(data, ~ !all(map_lgl(.x$TRANSIT_STOP_OSM_ID_HALF,is.na))),
           TRANSIT_STOP_TYPES_HALF = map_chr(data, ~ str_count_factor(.x$TRANSIT_STOP_TYPE_HALF))) %>% 
    transmute(PIN,
              FILTER_PROXIMITY_TRANSIT_HALF,
              TRANSIT_STOP_TYPES_HALF = if_else(FILTER_PROXIMITY_TRANSIT_HALF,TRANSIT_STOP_TYPES_HALF, NA_character_)) 
  
  p_prox_trans <- full_join(p_prox_trans_qtr,p_prox_trans_half, by = "PIN") %>% 
    mutate(FILTER_PROXIMITY_TRANSIT = case_when(
      FILTER_PROXIMITY_TRANSIT_QTR ~ "1/4 mile",
      FILTER_PROXIMITY_TRANSIT_HALF ~ "1/2 mile",
      TRUE ~ "Greater than 1/2 mile"
    ))
  
  
  filters_proximity_transit <- p_prox_trans
  
  return(filters_proximity_transit)
   
}

# COMMAND: MAKE_FILTERS_PROXIMITY_PLAY_SPACE ----
make_filters_proximity_play_space <- function(...){ 
  
  p_pt <- parcel_sf_ready %>% 
    st_set_geometry("geom_pt") %>% 
    st_transform(2926) %>% 
    transmute(PIN)
  
  buffer_dist_eighth <- set_units(1/8, "mile")
  buffer_dist_qtr <- set_units(1/4, "mile") 
  
  
  ps_buff <- play_spaces_osm
  
  ps_buff$geom_eighth_mi_buff <- st_buffer(st_geometry(ps_buff), buffer_dist_eighth)
  
  ps_buff$geom_qtr_mi_buff <- st_buffer(st_geometry(ps_buff), buffer_dist_qtr)
  
  
  
  append_eighth <- function(x) str_c(x, "EIGHTH",  sep = "_")
  
  append_qtr <- function(x) str_c(x,"QTR",  sep = "_")
  
  
  ps_buff_eighth <- ps_buff %>% 
    st_set_geometry("geom_eighth_mi_buff") %>% 
    select_if(not_sfc) %>% 
    rename_all(append_eighth)
  
  ps_buff_qtr <- ps_buff %>% 
    st_set_geometry("geom_qtr_mi_buff") %>% 
    select_if(not_sfc) %>% 
    rename_all(append_qtr)
  
 
    # ~ 1 min. operation
  p_ps_eighth <- st_join(p_pt, ps_buff_eighth)
  
  
  # ~ 1 min. operation
  p_ps_qtr <- st_join(p_pt, ps_buff_qtr)
  

    # ~ 12 min. operation
  
  p_prox_play_eighth <- p_ps_eighth %>% 
    st_drop_geometry() %>%    
    group_by(PIN) %>%  
    nest %>% 
    mutate(FILTER_PROXIMITY_PLAY_SPACE_EIGHTH = map_lgl(data, ~ !all(map_lgl(.x$PLAY_SPACE_OSM_ID_EIGHTH,is.na))),
           PLAY_SPACE_TYPE_EIGHTH = map_chr(data, ~ str_count_factor(.x$PLAY_SPACE_TYPE_EIGHTH))) %>% 
    transmute(PIN,
              FILTER_PROXIMITY_PLAY_SPACE_EIGHTH,
              PLAY_SPACE_TYPE_EIGHTH = if_else(FILTER_PROXIMITY_PLAY_SPACE_EIGHTH,PLAY_SPACE_TYPE_EIGHTH, NA_character_)) 

  # ~ 8 min. operation
  p_prox_play_qtr <- p_ps_qtr %>% 
    st_drop_geometry() %>%    
    group_by(PIN) %>%  
    nest %>% 
    mutate(FILTER_PROXIMITY_PLAY_SPACE_QTR = map_lgl(data, ~ !all(map_lgl(.x$PLAY_SPACE_OSM_ID_QTR,is.na))),
           PLAY_SPACE_TYPES_QTR = map_chr(data, ~ str_count_factor(.x$PLAY_SPACE_TYPE_QTR))) %>% 
    transmute(PIN,
              FILTER_PROXIMITY_PLAY_SPACE_QTR,
              PLAY_SPACE_TYPE_QTR = if_else(FILTER_PROXIMITY_PLAY_SPACE_QTR,PLAY_SPACE_TYPES_QTR, NA_character_)) 
  
  p_prox_play <- full_join(p_prox_play_eighth, p_prox_play_qtr, by = "PIN") %>% 
    mutate(FILTER_PROXIMITY_PLAY_SPACE = case_when(
      FILTER_PROXIMITY_PLAY_SPACE_EIGHTH ~ "1/8 mile",
      FILTER_PROXIMITY_PLAY_SPACE_QTR ~ "1/4 mile", 
      TRUE ~ "Greater than 1/4 mile"
    ))
  
  
  filters_proximity_play_space <- p_prox_play
  
  return(filters_proximity_play_space)
   
}

# COMMAND: MAKE_FILTERS_PROXIMITY_MARIJUANA ----
make_filters_proximity_marijuana <- function(...){ 
   p_prox_mj <-  parcel_sf_ready %>% 
    st_buffer(dist = set_units(1000, "ft")) %>% 
    transmute(PIN,
              FILTER_PROX_MJ_1000FT = st_intersects_any(.,mj_businesses),
              FILTER_PROX_MJ = if_else(FILTER_PROX_MJ_1000FT, "Less than 1000ft", "Greater than 1000ft")) %>% 
    st_drop_geometry()
  
  filters_proximity_marijuana <- p_prox_mj
  
  return(filters_proximity_marijuana)
   
}

# COMMAND: MAKE_FILTERS_PROXIMITY_EL_FACILITIES ----
make_filters_proximity_el_facilities <- function(...){ 
  
  p_prox_el <-  parcel_sf_ready %>% 
    st_buffer(dist = set_units(500, "ft")) %>% 
    transmute(PIN,
              FILTER_PROX_EL_FACILITIES_500FT = st_intersects_any(., el_facilities),
              FILTER_PROX_EL_FACILITIES = if_else(FILTER_PROX_EL_FACILITIES_500FT, "Less than 500ft", "Greater than 500ft")) %>% 
    st_drop_geometry()
  
  filters_proximity_el_facilities <- p_prox_el
  
  return(filters_proximity_el_facilities)
   
}

# COMMAND: MAKE_FILTERS_PROXIMITY_OPEN_SPACE ----
make_filters_proximity_open_space <- function(parcel_ready){
  
  # THIS IS NOT CURRENTLY IMPLEMENTED IN THE DRAKE PLAN (3/22/2018)
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

make_filters_leg_district <- function(...){
  
  p_pt <- parcel_sf_ready %>% 
    st_set_geometry("geom_pt") %>% 
    st_transform(2926) %>% 
    select(PIN)
  
  leg <- leg_districts %>% 
    transmute(LEGISLATIVE_DISTRICT = str_c("District ",LEGDST)) %>% 
    st_transform(2926) 
  
  leg_subd <- leg %>% 
    st_subdivide(max_vertices = 100) %>% 
    st_collection_extract() 
  
  p_leg <- p_pt
  
  p_leg$FILTER_LEGISLATIVE_DISTRICT <- st_over(p_leg, leg_subd, "LEGISLATIVE_DISTRICT") 
  
  # Deal with outliers
  
  outside_pins <- p_leg %>% 
    filter(is.na(FILTER_LEGISLATIVE_DISTRICT)) %>% 
    pluck("PIN")
  
  p_outside <- filter(p_pt, PIN %in% outside_pins)
  
  leg_buff_2000 <- st_buffer(leg, dist = 2000)
  
  p_outside$FILTER_LEGISLATIVE_DISTRICT_OUTSIDE <- st_over(p_outside, leg_buff_2000,"LEGISLATIVE_DISTRICT") %>% 
    st_drop_geometry()
  
  # Merge together
  
  p_leg_ready <- st_drop_geometry(p_leg) %>% 
    left_join(st_drop_geometry(p_outside), by = "PIN") %>% 
    arrange(FILTER_LEGISLATIVE_DISTRICT_OUTSIDE) %>% 
    transmute(PIN,
              FILTER_LEGISLATIVE_DISTRICT = case_when(
                !is.na(FILTER_LEGISLATIVE_DISTRICT_OUTSIDE) ~ FILTER_LEGISLATIVE_DISTRICT_OUTSIDE,
                !is.na(FILTER_LEGISLATIVE_DISTRICT) ~ FILTER_LEGISLATIVE_DISTRICT,
                TRUE ~ "Outside King County"
              ))
  
  leg_district <- p_leg_ready
  
  return(leg_district)
  
}

# COMMAND: MAKE_FILTERS_KC_COUNCIL_DISTRICT ----

make_filters_kc_council_district <- function(...){
  
  p_pt <- parcel_sf_ready %>% 
    st_set_geometry("geom_pt") %>% 
    st_transform(2926) %>% 
    select(PIN)
  
  kcc <- kc_council_districts %>% 
    transmute(KC_COUNCIL_DISTRICT = str_c("District ",KCCDST),
              KC_COUNCIL_MEMBER = COUNCILMEM) %>% 
    st_transform(2926) 
  
  kcc_members <- kcc %>% 
    st_drop_geometry %>% 
    select(KC_COUNCIL_DISTRICT,KC_COUNCIL_MEMBER) %>% 
    distinct()
  
  kcc_subd <- kcc %>% 
    st_subdivide(max_vertices = 100) %>% 
    st_collection_extract() 
  
  p_kcc <- p_pt
  
  p_kcc$FILTER_KC_COUNCIL_DISTRICT <- st_over(p_kcc, kcc_subd, "KC_COUNCIL_DISTRICT") 
  
  # Deal with outliers
  
  outside_pins <- p_kcc %>% 
    filter(is.na(FILTER_KC_COUNCIL_DISTRICT)) %>% 
    pluck("PIN")
  
  p_outside <- filter(p_pt, PIN %in% outside_pins)
  
  kcc_buff_2000 <- st_buffer(kcc, dist = 2000)
  
  p_outside$FILTER_KC_COUNCIL_DISTRICT_OUTSIDE <- st_over(p_outside, kcc_buff_2000,"KC_COUNCIL_DISTRICT") %>% 
    st_drop_geometry()
  
  # Merge together
  
  p_kcc_ready <- st_drop_geometry(p_kcc) %>% 
    left_join(st_drop_geometry(p_outside), by = "PIN") %>% 
    arrange(FILTER_KC_COUNCIL_DISTRICT_OUTSIDE) %>% 
    transmute(PIN,
              FILTER_KC_COUNCIL_DISTRICT = case_when(
                !is.na(FILTER_KC_COUNCIL_DISTRICT_OUTSIDE) ~ FILTER_KC_COUNCIL_DISTRICT_OUTSIDE,
                !is.na(FILTER_KC_COUNCIL_DISTRICT) ~ FILTER_KC_COUNCIL_DISTRICT,
                TRUE ~ "Outside King County"
              )) %>% 
    left_join(kcc_members, by = c(FILTER_KC_COUNCIL_DISTRICT = "KC_COUNCIL_DISTRICT"))
  
  kc_council_district <- p_kcc_ready
  
  return(kc_council_district)
  
}




# COMMAND: MAKE_FILTERS_SCHOOL_DISTRICT ----
make_filters_school_district <- function(...){
  
  p_pt <- parcel_sf_ready %>% 
    st_set_geometry("geom_pt") %>% 
    st_transform(2926) %>% 
    select(PIN)
  
  schl <- school_districts %>% 
    transmute(SCHOOL_DISTRICT = NAME) %>% 
    st_transform(2926) %>% 
    st_subdivide(max_vertices = 100) %>% 
    st_collection_extract() 
  
  p_schl <- p_pt
  
  p_schl$FILTER_SCHOOL_DISTRICT <- st_over(p_schl, schl, "SCHOOL_DISTRICT") 
  
  p_schl_ready <- st_drop_geometry(p_schl)
  
  school_district <- p_schl_ready
  
  return(school_district)
  
}

# COMMAND: MAKE_FILTERS_HISTORIC ----
make_filters_historic <- function(parcel_ready){
   
   p_ready_hist<- parcel_ready %>% 
    st_drop_geometry() %>% 
    transmute(PIN, 
              FILTER_HISTORIC_LGL = if_else(HISTORIC_SITE %in% "DESIGNATED",TRUE,FALSE, missing = FALSE)
              )
  
  filters_historic <- p_ready_hist
  
  return(filters_historic)
   
}

# COMMAND: MAKE_FILTERS_AFFORD_EXPIR_DATE ----
make_filters_afford_expir_date <- function(...){
  
  # THIS IS DUMMY DATA + SHOULD BE REPLACED  
 
  sample_dates <- c("20181231","20191231","20201231") %>% 
    map(ymd) %>% 
    map_dbl(pluck,1) %>% 
    as_date
  
   p_ready_afford_expir_date <- parcel_sf_ready %>% 
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
              FILTER_ELIGIBILITY_NMTC = if_else(NMTC %in% "Yes",TRUE,FALSE, missing = FALSE)
              ) 
  
  
   p_ready_eligibility_nmtc <- filters_census_tract %>% 
    st_drop_geometry() %>% 
    select(PIN, CENSUS_TRACT) %>% 
    left_join(elig_nmtc, by = "CENSUS_TRACT") %>% 
    transmute(PIN,
           FILTER_ELIGIBILITY_NMTC = if_else(FILTER_ELIGIBILITY_NMTC, TRUE, FALSE, missing = FALSE))
  
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
              FILTER_ELIGIBILITY_DDA = if_else(FILTER_ELIGIBILITY_DDA,TRUE,FALSE, missing = FALSE))
  
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
              FILTER_ELIGIBILITY_QCT = if_else(FILTER_ELIGIBILITY_QCT, TRUE, FALSE, missing = FALSE))
  
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

make_inventory <- function(...){
  
  inv <- reduce(list(...), left_join, by = "PIN") %>%  
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
    st_set_geometry("geometry") %>% 
    select(PIN)
  
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
                            "MUNICIPALITY",                                          "Municipality",
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
                             "MUNICIPALITY",                FALSE,              TRUE,              FALSE,             TRUE,            FALSE,
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
                       "MUNICIPALITY",  "King County Department of Assessments",
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

make_dd_google_drive <- function(trigger_dd_google_drive, ...){
  
  trigger <- trigger_dd_google_drive
  
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
