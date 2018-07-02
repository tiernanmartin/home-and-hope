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
  
  recode_key_list <- gs_read_all(recode_key_gs, delay_length = 6) 
  
  recode_key_list["ABBREVIATIONS"] <- NULL
  
  recode_key_ngram_long <- recode_key_list %>% 
    map_dfr(~ gather(.x, NGRAM_TYPE, WORD, -NAME_NEW, -NAME_ABBR))
  
  recode_key_ngram_wide <- recode_key_ngram_long %>% 
    group_by(NGRAM_TYPE) %>% 
    mutate(row = row_number()) %>% 
    spread(NGRAM_TYPE, WORD) %>% 
    replace_na(list(NAME_NEW = "")) %>% 
    arrange(row) %>% 
    select(-row)
  
  name_recode_key <- recode_key_ngram_wide
  
  write_rds(name_recode_key, recode_key_fp)
  
  return(name_recode_key)
  
  
}

# COMMAND: MAKE_PUBLIC_OWNER_NAME_CATEGORY_KEY ----
make_public_owner_name_category_key<- function(trigger_public_owner_name_category_key){
  
  trigger <- trigger_public_owner_name_category_key
  
  public_owner_name_category_key_fp <- here("1-data/1-raw/public_owner_name_category_key.rda")
  
  categories_gs <- gs_key("1Uhj9GcPP93hfGehK1dPmxgLbsAXxUOnHXY8PFSfMnRI")
  
  public_owner_name_category_key <- gs_read(categories_gs,ws = "JOIN_OFFICIAL_NAMES_TARGET")
  
  write_rds(public_owner_name_category_key, public_owner_name_category_key_fp)
  
  return(public_owner_name_category_key)
  
  
}

# COMMAND: MAKE_OTHER_EXEMPT_OWNER_NAME_CATEGORY_KEY ----------------------

make_other_exempt_owner_name_category_key<- function(trigger_other_exempt_owner_name_category_key){
  
  trigger <- trigger_other_exempt_owner_name_category_key
  
  other_exempt_owner_name_category_key_fp <- here("1-data/1-raw/other_exempt_owner_name_category_key.csv")
  
  categories_gs <- gs_key("1xRE5A2suzH_KcrrFpqdcX7fguFShokjFchm7khML6sQ")
  
  other_exempt_owner_name_category_key <- gs_read(categories_gs,ws = "OWNER_NAME_CATEGORIES")
  
  write_csv(other_exempt_owner_name_category_key, other_exempt_owner_name_category_key_fp)
  
  return(other_exempt_owner_name_category_key)
  
  
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
              ADDR_ADDRESS_FULL = ADDR_FULL,
              ADDR_CITY_NAME = CTYNAME,
              ADDR_CITY_NAME_POSTAL = POSTALCTYN,
              ADDR_ZIPCODE = factor(ZIP_5),
              ADDR_PRIMARY_ADDR_LGL = as.logical(PRIM_ADDR)) %>%
    mutate(ADDR_CITY_NAME = factor(case_when(
                all(is.na(ADDR_CITY_NAME),is.na(ADDR_CITY_NAME_POSTAL)) ~ NA_character_,
                is.na(ADDR_CITY_NAME) ~ ADDR_CITY_NAME_POSTAL,
                TRUE ~ ADDR_CITY_NAME
              ))) %>%
    mutate_if(is.factor, as.character) %>% 
  group_by(PIN) %>%
    arrange(desc(ADDR_PRIMARY_ADDR_LGL)) %>%
    slice(1) %>%
    ungroup

  parcel_addr_ready <- p_addr_ready
  
  return(parcel_addr_ready)
  
}

# COMMAND: MAKE_PARCEL_DF_READY ----

make_parcel_df_ready <- function(parcel_lookup, prop_type, pub_parcel, parcel_df){
  
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
    mutate(PIN = make_pin(MAJOR, MINOR),
           PROPERTY_NAME = str_clean_upper(PROP_NAME)) 

  pub_parcel_ready <- pub_parcel %>%
    transmute(PIN,
              ASSESSOR_PUB_LIST_LGL = TRUE)

  p_df_ready <- parcel_df_recoded %>% 
    left_join(prop_type, by = "PROP_TYPE") %>%
    left_join(pub_parcel_ready, by = "PIN") %>%
    mutate(ASSESSOR_PUB_LIST_LGL = if_else(is.na(ASSESSOR_PUB_LIST_LGL),FALSE,ASSESSOR_PUB_LIST_LGL)) %>%  
    transmute(PIN,
           PROPERTY_NAME,
           PROP_TYPE = PROP_TYPE_DESC,
           ASSESSOR_PUB_LIST_LGL, 
           DISTRICT_NAME = str_clean_upper(DISTRICT_NAME),
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
    transmute(PIN,
           TAXPAYER_NAME = str_clean_upper(TAXPAYERNAME),
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

make_parcel_ready <- function(parcel_addr_ready, parcel_env_ready, join_list){

  # MAKE PARCEL_READY 

  parcel_ready <- join_list %>%
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

# COMMAND: MAKE_KC_CITY ----
make_kc_city <- function(){
  
  kc_city_fp <- here("1-data/2-external/city_kc")
  
  kc_city_dr_id <- as_id("1ZdVJyon1ZFSSFq8-lBAYDDpsvN5tnSvk")
  
  kc_city_load <- 
    make_or_read2(fp = kc_city_fp,
                  dr_id = kc_city_dr_id,
                  skip_get_expr = FALSE,
                  get_expr = function(fp){
                    
                    # SOURCE: ftp://ftp.kingcounty.gov/gis-web/GISData/city_kc_SHP.zip
                      
                  },
                  make_expr = function(fp, dr_id){
                    zip_dir <- here("1-data/2-external")
                    
                    target_name <- "city_kc"
                    
                    drive_read_zip(dr_id = dr_id,
                                   dir_path = zip_dir,
                                   read_fun = st_read,
                                   target_name = target_name,
                                   .tempdir = FALSE, 
                                   stringsAsFactors = FALSE)
                  },
                  read_expr = function(fp){read_sf(fp,stringsAsFactors = FALSE)})
  
  
  kc_city <- kc_city_load %>% 
    transmute(CITY_NAME = toupper(CITYNAME)) %>% 
    st_transform(2926) 
  
  return(kc_city)
  
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

# COMMAND: MAKE_CENSUS_PLACE ----
make_census_place <- function(){
  
  census_place_fp <- here("1-data/2-external/census-place.gpkg")
  
  census_place_dr_id <- as_id("1YErKKDwe0rodI2ENDPdeVGCbBZ3EnNo3")
  
  census_place_load <- 
    make_or_read2(fp = census_place_fp,
                  dr_id = census_place_dr_id,
                  skip_get_expr = FALSE,
                  get_expr = function(fp){
                    
                    census_place <- tigris::places(state = "WA",cb = FALSE, year = 2015) %>% 
                      as_tibble() %>% 
                      st_sf()
                    
                    st_write(census_place, fp, driver = "GPKG")
                    
                    zip_path <- here("1-data/2-external/census_place.zip")
                    
                    zip_pithy(zip_path, fp)
                    
                    drive_folder <- as_id("0B5Pp4V6eCkhrdlJ3MXVaNW16T0U")
                  
                    drive_upload(zip_path, drive_folder)
                      
                  },
                  make_expr = function(fp, dr_id){
                    zip_dir <- here("1-data/2-external")
                    
                    target_name <- "census_place.gpkg"
                    
                    drive_read_zip(dr_id = dr_id,
                                   dir_path = zip_dir,
                                   read_fun = st_read,
                                   target_name = target_name,
                                   .tempdir = FALSE, 
                                   stringsAsFactors = FALSE)
                  },
                  read_expr = function(fp){read_sf(fp,stringsAsFactors = FALSE)})
  
  census_place <- census_place_load 
  
  return(census_place)
  
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


# COMMAND: MAKE_SEATTLE_COUNCIL_DISTRICTS ----
make_seattle_council_districts <- function(){
  
  seacc_dist_fp <- here("1-data/2-external/sccdst")
  
  seacc_dist_dr_id <- as_id("1XYq-tbAS2qJ57COhuQXZJfZOOlSRN-Fs")
  
  seacc_dist_load <- 
    make_or_read2(fp = seacc_dist_fp,
                  dr_id = seacc_dist_dr_id,
                  skip_get_expr = FALSE,
                  get_expr = function(fp){
                    
                    # SOURCE: ftp://ftp.kingcounty.gov/gis-web/GISData/sccdst_SHP.zip
                      
                  },
                  make_expr = function(fp, dr_id){
                    zip_dir <- here("1-data/2-external")
                    
                    target_name <- "sccdst"
                    
                    drive_read_zip(dr_id = dr_id,
                                   dir_path = zip_dir,
                                   read_fun = st_read,
                                   target_name = target_name,
                                   .tempdir = FALSE, 
                                   stringsAsFactors = FALSE)
                  },
                  read_expr = function(fp){read_sf(fp,stringsAsFactors = FALSE)})
  
  
  seattle_council_districts <- rename_if(seacc_dist_load, not_sfc, to_screaming_snake_case) %>% 
    transmute(SEATTLE_COUNCIL_DISTRICT = str_c("District", str_extract(SCCDST,"\\d"),sep = " "))
  
  return(seattle_council_districts)
  
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

# COMMAND: MAKE_MJ_BUSINESSES_RAW ----

make_mj_businesses_raw <- function(){
  
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
  
  
  mj_businesses_raw <- mj_biz
  
  return(mj_businesses_raw)
  
}

# COMMAND: MAKE_MJ_BUSINESSES ----

make_mj_businesses <- function(mj_businesses_raw){
  
  mj_biz_fp <- here("1-data/3-interim/mj-businesses.gpkg")
  
  mj_biz_dr_id <- as_id("1-H6XORJZscl4Pb5JNKoHbS4TZuqGYzkt")
  
  mj_biz_load <- 
    make_or_read2(fp = mj_biz_fp,
                  dr_id = mj_biz_dr_id,
                  skip_get_expr = FALSE,
                  mj_biz_raw = mj_businesses_raw,
                  get_expr = function(fp, mj_biz_raw){ 
                    
                    geocode_fun <- function(address){
                      geocode_url(address, 
                                  auth="standard_api", 
                                  privkey="AIzaSyAC19c3TtQwrSiQYKYDaf-9nDIqahirnD8",
                                  clean=TRUE, 
                                  add_date='today', 
                                  verbose=TRUE) 
                    }
                    
                    mj_biz_geocoded <- mj_biz_raw %>% 
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
                    
                    st_write(mj_biz_sf, fp, layer_options = "OVERWRITE=yes")
                    
                    drive_folder <- as_id("0B5Pp4V6eCkhrZ3NHOEE0Sl9FbWc")
                    
                    drive_upload(fp, drive_folder)
                    
                  },
                  make_expr = function(dr_id, fp){
                    drive_read(dr_id,.tempfile = FALSE,path = fp,read_fun = read_sf)
                    
                  },
                  read_expr = function(fp){read_sf(fp)})
   
  
  mj_businesses <- mj_biz_load
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

# COMMAND: MAKE_SEATTLE_DEV_CAP ----
make_seattle_dev_cap <- function(){
  
  sea_dev_cap_fp <- here("1-data/2-external/Capacity_For_All_Parcels_2015.csv")
  
  sea_dev_cap_dr_id <- as_id("1x8ZXulkzESeZBq5Ms2JrN0mnY9qH5uUv")
  
  sea_dev_cap_load <- 
    make_or_read2(fp = sea_dev_cap_fp,
                  dr_id = sea_dev_cap_dr_id,
                  skip_get_expr = FALSE,
                  get_expr = function(fp){
                    
                    # Source: https://data.seattle.gov/Land-Base/Capacity-For-All-Parcels-2015/n2mk-9di2
                      
                  },
                  make_expr = function(fp, dr_id){
                    
                    drive_read(dr_id = dr_id, .tempfile = FALSE, path = fp,read_fun = read_csv)
                  },
                  read_expr = function(fp){read_csv(fp)})
  
  seattle_dev_cap <- sea_dev_cap_load %>% 
    clean_names(case = "screaming_snake")
  
  return(seattle_dev_cap)
  
}

# COMMAND: MAKE_AFFORDABLE_HOUSING_SUBSIDIES_RAW ----

make_affordable_housing_subsidies_raw <- function(){
  
  subsidies_fp <- here("1-data/2-external/NHPD Subsidies Only Export.xlsx")
  
  subsidies_dr_id <- as_id("1s6XINugZf7aixbriH_rhzkuxC81PKp6z")
  
  subsidies_load <- 
    make_or_read2(fp = subsidies_fp,
                  dr_id = subsidies_dr_id,
                  skip_get_expr = FALSE,
                  get_expr = function(fp){
                    # SOURCE: http://nhpd.preservationdatabase.org/Data
                  },
                  make_expr = function(fp, dr_id){
                    drive_read(dr_id = dr_id,.tempfile = FALSE,path = fp,read_fun = read_excel)
                  },
                  read_expr = function(fp){read_excel(fp)}) 
  
  affordable_housing_subsidies_raw <- subsidies_load 
  
  return(affordable_housing_subsidies_raw)
  
}

# COMMAND: MAKE_AFFORDABLE_HOUSING_SUBSIDIES ----

make_affordable_housing_subsidies <- function(affordable_housing_subsidies_raw){
  
  subsidies_fp <- here("1-data/3-interim/affordable-housing-subsidies.gpkg")
  
  subsidies_dr_id <- as_id("13CoXbTeggKWbc91ucpVyp17XYp66SiDm")
  
  subsidies_load <- 
    make_or_read2(fp = subsidies_fp,
                  dr_id = subsidies_dr_id,
                  affordable_housing_subsidies_raw = affordable_housing_subsidies_raw, 
                  skip_get_expr = FALSE,
                  get_expr = function(fp, affordable_housing_subsidies_raw){ 
                    subsidies_df <- affordable_housing_subsidies_raw %>% 
                      clean_names(case = "screaming_snake") %>% 
                      filter(SUBSIDY_STATUS %in% c("Active","Inconclusive")) %>%  
                      select(SUBSIDY_NAME:MANAGER_TYPE, -LATITUDE, -LONGITUDE) %>% 
                      mutate(ADDRESS_FULL = str_c( STREET_ADDRESS, CITY, STATE, ZIP_CODE, sep = ", "))
                    
                    geocode_fun <- function(address){
                      geocode_url(address, 
                                  auth="standard_api", 
                                  privkey="AIzaSyCYhgjxQ0PRqo9VTHUrK1KnzaI65AGwZgs",
                                  clean=TRUE, 
                                  add_date='today', 
                                  verbose=TRUE) 
                    }
                    
                    subsidies_geocode_ready <- subsidies_df %>% 
                      slice(1) %>% 
                      bind_rows(subsidies_df)
                    
                    subsidies_geocoded <- subsidies_geocode_ready %>%  
                      mutate(geocode_output = map(ADDRESS_FULL, geocode_fun))  
                    
                    subsidies_sf <- subsidies_geocoded %>% 
                      unnest() %>% 
                      clean_names(case = "screaming_snake") %>% 
                      filter(!is.na(LAT)) %>% 
                      st_as_sf(coords = c("LNG", "LAT")) %>%   
                      st_set_crs(4326) %>% 
                      st_transform(2926) %>% 
                      distinct 
                    
                    st_write(subsidies_sf, fp, layer_options = "OVERWRITE=yes")
                    
                    drive_folder <- as_id("0B5Pp4V6eCkhrZ3NHOEE0Sl9FbWc")
                    
                    drive_upload(fp, drive_folder)
                    
                  },
                  make_expr = function(fp, dr_id){
                    drive_read(dr_id = dr_id,.tempfile = FALSE,path = fp,read_fun = read_sf)
                  },
                  read_expr = function(fp){read_sf(fp)})  
  
  affordable_housing_subsidies <- subsidies_load
  
  return(affordable_housing_subsidies)
  
}

# COMMAND: MAKE_AFFORDABLE_HOUSING_PROPERTIES_RAW ----

make_affordable_housing_properties_raw <- function(){
  
  properties_fp <- here("1-data/2-external/NHPD-properties-wa-active.xlsx")
  
  properties_dr_id <- as_id("1zm65rUoTqEGkKmcBz8ab2pKnWQnc5Aiv")
  
  properties_load <- 
    make_or_read2(fp = properties_fp,
                  dr_id = properties_dr_id,
                  skip_get_expr = FALSE,
                  get_expr = function(fp){
                    # SOURCE: http://nhpd.preservationdatabase.org/Data
                  },
                  make_expr = function(fp, dr_id){
                    drive_read(dr_id = dr_id,.tempfile = FALSE,path = fp,read_fun = read_excel)
                  },
                  read_expr = function(fp){read_excel(fp)}) 
  
  affordable_housing_properties_raw <- properties_load 
  
  return(affordable_housing_properties_raw)
}


# COMMAND: MAKE_AFFORDABLE_HOUSING_PROPERTIES ----

make_affordable_housing_properties <- function(affordable_housing_properties_raw){
  
  properties_fp <- here("1-data/3-interim/affordable-housing-properties.gpkg")
  
  properties_dr_id <- as_id("1VKYVytE7MbyBOEg0U4MEqMDZlHvBzETE")
  
  properties_load <- 
    make_or_read2(fp = properties_fp,
                  dr_id = properties_dr_id,
                  affordable_housing_properties_raw = affordable_housing_properties_raw, 
                  skip_get_expr = FALSE,
                  get_expr = function(fp, affordable_housing_properties_raw){ 
                    
                    # STEPS:
                    # 1. remove for-profit and profit-motivated properties
                    # 2. remove properties with fewer than 10 units
                    # 3. remove properties without property names
                    # 4. recode the target tenant type classes (reduce the number of levels)
                    # 5. convert to sf (skip geocoding - this can be done later if necessary)
                    
                    properties_sf <- affordable_housing_properties_raw %>% 
                      clean_names(case = "screaming_snake") %>% 
                      filter(OWNER_TYPE %!in% c("For Profit","Profit Motivated")) %>% 
                      filter(TOTAL_UNITS >= 10) %>% 
                      filter(!is.na(PROPERTY_NAME)) %>%   
                      transmute(PROPERTY_NAME,
                                PROPERTY_ADDRESS,
                                CITY = str_to_title(CITY),
                                STATE,
                                ZIP_CODE,
                                LATITUDE,
                                LONGITUDE,
                                TOTAL_UNITS, 
                                OWNER_NAME,
                                FAIR_MARKET_RENT,
                                TARGET_TENANT_TYPE = case_when(
                                  is.na(TARGET_TENANT_TYPE) ~ "Unknown",
                                  str_detect(TARGET_TENANT_TYPE, "Elderly|Disabled") ~ "Elderly or disabled",
                                  TRUE ~ TARGET_TENANT_TYPE
                                )) %>% 
                      st_as_sf(coords = c("LONGITUDE","LATITUDE" )) %>% 
                      st_set_crs(4326)
                    
                    
                    st_write(properties_sf, fp, layer_options = "OVERWRITE=yes")
                    
                    drive_folder <- as_id("0B5Pp4V6eCkhrZ3NHOEE0Sl9FbWc")
                    
                    drive_upload(fp, drive_folder)
                    
                  },
                  make_expr = function(fp, dr_id){
                    drive_read(dr_id = dr_id,.tempfile = FALSE,path = fp,read_fun = read_sf)
                  },
                  read_expr = function(fp){read_sf(fp)})  
  
  affordable_housing_properties <- properties_load
  
  return(affordable_housing_properties)
  
}

# COMMAND: MAKE_MUNICIPALITY ----

make_municipality <- function(parcel_ready){
  
  districts <- parcel_ready %>% 
    pluck("DISTRICT_NAME") %>% 
    discard(is.na) %>% 
    unique
  
  muni <- parcel_ready %>% 
    st_drop_geometry() %>% 
    transmute(PIN,
              DISTRICT_NAME = if_else(DISTRICT_NAME %in% "KING COUNTY", "UNINCORPORATED KC", DISTRICT_NAME),
              ADDR_CITY_NAME,
              ADDR_CITY_NAME_POSTAL,
              MUNICIPALITY = case_when(
                is.na(DISTRICT_NAME) & is.na(ADDR_CITY_NAME) ~ NA_character_,
                DISTRICT_NAME %in% "UNINCORPORATED KC" & ADDR_CITY_NAME %!in% districts ~ str_c(DISTRICT_NAME," (",ADDR_CITY_NAME,")"),
                TRUE ~ DISTRICT_NAME
              )) %>% 
    select(PIN,
           MUNICIPALITY)
  
municipality <- muni

return(muni)
  
  }




# COMMAND: MAKE_OFFICIAL_NAMES_SEATTLE----

make_official_names_seattle <- function(){
  
  names_seattle_fp <-  here("1-data/2-external/seattle-bureaucracy.csv")
  
  names_seattle_dr_id <- as_id("1ge_Gz-GS5Oa2KeGF97GTieVuIxtBYByo")
  
  names_seattle_load <- make_or_read2(fp = names_seattle_fp,dr_id = names_seattle_dr_id, skip_get_expr = TRUE,
                                 get_expr = function(fp){
                                   github_url <- "https://github.com/tiernanmartin/datasets/raw/master/seattle-bureaucracy/data/seattle-bureaucracy.csv"
                                 
                                   dat <- read_csv(github_url) 
                                   
                                   write_csv(dat, fp)
                                   
                                   drive_folder <- as_id("0B5Pp4V6eCkhrdlJ3MXVaNW16T0U")
                                   
                                   drive_upload(media = fp, drive_folder)
                                   
                                   },
                                 make_expr = function(fp, dr_id){
                                   drive_read(dr_id = dr_id,
                                              .tempfile = FALSE,
                                              path = fp,
                                              read_fun = read_csv)
                                 },
                                 read_expr = function(fp){read_csv(fp)})
  
  official_names_seattle <- names_seattle_load
  
  return(official_names_seattle)
  
}

# COMMAND: MAKE_OFFICIAL_NAMES_KC ----

make_official_names_kc <- function(){
  
  names_kc_fp <-  here("1-data/2-external/kc-bureaucracy.csv")
  
  names_kc_dr_id <- as_id("1c_e0BzC6vb-ddDtgTj_-2myPad06K0X5")
  
  names_kc_load <- make_or_read2(fp = names_kc_fp,dr_id = names_kc_dr_id, skip_get_expr = TRUE,
                                 get_expr = function(fp){
                                   github_url <- "https://github.com/tiernanmartin/datasets/raw/master/king-county-bureaucracy/data/kc-bureaucracy.csv"
                                 
                                   dat <- read_csv(github_url) 
                                   
                                   write_csv(dat, fp)
                                   
                                   drive_folder <- as_id("0B5Pp4V6eCkhrdlJ3MXVaNW16T0U")
                                   
                                   drive_upload(media = fp, drive_folder)
                                   
                                   },
                                 make_expr = function(fp, dr_id){
                                   drive_read(dr_id = dr_id,
                                              .tempfile = FALSE,
                                              path = fp,
                                              read_fun = read_csv)
                                 },
                                 read_expr = function(fp){read_csv(fp)})
  
  official_names_kc <- names_kc_load
  return(official_names_kc)
  
}

# COMMAND: MAKE_OFFICIAL_NAMES_WA ----
make_official_names_wa <- function(){
  
  names_wa_fp <-  here("1-data/2-external/wa-bureaucracy.csv")
  
  names_wa_dr_id <- as_id("1DavyPbOvzZTT-x4GiMVSoIfbFyanw3YS")
  
  names_wa_load <- make_or_read2(fp = names_wa_fp,dr_id = names_wa_dr_id, skip_get_expr = TRUE,
                                 get_expr = function(fp){
                                   github_url <- "https://raw.githubusercontent.com/tiernanmartin/datasets/master/wa-bureaucracy/data/wa-bureaucracy.csv"
                                 
                                   dat <- read_csv(github_url) 
                                   
                                   write_csv(dat, fp)
                                   
                                   drive_folder <- as_id("0B5Pp4V6eCkhrdlJ3MXVaNW16T0U")
                                   
                                   drive_upload(media = fp, drive_folder)
                                   
                                   },
                                 make_expr = function(fp, dr_id){
                                   drive_read(dr_id = dr_id,
                                              .tempfile = FALSE,
                                              path = fp,
                                              read_fun = read_csv)
                                 },
                                 read_expr = function(fp){read_csv(fp)})
  
  wa_state <- tibble(AGENCY_NAME = "State of Washington",
               FOCUS = NA_character_,
               TYPE = "State Government",
               ABBREVIATION = "WA")
  
  official_names_wa <- bind_rows(wa_state,names_wa_load) %>% 
    mutate(AGENCY_NAME = case_when(AGENCY_NAME %in% "Department of Transportation (DOT)" ~ "Department of Transportation (WSDOT)", TRUE ~ AGENCY_NAME),
           ABBREVIATION = case_when(ABBREVIATION %in% "DOT" ~ "WSDOT",TRUE ~ ABBREVIATION))
  
  
  
  return(official_names_wa)
  
}

# COMMAND: MAKE_OFFICIAL_NAMES_US ----

make_official_names_us <- function(){
  
  names_us_fp <-  here("1-data/2-external/us-bureaucracy.csv")
  
  names_us_dr_id <- as_id("1gK7o2sGuPD2glFPmEk65qIMqtdAOUH7w")
  
  names_us_load <- make_or_read2(fp = names_us_fp,dr_id = names_us_dr_id, skip_get_expr = TRUE,
                                 get_expr = function(fp){
                                   github_url <- "https://github.com/tiernanmartin/datasets/raw/master/us-bureaucracy/data/us-bureaucracy.csv"
                                 
                                   dat <- read_csv(github_url) 
                                   
                                   write_csv(dat, fp)
                                   
                                   drive_folder <- as_id("0B5Pp4V6eCkhrdlJ3MXVaNW16T0U")
                                   
                                   drive_upload(media = fp, drive_folder)
                                   
                                   },
                                 make_expr = function(fp, dr_id){
                                   drive_read(dr_id = dr_id,
                                              .tempfile = FALSE,
                                              path = fp,
                                              read_fun = read_csv)
                                 },
                                 read_expr = function(fp){read_csv(fp)}) 
  
  
  
  us <- tibble(NAME = "U.S. Federal Government",
               DEPARTMENT = NA_character_,
               ABBREVIATION = "US")
  
  official_names_us <- bind_rows(us, names_us_load)
  
  return(official_names_us)
  
}

# COMMAND: MAKE_OFFICIAL_NAMES_PLACES ----

make_official_names_places <- function(census_place){
  
  official_names_places <- census_place %>% 
    transmute(NAME = case_when(
                str_detect(NAMELSAD,"city$") ~ str_c("City of ",str_extract(NAMELSAD,".+(?=\\scity$)")),
                str_detect(NAMELSAD,"town$") ~ str_c("Town of ",str_extract(NAMELSAD,".+(?=\\stown$)")),
                str_detect(NAMELSAD,"CDP$") ~ str_c(str_extract(NAMELSAD,".+(?=\\sCDP$)")),
                TRUE ~ NA_character_
                
              )) %>% 
    st_drop_geometry()
    
    
    
  return(official_names_places)
  
}
# COMMAND: MAKE_OFFICIAL_NAMES_TRIBES ----

make_official_names_tribes <- function(){
  
  names_tribes_fp <-  here("1-data/2-external/wa-tribes.csv")
  
  names_tribes_dr_id <- as_id("1PHFhpBeRrDQm5UB0M0dlsAsa1tiJJYNU")
  
  names_tribes_load <- make_or_read2(fp = names_tribes_fp,dr_id = names_tribes_dr_id, skip_get_expr = TRUE,
                                 get_expr = function(fp){
                                   github_url <- "https://github.com/tiernanmartin/datasets/raw/master/wa-tribes/data/wa-tribes.csv"
                                 
                                   dat <- read_csv(github_url) 
                                   
                                   write_csv(dat, fp)
                                   
                                   drive_folder <- as_id("0B5Pp4V6eCkhrdlJ3MXVaNW16T0U")
                                   
                                   drive_upload(media = fp, drive_folder)
                                   
                                   },
                                 make_expr = function(fp, dr_id){
                                   drive_read(dr_id = dr_id,
                                              .tempfile = FALSE,
                                              path = fp,
                                              read_fun = read_csv)
                                 },
                                 read_expr = function(fp){read_csv(fp)})
  
  official_names_tribes <- names_tribes_load
  return(official_names_tribes)
  
}
# COMMAND: MAKE_OFFICIAL_NAMES_HOUSING_AUTHORITIES ----

make_official_names_housing_authorities <- function(){
  
  names_housing_authorities_fp <-  here("1-data/2-external/wa-housing-authorities.csv")
  
  names_housing_authorities_dr_id <- as_id("1wuuJJ37KyEbJUVAO865-t9VxxSfz3Bjc")
  
  names_housing_authorities_load <- make_or_read2(fp = names_housing_authorities_fp,dr_id = names_housing_authorities_dr_id, skip_get_expr = TRUE,
                                 get_expr = function(fp){
                                   github_url <- "https://github.com/tiernanmartin/datasets/raw/master/wa-housing-authorities/data/wa-housing-authorities.csv"
                                 
                                   dat <- read_csv(github_url) 
                                   
                                   write_csv(dat, fp)
                                   
                                   drive_folder <- as_id("0B5Pp4V6eCkhrdlJ3MXVaNW16T0U")
                                   
                                   drive_upload(media = fp, drive_folder)
                                   
                                   },
                                 make_expr = function(fp, dr_id){
                                   drive_read(dr_id = dr_id,
                                              .tempfile = FALSE,
                                              path = fp,
                                              read_fun = read_csv)
                                 },
                                 read_expr = function(fp){read_csv(fp)})
  
  official_names_housing_authorities <- names_housing_authorities_load
  return(official_names_housing_authorities)
  
}

# COMMAND: MAKE_OFFICIAL_NAMES_REGIONAL_TRANSIT_AUTHORITIES ----

make_official_names_regional_transit_authorities <- function(){
  
  
  official_names_regional_transit_authorities <- tibble(NAME = "Central Puget Sound Regional Transit Authority",
                                                        ORGANIZATION = "Sound Transit")
  
  return(official_names_regional_transit_authorities)
  
}
# COMMAND: MAKE_OFFICIAL_NAMES_SPECIAL_PURPOSE_DISTRICTS----

make_official_names_special_purpose_districts <- function(){
  
  names_special_purpose_districts_fp <-  here("1-data/2-external/wa-special-purpose-districts.csv")
  
  names_special_purpose_districts_dr_id <- as_id("1rArgC4Ft5SefwsmR9qaxA8OQj20PNNpw")
  
  names_special_purpose_districts_load <- make_or_read2(fp = names_special_purpose_districts_fp,dr_id = names_special_purpose_districts_dr_id, skip_get_expr = TRUE,
                                 get_expr = function(fp){
                                   github_url <- "https://github.com/tiernanmartin/datasets/raw/master//wa-special-purpose-districts/data/wa-special-purpose-districts.csv"
                                 
                                   dat <- read_csv(github_url) 
                                   
                                   write_csv(dat, fp)
                                   
                                   drive_folder <- as_id("0B5Pp4V6eCkhrdlJ3MXVaNW16T0U")
                                   
                                   drive_upload(media = fp, drive_folder)
                                   
                                   },
                                 make_expr = function(fp, dr_id){
                                   drive_read(dr_id = dr_id,
                                              .tempfile = FALSE,
                                              path = fp,
                                              read_fun = read_csv)
                                 },
                                 read_expr = function(fp){read_csv(fp)})
  
  official_names_special_purpose_districts <- names_special_purpose_districts_load %>% 
    filter(str_detect(COUNTIES_INCLUDED,"King")) %>%  
    filter(STATUS %in% "active") %>% 
    transmute(NAME = DISTRICT_NAME)
  
  return(official_names_special_purpose_districts)
  
}


# COMMAND: MAKE_OFFICIAL_NAMES_HIGHER-ED_PROVIDERS----

make_official_names_higher_ed_providers <- function(){
  
  names_higher_ed_providers_fp <-  here("1-data/2-external/wa-higher-ed-providers.csv")
  
  names_higher_ed_providers_dr_id <- as_id("1hqI1w6ocXg07ciEWA4hDHZ_e-CIhMK8N")
  
  names_higher_ed_providers_load <- make_or_read2(fp = names_higher_ed_providers_fp,dr_id = names_higher_ed_providers_dr_id, skip_get_expr = TRUE,
                                 get_expr = function(fp){
                                   github_url <- "https://raw.githubusercontent.com/tiernanmartin/datasets/master/wa-higher-ed-providers/data/wa-higher-ed-providers.csv"
                                 
                                   dat <- read_csv(github_url) 
                                   
                                   write_csv(dat, fp)
                                   
                                   drive_folder <- as_id("0B5Pp4V6eCkhrdlJ3MXVaNW16T0U")
                                   
                                   drive_upload(media = fp, drive_folder)
                                   
                                   },
                                 make_expr = function(fp, dr_id){
                                   drive_read(dr_id = dr_id,
                                              .tempfile = FALSE,
                                              path = fp,
                                              read_fun = read_csv)
                                 },
                                 read_expr = function(fp){read_csv(fp)})
  
  official_names_higher_ed_providers <- names_higher_ed_providers_load 
  
  return(official_names_higher_ed_providers)
  
}

# COMMAND: MAKE_OFFICIAL_NAMES_HOSPITALS----

make_official_names_hospitals <- function(){
  
  names_hospitals_fp <-  here("1-data/2-external/wa-hospitals.csv")
  
  names_hospitals_dr_id <- as_id("1OS_Yc0eX5lecEaQabkonRSrms2M4s-Z4")
  
  names_hospitals_load <- make_or_read2(fp = names_hospitals_fp,dr_id = names_hospitals_dr_id, skip_get_expr = TRUE,
                                 get_expr = function(fp){
                                   github_url <- "https://raw.githubusercontent.com/tiernanmartin/datasets/master/wa-hospitals/data/wa-hospitals.csv"
                                 
                                   dat <- read_csv(github_url) 
                                   
                                   write_csv(dat, fp)
                                   
                                   drive_folder <- as_id("0B5Pp4V6eCkhrdlJ3MXVaNW16T0U")
                                   
                                   drive_upload(media = fp, drive_folder)
                                   
                                   },
                                 make_expr = function(fp, dr_id){
                                   drive_read(dr_id = dr_id,
                                              .tempfile = FALSE,
                                              path = fp,
                                              read_fun = read_csv)
                                 },
                                 read_expr = function(fp){read_csv(fp)})
  
  official_names_hospitals <- names_hospitals_load %>% 
    select(NAME)
  
  return(official_names_hospitals)
  
}
# COMMAND: MAKE_OFFICIAL_NAMES ----

make_official_names <- function(official_names_seattle, official_names_kc, official_names_wa, official_names_us, official_names_places, official_names_tribes, official_names_housing_authorities, official_names_regional_transit_authorities, official_names_special_purpose_districts, official_names_higher_ed_providers, official_names_hospitals){
  
  # loadd(official_names_seattle, official_names_kc, official_names_wa, official_names_us, official_names_places, official_names_tribes, official_names_housing_authorities, official_names_special_purpose_districts,official_names_higher_ed_providers)
  
  
  names_list <- list(official_names_seattle, official_names_kc, official_names_wa, official_names_us, official_names_places, official_names_tribes, official_names_housing_authorities, official_names_regional_transit_authorities, official_names_special_purpose_districts, official_names_higher_ed_providers, official_names_hospitals)
  
  category_list <- list("city","county","state", "federal", "city", "tribal", "housing authority", "regional transit authority","special purpose district", "higher-education provider", "hospital")
  
  org_list <- list("City of Seattle", "King County", "Washington State", "U.S. Federal Government", NA_character_, NA_character_, NA_character_, "Sound Transit", NA_character_, NA_character_, NA_character_)
  
  make_category <- function(name, category){mutate(name, CATEGORY = category)}
  
  make_org <- function(name, org){mutate(name, ORGANIZATION = org)}
  
  names <- map2(names_list, category_list, make_category) %>%  
    map2(org_list, make_org) %>% 
    reduce(bind_rows) %>%   
    transmute(OWNER_NAME_OFFICIAL = case_when(!is.na(AGENCY_NAME) ~ AGENCY_NAME, 
                               !is.na(NAME) ~ NAME,
                               TRUE ~ DEPARTMENT),
              OWNER_NAME_DEPT = DEPARTMENT,
              OWNER_NAME_CATEGORY = CATEGORY,
              OWNER_NAME_ORG = case_when(is.na(ORGANIZATION) ~ NAME,TRUE ~ ORGANIZATION))
  
  official_names <- names
  
  return(official_names)
}

# COMMAND: MAKE_OWNER_NAME_FULL ----
make_owner_name_full <- function(...){
  
  # loadd(suitability, name_recode_key, owner_antijoin_names)
   
  
  # Drop unnecessary columns (too big if you don't)
  names <- suitability %>% 
    st_drop_geometry() %>% 
    transmute(PIN, 
           OWNER_NAME = str_trim(str_squish(str_replace_all(TAXPAYER_NAME,"[[:punct:]]"," "))),
           SUIT_OWNER_TAX_E)  
  
  names_tax_exempt <- names %>% 
    filter(SUIT_OWNER_TAX_E) %>% 
    select(-SUIT_OWNER_TAX_E)
  
  names_clean_ngram3_only <- names_tax_exempt %>% 
    mutate(TOKEN = OWNER_NAME) %>% 
    unnest_tokens(NGRAM3, TOKEN, token = "ngrams", n = 3, to_lower = FALSE) %>% 
    separate(NGRAM3, c("NAME_NGRAM_3A","NAME_NGRAM_3B", "NAME_NGRAM_3C"), sep = " ") %>% 
    left_join(name_recode_key) %>%  
    transmute(PIN,
              OWNER_NAME,
              NAME_NEW,
              PATTERN = str_c(NAME_NGRAM_3A,NAME_NGRAM_3B,NAME_NGRAM_3C, sep = " "),
              NEW = case_when(
                !is.na(NAME_NEW) ~ pmap_chr(list(OWNER_NAME,PATTERN, NAME_NEW), str_replace),
                TRUE ~ NA_character_
              )) %>%  
    group_by(PIN)  %>%   
    summarise(OWNER_NAME = if_else(all(is.na(NEW)), first(OWNER_NAME), first_not_na(NEW))) 
  
  names_clean_ngram3_all <- names_tax_exempt %>% 
    filter(PIN %!in% names_clean_ngram3_only$PIN) %>% 
    union(names_clean_ngram3_only)
  
  names_clean_ngram2_only <- names_clean_ngram3_all %>% 
    mutate(TOKEN = OWNER_NAME) %>% 
    unnest_tokens(NGRAM2, TOKEN, token = "ngrams", n = 2, to_lower = FALSE) %>% 
    separate(NGRAM2, c("NAME_NGRAM_2A","NAME_NGRAM_2B"), sep = " ") %>% 
    left_join(name_recode_key) %>%   
    transmute(PIN,
              OWNER_NAME,
              PATTERN = map2_chr(NAME_NGRAM_2A,NAME_NGRAM_2B, str_c, sep = " "),
              NEW = if_else(is.na(NAME_NEW), NA_character_, pmap_chr(list(OWNER_NAME, PATTERN, NAME_NEW), str_replace))) %>% 
    group_by(PIN)  %>%   
    summarise(OWNER_NAME = if_else(all(is.na(NEW)), first(OWNER_NAME), first_not_na(NEW))) 
  
  names_clean_ngram2_all <- names_tax_exempt %>% 
    filter(PIN %!in% names_clean_ngram2_only$PIN) %>% 
    union(names_clean_ngram2_only)
  
  names_clean_ngram1_only <- names_clean_ngram2_all %>% 
    unnest_tokens(ORIG, OWNER_NAME, token = "ngrams", n = 1, to_lower = FALSE) %>%  
    left_join(name_recode_key, by = c(ORIG = "NAME_NGRAM_1")) %>% 
    mutate(OWNER_NAME = if_else(is.na(NAME_NEW),ORIG,NAME_NEW)) %>% 
    group_by(PIN) %>% 
    summarise(OWNER_NAME = str_c(OWNER_NAME, collapse = " "))
  
  names_clean_ngram1_all <- names_tax_exempt %>% 
    filter(PIN %!in% names_clean_ngram1_only$PIN) %>% 
    union(names_clean_ngram1_only)
  
  # Fix inverted city names (Seattle City of -> City of Seattle)
  # also fix ports and state of wa
  names_clean_all_ngrams <- names_clean_ngram1_all %>% 
    mutate(NO_XXX_OF = str_replace_all(OWNER_NAME, "CITY OF","") %>% str_replace_all("PORT OF", ""), 
           OWNER_NAME = case_when(
             str_detect(OWNER_NAME, "CITY OF") ~  str_clean_upper(str_c("CITY OF ", NO_XXX_OF)),
             str_detect(OWNER_NAME, "PORT OF") ~  str_clean_upper(str_c("PORT OF ", NO_XXX_OF)), 
             str_detect(OWNER_NAME, "WASHINGTON STATE") ~ str_replace(OWNER_NAME, "WASHINGTON STATE", "STATE OF WASHINGTON"),
             TRUE ~ OWNER_NAME
           )) %>% 
    select(PIN,
           OWNER_NAME)
  
  # names_clean_all_ngrams %>%
  #   count(OWNER_NAME, sort = TRUE) %>%
  #   filter(!str_detect(OWNER_NAME, "CITY OF")) %>%
  #   filter(n>5) %>%
  #   print(n=Inf)
  
  # Remove typo duplicates
  common_owner_names <- 
  names_clean_all_ngrams %>% 
    count(OWNER_NAME, sort = TRUE) %>% 
    filter(n >= 5 ) %>% 
    transmute(ID = row_number(),
              OWNER_NAME,
              TOKEN = OWNER_NAME) %>% 
    unnest_tokens(WORD, TOKEN, token = "ngrams", n = 1, to_lower = FALSE) %>% 
    anti_join(owner_antijoin_names, by = c("WORD" = "word")) %>%  
    group_by(ID) %>% 
    summarize(OWNER_NAME = first(OWNER_NAME),
              OWNER_NAME_TRIM = str_c(WORD, collapse = " ")) %>% 
    filter(!duplicated(OWNER_NAME_TRIM)) %>% 
    transmute(OWNER_NAME_NO_TYPO = OWNER_NAME, 
              OWNER_NAME_TRIM_NO_TYPO = OWNER_NAME_TRIM)
  
  # Pull out the parcels with digits in the OWNER_NAME (these don't work for the typo join step)
  names_with_digits <- names_clean_all_ngrams %>% 
    filter(str_detect(OWNER_NAME, "[[:digit:]]"))
  
  names_without_digits <- names_clean_all_ngrams %>% 
    filter(PIN %!in% names_with_digits$PIN)
  
  names_without_typos <-   
    names_without_digits %>%  
    transmute(ID = row_number(), 
              PIN,
              OWNER_NAME,
              TOKEN = OWNER_NAME) %>%  
    unnest_tokens(WORD, TOKEN, token = "ngrams", n = 1, to_lower = FALSE) %>% 
    anti_join(owner_antijoin_names, by = c("WORD" = "word")) %>%  
    group_by(ID) %>% 
    summarize(PIN = first(PIN),
              OWNER_NAME = first(OWNER_NAME),
              OWNER_NAME_TRIM = str_c(WORD, collapse = " ")) %>% 
    stringdist_left_join(common_owner_names, by = c("OWNER_NAME_TRIM" = "OWNER_NAME_TRIM_NO_TYPO"),method = "jw",distance_col = "DIST",p=0.1, max_dist = .04) %>% 
    group_by(ID) %>%
    arrange(DIST) %>% 
    slice(1) %>% 
    ungroup %>% 
    bind_rows(names_with_digits) %>%  
    transmute(PIN,
              OWNER_NAME_FULL = if_else(is.na(OWNER_NAME_TRIM_NO_TYPO), OWNER_NAME, OWNER_NAME_NO_TYPO))  
  
  # Check what was removed
  # names_clean_all_ngrams %>% 
  #   filter(OWNER_NAME_FULL %!in% names_without_typos$OWNER_NAME) %>% 
  #   count(OWNER_NAME_FULL, sort = TRUE) %>% 
  #   print(n=Inf)
  
  
  owner_name_full <- names %>% 
    left_join(names_without_typos, by = "PIN") %>%   
    transmute(PIN, 
             OWNER_NAME_FULL = if_else(is.na(OWNER_NAME_FULL), OWNER_NAME, OWNER_NAME_FULL)) 
  
  # Check out the result
  # owner_name_full %>%
  #   filter(PIN %in% names_tax_exempt$PIN) %>%
  #   count(OWNER_NAME_FULL, sort = TRUE) %>% print(n=100)
  
  return(owner_name_full)
  
}
# COMMAND: MAKE_OWNER_CATEGORY ----
make_owner_category <- function(owner_name_full, public_owner_name_category_key, other_exempt_owner_name_category_key){
  
  
  loadd(owner_name_full, public_owner_name_category_key, other_exempt_owner_name_category_key)
  
  
  # 1. join public owners
  # 2. join other tax exempt (regex) and remove duplicates
  # 3. case_when() to condense owners whose names show up in public and other tax exempt
  
  category_types <- tibble(OWNER_CATEGORY = c("federal","state","county","city","special purpose district","housing authority","higher-education provider", "tribal","religious association","residential association","uncategorized")) %>% 
    mutate(OWNER_CATEGORY_TYPE = case_when(
      OWNER_CATEGORY %in% "higher-education provider" ~ "uncertain",
      OWNER_CATEGORY %in% c("tribal","religious association","residential association","uncategorized") ~ "private",
      TRUE ~ "public"
    ))
  
  
  owner_public <- owner_name_full %>% 
    left_join(public_owner_name_category_key, by = "OWNER_NAME_FULL")
  
  public_pins <- owner_name_full %>% 
    semi_join(public_owner_name_category_key, by = "OWNER_NAME_FULL") %>% 
    pluck("PIN")
  
  owner_tax_exempt <- owner_public %>% 
    regex_left_join(other_exempt_owner_name_category_key, by = c(OWNER_NAME_FULL = "OWNER_NAME_FULL_OTHER")) %>% 
    group_by(PIN) %>% 
    summarise_all(first_not_na)
  
  owner_category <- owner_tax_exempt %>% 
    transmute(PIN,
              OWNER_NAME_FULL = toupper(OWNER_NAME_FULL),
              OWNER_CATEGORY = case_when(
                !is.na(OWNER_CATEGORY) ~ OWNER_CATEGORY,
                !is.na(OWNER_NAME_FULL_OTHER) ~ OWNER_CATEGORY_OTHER,
                TRUE ~ "uncategorized"
              ),
              OWNER_NAME_ORG = case_when(
                !is.na(OWNER_NAME_ORG) ~ toupper(OWNER_NAME_ORG),
                TRUE ~ toupper(OWNER_NAME_FULL)
              ),
              OWNER_NAME_DEPT = toupper(OWNER_NAME_DEPT)) %>% 
    left_join(category_types, by = "OWNER_CATEGORY") %>% 
    mutate(OWNER_PUBLIC_LGL = case_when(
      OWNER_CATEGORY_TYPE %in% "uncertain" & PIN %in% public_pins ~ TRUE,
      OWNER_CATEGORY_TYPE %in% "public" ~ TRUE,
      TRUE ~ FALSE
    )
    ) %>% 
    select(PIN,
           OWNER_PUBLIC_LGL,
           OWNER_CATEGORY,
           OWNER_NAME_ORG,
           OWNER_NAME_DEPT)
    

# RETURN----

return(owner_category)


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
  
  dev_asmpt_fp <- here("1-data/3-interim/dev-assumptions-zoning.csv")
  
  dev_asmpt_dr_id <- as_id("1UHRi5NDgSQ-ideq74xMqHxMPCCfuPjOoxsWUgjCVWI8")
  
  dev_asmpt_load <- make_or_read2(fp = dev_asmpt_fp,
                                  dr_id = dev_asmpt_dr_id,
                                  skip_get_expr = FALSE,
                                  get_expr = function(fp){
                                    
                                    # This script created the original set of development assumptions.
                                    # 
                                    # The drive document is intended to be edited in Drive, which means
                                    # that it may *not* match the original version below. 
                                    
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
                 "Industrial/Manufacturing",             TRUE,                                      "no assumption for this class",
                             "Undesignated",             TRUE,                                      "no assumption for this class",
                                         NA,             TRUE,                                      "no assumption for this class",
                      "Agriculture-Related",            FALSE,                                      "no assumption for this class",
      "Aviation and Transportation-Related",            FALSE,                                      "no assumption for this class",
                                   "Forest",            FALSE,                                      "no assumption for this class",
                 "Mineral Resource-Related",            FALSE,                                      "no assumption for this class",
              "Mixed Use Commercial/Office",            FALSE,                                      "no assumption for this class",
                     "Office/Business Park",            FALSE,                                      "no assumption for this class",
        "Park/Golf Course/Trail/Open Space",            FALSE,                                      "no assumption for this class",
                               "Rural Area",            FALSE,                                      "no assumption for this class",
                  "Sensitive/Critical Area",            FALSE,                                      "no assumption for this class"
     )
                                    orig_fp <- here("1-data/1-raw/dev-assumptions-zoning-original.csv")
                                    
                                    write_csv(dev_assumptions_tbl, orig_fp)
                                    
                                    dr_folder <- as_id("0B5Pp4V6eCkhrb1lDdlNaOFY4V0U")
                                    
                                    drive_upload(orig_fp, dr_folder, type = "spreadsheet")
                                    
                                  },
                                  make_expr = function(fp, dr_id){
                                    drive_download(dr_id, fp, type = "csv")
                                    read_csv(fp)
                                    
                                  },
                                  read_expr = function(fp){read_csv(fp)})
  
  
  
  development_assumptions_zoning <- dev_asmpt_load
  
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
  
   list_uses <- function(){
                      parcel_ready %>%
                        st_drop_geometry() %>%
                        count(PRESENT_USE, sort = TRUE) %>%
                        print(n = Inf)
                    }
   
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
    summarise(BLDG_NBR = max(n(),na.rm = TRUE),  
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

# COMMAND: MAKE_CRITERIA_UTILIZATION_RATIO_BINS ----

make_util_criteria_utilization_ratio_bins <- function(){
  
 crit_util_ratio_bins <-  list("util_ratio_breaks" = c(-Inf,.25,.5,.75,1,Inf),
                      "util_ratio_labels" =  c("Less than 1/4","1/4 to 1/2", "1/2 to 3/4", "3/4 to 1", "Greater than 1")
                      ) 
  
 criteria_utilization_ratio_bins <- crit_util_ratio_bins
 
 return(criteria_utilization_ratio_bins)
  
  
}

# COMMAND: MAKE_UTILIZATION_CRITERIA ----

make_utilization_criteria <- function(...){
  utilization_criteria <- c(...)
  
  return(utilization_criteria)
}

# COMMAND: MAKE_UTILILIZATION_SEATTLE_UTIL_RATIO ----

make_seattle_util_ratio <- function(...){
  
  p <- parcel_sf_ready %>% 
    select(PIN) %>% 
    st_drop_geometry()
  
  sea_util_ratio <- p %>% 
    inner_join(seattle_dev_cap, by = "PIN") %>%  
    transmute(PIN,
              SEATTLE_UTIL_RATIO_DBL = DR) %>% 
    right_join(p, by = "PIN") 
  
  seattle_util_ratio <- sea_util_ratio
  
  return(seattle_util_ratio)
  
}

# COMMAND: MAKE_UTILILIZATION_PRESENT ----

make_utilization_present <- function(parcel_ready, building){
  
  util_present <- parcel_ready %>% 
    st_drop_geometry() %>% 
    select(PIN) %>% 
    left_join(building, by = "PIN") %>% 
    mutate(BLDG_NBR = if_else(is.na(BLDG_NBR), as.integer(0), BLDG_NBR),
           UTIL_PRESENT = if_else(is.na(BLDG_NET_SQ_FT),0,as.double(BLDG_NET_SQ_FT),missing = 0),
           UTIL_BUILDING = if_else(BLDG_NBR > 0, TRUE, FALSE, missing = TRUE)
           )
  
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
    left_join(utilization_potential, by = "PIN") %>% 
    left_join(seattle_util_ratio, by = "PIN")
  
  util_ready <-  util_join %>%  
    mutate(SEATTLE_UTIL_RATIO_DBL_TRIM = if_else(SEATTLE_UTIL_RATIO_DBL > 5, 5, SEATTLE_UTIL_RATIO_DBL),
           SEATTLE_UTIL_RATIO_CAT = cut(SEATTLE_UTIL_RATIO_DBL, breaks = utilization_criteria[["util_ratio_breaks"]], labels = utilization_criteria[["util_ratio_labels"]]) %>% as.character(),
           UTILIZATION_RATIO = round(safe_divide(UTIL_PRESENT, UTIL_POTENTIAL_UTILIZATION_SQFT),2),
           UTILIZATION_RATIO_CAT = cut(UTILIZATION_RATIO, right = FALSE, breaks = utilization_criteria[["util_ratio_breaks"]], labels = utilization_criteria[["util_ratio_labels"]]) %>% as.character(),
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

# COMMAND: MAKE_FILTERS_PLACE ----
make_filters_place <- function(parcel_sf_ready, census_place){
  
  p <- parcel_sf_ready %>% 
    st_set_geometry("geom_pt") %>% 
    st_transform(2926) %>% 
    transmute(PIN) 
  
  census_place_subdivide <- census_place %>% 
    transmute(PLACE = str_squish(NAME)) %>% 
    st_subdivide(100) %>% 
    st_transform(2926) %>% 
    st_collection_extract()
    
  p$PLACE <- st_over(p$geom_pt,census_place_subdivide, "PLACE")  
  
  
  p_ready <- p %>% 
    transmute(PIN,
              FILTER_PLACE = if_else(is.na(PLACE),"Unnamed, Unincorporated King County",PLACE) %>% toupper) %>% 
    st_drop_geometry() 
  
  filters_place <- p_ready
  
  return(filters_place)
   
}

# COMMAND: MAKE_FILTERS_PLACE_NAME ----

make_filters_place_name <- function(parcel_df_ready, filters_place){
  
  loadd(parcel_df_ready, filters_place)
  
  
  districts <- parcel_df_ready %>% 
    pluck("DISTRICT_NAME") %>% 
    discard(is.na) %>% 
    unique
  
   filters_place_name <- parcel_df_ready %>% 
    st_drop_geometry() %>% 
    select(PIN, DISTRICT_NAME) %>% 
    left_join(filters_place, by = "PIN") %>% 
    transmute(PIN,
              FILTER_PLACE_NAME = case_when(
                DISTRICT_NAME %in% "KING COUNTY" & FILTER_PLACE %in% districts ~ "UNINCORPORATED KC",
                DISTRICT_NAME %in% "KING COUNTY" & is.na(FILTER_PLACE) ~ "UNINCORPORATED KC",
                DISTRICT_NAME %in% "KING COUNTY" & str_detect(FILTER_PLACE, "UNNAMED") ~ "UNINCORPORATED KC",
                DISTRICT_NAME %in% "KING COUNTY" ~ str_c("UNINCORPORATED KC (",FILTER_PLACE,")"),
                DISTRICT_NAME %in% districts ~ DISTRICT_NAME,
                is.na(DISTRICT_NAME) & !is.na(FILTER_PLACE) ~ FILTER_PLACE,
                TRUE ~ "OTHER"
              ))  


return(filters_place_name)
  
  }

# COMMAND: MAKE_FILTERS_OWNER_CATEGORY ----

make_filters_owner_category <- function(owner_category){
  
  filters_owner_category <- owner_category %>% 
    transmute(PIN,
             FILTER_OWNER_CATEGORY = OWNER_CATEGORY)
  
  return(filters_owner_category)
   
}

# COMMAND: MAKE_FILTERS_PUBLIC_OWNER ----
make_filters_public_owner <- function(owner_category){ 
  
  filters_public_owner <- owner_category %>% 
    transmute(PIN,
             FILTER_PUBLIC_OWNER = case_when(
               OWNER_PUBLIC_LGL ~ OWNER_NAME_ORG,
               TRUE ~ NA_character_
             ))
  
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

# COMMAND: MAKE_FILTERS_PROXIMITY_AFFORDABLE_HOUSING ----
make_filters_proximity_affordable_housing <- function(parcel_sf_ready, affordable_housing_properties){ 
  
  loadd(parcel_sf_ready, affordable_housing_properties)
  
  p_pt <- parcel_sf_ready %>% 
    st_set_geometry("geom_pt") %>% 
    st_transform(2926) %>% 
    transmute(PIN)
  
  buffer_dist_qtr <- set_units(1/4, "mile") 
  buffer_dist_half <- set_units(1/2, "mile")
  
  ah_buff <- affordable_housing_properties %>% 
    st_transform(2926)
  
  ah_buff$geom_qtr_mi_buff <- st_buffer(st_geometry(ah_buff), buffer_dist_qtr)
  
  ah_buff$geom_half_mi_buff <- st_buffer(st_geometry(ah_buff), buffer_dist_half)
  
  
  append_qtr <- function(x) str_c(x,"QTR",  sep = "_")
  
  append_half <- function(x) str_c(x, "HALF",  sep = "_")
  
  
  ah_buff_qtr <- ah_buff %>% 
    st_set_geometry("geom_qtr_mi_buff") %>% 
    select_if(not_sfc) %>% 
    rename_all(append_qtr)
  
    ah_buff_half <- ah_buff %>% 
    st_set_geometry("geom_half_mi_buff") %>% 
    select_if(not_sfc) %>% 
    rename_all(append_half)
  
  # ~ 1 min. operation
 
  p_ah_qtr <- st_join(p_pt, ah_buff_qtr)
   
   # ~ 1 min. operation
 
  p_ah_half <- st_join(p_pt, ah_buff_half)

  
  
  # ~ 11 min. operation
  
 
  p_prox_afford_qtr <- p_ah_qtr %>% 
    st_drop_geometry() %>%    
    group_by(PIN) %>%  
    nest %>% 
    mutate(FILTER_PROXIMITY_AFFORDABLE_HOUSING_QTR = map_lgl(data, ~ !all(map_lgl(.x$PROPERTY_NAME_QTR,is.na))),
           AFFORDABLE_HOUSING_TYPES_QTR = map_chr(data, ~ str_count_factor(.x$TARGET_TENANT_TYPE_QTR))) %>% 
    transmute(PIN,
              FILTER_PROXIMITY_AFFORDABLE_HOUSING_QTR,
              AFFORDABLE_HOUSING_TYPE_QTR = if_else(FILTER_PROXIMITY_AFFORDABLE_HOUSING_QTR,AFFORDABLE_HOUSING_TYPES_QTR, NA_character_)) 
  
  
      # ~ 10 min. operation
 
  p_prox_afford_half <- p_ah_half %>%  
    st_drop_geometry() %>%    
    group_by(PIN) %>%  
    nest %>% 
    mutate(FILTER_PROXIMITY_AFFORDABLE_HOUSING_HALF = map_lgl(data, ~ !all(map_lgl(.x$PROPERTY_NAME_HALF,is.na))),
           AFFORDABLE_HOUSING_TYPE_HALF = map_chr(data, ~ str_count_factor(.x$TARGET_TENANT_TYPE_HALF))) %>% 
    transmute(PIN,
              FILTER_PROXIMITY_AFFORDABLE_HOUSING_HALF,
              AFFORDABLE_HOUSING_TYPE_HALF = if_else(FILTER_PROXIMITY_AFFORDABLE_HOUSING_HALF,AFFORDABLE_HOUSING_TYPE_HALF, NA_character_)) 

  
  p_prox_afford <- full_join(p_prox_afford_qtr, p_prox_afford_half, by = "PIN") %>% 
    mutate(FILTER_PROXIMITY_AFFORDABLE_HOUSING = case_when( 
      FILTER_PROXIMITY_AFFORDABLE_HOUSING_QTR ~ "1/4 mile", 
      FILTER_PROXIMITY_AFFORDABLE_HOUSING_HALF ~ "1/2 mile",
      TRUE ~ "Greater than 1/2 mile"
    ))
  
  
  filters_proximity_affordable_housing <- p_prox_afford
  
  return(filters_proximity_affordable_housing)
   
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




# COMMAND: MAKE_FILTERS_SEA_COUNCIL_DISTRICT ----

make_filters_seattle_council_district <- function(...){
  
  p_pt <- parcel_sf_ready %>% 
    st_set_geometry("geom_pt") %>% 
    st_transform(2926) %>% 
    select(PIN)
  
  seacc <- seattle_council_districts %>%   
    st_transform(2926) 
  
  seacc_subd <- seacc %>% 
    st_subdivide(max_vertices = 100) %>% 
    st_collection_extract() 
  
  p_seacc <- p_pt
  
  p_seacc$FILTER_SEATTLE_COUNCIL_DISTRICT <- st_over(p_seacc, seacc_subd, "SEATTLE_COUNCIL_DISTRICT")  
  
  filter_seattle_council_district <- p_seacc %>% 
    st_drop_geometry()
  
  return(filter_seattle_council_district)
  
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
  
   p_ready_afford_expir_date <- parcel_sf_ready %>%  
     st_join(affordable_housing_subsidies) %>% 
    st_drop_geometry() %>% 
    transmute(PIN, 
              FILTER_AFFORD_TYPE = SUBSIDY_NAME,
              FILTER_AFFORD_UNITS = ASSISTED_UNITS,
              FILTER_AFFORD_EXPIR_DATE = as.Date(END_DATE)
              ) %>% 
     group_by(PIN) %>% 
     arrange(FILTER_AFFORD_EXPIR_DATE) %>% 
     slice(1) %>% 
     ungroup 
   
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
  
  list_pages <- extract_tables(elig_dda_fp, output = "data.frame")
  
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

# COMMAND: MAKE_FILTERS_ELIGIBILITY_OZ ----
make_filters_eligibility_oz <- function(filters_census_tract){
  
  elig_oz_fp <- here("1-data/3-interim/KC-Opportunity-Zones-2018.xlsx")
  
  elig_oz_dr_id <- as_id("1-xCiQoCCO1esgmUUdlVt6Wlm-HLN4ONw0lUwrBYoXZA")
  
  elig_oz_load <- 
    make_or_read2(fp = elig_oz_fp,
                  dr_id = elig_oz_dr_id,
                  skip_get_expr = FALSE,
                  get_expr = function(fp){
                    # SOURCE:  http://www.commerce.wa.gov/growing-the-economy/opportunity-zones/
                  },
                  make_expr = function(fp, dr_id){
                     drive_read(dr_id = elig_oz_dr_id, .tempfile = FALSE, path = fp, read_fun = read_excel)
                  },
                  read_expr = function(fp){
                    read_excel(fp)
                  })
  
  elig_oz <- elig_oz_load %>% 
    transmute(CENSUS_TRACT = as.character(GEOID),
              FILTER_ELIGIBILITY_OZ = TRUE)
    
  
  p_ready_eligibility_oz <- filters_census_tract %>%   
    left_join(elig_oz, by = "CENSUS_TRACT") %>% 
    transmute(PIN,
              FILTER_ELIGIBILITY_OZ = if_else(FILTER_ELIGIBILITY_OZ, TRUE, FALSE, missing = FALSE))
  
  filters_eligibility_oz <- p_ready_eligibility_oz
  
  return(filters_eligibility_oz)
  
}


# COMMAND: MAKE_FILTERS_PARKING -------------------------------------------

make_filters_parking <- function(parcel_df_ready){
  
  # CHECK OUT HOW MANY PARCELS THIS WILL INCLUDE
  
  # parcel_df_ready %>% 
  #   filter(str_detect(toupper(PRESENT_USE),"PARKING" )) %>% 
  #   count(PRESENT_USE, sort = TRUE)
  
  # Total: 1,836
  
  filters_parking <- parcel_df_ready %>% 
    transmute(PIN,
              FILTER_PARKING = PRESENT_USE %in% c("Parking Assoc","Parking Commercial Lot"))
  
  return(filters_parking)
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
                           "ADDR_CITY_NAME",  "King County Department of Assessments",
                    "ADDR_CITY_NAME_POSTAL",  "King County Department of Assessments",
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


# COMMAND: WRITE_INVENTORY_XML ----

write_inventory_xml <- function(inventory_suitable, dd, path){
  
  table_fields <- dd %>% 
    filter(FIELD_TAG_TABLE) %>% 
    pull(FIELD_NAME_DEV)
  
  inventory_table <- inventory_suitable %>% 
    st_drop_geometry() %>% 
    select_at(vars(table_fields))  
  
  rio::export(inventory_table, file = path)

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
