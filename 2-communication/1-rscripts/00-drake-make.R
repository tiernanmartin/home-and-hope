# SETUP ----
library(drake)
library(sf)
library(googledrive)
library(miscgis)  
library(snakecase)
library(magrittr)
library(rprojroot) 
library(RSocrata)
library(tidyverse) 


root <- rprojroot::is_rstudio_project
root_file <- root$make_fix_file()
options(httr_oob_default=TRUE) 
htmltools::tagList(rmarkdown::html_dependency_font_awesome())
# MAKE PLANS ----

lookup_plan <- make_plan( 
  lu = make_lu(),
  tax_status = make_tax_status(),
  tax_reason = make_tax_reason()
  
)
  
parcels_plan <- make_plan(
  acct = make_acct(),
  parcel_sf = make_parcel_df(acct, tax_status,tax_reason)
)

suitability_plan <- drake_plan(
  tax_e = make_tax_e(),
  water_coverage = make_water(),
  uga = make_uga(),
  zoning = make_zoning(),
  present_use = make_present_use(),
  parcel_suitability = make_suitability(parcel_sf, tax_e, water_coverage, uga, zoning, present_use)
)

utilization_plan <- drake_plan(
  util_present = make_util_present(),
  util_potential = make_util_potential(),
  parcel_utilization = make_parcel_utilization(parcel_sf, util_present, util_potential)
)

project_plan <- rbind(parcels_plan,
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

# COMMAND: MAKE_LU ----

# COMMAND: MAKE_TAX_STATUS ----

# COMMAND: MAKE_TAX_REASON ----

make_tax_reason <- function(){
  tax_r_fp <- root_file("1-data/1-raw/tax_reason.txt")
  
  tax_r_dr_id <- as_id("118oisrou6ieTP2WXpg8IE1dtmsybRhkT")
  
  tax_reason_load <- tax_r_fp %>% 
    make_or_read2(dr_id = tax_r_dr_id,
                  get_expr = {
                    
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
                    writeLines(string, tax_r_fp)
                    
                    drive_folder <- as_id("0B5Pp4V6eCkhrb1lDdlNaOFY4V0U")
                    
                    # drive_upload(media = tax_r_fp, path = drive_folder)
                    
                    drive_update(tax_r_dr_id, tax_r_fp)
                  },
                  make_expr = {
                    drive_read(dr_id = tax_r_dr_id,path = tax_r_fp,.tempfile = FALSE,read_fun = readLines,con = tax_r_fp)
                  },
                  read_expr = {readLines(tax_r_fp)})
  
  tax_reason <- 
    tax_reason_load %>% 
    str_c(collapse = "\n") %>% 
    parse_lu_string(col_sep = "\\s=\\s", row_sep = "\n",join_name = "TAX_REASON","TAX_REASON_DESC")
}


# COMMAND: MAKE_ACCT ----

make_acct <- function(){ 
  
  acct_fp <- root_file("./1-data/2-external/kc_real_prop_acct_extract.rds")
  
  acct_dr_id <- as_id("19f4AUMAEshnDNJqGjVsurFthbKGcKYvh")
  
  acct <- acct_fp %>% 
    make_or_read2(dr_id = acct_dr_id,
                  get_expr = {
                    realprop <- read.socrata("https://data.kingcounty.gov/resource/mmfz-e8xr.csv",
                                             email = "FAKE_NAME@FAKE_EMAIL.COM",
                                             password = "FAKE_PASSWORD" # CHANGE TO REAL BEFORE RUNNING
                    )
                    
                    r <- realprop %>% 
                      rename_all(to_screaming_snake_case) 
                    
                    r_fp <- root_file("./1-data/2-external/kc_real_prop_acct_extract.rds")
                    
                    drive_folder_id <- as_id("0B5Pp4V6eCkhrdlJ3MXVaNW16T0U")
                    
                    write_rds(r,r_fp, compress = "gz")
                    
                    # drive_upload(media = r_fp, path = drive_folder_id)
                    
                    drive_update(as_id("19f4AUMAEshnDNJqGjVsurFthbKGcKYvh"), r_fp)
                  },
                  make_expr = {
                    drive_read(dr_id = acct_dr_id,path = acct_fp,.tempfile = FALSE,read_fun = read_rds)
                  },
                  read_expr = {read_rds(acct_fp)})
  
}

# COMMAND: MAKE_PARCELS_SF ----
 
# COMMAND: MAKE_TAX_E ----

# COMMAND: MAKE_WATER ----

# COMMAND: MAKE_UGA ----

# COMMAND: MAKE_ZONING ----

# COMMAND: MAKE_PRESENT_USE ----

# COMMAND: MAKE_PARCEL_SUITABILITY ----

# COMMAND: MAKE_UTIL_PRESENT ----

# COMMAND: MAKE_UTIL_POTENTIAL ---- 

# COMMAND: MAKE_PARCEL_UTILIZATION----
# RUN PROJECT PLAN ----
make(project_plan)
