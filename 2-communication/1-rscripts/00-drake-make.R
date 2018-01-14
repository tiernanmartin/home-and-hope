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
# CREATE MAKE PLANS ----

parcels_plan <- make_plan(
  acct = make_acct(),
  parcels_pt = make_parcels_pt(acct),
  parcels_poly = make_parcels_poly(parcels_pt)
)

suitability_plan <- drake_plan(
  tax_e = make_tax_e(),
  water_coverage = make_water(),
  uga = make_uga(),
  zoning = make_zoning(),
  present_use = make_present_use()
)

utilization_plan <- drake_plan(
  util_present = make_util_present(),
  util_potential = make_util_potential()
)

project_plan <- rbind(parcels_plan,
                     suitability_plan,
                     utilization_plan)    # rbind all plans together


# COMMAND: MAKE_ACCT ----

make_acct <- function(){
  
  acct_fp <- root_file("./1-data/2-external/kc_real_prop_acct_extract.rds")
  
  acct <- acct_fp %>% 
    make_or_read({
      dr_id <- as_id("19f4AUMAEshnDNJqGjVsurFthbKGcKYvh")
      
      drive_read(dr_id = dr_id,path = acct_fp,.tempfile = FALSE,read_fun = read_rds)
      
    },
                 {read_rds(acct_fp)})
}


# COMMAND: MAKE_PARCELS_PT ----

# COMMAND: MAKE_PARCELS_POLY ----

# COMMAND: MAKE_TAX_E ----

# COMMAND: MAKE_WATER ----

# COMMAND: MAKE_UGA ----

# COMMAND: MAKE_ZONING ----

# COMMAND: MAKE_PRESENT_USE ----

# COMMAND: MAKE_UTIL_PRESENT ----

# COMMAND: MAKE_UTIL_POTENTIAL ---- 

# RUN PLAN ----
make(project_plan)
