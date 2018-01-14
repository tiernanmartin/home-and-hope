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

pub_plan <- drake_plan(
  acct = make_acct()
)

master_plan <- rbind(pub_plan)    # rbind all plans together


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


# RUN PLAN ----
make(master_plan)
