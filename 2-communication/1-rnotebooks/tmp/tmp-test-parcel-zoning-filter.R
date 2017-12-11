# Setup ----

library(tidyverse)
library(stringr)
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

p <- drive_read(as_id("1wiBedRBk8Ygx7jZGctdH0kdLe-4jlQr4"),TRUE,read_fun = st_read, stringsAsFactors = FALSE) 

p %<>% 
  st_set_crs(2926) %>% 
  st_transform(4326)

p_nest <- st_nest_sf(p)

url <- "http://blue.kingcounty.com/Assessor/eRealProperty/Dashboard.aspx?ParcelNbr="

p_sf <- 
  p_nest %>% 
  mutate(PROP_NAME = map_chr(data, "PROP_NAME"),
         PIN = map_chr(data, "PIN"),
         PIN = map_chr(PIN, ~ a(href = str_c(url,.x),target="_blank",.x) %>% as.character),
         CURRENT_ZONING = map_chr(data, "CURRENT_ZONING"),
         CAT = map_chr(data, "CAT"),
         POPUP = str_c(PROP_NAME,PIN,CURRENT_ZONING,CAT,sep = br())
         ) %>% 
  select(-data,-POPUP)


mapview(p_sf)
