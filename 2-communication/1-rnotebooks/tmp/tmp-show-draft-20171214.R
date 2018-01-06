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
# Sys.setlocale("LC_CTYPE", "Chinese")  # if working on a Windows machine, this is required for skimr::skim()

# Access Data ----

kc_url <- "https://opendata.arcgis.com/datasets/19b7f1e85a0f4c9ebfcc2830bd1d783e_121.geojson"

kc <- read_sf(kc_url)



p_id <- as_id("15B9sduv0IBIA2giXnGR8sVigus73qvcc")

p <- drive_read(p_id, TRUE, read_fun = read_sf, stringsAsFactors = FALSE) 

p %<>% 
  st_set_crs(4326) %>% 
  mutate_at(vars(ends_with("LGL")),as.logical)

url <- "http://blue.kingcounty.com/Assessor/eRealProperty/Dashboard.aspx?ParcelNbr="

p_sf <- 
  p %>%  
  mutate(PIN = map_chr(PIN, ~ a(href = str_c(url,.x),target="_blank",.x) %>% as.character),
         UTILIZATION = factor(case_when(
           UNDER_UTILIZED_LGL ~ "Under-utilized",
           !UNDER_UTILIZED_LGL ~ "Fully-utilized",
           TRUE ~ "Not developable"),
           levels = c("Not developable",
                      "Fully-utilized",
                      "Under-utilized"))
  ) %>% 
  select(PROP_NAME:PIN,UTILIZATION,everything(),UNDER_UTILIZED_LGL)

# View Data ----

# All parcels included in this subset

mapview(list(kc, p_sf), 
        layer.name = c("COUNTY","PARCELS"), 
        alpha.regions = c(0,.5), 
        zcol = c("COUNTY", "UTILIZATION"),
        legend = TRUE)

# No County boundary
mapview(p_sf, 
        layer.name = "PARCELS", 
        alpha.regions = .5, 
        zcol = "UTILIZATION",
        legend = TRUE)
