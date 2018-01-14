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
library(htmltools)

options(httr_oob_default=TRUE) 

# Access Data ----

pub_dr_id <-  "0B5Pp4V6eCkhrRmwzOEdPRklhMEk"

fp <- "./1-data/2-external/public_parcels_SHP.zip"

d <- drive_get(id = pub_dr_id)

d %>% drive_download(path = fp)

unzip('1-data/2-external/public_parcels_SHP.zip',exdir = '1-data/2-external/public_parcels_SHP')


# Clean Data ----

pu_dr_id <- "0B5Pp4V6eCkhrSThkY055LUdxakE"
pu_fp <- "./1-data/2-external/EXTR_LookUp.csv"

d <-  drive_get(id = pu_dr_id) 

drive_download(d, path = pu_fp)

pu <- read_csv(pu_fp)

pu_ready <- 
  pu %>%
  rename_all(list(toupper)) %>% 
  filter(LUTYPE %in% 102) %>% 
  mutate(NM_LIST = map2(LUDESCRIPTION, LUITEM, purrr::set_names)) %>% 
  pluck(4) %>% 
  flatten

  pub <- 
  st_read('1-data/2-external/public_parcels_SHP/public_parcels/public_parcels.shp') %>% 
  st_transform(4326)

pub_ready <- 
  pub %>% 
  select(PIN,
         TYPE = TYPE_,
         PRESENTUSE,
         LOTSQFT,
         APPRLNDVAL,
         APPR_IMPR,
         ADDR_FULL,
         ZIP5,
         POSTALCTYNAME = POSTALCTYN,
         LAT,
         LON,
         JURIS) %>%  
  mutate(POPUP = paste0('http://blue.kingcounty.com/Assessor/eRealProperty/default.aspx?ParcelNbr=',PIN),
         PRESENTUSE = as.character(PRESENTUSE)) %>% 
  filter(PRESENTUSE %in% names(pu_ready)) %>% 
  mutate(PRESENTUSE = map_chr(PRESENTUSE, ~ pu_ready[[.x]]),
         PU_TOP10 = fct_lump(as.factor(PRESENTUSE),n = 10)) %>% 
  select(PIN, matches('PRES|PU'), everything())


pub_sf <- 
  pub_ready %>% 
  filter(ntile(LOTSQFT,5) > 3,
         ntile(APPR_IMPR, 5) > 3) %>% 
  arrange(desc(APPR_IMPR)) 


pal <- colorFactor("Set3",domain = pub_ready$PU_TOP10, ordered = TRUE)


myLflt() %>% 
  addPolygons(data = pub_ready,
              popup = ~ POPUP,
              fillColor = ~pal(PU_TOP10))

%>% 
  addLegend(pal = pal, values = levels(pub_ready[1:10,'PU_TOP10']))



# Upload the data to Drive ----

folder <- drive_get("~/Futurewise/Enterprise/1-data/2-external")

drive_upload_obj(x = d,
                 filename = "kc-schools.rds",
                 write_fun = write_rds, 
                 folder = folder)