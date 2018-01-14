# Setup ----

library(tidyverse)
library(magrittr)
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

# Raw Data ----

string <- 
"NULL	NULL
AL	Algona
AU	Auburn
BD	Black Diamond
BE	Bellevue
BO	Bothell
BU	Burien
CA	Carnation
CH	Clyde Hill
CO	Covington
DM	Des Moines
DU	Duvall
EN	Enumclaw
FW	Federal Way
HP	Hunts Point
IS	Issaquah
KE	Kent
KI	Kirkland
KM	Kenmore
LF	Lake Forest Park
ME	Medina
MI	Mercer Island
MT	Milton
MV	Maple Valley
NB	North Bend
NC	Newcastle
NP	Normandy Park
PA	Pacific
RM	Redmond
RN	Renton
SE	Seattle
SH	Shoreline
SK	Skykomish
SM	Sammamish
SN	Snoqualmie
ST	SeaTac
TU	Tukwila
WO	Woodinville
YP	Yarrow Point"

# Make Oobject ----

juris <- 
  str_split(string, pattern = "\n") %>% 
  map(~ t(as.data.frame(str_split(.x,pattern = "\t")))) %>% 
  as.data.frame() %>% 
  set_names(c("CODE","NAME")) %>% 
  set_rownames(NULL) %>% 
  mutate_all(as.character) %>%  
  pmap(~ set_names(..2, ..1))


# Upload the data to Drive ----

folder <- as_id("0B5Pp4V6eCkhrZ3NHOEE0Sl9FbWc")

tmp <- tempfile()

write_rds(juris,tmp)

drive_upload(tmp, folder,name = "kc-jurisdictions.rds")
