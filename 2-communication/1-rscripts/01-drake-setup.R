# SETUP ----
library(rio)
library(janitor)
library(placement)
library(osmdata)
library(units)
library(ggraph)
library(igraph)
library(tidytext)
library(janitor)
library(tigris)
library(tabulizer)
library(foreign)
library(drake) 
library(tigris)
library(sf)
library(lwgeom)
library(googlesheets)
library(googledrive)
library(miscgis)  
library(snakecase)
library(magrittr)
library(lubridate)
library(RSocrata)
library(glue)
library(fuzzyjoin)
library(datapasta)
library(readxl)
library(writexl)
library(here) 

library(tidyverse)  
options(httr_oob_default=TRUE,
        tigris_class = "sf",
        tigris_use_cache = TRUE) 
pkgconfig::set_config("drake::strings_in_dots" = "literals")
htmltools::tagList(rmarkdown::html_dependency_font_awesome())
set.seed(98104)
