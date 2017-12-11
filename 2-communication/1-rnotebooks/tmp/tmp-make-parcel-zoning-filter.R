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

root <- rprojroot::is_rstudio_project
root_file <- root$make_fix_file()
options(httr_oob_default=TRUE) 
htmltools::tagList(rmarkdown::html_dependency_font_awesome())

# Access Data ----

# URBAN GROWTH AREA

uga_fp <- root_file("./1-data/2-external/uga")

uga_sf <- 
  make_or_read(uga_fp,
               {
                 
                 dr_id <- as_id("0B5Pp4V6eCkhrZGQ0Q0h5aHNUVW8")
                 
                 zip_dir <- root_file("./1-data/2-external")
                 
                 target_name <- "uga"
                 
                 drive_read_zip(dr_id = dr_id,
                                .tempdir = FALSE,
                                dir_path = zip_dir,
                                read_fun = st_read,
                                target_name = target_name,
                                layer = "uga", 
                                stringsAsFactors = FALSE)
                 
               },
               {
                 st_read(uga_fp, layer = "uga", stringsAsFactors = FALSE)
               }) %>% 
  st_transform(4326)


# ZONING

zng_fp <- root_file("./1-data/3-interim/zoning_sf.gpkg")

zng_sf <- 
  make_or_read(zng_fp,
               {
                 dr_id <- as_id("0B5Pp4V6eCkhrZHZuTlhPY3ZYOWs")
                 
                 drive_read(dr_id = dr_id,
                            .tempfile = FALSE,
                            path = zng_fp,
                            read_fun = st_read, 
                            stringsAsFactors = FALSE)
                 
                 
                 
               },
               {
                 st_read(zng_fp, stringsAsFactors = FALSE)
               }) %>% 
  st_transform(2926) %>% 
  lwgeom::st_make_valid() %>% 
  st_simplify(dTolerance = 5) %>%  
  st_transform(4326)

# PARCELS (public or tax-exempt)

p_fp <- "./1-data/3-interim/pub_or_tax_exempt_sf_20171013.gpkg"

p_sf <- 
  make_or_read(p_fp,
               {
                 dr_id <- as_id("0B5Pp4V6eCkhrZnRtUjlWaFVrb0E")
                 
                 drive_read(dr_id = dr_id,
                            .tempfile = FALSE,
                            path = p_fp,
                            read_fun = st_read, 
                            stringsAsFactors = FALSE)
                 
               },
               {
                 st_read(p_fp, stringsAsFactors = FALSE)
               }) %>% 
  st_transform(4326)




# Filters ----


st_intersects_any <- function(x,y){
  st_intersects(x,y) %>% 
    map_lgl(~ length(.x)>0)
}

# st_intersects_which <- function(x,y, y_col_name){
#   st_intersects(x,y) %>% 
#     map(~ {extract2(y,y_col_name)[.x]})
# }

# st_intersects_most <- function(x, y){ 
#   ints <- st_intersects(x,y)
#   
#   tibble(
#     ROW_ID = imap(ints, ~rep(.y, times = length(.x))) %>% flatten_chr(),
#     INT_DF = flatten_int(ints) %>% map(~y[.x,] %>% st_drop_geometry),
#     INT_AREA = st_intersection(x,y) %>% st_area %>% map_dbl(1)
#   ) %>%  
#     unnest %>%  
#     mutate(geometry = st_geometry(x[ROW_ID,])) %>% 
#     st_as_sf
# }


# UGA FILTER

p_uga_sf <- 
  tibble(UGA_LGL = st_intersects_any(st_transform(p_sf, 2926), st_transform(uga_sf, 2926))) %>% 
  bind_cols(p_sf) %>% 
  filter(UGA_LGL) %>% 
  st_as_sf %>% 
  st_transform(2926) %>% 
  lwgeom::st_make_valid() %>% 
  st_simplify(dTolerance = 1, preserveTopology = TRUE) %>% 
  st_transform(4326)



# ZONING FILTER

set.seed(999)

p_100 <- 
  p_uga_sf %>% 
  sample_n(100) %>% 
  st_transform(2926)


zng_2926 <- st_transform(zng_sf, 2926)

p_10 <- slice(p_100,1:10)

p_12_13 <- slice(p_100, 12:13)

p_11_20 <- slice(p_100,11:20)


benchmark({st_join(p_12_13,zng_2926, largest = TRUE) }, replications = 5)

tictoc::tic()
test <- st_join(p_100, zng_2926, largest = TRUE)
tictoc::toc()

# WARNING -- THIS OPERATION TOOK OVER 10 HOURS TO COMPLETE!
tic()
# p_uga_zng_sf <- st_join(st_transform(p_uga_sf,2926), zng_2926, largest = TRUE)
# write_rds(p_uga_zng_sf, "./1-data/3-interim/pub-uga-zng-sf.rds")
toc()


st_nest_sf <- function(x){
  
  x %>% 
    rownames_to_column('ROW') %>% 
    nest(-ROW) %>% 
    select(-ROW) %>% 
    mutate(geometry = map(data, "geometry") %>% flatten %>% st_sfc,
           data = map(data, st_drop_geometry)) %>% 
    st_sf %>% 
    st_set_crs(st_crs(x))
}


p_uga_zng_no_other_sf <- 
  p_uga_zng_sf %>%   
  st_nest_sf %>% 
  mutate(CAT_FCT_OTHER = map_chr(data, 'CAT_FCT_OTHER')) %>% 
  filter(CAT_FCT_OTHER %!in% "Other") %>% 
  select(CAT_FCT_OTHER, everything()) %>% 
  unnest() %>% 
  mutate_if(is_logical,as.character)

# Upload to Drive ----

p_uga_zng_no_other_fp <- root_file("./1-data/3-interim/pub-uga-zng-no-other-sf.gpkg")

folder_id <- as_id("0B5Pp4V6eCkhrZ3NHOEE0Sl9FbWc")

st_write(obj = p_uga_zng_no_other_sf, dsn = p_uga_zng_no_other_fp, layer = 'p_uga_zng_no_other_sf', driver = 'GPKG', layer_options = "OVERWRITE=YES")

drive_upload(media = p_uga_zng_no_other_fp, path = folder_id)

drive_update(media = p_uga_zng_no_other_fp,file = as_id("12IEjuIlWxXjWhJCwAo-nxDBI8l6ZJlIa"))
