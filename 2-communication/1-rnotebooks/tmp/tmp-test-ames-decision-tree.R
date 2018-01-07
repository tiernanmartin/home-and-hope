# Setup ----
library(skimr)
library(scales)
library(tidyverse) 
library(sf)
library(mapview)
library(snakecase)
library(AmesHousing)
library(rpart)
library(rattle) 
library(janitor)
library(devtools)

Sys.setlocale("LC_CTYPE", "Chinese")     # use skim() on Windows (See README.md on pkg Github repo)

# Access Data ----

a <- make_ames() %>% 
  rename_all(to_screaming_snake_case) %>% 
  mutate(geometry = st_sfc(map2(LONGITUDE, LATITUDE, ~ st_point(c(.x,.y))))) %>% 
  st_sf %>% 
  st_set_crs(4326)

# View Data ----

skim(a)

mapview(a)

make_ames() %>% select_if(is.factor) %>% map(table)

# Create a decision tree ----

# VAR ONE: single-family ONLY

# Check out the crosstabulation of zoning and sub class

a %>% 
  split(.$MS_ZONING) %>% 
  map(~ tabyl(.x, MS_SUB_CLASS, sort = TRUE))

# get the most common sub classes from the single family zoning categories

sf_zones <- c("Residential_Low_Density", "Residential_Medium_Density")

sf_classes <- a %>% 
  select(MS_ZONING,MS_SUB_CLASS) %>% 
  filter(MS_ZONING %in% sf_zones) %>% 
  group_by(MS_SUB_CLASS) %>% 
  summarize(N = n()) %>% 
  arrange(desc(N)) %>% 
  slice(1:6) %>% 
  transmute(MS_SUB_CLASS = as.character(MS_SUB_CLASS)) %>% 
  pull(MS_SUB_CLASS)


a_var_one <- a %>% 
  mutate(MS_ZONING_CHR = as.character(MS_ZONING),
         MS_SUB_CLASS_CHR = as.character(MS_SUB_CLASS)) %>% 
  mutate(SF_LGL = if_else(MS_ZONING_CHR %in% sf_zones & MS_SUB_CLASS_CHR %in% sf_classes,TRUE,FALSE))

glimpse(a_var_one)

# VAR TWO: top-5 most expensive neighborhoods (sale price per lot square foot)

a_var_two <- a_var_one %>% 
  mutate(PRICE_SQFT = dollar(SALE_PRICE/LOT_AREA),
         PRICE_SQFT_LOG = log10(SALE_PRICE)/log10(LOT_AREA)) %>% 
  mutate(PRICE_SQFT = if_else(SF_LGL,PRICE_SQFT,NA_character_),
         PRICE_SQFT_LOG = if_else(SF_LGL, PRICE_SQFT_LOG, NA_real_)) %>% 
  mutate(NHOOD_TOP5 = fct_reorder(NEIGHBORHOOD,PRICE_SQFT_LOG, .desc = TRUE,fun = mean, na.rm = TRUE),
         NHOOD_TOP5 = fct_other(NHOOD_TOP5, keep = levels(NHOOD_TOP5)[1:5])) 

a_var_two %>% 
  select(NEIGHBORHOOD,
         NHOOD_TOP5,
         SALE_PRICE,
         LOT_AREA,
         PRICE_SQFT,
         PRICE_SQFT_LOG) %>% 
  arrange(NHOOD_TOP5, desc(PRICE_SQFT)) %>% 
  print(n = 200)

# VAR THREE: have a pool

a_var_two %>% 
  filter(POOL_AREA > 0 ) %>% 
  select(NEIGHBORHOOD,
         NHOOD_TOP5,
         SALE_PRICE,
         LOT_AREA,
         PRICE_SQFT,
         PRICE_SQFT_LOG,
         POOL_AREA) %>%  
  print(n = 200)

# Oh no! All of the top 5 neighborhoods get filtered out when we select for pools - shoot!
