# Setup ----

library(tidyverse) 
library(scales)
library(sf)
library(mapview)
library(snakecase)
library(AmesHousing)
library(rpart)
library(rattle)
library(skimr)
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

# work in progress

# VAR TWO: top-5 most expensive neighborhoods (sale price per lot square foot)

test1 <- a %>% 
  mutate(PRICE_SQFT = dollar(SALE_PRICE/LOT_AREA),
         PRICE_SQFT_LOG = log10(SALE_PRICE)/log10(LOT_AREA)) %>% 
  mutate(NHOOD_TOP5 = fct_reorder(NEIGHBORHOOD,PRICE_SQFT_LOG, .desc = TRUE,fun = mean),
         NHOOD_TOP5 = fct_other(NHOOD_TOP5, keep = levels(NHOOD_TOP5)[1:5])) 

test1 %>% 
  select(NEIGHBORHOOD,
         NHOOD_TOP5,
         SALE_PRICE,
         LOT_AREA,
         PRICE_SQFT,
         PRICE_SQFT_LOG) %>% 
  arrange(NHOOD_TOP5, desc(PRICE_SQFT)) %>% 
  print(n = 200)

a %>% 
  mutate(PRICE_SQFT = SALE_PRICE/LOT_AREA) %>% 
  group_by(NEIGHBORHOOD) %>% 
  summarise(MEDIAN_SQFT_VAL = median(PRICE_SQFT)) %>% 
  arrange(desc(MEDIAN_SQFT_VAL)) %>% print(n = Inf)
