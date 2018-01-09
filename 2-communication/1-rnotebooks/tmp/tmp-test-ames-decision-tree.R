# Setup ---- 
library(tidyverse) 
library(sf)
library(snakecase)
library(mapview) 
library(AmesHousing)
library(skimr)
library(rpart) 
library(rattle)  

Sys.setlocale("LC_CTYPE", "Chinese")     # use skim() on Windows (See README.md on pkg Github repo)

# Create Data ----

houses <- make_ames() %>% 
  rename_all(to_screaming_snake_case) %>% 
  mutate_if(is.factor,as.character) %>% 
  select(FIRST_FLR_SF,POOL_QC, POOL_AREA, LONGITUDE,LATITUDE)

skim(houses)

houses_sm <- tibble::tribble(
  ~FIRST_FLR_SF,     ~POOL_QC, ~POOL_AREA,  ~LONGITUDE,  ~LATITUDE,
          1392L,       "Fair",       519L,  -93.637387,  42.050514,
          1309L,       "Fair",       648L,  -93.630289,   42.04977,
          1118L,       "Good",       576L,  -93.604549,  41.997069,
          2411L,  "Excellent",       555L, -93.6575919, 42.0533209,
          2151L,       "Good",       800L,  -93.660643,  42.037065,
          1360L,  "Excellent",       512L,  -93.620355,  42.042156,
          1575L,       "Good",       738L,  -93.663475,  42.026614,
          1656L,    "No_Pool",         0L,  -93.619754,  42.054035,
          1329L,    "No_Pool",         0L, -93.6193873,  42.052659,
          2110L,    "No_Pool",         0L,   -93.61732,  42.051245,
          1338L,    "No_Pool",         0L,  -93.633792,  42.062978,
          1280L,    "No_Pool",         0L,  -93.633826,  42.060728,
           896L,    "No_Pool",         0L,  -93.619756,  42.053014,
           928L,    "No_Pool",         0L,  -93.638933,  42.060899,
           926L,    "No_Pool",         0L,  -93.638925,  42.060779,
           763L,    "No_Pool",         0L,  -93.636947,   42.05848,
           789L,    "No_Pool",         0L,  -93.638647,  42.058151,
          2470L,  "Excellent",       144L,  -93.656958,  42.058484,
          4692L,       "Good",       480L,  -93.674898,  42.016804,
          1647L,    "Typical",       368L,  -93.618606,  42.034789,
          1105L,    "Typical",       444L,  -93.693153,  42.034453,
          2726L,  "Excellent",       228L,  -93.640182,  42.010076
  )

skim(houses_sm)

# First attempt ----

# Criteria: 
#   1) big house...
#   2) with a pool!

pool_party_1 <- houses_sm %>% 
  mutate(IS_A_MANSION = if_else(FIRST_FLR_SF >= 1000, TRUE, FALSE,missing = FALSE)) %>% 
  mutate(HAS_POOL = !if_else(POOL_QC %in% "No_Pool", TRUE, FALSE,missing = FALSE)) %>% 
  mutate(PARTY_HOUSE = if_else(IS_A_MANSION & HAS_POOL, TRUE, FALSE, missing = FALSE))

# Map it:
p_1 <- pool_party_1 %>% 
  mutate(geometry = st_sfc(map2(LONGITUDE, LATITUDE, ~ st_point(c(.x,.y))))) %>% 
  mutate(PARTY_HOUSE = factor(PARTY_HOUSE) %>% fct_rev) %>% 
  select(-LONGITUDE, -LATITUDE) %>% 
  st_sf %>% 
  st_set_crs(4326)

mapview(p_1, zcol = "PARTY_HOUSE", legend = TRUE) 

# Second attempt ----

# Criteria: 
#   1) big house...
#   2) with a _nice_ pool!

pool_party_2 <- pool_party_1 %>% 
  mutate(POOL_IS_NICE = if_else(POOL_QC %in% "Excellent", TRUE, FALSE,missing = FALSE)) %>% 
  mutate(PARTY_HOUSE = if_else(IS_A_MANSION & HAS_POOL & POOL_IS_NICE, TRUE, FALSE, missing = FALSE))

# Map it:
p_2 <- pool_party_2 %>% 
  mutate(geometry = st_sfc(map2(LONGITUDE, LATITUDE, ~ st_point(c(.x,.y))))) %>% 
  mutate(PARTY_HOUSE = factor(PARTY_HOUSE) %>% fct_rev) %>% 
  select(-LONGITUDE, -LATITUDE) %>% 
  st_sf %>% 
  st_set_crs(4326)

mapview(p_2, zcol = "PARTY_HOUSE", legend = TRUE) 


# Third attempt ----

# Criteria: 
#   1) big house...
#   2) with a pool that's big enough and...
#   3) not too nasty!

pool_party_3 <- houses_sm %>% 
  mutate(IS_A_MANSION = if_else(FIRST_FLR_SF >= 1000, TRUE, FALSE, missing = FALSE)) %>% 
  mutate(POOL_SIZE = case_when(
    POOL_AREA > 500 ~ "large",
    between(POOL_AREA,1,500) ~ "regular",
    TRUE ~ "none"
  )) %>% 
  mutate(PARTY_HOUSE = case_when(
    (!IS_A_MANSION) ~ "not a mansion",
    POOL_QC %in% "No_Pool" ~ "no pool",
    POOL_SIZE %in% 'large' & POOL_QC %in% c("Excellent", "Good") ~ "great",
    POOL_SIZE %in% "large" & POOL_QC %in% c("Typical") ~ "ok",
    POOL_SIZE %in% "regular" & POOL_QC %in% c("Excellent", "Good", "Typical") ~ "ok", 
    TRUE ~ "bad"
  ))

lvls <- c("not a mansion", "no pool", "bad", "ok", "great")

# Map it:
p_3 <- pool_party_3 %>% 
  mutate(geometry = st_sfc(map2(LONGITUDE, LATITUDE, ~ st_point(c(.x,.y))))) %>% 
  mutate(PARTY_HOUSE = factor(PARTY_HOUSE, levels = lvls) %>% fct_rev) %>% 
  select(-LONGITUDE, -LATITUDE) %>% 
  st_sf %>% 
  st_set_crs(4326)

mapview(p_3, zcol = "PARTY_HOUSE", legend = TRUE) 

# Create a decision tree ----

my_model <- rpart(PARTY_HOUSE ~ IS_A_MANSION + POOL_SIZE + POOL_QC, 
                  data = pool_party_3, 
                  cost = c(1, 100, 50),
                  cp = -1, 
                  minsplit = 2)

drawTreeNodes(my_model)

asRules(my_model)


