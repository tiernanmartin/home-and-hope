# Setup ----
 
library(sf)
library(googledrive)
library(miscgis) 
library(snakecase)
library(magrittr)
library(rprojroot)
library(RSocrata) 
library(miscgis)

library(tidyverse)

root <- rprojroot::is_rstudio_project
root_file <- root$make_fix_file()
options(httr_oob_default=TRUE)  

# Load data ----

# This data is from a special dataset created by the King County 
# Assessor office (or maybe the GIS Dept?) to facilitate
# the analysis of public land.
# See "./2-communication/1-rscripts/tmp/get-parcel-public-taxpayer.R"

pub_fp <- root_file("1-data/2-external/kc-public-parcels.csv")

pub_dr_id <- as_id("1ERHvVa9K6F-lk1L8X47Oy1sdw_s2t9kv")

pub <- pub_fp %>% 
  make_or_read2(pub_dr_id,
                make_expr = {drive_read(dr_id, .tempfile = FALSE,path = pub_fp,read_fun = read_csv)},
                read_expr = {read_csv(pub_fp)})


# KC Real prop extract

acct_fp <- root_file("./1-data/2-external/kc_real_prop_acct_extract.rds")
  
acct_dr_id <- as_id("19f4AUMAEshnDNJqGjVsurFthbKGcKYvh")

acct <- acct_fp %>% 
  make_or_read2(dr_id = acct_dr_id,
                get_expr = {
                  realprop <- read.socrata("https://data.kingcounty.gov/resource/mmfz-e8xr.csv",
                                           email = "FAKE_NAME@FAKE_EMAIL.COM",
                                           password = "FAKE_PASSWORD" # CHANGE TO REAL BEFORE RUNNING
                  )
                  
                  r <- realprop %>% 
                    rename_all(to_screaming_snake_case) 
                  
                  r_fp <- root_file("./1-data/2-external/kc_real_prop_acct_extract.rds")
                  
                  drive_folder_id <- as_id("0B5Pp4V6eCkhrdlJ3MXVaNW16T0U")
                  
                  write_rds(r,r_fp, compress = "gz")
                  
                  drive_upload(media = r_fp, path = drive_folder_id)
                  
                  # drive_update(as_id("19f4AUMAEshnDNJqGjVsurFthbKGcKYvh"), r_fp)
                },
                make_expr = {
                  drive_read(dr_id = acct_dr_id,path = acct_fp,.tempfile = FALSE,read_fun = read_rds)
                },
                read_expr = {read_rds(acct_fp)})
  


# Check for patterns ----

names_count <- 
  pub %>% 
  st_drop_geometry() %>%  
  group_by(KCTP_NAME) %>% 
  tally %>% 
  arrange(desc(n))

data(stop_words)

names <- pub %>% 
  st_drop_geometry() %>% 
  select(PIN,KCTP_NAME) %>% 
  unnest_tokens(word, KCTP_NAME)

key_words <- names %>% 
  anti_join(stop_words) %>% 
  pull %>% 
  unique()

pin_name <- pub %>% st_drop_geometry() %>% select(PIN,KCTP_NAME)

names_ngram2 <- pub %>% 
  st_drop_geometry() %>% 
  select(PIN, KCTP_NAME) %>% 
  unnest_tokens(phrase, KCTP_NAME,token = "ngrams", n = 2,to_lower = FALSE) %>%
  mutate(phrase_sep = phrase) %>% 
  separate(phrase_sep, c("word1", "word2"), sep = " ") %>% 
  left_join(pin_name) %>% 
  select(PIN,KCTP_NAME,everything())

names_ngram3 <- pub %>% 
  st_drop_geometry() %>% 
  select(PIN, KCTP_NAME) %>% 
  unnest_tokens(phrase, KCTP_NAME,token = "ngrams", n = 3, to_lower = FALSE) %>%
  mutate(phrase_sep = phrase) %>% 
  separate(phrase_sep, c("word1", "word2", "word3"), sep = " ")

names_ngram2_counts <- names_ngram2 %>% 
  count(word1, word2, sort = TRUE)

names_ngram2_counts_cityof <- names_ngram2 %>% 
  count(word1, word2, sort = TRUE)

names_ngram3_counts <- names_ngram3 %>% 
  count(word1, word2, word3, sort = TRUE)

bigram_graph <- names_ngram2_counts %>%
  filter(n > 20) %>% 
  graph_from_data_frame()

bigram_graph_cityof <- names_ngram2_counts %>%
  filter(n > 20) %>%
  slice(-1) %>% 
  graph_from_data_frame()

set.seed(2016)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph_cityof, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

# Patterns ----

patterns <- c("CITY OF",
              "SCHOOL DIST",
              "DEPT OF",
              "OF DPR",
              "NATURAL RESOURCES",
              "SPU",
              "SDOT",
              "WSDOT",
              "STATE OF WASHINGTON",
              "PORT OF",
              "WLRD RFMS",
              "OF FAS",
              "OF SCL",
              "OF UTILITIES",
              "KING COUNTY-WLRD",
              "COUNTY PARKS",
              "COUNTY ROADS",
              "COUNTY WASTE",
              "COUNTY TRANSIT",
              "COUNTY SVCS",
              "COUNTY PROPERTY",
              "UNIVERSITY OF",
              "STATE DOT",
              "WA STATE",
              "SOLID WASTE",
              "NATIONAL FOREST",
              "HOUSING AUTH")

patterns_upper <- toupper(patterns)

patterns_regex <- patterns_upper %>% 
  str_replace_all("\\s","\\\\s") %>% 
  str_c(collapse = "|")
  

housing_authorities <- 
  acct %>% 
  filter(str_detect(TAXPAYERNAME,"AUTHORITY")) %>% 
  group_by(TAXPAYERNAME) %>% 
  tally %>% 
  arrange(desc(n))
