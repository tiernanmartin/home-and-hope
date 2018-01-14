# Setup ----

library(igraph)
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
library(RColorBrewer) 
library(RSocrata)
library(tidytext)
library(widyr)
library(ggrepel)
library(ggraph)


root <- rprojroot::is_rstudio_project
root_file <- root$make_fix_file()
options(httr_oob_default=TRUE) 
htmltools::tagList(rmarkdown::html_dependency_font_awesome())

# Load data ----

# Public parcels from KC GIS

#NOTE: change this to the Google Drive process

pub_fp <- root_file("1-data/2-external/public_parcels/")

pub <- read_sf(dsn = pub_fp, stringsAsFactors = FALSE)


# KC Real prop extract

r_fp <- root_file("./1-data/2-external/kc_real_prop_acct_extract.rds")

r <- r_fp %>% 
  make_or_read({NULL}, #fill this in later
               {read_rds(r_fp)})


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
  r %>% 
  filter(str_detect(TAXPAYERNAME,"AUTHORITY")) %>% 
  group_by(TAXPAYERNAME) %>% 
  tally %>% 
  arrange(desc(n))
