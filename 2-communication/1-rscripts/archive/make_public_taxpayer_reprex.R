# Setup ----
 
library(ggraph)
library(igraph)
library(sf)
library(googlesheets)
library(googledrive)
library(miscgis) 
library(snakecase)
library(magrittr)
library(rprojroot)
library(RSocrata) 
library(tidytext)
library(drake)

library(tidyverse)


# LOAD OBJECTS ----

loadd(pub_parcel)
loadd(acct)
data(stop_words)

numbers_0_500 <- tibble(word = as.character(0:500))

ok_words <- c("STATES")

stop_words_upper <- transmute(stop_words, word = toupper(word)) %>% 
  bind_rows(numbers_0_500) %>% 
  filter(word %!in% ok_words)

names <- pub_parcel %>%  
  transmute(PIN,
           TAXPAYER_NAME = toupper(TAXPAYER_NAME))
 
pub_categories_gs <- gs_key("1cYNIpQpDJTZWi46S_9bZ6rjgRu8JWes1BxOeoJJD2tg")

cat_ngram_1 <- gs_read(ss = pub_categories_gs,ws = "NGRAM_1")

cat_ngram_2 <- gs_read(ss = pub_categories_gs,ws = "NGRAM_2")

cat_ngram_3 <- gs_read(ss = pub_categories_gs,ws = "NGRAM_3")

# CLEAN/RECODE NAMES ----

tp_name_recode <- 
                      tribble(
                        ~ ORIG, ~ NEW,
                        "-"," ",
                        "(|)","",
                        "CTR", "CENTER",
                        "DIST", "DISTRICT",
                        "DIS", "DISTRICT",
                        "CTY", "CITY",
                        "SVCS", "SERVICES",
                        "SVSC", "SERVICES",
                        "WTR", "WATER",
                        "AUTH", "AUTHORITY",
                        "KC", "KING COUNTY",
                        "WA", "WASHINGTON",
                        "WASH", "WASHINGTON",
                        "WS", "WASHINGTON STATE",
                        "SCH", "SCHOOL",
                        "SCHL", "SCHOOL"
                      )  

names_cleaned <- names %>% 
  unnest_tokens(ORIG, TAXPAYER_NAME, token = "ngrams", n = 1, to_lower = FALSE) %>%  
  left_join(tp_name_recode, by = "ORIG") %>% 
  mutate(TAXPAYER_NAME = if_else(is.na(NEW),ORIG,NEW)) %>% 
  group_by(PIN) %>% 
  summarise(TAXPAYER_NAME_CLEAN = str_c(TAXPAYER_NAME, collapse = " ")) %>% 
  full_join(names, by = "PIN") %>%  
  select(PIN,TAXPAYER_NAME,TAXPAYER_NAME_CLEAN)

names_trimmed <- names_cleaned %>% 
  unnest_tokens(ORIG, TAXPAYER_NAME_CLEAN, token = "ngrams", n = 1, to_lower = FALSE) %>% 
  anti_join(stop_words_upper, by = c("ORIG" = "word")) %>%   
  group_by(PIN) %>% 
  summarise(TAXPAYER_NAME_TRIM = str_c(ORIG, collapse = " ")) %>% 
  full_join(names_cleaned, by = "PIN") %>%  
  select(PIN,TAXPAYER_NAME,TAXPAYER_NAME_CLEAN,TAXPAYER_NAME_TRIM)

names_trimmed_cnt <- names_trimmed %>% 
  count(TAXPAYER_NAME_TRIM, sort = TRUE)

# CREATE NGRAMS ----


names_ngrams_1 <- names_trimmed %>%  
  unnest_tokens(NGRAM_1, TAXPAYER_NAME_TRIM, token = "ngrams", n = 1, to_lower = FALSE) %>% 
  select(-TAXPAYER_NAME)

names_ngrams_1_cnt <- names_ngrams_1 %>% 
  count(NGRAM_1, sort = TRUE)

names_ngrams_2 <- names_trimmed %>%  
  unnest_tokens(NGRAM_2, TAXPAYER_NAME_TRIM, token = "ngrams", n = 2, to_lower = FALSE) %>% 
  separate(NGRAM_2, c("NGRAM_2_A", "NGRAM_2_B"),sep = " ") %>% 
  select(-TAXPAYER_NAME)

names_ngrams_2_cnt <- names_ngrams_2 %>% 
  count(NGRAM_2_A,NGRAM_2_B, sort = TRUE)

names_ngrams_3 <- names_trimmed %>%  
  unnest_tokens(NGRAM_3, TAXPAYER_NAME_TRIM, token = "ngrams", n = 3, to_lower = FALSE) %>% 
  separate(NGRAM_3, c("NGRAM_3_A", "NGRAM_3_B", "NGRAM_3_C"),sep = " ") %>% 
  select(-TAXPAYER_NAME)

names_ngrams_3_cnt <- names_ngrams_3 %>% 
  count(NGRAM_3_A,NGRAM_3_B,NGRAM_3_C, sort = TRUE)


names_ngrams <- 
  list(names_cleaned,names_ngrams_1,names_ngrams_2,names_ngrams_3) %>% 
  reduce(left_join, by = c("PIN","TAXPAYER_NAME_CLEAN"))

# VIEW NGRAMS NETWORK ----

make_bigram_graph <- function(){
  bigram_graph <- names_ngrams_2 %>% 
  count(NGRAM_2_A,NGRAM_2_B, sort = TRUE) %>% 
  filter(n > 20) %>% 
  graph_from_data_frame
  
  set.seed(2016)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()
}

make_bigram_graph()

# JOIN CATEGORIES TABLE ----

ngram_1_categorized <- names_ngrams %>% 
  left_join(cat_ngram_1, by = "NGRAM_1") %>% 
  group_by(PIN,TAXPAYER_NAME_CLEAN) %>% 
  summarise(CATEGORY = first_not_na(CATEGORY)) %>% 
  select(-TAXPAYER_NAME_CLEAN) %>% 
  drop_na

ngram_2_categorized <- names_ngrams %>% 
  left_join(cat_ngram_2, c("NGRAM_2_A", "NGRAM_2_B")) %>% 
  group_by(PIN,TAXPAYER_NAME_CLEAN) %>% 
  summarise(CATEGORY = first_not_na(CATEGORY)) %>% 
  select(-TAXPAYER_NAME_CLEAN) %>% 
  drop_na

ngram_3_categorized <- names_ngrams %>% 
  left_join(cat_ngram_3, c("NGRAM_3_A", "NGRAM_3_B", "NGRAM_3_C")) %>% 
  group_by(PIN,TAXPAYER_NAME_CLEAN) %>% 
  summarise(CATEGORY = first_not_na(CATEGORY)) %>% 
  select(-TAXPAYER_NAME_CLEAN) %>% 
  drop_na

names_categorized <- 
  list(names_trimmed, 
       ngram_1_categorized,
       ngram_2_categorized,
       ngram_3_categorized) %>% 
  reduce(left_join, by = "PIN") %>% 
  group_by(PIN) %>% 
  summarise(TAXPAYER_NAME = first(TAXPAYER_NAME),
            TAXPAYER_NAME_CLEAN = first(TAXPAYER_NAME_CLEAN),
            TAXPAYER_NAME_TRIM = first(TAXPAYER_NAME_TRIM),
            CATEGORY = first_not_na(c(CATEGORY.x,CATEGORY.y,CATEGORY))
  )

