# Setup ----

library(tidyverse)
library(purrr)
library(stringr)
library(tibble)
library(rvest)
library(snakecase)
library(miscgis)

options(httr_oob_default=TRUE) 

# Scrape the urls





# Scrape the content from each URL ----

test_url <- "https://apps.del.wa.gov/Check/LicenseView.aspx?id=-424994"

page <- read_html(test_url)

upper_tbl <- 
  page %>%
  html_nodes("#LicenseView > table") %>% 
  html_table(fill = TRUE) %>% 
  flatten %>% 
  as_tibble %>% 
  select(-4) %>% 
  unlist %>% 
  unique %>% 
  keep(~ nchar(.x)>1) %>% 
  map_chr(~ .x %>% str_split(pattern = "[[:lower:]]{1}[[:upper:]]{1}") %>% str_c(sep = " ")) 
  
t_tibble <- function(x){as_tibble(cbind(nms = names(x), t(x)))}

upper_tbl %>% 
  select(-4) %>% 
  t_tibble

lower_tbl <- 
  page %>%
  html_nodes("#ctl00_bodyContentPlaceHolder_divLicenseOpenTable") %>% 
  html_table(fill = TRUE) %>% 
  flatten %>% 
  as_tibble %>% 
  select(-3) %>% 
  spread(1,2) %>% 
  spread(1,2) %>% 
  summarise_all(miscgis::first_not_na) %>% 
  rename_all(snakecase::to_screaming_snake_case)%>% 
  mutate(LICENSED_CAPACITY = as.integer(str_extract(LICENSED_CAPACITY,"[[:digit:]]*"))) 



#ctl00_bodyContentPlaceHolder_divLicenseOpenTable

  # Clean Data ----


# Upload the data to Drive ----

