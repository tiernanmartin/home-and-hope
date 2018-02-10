library(snakecase)
library(fuzzyjoin)
library(skimr)
library(tidyverse)

df <- mtcars %>%  
  rename_all(to_screaming_snake_case) %>% 
  rownames_to_column("NAME") %>%  
  as_tibble() %>% 
  mutate_at(vars(matches("AM$|VS")),as.logical) %>%  
  mutate_at(vars(matches("CARB|CYL|GEAR")),factor) 

df1 <- df %>% 
  transmute(NAME, 
            DISP, 
            VS)

df2 <- df %>% 
  transmute(VS,
            DISP, 
         COMBO = case_when(
              VS & DISP>mean(DISP) ~ "TRUE_BIG",
              VS & DISP<=mean(DISP) ~ "TRUE_SMALL",
              !VS & DISP>mean(DISP) ~ "FALSE_BIG",
              TRUE ~ "FALSE_SMALL"
            )) %>% 
  distinct

`%greater_than_mean%` <- function(x,range) x > mean(range, na.rm = TRUE)


fuzzy_left_join(df1, df2, by = c("VS","DISP"), match_fun = c(`==`,`%greater_than_mean%`))
