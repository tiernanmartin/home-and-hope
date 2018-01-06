# Setup ----

library(tidyverse)
library(snakecase)
library(AmesHousing)
library(rpart)
library(rattle)

root <- rprojroot::is_rstudio_project
root_file <- root$make_fix_file()
options(httr_oob_default=TRUE) 
htmltools::tagList(rmarkdown::html_dependency_font_awesome())

# Access Data ----



# View Data ----


