# Setup ----

library(tidyverse) 
library(seleniumPipes)
library(googledrive)
library(miscgis)

options(httr_oob_default=TRUE) 


# Set up Sauce Labs testing ----


user <- "tiernan_martin" # Your Sauce Labs username
pass <- "8191a0e4-a499-45d5-860d-50a25bf0432d" # Your Sauce Labs access key 
port <- 80
ip <- paste0("http://",user, ':', pass, "@ondemand.saucelabs.com")
rdBrowser <- "chrome"
version <- "52"   
platform <- "Windows 10"  # helpful: https://wiki.saucelabs.com/display/DOCS/Platform+Configurator#/


remDr <- seleniumPipes::remoteDr(remoteServerAddr = ip,
                                   port = port,
                                   browserName = rdBrowser,
                                   version = version,
                                   platform = platform)

# Test ----

nav_to_results <- function(remDr, fac_name = "a"){
  # Landing Page ----
  
  # open the website
  remDr %>% go("https://apps.del.wa.gov/Check/CheckSearch.aspx")
  
  # get the facility name box
  facilty_name_box <- remDr %>% findElement("name", "ctl00$bodyContentPlaceHolder$txtFacilityName")
  
  # get the search button
  search_btn <- remDr %>% findElement("name", "ctl00$bodyContentPlaceHolder$btnSearch")
  
  # get the county box
  county_box <- remDr %>% findElement("name", "ctl00$bodyContentPlaceHolder$ddlCounty")
  
  # send the query to the box and click the search button
  facilty_name_box %>% elementSendKeys(fac_name)
  
  county_box %>% elementSendKeys("King")
  
  search_btn %>% elementClick()
}

max_results <- function(remDr){
  
  
  # Scroll down to bottom (this helps in debugging)
  
  remDr %>% findElement("css selector", "body") %>% elementSendKeys(selKeys$end)
  
  # get the RadComboBox dropdown  
  dd <- 
    remDr %>% 
    findElement(using = "name", value = "ctl00$bodyContentPlaceHolder$rgSearchResults$ctl00$ctl03$ctl01$PageSizeComboBox")
  
  # Change the value to 50 and send the 'enter' key
  dd %>% elementSendKeys("50")
  
  Sys.sleep(2)
  
  dd %>% elementSendKeys(selKeys$return)

  Sys.sleep(5)
  
  
}

check_current_page <- function(remDr){
  
 remDr %>% 
    findElement("class name", "rgCurrentPage") %>%
    getElementText() %>%
    as.integer()
}

get_n_pages <- function(remDr){
  remDr %>% 
    findElement("xpath",'//*[@id="ctl00_bodyContentPlaceHolder_rgSearchResults_ctl00"]/tfoot/tr/td/table/tbody/tr/td/div[5]/strong[2]') %>% 
    getElementText() %>% 
    as.integer()
}

get_urls <- function(remDr){

  remDr %>%
    findElements("link text", "View History")  %>% 
    map(getElementAttribute, attribute = "href")  
}

click_next_page_btn <- function(remDr){
  remDr %>% 
    findElement("class name", "rgPageNext") %>% 
    elementSendKeys(selKeys$return)
  Sys.sleep(6)
}

run_loop <- function(current_page, urls, remDr, n_pages){
  
  urls<- get_urls(remDr) 
  
  if(current_page <= n_pages){
    
    click_next_page_btn(remDr) 
    urls
    
  }else{ return(urls) }

}

end_session <- function(remDr){remDr %>% deleteSession()}


# NOTE:   this only pulls facilities
#         with "a" in their name.
#         Augment the script to rotate throught the vowels.


nav_to_results(remDr)
max_results(remDr)
n_pages <- get_n_pages(remDr) 
urls_list <- map(1:n_pages,~ run_loop(current_page = .x, urls = empty_urls_list,remDr = remDr,n_pages = n_pages))
end_session(remDr)

urls_list %>% 
  unlist %>% 
  drive_upload_obj("urls.rds", 
                   readr::write_rds, 
                   .share = FALSE, 
                   folder = drive_get("~/Futurewise/Enterprise/1-data/1-raw/"))
