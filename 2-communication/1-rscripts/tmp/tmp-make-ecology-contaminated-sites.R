source(here::here("2-communication/1-rscripts/01-drake-setup.R"))

cleanup_sites_url <- "https://data.wa.gov/resource/2tkm-ssw6.geojson"

dat <-read_sf(cleanup_sites_url) %>% 
  as_tibble %>% 
  clean_names(case = "screaming_snake") %>% 
  rename(geometry = GEOMETRY) %>% 
  st_as_sf()

site_cleanup_details_url <- "https://fortress.wa.gov/ecy/tcpwebreporting/tcpreportviewer.aspx?id=csd&format=pdf&csid="


dat %>% 
  transmute(CLEANUP_SITE_ID,
            CLEANUP_SITE_NAME,
            CLEANUP_SITE_STATUS = CLEANUP_STATUS,
            CLEANUP_SITE_URL = WEBSITE,
            CLEANUP_SITE_DETAILS_URL = str_c(site_cleanup_details_url,CLEANUP_SITE_ID, sep = ""),
            geometry) %>% glimpse

# OR WHAT ABOUT THE BROWNSFIELDS?

# source: https://fortress.wa.gov/ecy/tcpwebreporting/report.aspx

dr_id <- as_id("1fUdnvNaJtTNXNLvlCNlb9GWv_10jpkLm")

brown <- drive_read(dr_id = dr_id,.tempfile = TRUE,read_fun = read_csv)


glimpse(brown)


# OK -- the reports are accessible as a multi-sheet xlsx file: "https://fortress.wa.gov/ecy/tcpwebreporting/report.aspx"

# here's the plan:
#  1. there should be a brownfields indicator (and github issue) even though there currently are none in KC (this might change in the future)
#  2. the site clean and report .xlsx should be an indicator. The data will need some cleaning and transforming but it appears to be the most complete contamination data that ecology provides