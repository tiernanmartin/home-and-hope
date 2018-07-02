
# LOAD DATA ---------------------------------------------------------------


loadd(owner_name_full, suitability_tax_exempt, official_names)



# EXPLORE OWNER NAMES -----------------------------------------------------


d <- owner_name_full %>% 
  left_join(suitability_tax_exempt, by = "PIN")  %>% 
  filter(SUIT_OWNER_TAX_E)

# How many names are we talking about?

length(unique(d$OWNER_NAME_FULL))

# 3529
  
# How many unique names are in each tax exemption type?

d %>%
  gather(TYPE, VAL, matches("SUIT")) %>% 
  filter(VAL) %>% 
  filter(! TYPE %in% "SUIT_OWNER_TAX_E") %>%   
  group_by(TYPE) %>% 
  count(OWNER_NAME_FULL) %>%   
  count(TYPE)

# 1 SUIT_OWNER_NONPROFIT      752
# 2 SUIT_OWNER_OTHER_EXEMPT  2563
# 3 SUIT_OWNER_PUBLIC         467


# So - how about all public and non-profit, plus any other with more than 2


d %>%
  gather(TYPE, VAL, matches("SUIT")) %>% 
  filter(VAL) %>% 
  filter(! TYPE %in% "SUIT_OWNER_TAX_E") %>%   
  group_by(TYPE) %>% 
  count(OWNER_NAME_FULL) %>%  
  filter(n > 2 | TYPE %in% c("SUIT_OWNER_PUBLIC")) %>% 
  count(TYPE)

# We definitely want the public names, but what is actually in the np and other exempt?

# np

d %>% 
  filter(SUIT_OWNER_NONPROFIT) %>% 
  count(OWNER_NAME_FULL, sort = T) %>% 
  filter(n>4) %>% 
  print(n=Inf)  # the result looks pretty good (33 names, all coherent)

# other exempt

d %>% 
  filter(SUIT_OWNER_OTHER_EXEMPT) %>% 
  count(OWNER_NAME_FULL, sort = T) %>% 
  filter(n>4) %>% 
  print(n=Inf) 

# Ok - this is going to take a really long time
# and we only have official names for the public owners,
# so let's just do the public owner names


# SETUP GOOGLESHEETS UPLOADING --------------------------------------------

sheet_key <- as_id("1Uhj9GcPP93hfGehK1dPmxgLbsAXxUOnHXY8PFSfMnRI")
  
dd_ss <- gs_key(sheet_key, lookup = NULL, visibility = NULL, verbose = TRUE)


# UPLOAD PUBLIC OWNER NAMES -----------------------------------------------

names_public <- d %>% 
  filter(SUIT_OWNER_PUBLIC) %>% 
  count(OWNER_NAME_FULL, sort = TRUE) %>% 
  select(OWNER_NAME_FULL)

gs_edit_cells(dd_ss, 
              ws = "UPLOAD_NAME_FULL", 
              input = names_public, 
              anchor = "A1", 
              byrow = FALSE,
              col_names = TRUE, 
              trim = FALSE, 
              verbose = TRUE)


# UPLOAD OFFICIAL NAMES ---------------------------------------------------

# This list is long, so it needs to be split into a list of bite-size chunks

official_names_ready <- official_names %>% 
  select(OWNER_NAME_OFFICIAL,OWNER_NAME_CATEGORY, OWNER_NAME_ORG, OWNER_NAME_DEPT) %>%  
  mutate(N = row_number(),
         SPLIT = ntile(N,10)) %>% 
  group_by(SPLIT) %>% 
  mutate( START = str_c("A",first(N)+1, sep = "")) %>% 
  ungroup()


edit_gs <- function(x){
  
  anchor <- unique(x$START)
  
  official_names <- select(x,OWNER_NAME_OFFICIAL:OWNER_NAME_DEPT)
  
  gs_edit_cells(dd_ss, 
                ws = "UPLOAD_OFFICIAL", 
                input = official_names, 
                anchor = anchor, 
                byrow = FALSE,
                col_names = FALSE, 
                trim = FALSE, 
                verbose = TRUE)
  
}

official_names_ready %>% 
  split(.$SPLIT) %>% 
  walk(edit_gs)

