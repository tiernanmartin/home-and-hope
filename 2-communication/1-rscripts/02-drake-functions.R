# FUNCTION: STR_TO_clean_upper ----

str_clean_upper <- function(x){str_to_upper(str_trim(str_replace_all(x,"^[:graph:]]","")))}

# FUNCTION: GS_READ_ALL ----

# source: https://github.com/jennybc/googlesheets/issues/320#issue-230901546

gs_read_all <- function(ss, delay_length = 5){ 
  ws_names <- gs_ws_ls(ss)
   
  
  gs_read_delayed <- function(ss, ws){
    result <- gs_read(ss, ws)
    Sys.sleep(delay_length)
    return(result) 
  }
  
  worksheets <- map(ws_names, ~ gs_read_delayed(ss, ws = .x)) %>% 
    set_names(ws_names)
  
  
  return(worksheets)
}


# FUNCTIONS: LOGICAL RECODING ----

is_logical_yn <- function(x){all(unique(x) %in% c("Y","N",NA_character_))}

recode_logical_yn <- function(x){ if_else(x %in% "Y",TRUE,FALSE,missing = NA)}

is_logical_01 <- function(x){all(unique(x) %in% c(1,0, NA))}

recode_logical_01 <- function(x){ if_else(x %in% 1,TRUE,FALSE,missing = NA)}

is_logical_yesno <- function(x){
  
  strings <- list("yes", "no") %>% 
  map(~ list(to_screaming_snake_case,to_snake_case,to_upper_camel_case) %>% 
        invoke_map_chr(.x) 
        ) %>% flatten_chr() %>% 
    prepend(NA_character_)
  
  all(unique(x) %in% strings)
  
  }

recode_logical_yesno <- function(x){ 
  
  yes <- map("yes",~ list(to_screaming_snake_case,to_snake_case,to_upper_camel_case) %>% 
        invoke_map_chr(.x) 
        ) %>% flatten_chr()
  
  if_else(x %in% yes,TRUE,FALSE,missing = NA)
  
  }


# FUNCTION: NAME_TBL_VECTOR ----
name_tbl_vector <- function(x, name, value){
  x %>% 
    transpose %>% 
  {set_names(map_chr(., value), map_chr(.,name))}

}

# FUNCTION: ANY_NOT_NA ----

any_not_na <- function(x){ reduce( map(x, is.na), `+`) != ncol(x)}


# FUNCTION: EMPTY_AS_NA ----

empty_as_na <- function(x){
    if("factor" %in% class(x)) x <- as.character(x) ## since ifelse wont work with factors
    ifelse(as.character(x)!="", x, NA_character_)
}


# FUNCTION: EXTRACT_TARGET_PATHS ----

extract_target_paths <- function(plan){
 
  paths <- pluck(plan,"target") %>% map_chr(str_replace_all, pattern = "'", replacement = "")

  shp_pattern <- "\\.shp$"
  
  paths_contain_shapefiles <- any(str_detect(paths, shp_pattern))
  
 if(paths_contain_shapefiles){ 
   
   shapefile_exts <- c(".shx",".dbf",".prj") 
   
   shps <- keep(paths, ~ str_detect(.,shp_pattern)) 
   
   new_paths <- shps %>% 
     map(str_replace, pattern = shp_pattern, replacement = shapefile_exts) %>% 
     flatten_chr %>% 
     c(paths)
   
   return(new_paths)
   
 }else{ return(paths) }

}



# FUNCTION: INTEGER_DUMMY ----
  integer_dummy <- function(n, mean = 0, sd = 1){
    
    x <- as.integer(round(abs(rnorm(n, mean , sd)),0))
   
    return(x)
  }

# FUNCTION: SPATIAL_DUMMY ----
  spatial_dummy <- function(n, mean = 0, sd = 1){
    
    jitter <- abs(rnorm(n, mean = 0, sd = 1)) 
    
    x <- rnorm(n, mean , sd) 
    
    x[x<0] <- .1
    
    x <- round(x + jitter,2)
    
    return(x)
  }

# FUNCTION: DRAKE_HERE ----
drake_here <- function(x) {
  x %>% str_replace_all('\"', "") %>% here() %>% as_drake_filename()
}
# FUNCTION: WITHIN_RANGE ----
`%within_range%` <- function(x,range){ 
  between(x,min(range),max(range))
          
          } 

# FUNCTION: LESSER_OF ----
lesser_of <- function(x,y){
  x <- as.double(x)
  y <- as.double(y)
  
  if_else(x<= y, x,y, missing = x)
  
  } 

# FUNCTION: MAKE_PARSE_LU_STRING ----
parse_lu_string <- function(string, col_sep, row_sep, join_name, long_name){ 
  str_split(string, pattern = row_sep) %>% 
    flatten() %>% 
    keep(~ str_detect(.x,"")) %>%  
    str_replace_all("\\\n","") %>% 
    map_chr(c) %>% 
    map_df(~.x %>% str_split(pattern = col_sep) %>% as.data.frame %>% t %>% as_data_frame ) %>%  
    set_names(c(join_name,long_name))
}


# FUNCTION: ST_INTERSECTS_ANY ----
st_intersects_any <- function(x, y) {
  sapply(st_intersects(x, y), function(z) length(z)> 0)
}
# FUNCTION: ST_INTERSECT_AREA ----
st_intersect_area <- function(x, y){ 
  
  x_sfc <- x %>% 
    st_geometry %>% 
    st_transform(st_crs(y)) 
  
  area_x <- x_sfc %>% st_area() %>% as.double()
  
  area_xy <- st_intersection(x_sfc, y) %>% st_area %>% as.double()
  
  if(is_empty(area_xy)){return(as.double(0))}
  
  overlap_pct <- area_xy %>% 
    magrittr::divide_by(area_x) %>% 
    as.double() %>% 
    round(2)
  
  return(overlap_pct)
}

# FUNCTION: ST_OVER ----

st_over <- function(x,y,col){
  idx <- sapply(st_intersects(x,y), function(z) if (length(z)==0) NA_integer_ else z[1])
  
  y[idx,col][[1]]
}
