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
