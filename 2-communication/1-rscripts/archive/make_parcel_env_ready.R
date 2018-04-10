make_parcel_env_ready <- function(env_restrictions){
  
  env_frmt <- env_restrictions %>% 
    rename_all(to_screaming_snake_case) %>%   
    filter(!is.na(TYPE)) %>% 
    transmute(PIN = make_pin(MAJOR, MINOR),
              TYPE = to_parsed_case(TYPE) %>% toupper %>% str_replace_all("_"," "), 
              PCNT_AFFECTED,
              PCT = if_else(PCNT_AFFECTED>0, str_c(PCNT_AFFECTED,"%"),""),
              RESTRICTION = str_c(TYPE, " (",PCT,")") %>% str_remove_all("\\s\\(\\)"))
  
  env_res <- env_frmt %>% 
    group_by(PIN) %>%  
    nest() %>% 
    transmute(PIN,
              ENV_RESTRICTIONS = map_chr(data, ~ pluck(.x,"RESTRICTION") %>% str_c(collapse = ", "))) 
  
  parcel_env_ready <- env_res
  
  return(parcel_env_ready)
  
}