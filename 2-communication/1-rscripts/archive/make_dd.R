make_dd <- function(dd_google_drive){
  
  dd <- gs_read(dd_google_drive, ws = "STATIC_VERSION", col_types = cols(
    FIELD_NAME_DEV = col_character(),
    FIELD_NAME_USER = col_character(),
    FIELD_FORMAT = col_character(),
    FIELD_DESCRIPTION = col_character(),
    DATA_SOURCE = col_character(),
    FIELD_TAG_SENSITIVE = col_logical(),
    FIELD_TAG_FILTER = col_logical(),
    FIELD_TAG_TOOLTIP = col_logical(),
    FIELD_TAG_TABLE = col_logical(),
    FIELD_TAG_DUMMY = col_logical(),
    DICTIONARY_VERSION = col_character()
  ))
  
  return(dd)
  
}