sheet_key <- as_id("1EAjo_iL_wibBQUqZ9hvE1My6by4ip57b-dWB8mzmhN0")

dd_upload <- dd %>% 
  select(FIELD_NAME_DEV, FIELD_FORMAT, DICTIONARY_VERSION) 

dd_ss <- gs_key(sheet_key, lookup = NULL, visibility = NULL, verbose = TRUE)

gs_edit_cells(dd_ss, 
              ws = 1, 
              input = dd_upload, 
              anchor = "A1", 
              byrow = FALSE,
              col_names = TRUE, 
              trim = FALSE, 
              verbose = TRUE)
