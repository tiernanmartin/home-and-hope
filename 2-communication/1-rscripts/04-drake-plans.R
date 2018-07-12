# MAKE PLANS: TRIGGERS ----

triggers_plan <- drake_plan(
  trigger_name_recode_key = drive_get_datetime_modified("1aInQqXPK3tqrXKd80PXPugR8G7Nysz46tirCTAdKn6s"), 
  trigger_public_owner_name_category_key = drive_get_datetime_modified("1Uhj9GcPP93hfGehK1dPmxgLbsAXxUOnHXY8PFSfMnRI"),
  trigger_other_exempt_owner_name_category_key = drive_get_datetime_modified("1xRE5A2suzH_KcrrFpqdcX7fguFShokjFchm7khML6sQ"),
  trigger_suit_other = drive_get_datetime_modified("1a-xqAjyCI3XITm9BxfTdw6UNyoG5r2UyacNzE4N60QU"),
  trigger_dd_google_drive = drive_get_datetime_modified("1EAjo_iL_wibBQUqZ9hvE1My6by4ip57b-dWB8mzmhN0"),
  strings_in_dots = "literals"
)

# MAKE PLANS: PARCEL ----

lookup_plan <- drake_plan( 
  parcel_metadata_table = make_parcel_metadata_table(),
  lu = make_lu(),
  prop_type = make_prop_type(),
  tax_status = make_tax_status(),
  tax_reason = make_tax_reason(),
  present_use_recode = make_present_use_recode(), 
  parcel_lookup = make_parcel_lookup(parcel_metadata_table, lu, present_use_recode),
  name_recode_key = make_name_recode_key(trigger_name_recode_key),
  owner_antijoin_names = make_owner_antijoin_names(),
  public_owner_name_category_key = make_public_owner_name_category_key(trigger_public_owner_name_category_key),
  other_exempt_owner_name_category_key = make_other_exempt_owner_name_category_key(trigger_other_exempt_owner_name_category_key)
) %>% 
  bind_rows(triggers_plan) %>% 
  mutate(trigger = if_else(str_detect(target, "trigger"),
                           "always",
                           drake::default_trigger())) 

parcel_plan <- drake_plan(
  pub_parcel = make_pub_parcel(),
  acct = make_acct(),
  env_restrictions = make_env_restrictions(),
  parcel_addr = make_parcel_addr(),
  parcel_df = make_parcel_df(),
  parcel_sf_poly = make_parcel_sf_poly(),
  parcel_sf = make_parcel_sf(parcel_sf_poly),
  parcel_sf_ready = make_parcel_sf_ready(parcel_sf),
  parcel_addr_ready = make_parcel_addr_ready(parcel_addr),
  parcel_df_ready = make_parcel_df_ready(parcel_lookup, prop_type, pub_parcel, parcel_df),
  parcel_acct_ready = make_parcel_acct_ready(acct, tax_status, tax_reason), 
  parcel_env_ready = make_parcel_env_ready(env_restrictions),
  parcel_ready = make_parcel_ready(parcel_addr_ready, parcel_env_ready, join_list = list(parcel_acct_ready, parcel_sf_ready, parcel_df_ready)) 
) %>% 
  bind_rows(lookup_plan) %>% 
  mutate(trigger = if_else(str_detect(target, "trigger"),
                           "always",
                           drake::default_trigger())) 


# MAKE PLANS: EXTERNAL_DATA ----

external_data_plan <- drake_plan(
  waterbodies = make_waterbodies(),
  uga = make_uga(),
  zoning = make_zoning(),
  kc_city = make_kc_city(),
  census_tracts = make_census_tracts(),
  king_county = make_king_county(),
  zcta = make_zcta(king_county),
  census_place = make_census_place(),
  school_districts = make_school_districts(),
  leg_districts = make_leg_districts(),
  kc_council_districts = make_kc_council_districts(),
  seattle_council_districts = make_seattle_council_districts(),
  mj_businesses_raw = make_mj_businesses_raw(),
  mj_businesses = make_mj_businesses(mj_businesses_raw = mj_businesses_raw),
  el_facilities = make_el_facilities(),
  other_suitability_characteristics = make_other_suitability_characteristics(trigger_suit_other),
  affordable_housing_subsidies_raw = make_affordable_housing_subsidies_raw(),
  affordable_housing_subsidies = make_affordable_housing_subsidies(affordable_housing_subsidies_raw = make_affordable_housing_subsidies),
  affordable_housing_properties_raw = make_affordable_housing_properties_raw(),
  affordable_housing_properties = make_affordable_housing_properties(affordable_housing_properties_raw),
  transit_stops_osm = make_transit_stops_osm(),
  play_spaces_osm = make_play_spaces_osm(),
  seattle_dev_cap = make_seattle_dev_cap(),
  future_lightrail = make_future_lightrail(),
  brownfield_sites = make_brownfield_sites(),
  contaminated_sites_raw = make_contaminated_sites_raw(),
  contaminated_sites = make_contaminated_sites()
)

# MAKE PLANS: SUITABILITY AND UTILIZATION ----

development_assumptions_plan <- drake_plan(
  city_block_sqft = make_city_block_sqft(),
  city_block_acre = make_city_block_acre(city_block_sqft),
  lot_types = make_lot_types(city_block_sqft),
  lot_size_breaks = make_lot_size_breaks(city_block_acre),
  development_assumptions_zoning = make_development_assumptions_zoning(),
  development_assumptions_lot = make_development_assumptions_lot(lot_types, development_assumptions_zoning)
)

suitability_criteria_plan <- drake_plan(
  criteria_tax_exempt = make_criteria_tax_exempt(),
  criteria_max_water_overlap_pct = make_criteria_max_water_overlap_pct(),
  criteria_within_uga = make_criteria_within_uga(),
  criteria_developable_zoning = make_criteria_developable_zoning(development_assumptions_zoning),
  criteria_undevelopable_present_use = make_criteria_undevelopable_present_use(),
  criteria_lot_size = make_criteria_lot_size(lot_size_breaks),
  criteria_area_ratio = make_criteria_area_ratio(),
  criteria_steep_vacant = make_criteria_steep_vacant(),
  criteria_other = make_criteria_other(),
  suitability_criteria = make_suitability_criteria(criteria_tax_exempt, criteria_max_water_overlap_pct, criteria_within_uga, criteria_developable_zoning, criteria_undevelopable_present_use, criteria_lot_size, criteria_area_ratio, criteria_steep_vacant, criteria_other)
)

suitability_plan <- drake_plan(
  suitability_tax_exempt = make_suitability_tax_exempt(parcel_ready),
  suitability_water_overlap = make_suitability_water_overlap(parcel_sf_ready, waterbodies),
  suitability_within_uga = make_suitability_within_uga(parcel_sf_ready, uga),
  suitability_developable_zoning = make_suitability_developable_zoning(parcel_sf_ready, zoning),
  suitability_present_use = make_suitability_present_use(parcel_ready),
  suitability_lot_size = make_suitability_lot_size(parcel_sf_ready, lot_size_breaks),
  suitability_parcel_area_ratio = make_suitability_parcel_area_ratio(parcel_sf_ready),
  suitability_steep_vacant = make_suitability_steep_vacant(parcel_ready),
  suitability_other = make_suitability_other(parcel_sf_ready, other_suitability_characteristics),
  suitability = make_suitability(parcel_ready, suitability_criteria, suitability_tax_exempt, suitability_water_overlap, suitability_within_uga, suitability_developable_zoning, suitability_present_use, suitability_lot_size, suitability_parcel_area_ratio, suitability_steep_vacant, suitability_other)
)


building_plan <- drake_plan(
  building_residential = make_building_residential(),
  building_apartment = make_building_apartment(),
  building_condo = make_building_condo(),
  building_commercial = make_building_commercial(),
  building = make_building(building_residential, building_apartment, building_condo, building_commercial)
)

utilization_criteria_plan <-  drake_plan(
  util_criteria_lot_size = make_util_criteria_lot_size(city_block_sqft, lot_types),
  util_criteria_utilization_ratio =  make_util_criteria_utilization_ratio(),
  util_criteria_utilization_ratio_bins = make_util_criteria_utilization_ratio_bins(),
  utilization_criteria = make_utilization_criteria(util_criteria_lot_size, util_criteria_utilization_ratio, util_criteria_utilization_ratio_bins)
)
 
utilization_plan <- drake_plan(
  seattle_util_ratio = make_seattle_util_ratio(parcel_sf_ready, seattle_dev_cap),
  utilization_present = make_utilization_present(parcel_ready, building),
  utilization_lot_size = make_utilization_lot_size(parcel_ready, utilization_criteria),
  utilization_potential = make_utilization_potential(suitability, development_assumptions_lot, utilization_lot_size),
  utilization = make_utilization(utilization_criteria, suitability, utilization_present, utilization_potential, seattle_util_ratio)
)



suit_util_plan <- bind_rows(
  parcel_plan, 
  external_data_plan,
  development_assumptions_plan,
  suitability_criteria_plan,
  suitability_plan,
  building_plan,
  utilization_criteria_plan,
  utilization_plan 
) %>% 
  mutate(trigger = if_else(str_detect(target, "trigger"),
                           "always",
                           drake::default_trigger())) 

# MAKE PLANS: FILTERS AND HELPERS ----

official_names_plan <- drake_plan(
  official_names_seattle = make_official_names_seattle(),
  official_names_kc = make_official_names_kc(),
  official_names_wa = make_official_names_wa(),
  official_names_us = make_official_names_us(),
  official_names_places = make_official_names_places(census_place),
  official_names_tribes = make_official_names_tribes(),
  official_names_housing_authorities = make_official_names_housing_authorities(),
  official_names_regional_transit_authorities = make_official_names_regional_transit_authorities(),
  official_names_special_purpose_districts = make_official_names_special_purpose_districts(),
  official_names_higher_ed_providers = make_official_names_higher_ed_providers(),
  official_names_hospitals = make_official_names_hospitals(),
  official_names = make_official_names(official_names_seattle, official_names_kc, official_names_wa, official_names_us, official_names_places, official_names_tribes, official_names_housing_authorities, official_names_regional_transit_authorities, official_names_special_purpose_districts, official_names_higher_ed_providers, official_names_hospitals)
)

owner_plan <- drake_plan( 
  owner_name_full = make_owner_name_full(suitability, name_recode_key, owner_antijoin_names),
  owner_category = make_owner_category(owner_name_full, public_owner_name_category_key, other_exempt_owner_name_category_key) 
  
)


filter_plan <- drake_plan(
  filters_census_tract = make_filters_census_tract(parcel_sf_ready, census_tracts),
  filters_zcta = make_filters_zcta(parcel_sf_ready, zcta),
  filters_place = make_filters_place(parcel_sf_ready, census_place),
  filters_place_name = make_filters_place_name(parcel_df_ready, filters_place),
  filters_owner_category = make_filters_owner_category(owner_category),
  filters_public_owner = make_filters_public_owner(owner_category), 
  filters_zoning_category = make_filters_zoning_category(suitability_developable_zoning),
  filters_proximity_transit = make_filters_proximity_transit(parcel_sf_ready, transit_stops_osm),
  filters_proximity_play_space = make_filters_proximity_play_space(parcel_sf_ready, play_spaces_osm),
  filters_proximity_marijuana = make_filters_proximity_marijuana(parcel_sf_ready, mj_businesses),
  filters_proximity_el_facilities = make_filters_proximity_el_facilities(parcel_sf_ready, el_facilities), 
  filters_proximity_affordable_housing = make_filters_proximity_affordable_housing(parcel_sf_ready, affordable_housing_properties),
  filters_potential_units = make_filters_potential_units(parcel_ready),
  filters_leg_district = make_filters_leg_district(parcel_sf_ready, leg_districts), 
  filters_kc_council_district = make_filters_kc_council_district(parcel_sf_ready, kc_council_districts),
  filters_seattle_council_district = make_filters_seattle_council_district(parcel_sf_ready, seattle_council_districts),
  filters_school_district = make_filters_school_district(parcel_sf_ready, school_districts),
  filters_historic = make_filters_historic(parcel_ready),
  filters_afford_expir_date = make_filters_afford_expir_date(parcel_sf_ready, affordable_housing_subsidies),
  filters_eligibility_nmtc = make_filters_eligibility_nmtc(filters_census_tract),
  filters_eligibility_dda = make_filters_eligibility_dda(filters_zcta),
  filters_eligibility_qct = make_filters_eligibility_qct(filters_census_tract),
  filters_eligibility_oz = make_filters_eligibility_oz(filters_census_tract),
  filters_parking = make_filters_parking(parcel_df_ready),
  filters_proximity_lightrail = make_filters_proximity_lightrail(parcel_sf_ready, future_lightrail),
  filters_brownfield = make_filters_brownfield(parcel_sf_ready, brownfield_sites),
  filters = make_filters(parcel_ready, 
                         filters_census_tract, 
                         filters_zcta,
                         filters_place,
                         filters_place_name,
                         filters_owner_category,
                         filters_public_owner,  
                         filters_zoning_category,
                         filters_proximity_transit,
                         filters_proximity_play_space,
                         filters_proximity_marijuana,
                         filters_proximity_el_facilities,
                         filters_proximity_affordable_housing,
                         filters_leg_district,
                         filters_kc_council_district,
                         filters_seattle_council_district,
                         filters_school_district, 
                         filters_historic, 
                         filters_afford_expir_date,
                         filters_eligibility_nmtc,
                         filters_eligibility_dda,
                         filters_eligibility_qct,
                         filters_eligibility_oz,
                         filters_parking,
                         filters_proximity_lightrail,
                         filters_brownfield)
)

helper_plan <- drake_plan(
  helpers_opp360_xwalk = make_helpers_opp360_xwalk(),
  helpers_url_parcel_viewer = make_helpers_url_parcel_viewer(parcel_ready),
  helpers_url_opp360 = make_helpers_url_opp360(filters_census_tract, helpers_opp360_xwalk),
  helpers = make_helpers(helpers_url_parcel_viewer,helpers_url_opp360)
  
)

filter_helper_plan <- bind_rows(
  suit_util_plan,
  owner_plan,
  filter_plan,
  helper_plan
) %>% 
  mutate(trigger = if_else(str_detect(target, "trigger"),
                           "always",
                           drake::default_trigger())) 

# MAKE PLANS: INVENTORY_PLAN ----

inventory_plan <- drake_plan(
  inventory = make_inventory(owner_category, filters, helpers, suitability, utilization),
  inventory_suitable = make_inventory_suitable(inventory), 
  inventory_suitable_poly = make_inventory_suitable_poly(inventory_suitable),
  inventory_suitable_point = make_inventory_suitable_point(inventory_suitable)
  
) %>% 
  bind_rows(filter_helper_plan) %>% 
  mutate(trigger = if_else(str_detect(target, "trigger"),
                           "always",
                           drake::default_trigger()))

# MAKE PLANS: DOCUMENTATION ----


data_dictionary_plan <- drake_plan(
  dd_field_name_dev = make_dd_field_name_dev(inventory), 
  dd_field_format = make_dd_field_format(inventory),
  dd_data_source = make_dd_data_source(),
  dd_dictionary_version = make_dd_dictionary_version(dd_field_name_dev, "v0.3"),
  dd_google_drive = make_dd_google_drive(trigger_dd_google_drive, dd_field_name_dev, dd_field_format, dd_dictionary_version),
  dd = make_dd(dd_google_drive), 
  strings_in_dots = "literals"
) 

documentation_plan <- bind_rows(
  inventory_plan,
  data_dictionary_plan
) %>%  
  mutate(trigger = if_else(str_detect(target, "trigger"),
                           "always",
                           drake::default_trigger()))

# MAKE PLANS: EXPORT ----

export_plan <- drake_plan( 
  write_csv(dd, file_out(here("1-data/4-ready/data_dictionary.csv"))), 
  write_inventory_csv(inventory, file_out(here("1-data/4-ready/inventory_table.csv"))), 
  write_inventory_rda(inventory, file_out(here("1-data/4-ready/inventory_table.rda"))), 
  write_inventory_xlsx(inventory, file_out(here("1-data/4-ready/inventory_table.xlsx"))), 
  write_inventory_csv(inventory_suitable, file_out(here("1-data/4-ready/inventory_suitable_table.csv"))), 
  write_inventory_rda(inventory_suitable, file_out(here("1-data/4-ready/inventory_suitable_table.rda"))), 
  write_inventory_xlsx(inventory_suitable, file_out(here("1-data/4-ready/inventory_suitable_table.xlsx"))), 
  write_inventory_shp(inventory_suitable_poly, file_out(here("1-data/4-ready/inventory_suitable_poly.shp"))),
  write_inventory_shp(inventory_suitable_point, file_out(here("1-data/4-ready/inventory_suitable_point.shp"))),
  c(file_out(here("1-data/4-ready/inventory_suitable_poly.EXTN")), file_in(here("1-data/4-ready/inventory_suitable_poly.shp"))),
  c(file_out(here("1-data/4-ready/inventory_suitable_point.EXTN")), file_in(here("1-data/4-ready/inventory_suitable_point.shp"))),
  zip_pithy(file_out(here("1-data/4-ready/site-inventory-20180705.zip")), c(file_in(here("1-data/4-ready/data_dictionary.csv")),
                                                                            file_in(here("1-data/4-ready/inventory_table.csv")),
                                                                            file_in(here("1-data/4-ready/inventory_table.rda")),
                                                                            file_in(here("1-data/4-ready/inventory_table.xlsx")),
                                                                            file_in(here("1-data/4-ready/inventory_suitable_table.csv")),
                                                                            file_in(here("1-data/4-ready/inventory_suitable_table.rda")),
                                                                            file_in(here("1-data/4-ready/inventory_suitable_table.xlsx")),
                                                                            file_in(here("1-data/4-ready/inventory_suitable_poly.shp")),
                                                                            file_in(here("1-data/4-ready/inventory_suitable_point.shp")),
                                                                            file_in(here("1-data/4-ready/inventory_suitable_poly.dbf")),
                                                                            file_in(here("1-data/4-ready/inventory_suitable_poly.prj")),
                                                                            file_in(here("1-data/4-ready/inventory_suitable_poly.shx")),
                                                                            file_in(here("1-data/4-ready/inventory_suitable_point.dbf")),
                                                                            file_in(here("1-data/4-ready/inventory_suitable_point.prj")),
                                                                            file_in(here("1-data/4-ready/inventory_suitable_point.shx"))
                                                                            ))
) %>% 
  evaluate_plan(wildcard = "EXTN", values = c("dbf", "prj", "shx")) %>% 
  modify_at("target", drake_here) %>% 
  bind_rows(documentation_plan) %>% 
  mutate(trigger = if_else(str_detect(target, "trigger")| target %in% c("dd_dictionary_version"),
                           "always",
                           drake::default_trigger())) 
  
