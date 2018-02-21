# MAKE PLANS: PARCEL ----

lookup_plan <- drake_plan( 
  lu = make_lu(),
  prop_type = make_prop_type(),
  tax_status = make_tax_status(),
  tax_reason = make_tax_reason()
)
 
parcel_plan <- drake_plan(
  pub_parcel = make_pub_parcel(),
  acct = make_acct(),
  parcel_addr = make_parcel_addr(),
  parcel_df = make_parcel_df(),
  parcel_sf_poly = make_parcel_sf_poly(),
  parcel_sf = make_parcel_sf(parcel_sf_poly),
  parcel_ready = make_parcel_ready(lu, prop_type, tax_status, tax_reason, pub_parcel, acct, parcel_addr, parcel_df, parcel_sf)
) %>% bind_rows(lookup_plan)

# MAKE PLANS: SUITABILITY AND UTILIZATION ----

miscellaneous_plan <- drake_plan(
  waterbodies = make_waterbodies(),
  uga = make_uga(),
  zoning = make_zoning(),
  census_tracts = make_census_tracts()
)

development_assumptions_plan <- drake_plan(
  city_block_sqft = make_city_block_sqft(),
  lot_types = make_lot_types(city_block_sqft),
  development_assumptions_zoning = make_development_assumptions_zoning(),
  development_assumptions_lot = make_development_assumptions_lot(lot_types, development_assumptions_zoning)
)

suitability_criteria_plan <- drake_plan(
  criteria_tax_exempt = make_criteria_tax_exempt(),
  criteria_max_water_overlap_pct = make_criteria_max_water_overlap_pct(),
  criteria_within_uga = make_criteria_within_uga(),
  criteria_developable_zoning = make_criteria_developable_zoning(development_assumptions_zoning),
  criteria_undevelopable_present_use = make_criteria_undevelopable_present_use(),
  suitability_criteria = make_suitability_criteria(criteria_tax_exempt, criteria_max_water_overlap_pct, criteria_within_uga, criteria_developable_zoning, criteria_undevelopable_present_use)
)

suitability_plan <- drake_plan(
  suitability_tax_exempt = make_suitability_tax_exempt(parcel_ready),
  suitability_water_overlap = make_suitability_water_overlap(parcel_ready, waterbodies),
  suitability_within_uga = make_suitability_within_uga(parcel_ready, uga),
  suitability_developable_zoning = make_suitability_developable_zoning(parcel_ready, zoning),
  suitability_present_use = make_suitability_present_use(parcel_ready),
  suitability = make_suitability(parcel_ready, suitability_criteria, suitability_tax_exempt, suitability_water_overlap, suitability_within_uga, suitability_developable_zoning, suitability_present_use)
)

building_plan <- drake_plan(
  building_residential = make_building_residential(),
  building_apartment = make_building_apartment(),
  building_condo = make_building_condo(),
  building_commercial = make_building_commercial(),
  building = make_building(building_residential, building_apartment, building_condo, building_commercial)
)

utilization_criteria_plan <-  drake_plan(
  criteria_lot_size = make_criteria_lot_size(city_block_sqft, lot_types)
)

utilization_plan <- drake_plan(
  utilization_present = make_utilization_present(parcel_ready, building),
  utilization_lot_size = make_utilization_lot_size(parcel_ready, criteria_lot_size),
  utilization_potential = make_utilization_potential(suitability, development_assumptions_lot, utilization_lot_size),
  utilization = make_utilization(suitability, utilization_present, utilization_potential)
)

suit_util_plan <- bind_rows(
  parcel_plan,
  miscellaneous_plan,
  development_assumptions_plan,
  suitability_criteria_plan,
  suitability_plan,
  building_plan,
  utilization_criteria_plan,
  utilization_plan
)

# MAKE PLANS: FILTERS AND HELPERS ----

filter_plan <- drake_plan(
  filters_census_tract = make_filters_census_tract(parcel_ready, census_tracts),
  filters_public_owner = make_filters_public_owner(parcel_ready),
  filters_surplus_status = make_filters_surplus_status(parcel_ready),
  filters_proximity_marijuana = make_filters_proximity_marijuana(parcel_ready),
  filters_proximity_preschool = make_filters_proximity_preschool(parcel_ready),
  filters_proximity_open_space = make_filters_proximity_open_space(parcel_ready),
  filters_potential_units = make_filters_potential_units(parcel_ready),
  filters_leg_district = make_filters_leg_district(parcel_ready),
  filters_school_district = make_filters_school_district(parcel_ready),
  filters_historic = make_filters_historic(parcel_ready),
  filters_afford_expir_date = make_filters_afford_expir_date(parcel_ready),
  filters_eligibility_nmtc = make_filters_eligibility_nmtc(parcel_ready),
  filters_eligibility_dda = make_filters_eligibility_dda(parcel_ready),
  filters_eligibility_qct = make_filters_eligibility_qct(parcel_ready),
  filters = make_filters(parcel_ready, 
                         filters_census_tract, 
                         filters_public_owner, 
                         filters_surplus_status, 
                         filters_proximity_marijuana, 
                         filters_proximity_preschool, 
                         filters_proximity_open_space, 
                         filters_school_district, 
                         filters_historic, 
                         filters_afford_expir_date,
                         filters_eligibility_nmtc,
                         filters_eligibility_dda,
                         filters_eligibility_qct)
)

helper_plan <- drake_plan(
  helpers_opp360_xwalk = make_helpers_opp360_xwalk(),
  helpers_url_parcel_viewer = make_helpers_url_parcel_viewer(parcel_ready),
  helpers_url_opp360 = make_helpers_url_opp360(filters_census_tract, helpers_opp360_xwalk),
  helpers = make_helpers(helpers_url_parcel_viewer,helpers_url_opp360)
  
)

filter_helper_plan <- bind_rows(
  suit_util_plan,
  filter_plan,
  helper_plan
)

# MAKE PLANS: INVENTORY_PLAN ----

inventory_plan <- drake_plan(
  inventory = make_inventory(filters, helpers, suitability,  utilization),
  inventory_suitable = make_inventory_suitable(inventory), 
  inventory_suitable_poly = make_inventory_suitable_poly(inventory_suitable),
  inventory_suitable_point = make_inventory_suitable_point(inventory_suitable)
  
) %>% 
  bind_rows(filter_helper_plan)

# MAKE PLANS: DOCUMENTATION ----


data_dictionary_plan <- drake_plan(
  dd_field_name_dev = make_dd_field_name_dev(inventory), 
  dd_field_name_user = make_dd_field_name_user(),
  dd_field_tags = make_dd_field_tags(),
  dd_field_format = make_dd_field_format(inventory),
  dd_field_description = make_dd_field_description(dd_field_name_dev),
  dd_data_source = make_dd_data_source(),
  dd_dictionary_version = make_dd_dictionary_version(dd_field_name_dev, "v0.2"),
  dd = make_dd(dd_field_name_dev, dd_field_name_user, dd_field_format, dd_field_description, dd_data_source, dd_field_tags, dd_dictionary_version), 
  strings_in_dots = "literals"
) %>% 
  mutate(trigger = if_else(target %in% "dd_dictionary_version",
                           "always",
                           drake::default_trigger()))

documentation_plan <- bind_rows(
  inventory_plan,
  data_dictionary_plan
)

# MAKE PLANS: EXPORT ----

export_plan <- drake_plan(
  '1-data/4-ready/data_dictionary.csv' = write_csv(dd, here("1-data/4-ready/data_dictionary.csv")),
  '1-data/4-ready/inventory_table.csv' = write_inventory_csv(inventory, here("1-data/4-ready/inventory_table.csv")), 
  '1-data/4-ready/inventory_table.rda' = write_inventory_rda(inventory, here("1-data/4-ready/inventory_table.rda")), 
  '1-data/4-ready/inventory_table.xlsx' = write_inventory_xlsx(inventory, here("1-data/4-ready/inventory_table.xlsx")), 
  '1-data/4-ready/inventory_suitable_table.csv' = write_inventory_csv(inventory_suitable, here("1-data/4-ready/inventory_suitable_table.csv")), 
  '1-data/4-ready/inventory_suitable_table.rda' = write_inventory_rda(inventory_suitable, here("1-data/4-ready/inventory_suitable_table.rda")), 
  '1-data/4-ready/inventory_suitable_table.xlsx' = write_inventory_xlsx(inventory_suitable, here("1-data/4-ready/inventory_suitable_table.xlsx")), 
  '1-data/4-ready/inventory_suitable_poly.shp' = write_inventory_shp(inventory_suitable_poly, here("1-data/4-ready/inventory_suitable_poly.shp")),
  '1-data/4-ready/inventory_suitable_point.shp' = write_inventory_shp(inventory_suitable_point, here("1-data/4-ready/inventory_suitable_point.shp")), 
  strings_in_dots = "literals",
  file_targets = TRUE
) %>% purrr::modify_at("target", drake_here) %>% 
  bind_rows(documentation_plan)

# MAKE PLANS: ZIP ----

zip_plan <- drake_plan( 
  '1-data/4-ready/site-inventory-20180218.zip' = zip_pithy(here("1-data/4-ready/site-inventory-20180218.zip"), extract_target_paths(export_plan)),
  strings_in_dots = "literals", 
  file_targets = TRUE
) %>% purrr::modify_at("target", drake_here) %>% 
  bind_rows(export_plan)

# project_plan <- bind_rows(
#   lookup_plan,
#   parcel_plan,
#   miscellaneous_plan,
#   development_assumptions_plan,
#   suitability_criteria_plan,
#   suitability_plan,
#   building_plan,
#   utilization_criteria_plan,
#   utilization_plan,
#   filter_plan,
#   helper_plan,
#   inventory_plan,
#   data_dictionary_plan,
#   export_plan
#   ) %>% 
#   mutate(trigger = if_else(is.na(trigger),
#                            drake::default_trigger(),
#                            trigger))
# 
# publish_plan <- bind_rows( 
#   project_plan,  
#   zip_plan
# ) %>% 
#   mutate(trigger = if_else(is.na(trigger),
#                            drake::default_trigger(),
#                            trigger))
