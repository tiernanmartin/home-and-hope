# SETUP ---- 

source(here::here("2-communication/1-rscripts/01-drake-setup.R"))

# FUNCTIONS ----

source(here("2-communication/1-rscripts/02-drake-functions.R"))

# COMMANDS ----

source(here("2-communication/1-rscripts/03-drake-commands.R"))

# PLANS ----

source(here("2-communication/1-rscripts/04-drake-plans.R"))

# MAKE PLANS ---- 

make(triggers_plan)
make(lookup_plan)
make(parcel_plan)
make(external_data_plan)
make(development_assumptions_plan)
make(suitability_criteria_plan)
make(suitability_plan)
make(building_plan)
make(utilization_criteria_plan)
make(utilization_plan)
make(suit_util_plan)
make(municipality_plan)
# make(official_names_plan)
make(owner_name_full_plan)
make(owner_plan)
make(filter_plan)
make(helper_plan)
make(filter_helper_plan)
make(inventory_plan)
make(documentation_plan)
make(export_plan)

