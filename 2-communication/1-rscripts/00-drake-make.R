# SETUP ---- 

source(here::here("2-communication/1-rscripts/01-drake-setup.R"))

# FUNCTIONS ----

source(here("2-communication/1-rscripts/02-drake-functions.R"))

# COMMANDS ----

source(here("2-communication/1-rscripts/03-drake-commands.R"))

# PLANS ----

source(here("2-communication/1-rscripts/04-drake-plans.R"))

# MAKE PLANS ---- 

make(export_plan)

