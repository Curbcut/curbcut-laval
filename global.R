#### CC GLOBALS ################################################################

# Packages ----------------------------------------------------------------

suppressPackageStartupMessages({
  library(curbcut)
  library(cc.landing)
  library(cc.map)
})


# Data --------------------------------------------------------------------

curbcut::load_data(site_name = "Curbcut Laval",
                   site_url = "https://laval.curbcut.ca",
                   stories_page = "Laval stories",
                   inst_prefix = "lvl",
                   mapbox_username = "curbcut",
                   default_random_address = "1950 Rue Claude-Gagné, Laval, Québec",
                   map_zoom = 11.1,
                   map_loc = c(lat = -73.71, lon = 45.61))


# Declare temporary folder ------------------------------------------------

temp_folder <- tempdir()
addResourcePath("temp_folder_shortcut", temp_folder)


# Create the UI and server functions for basic modules --------------------

curbcut::create_ui_server_mods(modules = modules)


# Set up fonts ------------------------------------------------------------

systemfonts::register_font(
  name = "acidgrotesk-book",
  plain = list("www/fonts/acidgrotesk-book.woff", 0)
)


# Source the R folder -----------------------------------------------------

# Curbcut works with global environment. Must source to the current global env
lapply(list.files("R", full.names = TRUE), source, verbose = FALSE)
