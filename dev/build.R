#### BUILD ALL CURBCUT DATA ####################################################


# Load libraries ----------------------------------------------------------
tictoc::tic()
library(cc.buildr)
library(sf)
x <- lapply(list.files("dev/data_import", full.names = TRUE), source, verbose = FALSE)

# Base of the study region and dictionaries -------------------------------

inst_prefix <- "lvl"
db_create_schema_prod(inst_prefix)

# Possible sequences for autozooms. Every module must have one or multiple of these
# possible scale sequences.
scales_sequences <- list(c("secteur", "CT", "DA", "building"),
                         c("secteur", "CT"),
                         c("exmuni", "CT", "DA", "building"),
                         c("exmuni", "CT"),
                         c("cmhczone", "CT", "DA", "building"),
                         c("cmhczone", "CT"),
                         c("cmhczone"),
                         c("CLSC", "CT", "DA", "building"),
                         c("CLSC", "CT"),
                         c("grd600", "grd300", "grd120", "grd60", "grd30"))


# List all the regions geometries to create the master polygon
cancensus_csd_code <- 2465005
all_regions <- list(city = list(CSD = 2465005))

base_polygons <- create_master_polygon(all_regions = all_regions)
crs <- base_polygons$crs

# Create the region dictionary
regions_dictionary <-
  regions_dictionary(
    all_regions = all_regions,
    region = c("city"),
    name = c(city = "City of Laval"),
    to_compare = c(city = "in the City of Laval"),
    to_compare_determ = c(city = "the City of Laval"),
    to_compare_short = c(city = "in the City"),
    pickable = c(city = TRUE))


# Build census scales -----------------------------------------------------

census_scales <-
  build_census_scales(master_polygon = base_polygons$master_polygon,
                      regions = base_polygons$province_cancensus_code,
                      crs = crs, levels = c("CT", "DA", "DB"),
                      DA_carto = base_polygons$DA_carto)

# Create the census scale dictionary
scales_dictionary <- census_scales_dictionary(census_scales)

# # Track DBs
# DB_table <- cc.data::DB_get(crs = crs, DA_carto = base_polygons$DA_carto)
# qs::qsave(DB_table, "dev/data/built/DB.qs")
DB_table <- qs::qread("dev/data/built/DB.qs")

# Add a CLSC scale ------------------------------------------------------

# CLSC <- cc.data::arcgis_rest_services_ret(paste0("https://public.arcgis.msss.rtss.qc.ca/arcgis/rest/s",
#                           "ervices/LimitesTerritoriales/TerritoiresSociosanita",
#                           "ires/MapServer/0/query"))
# qs::qsave(CLSC, "dev/data/shp/CLSC.qs")
CLSC <- qs::qread("dev/data/shp/CLSC.qs")

CLSC <- CLSC[CLSC$OBJECTID %in% c(130:133), ]

CLSC$name <- CLSC$CLSC_nom
CLSC <- CLSC["name"]

CLSC <- additional_scale(additional_table = CLSC,
                         DB_table = census_scales$DB,
                         ID_prefix = "CLSC",
                         name_2 = "CLSC",
                         crs = crs,
                         DA_carto = base_polygons$DA_carto)

# Switch the CSD scale for borough/city
scales_dictionary <- append_scale_to_dictionary(
  scales_dictionary,
  scale = "CLSC",
  sing = "local community service centre",
  sing_with_article = "the local community service centre",
  plur = "local community services centres",
  slider_title = "CLSC",
  place_heading = "CLSC {name}",
  place_name = "{name}",
  subtext = paste0("Territories for which the local community service centre ",
                   "(CLSC) has the mission to provide routine, front-line health ",
                   "and social services to the population"))

# Add a Laval Sector scale ------------------------------------------------

# Switch the City of Laval for the Sector
lavalsector <- sf::st_read(paste0("dev/data/shp/",
                                  "limites_des_secteurs_d_amenagement.shp")) |>
  sf::st_transform(4326)
lavalsector <- lavalsector[c("SECTEUR")]
names(lavalsector)[1] <- "name"

lavalsector <- additional_scale(additional_table = lavalsector,
                                DB_table = census_scales$DB,
                                ID_prefix = "secteur",
                                name_2 = "Sector",
                                crs = crs,
                                DA_carto = base_polygons$DA_carto)

scales_dictionary <- append_scale_to_dictionary(
  scales_dictionary,
  scale = "secteur",
  sing = "Laval Sector",
  sing_with_article = "the Laval Sector",
  plur = "Laval Sectors",
  slider_title = "Sector",
  place_heading = "Laval Sector of {name}",
  place_name = "{name}",
  subtext = paste0("Delimitation of Ville de Laval's development sectors. ",
                   "Development sectors serve the various territorial ",
                   "management needs of Ville de Laval."))

# Add a Laval Sector scale ------------------------------------------------

# Switch the City of Laval for the Sector
anciennemuni <- sf::st_read(paste0("dev/data/shp/",
                                  "limite_ancienne_municipalite.shp")) |>
  sf::st_transform(4326)
names(anciennemuni)[1] <- "name"

anciennemuni <- additional_scale(additional_table = anciennemuni,
                                DB_table = census_scales$DB,
                                ID_prefix = "exmuni",
                                name_2 = "",
                                crs = crs,
                                DA_carto = base_polygons$DA_carto)

scales_dictionary <- append_scale_to_dictionary(
  scales_dictionary,
  scale = "exmuni",
  sing = "former municipality",
  sing_with_article = "the former municipality",
  plur = "former municipalities",
  slider_title = "Former municipality",
  place_heading = "Former municipality of {name}",
  place_name = "{name}",
  subtext = paste0("Delineation of the former municipalities before the 1965 ",
                   "merger to form what is now Ville de Laval."))


# Building scale ----------------------------------------------------------

# # Build building scale
# # From MySQL
# building <- cc.data::db_read_data("buildings", column_to_select = "DA_ID",
#                                   IDs = census_scales$DA$ID)
# qs::qsave(building, file = "dev/data/built/building.qs")
building <- qs::qread("dev/data/built/building.qs")

# Add building scale to the dictionary
scales_dictionary <-
  append_scale_to_dictionary(
    scales_dictionary,
    scale = "building",
    sing = "building",
    sing_with_article = "the building",
    plur = "buildings",
    slider_title = "Building",
    place_heading = "{name}",
    place_name = "{name}",
    subtext = NA)

### Build CMHC scale
cmhczone <- get_cmhc_zones(list(CSD = cancensus_csd_code))
cmhczone <- additional_scale(additional_table = cmhczone,
                             DB_table = census_scales$DB,
                             ID_prefix = "cmhc",
                             name_2 = "CMHC zone",
                             crs = crs,
                             DA_carto = base_polygons$DA_carto)
scales_dictionary <-
  append_scale_to_dictionary(
    scales_dictionary,
    scale = "cmhczone",
    sing = "CMHC zone",
    sing_with_article = "the CMHC zone",
    plur = "CMHC zones",
    slider_title = "CMHC zone",
    place_heading = "{name}",
    place_name = "{name}",
    subtext = paste0("Designated areas in Canada for housing market analysis ",
                     "by Canada Mortgage and Housing Corporation (CMHC)"))


# NDVI's grid -------------------------------------------------------------

ndvigrids <- ndvi_grids(census_scales = census_scales,
                        base_polygons = base_polygons,
                        overwrite_ndvi_tiles = FALSE,
                        overwrite_final_grids = FALSE,
                        crs = crs)

scales_dictionary <- scales_dictionary_ndvi(scales_dictionary)


# Consolidate scales ------------------------------------------------------

all_scales <- c(census_scales,
                list(secteur = lavalsector),
                list(exmuni = anciennemuni),
                list(building = building),
                list(cmhczone = cmhczone),
                list(CLSC = CLSC),
                ndvigrids)

# Character vector of the tables that will be saved in the database instead of
# in the container. These tables WON'T be available for dynamic filtering using
# region in the `curbcut::data_get()` function.
large_tables_db <- c("building", "grd30", "grd60", "grd120", "grd300")

save.image("dev/data/built/pre_consolidate.RData")
load("dev/data/built/pre_consolidate.RData")

future::plan(future::sequential())

scales_consolidated <- consolidate_scales(scales_sequences = scales_sequences,
                                          all_scales = all_scales,
                                          regions = base_polygons$regions,
                                          crs = crs,
                                          large_tables_db = large_tables_db)

regions_dictionary <- regions_dictionary_add_scales(
  regions_dictionary = regions_dictionary,
  region_dict_scales = scales_consolidated$for_region_dict_scales)

scales_dictionary <- add_regions_to_scales_dictionary(
  scales_dictionary = scales_dictionary, regions = base_polygons$regions,
  scales_consolidated = scales_consolidated,
  DA_carto = base_polygons$DA_carto,
  regions_dictionary = regions_dictionary,
  crs = crs)

save.image("dev/data/built/post_consolidate.RData")
load("dev/data/built/post_consolidate.RData")


# Verify conformity -------------------------------------------------------

verify_dictionaries(scales = scales_consolidated$scales,
                    regions_dictionary = regions_dictionary,
                    scales_dictionary = scales_dictionary)


# Save pieces that will be untouched from this point ----------------------

save_geometry_export(data_folder = "data/",
                     all_scales = scales_consolidated$scales,
                     skip_scales = large_tables_db)
save_short_tables_qs(data_folder = "data/",
                     all_scales = scales_consolidated$scales,
                     skip_scales = large_tables_db)
save_bslike_postgresql(all_scales = scales_consolidated$scales,
                       tables_to_save_db = large_tables_db,
                       inst_prefix = inst_prefix,
                       overwrite = FALSE)

qs::qsave(scales_dictionary, file = "data/scales_dictionary.qs")
qs::qsave(regions_dictionary, file = "data/regions_dictionary.qs")


# Create the modules and variables tables ---------------------------------

scales_variables_modules <-
  append_empty_variables_table(scales_consolidated = scales_consolidated$scales)
scales_variables_modules <-
  append_empty_modules_table(scales = scales_variables_modules)
scales_variables_modules$data <- lapply(scales_consolidated$scales, \(x) list())

qs::qsave(scales_consolidated, "dev/data/built/scales_consolidated.qs")
qs::qsavem(census_scales, scales_variables_modules, crs, base_polygons,
           cancensus_csd_code, scales_consolidated, scales_sequences, DB_table,
           scales_dictionary, regions_dictionary, inst_prefix,
           file = "dev/data/built/empty_scales_variables_modules.qsm")
rm(list = ls())
library(cc.buildr)
library(sf)
qs::qload("dev/data/built/empty_scales_variables_modules.qsm")

# Build the datasets ------------------------------------------------------

future::plan(future::multisession, workers = 4)

# No data is added to the buildings yet, onload it
scales_variables_modules$scales <-
  unload_scales(scales_variables_modules$scales, unload = c("building"))

scales_variables_modules <-
  ba_census_data(scales_variables_modules = scales_variables_modules,
                 region_DA_IDs = census_scales$DA$ID,
                 DB_table = DB_table,
                 crs = crs,
                 scales_sequences = scales_sequences,
                 overwrite = FALSE,
                 inst_prefix = inst_prefix)
census_variables <- get_census_vectors_details()

# Build NDVI first to unload heavy grids
scales_variables_modules <-
  ba_ndvi(scales_variables_modules = scales_variables_modules,
          scales_sequences = scales_sequences,
          crs = crs,
          overwrite = FALSE,
          inst_prefix = inst_prefix)

scales_variables_modules$scales <-
  unload_scales(scales_variables_modules$scales,
                unload = c("grd30", "grd60", "grd120", "grd300"))

future::plan(future::multisession(), workers = 6)
scales_variables_modules <-
  ru_vac_rate(scales_variables_modules = scales_variables_modules,
              scales_sequences = scales_sequences,
              crs = crs,
              geo_uid = 24462,
              inst_prefix = inst_prefix,
              overwrite = FALSE)
scales_variables_modules <-
  ru_alp(scales_variables_modules = scales_variables_modules,
         scales_sequences = scales_sequences,
         crs = crs,
         region_DA_IDs = census_scales$DA$ID,
         overwrite = FALSE,
         inst_prefix = inst_prefix)
scales_variables_modules <-
  ru_canbics(scales_variables_modules = scales_variables_modules,
             scales_sequences = scales_sequences,
             crs = crs,
             region_DA_IDs = census_scales$DA$ID,
             overwrite = FALSE,
             inst_prefix = inst_prefix)
scales_variables_modules <-
  ru_lst(scales_variables_modules = scales_variables_modules,
         region_DA_IDs = census_scales$DA$ID,
         scales_sequences = scales_sequences,
         crs = crs,
         overwrite = FALSE,
         inst_prefix = inst_prefix)


# # Add access to amenities module
# traveltimes <-
#   accessibility_get_travel_times(region_DA_IDs = census_scales$DA$ID)
# qs::qsave(traveltimes, "dev/data/built/traveltimes.qs")
traveltimes <- qs::qread("dev/data/built/traveltimes.qs")

invisible(lapply(list.files("dev/data_import", full.names = TRUE), source))
future::plan(future::multisession(), workers = 4)

# Additional access variables
scales_variables_modules <-
  build_and_append_access(scales_variables_modules = scales_variables_modules,
                          DA_table = census_scales$DA,
                          traveltimes = traveltimes,
                          scales_sequences = scales_sequences,
                          crs = crs,
                          overwrite = FALSE,
                          inst_prefix = inst_prefix)

scales_variables_modules <-
  build_and_append_laval_access(scales_variables_modules = scales_variables_modules,
                                DA_table = census_scales$DA,
                                traveltimes = traveltimes,
                                scales_sequences = scales_sequences,
                                crs = crs,
                                overwrite = FALSE,
                                inst_prefix = inst_prefix)

scales_variables_modules <-
  ba_accessibility_points(scales_variables_modules = scales_variables_modules,
                          region_DA_or_DB_IDs = census_scales$DA$ID,
                          traveltimes = traveltimes,
                          scales_sequences = scales_sequences,
                          crs = crs,
                          overwrite = FALSE,
                          inst_prefix = inst_prefix)

future::plan(future::multisession(), workers = 4)

scales_variables_modules <-
  build_and_append_tenure(
    scales_variables_modules = scales_variables_modules,
    scales_sequences = scales_sequences,
    crs = crs,
    overwrite = FALSE,
    inst_prefix = inst_prefix)

scales_variables_modules <-
  build_and_append_afford_pop(
    scales_variables_modules = scales_variables_modules,
    scales_sequences = scales_sequences,
    crs = crs,
    overwrite = FALSE,
    inst_prefix = inst_prefix)

scales_variables_modules <-
  build_and_append_afford_hou(
    scales_variables_modules = scales_variables_modules,
    scales_sequences = scales_sequences,
    crs = crs,
    overwrite = FALSE,
    inst_prefix = inst_prefix)

# Post process
scales_variables_modules$scales <-
  cc.buildr::post_processing(scales = scales_variables_modules$scales)

qs::qsavem(census_scales, scales_variables_modules, crs, census_variables,
           base_polygons, scales_sequences, regions_dictionary, inst_prefix,
           scales_dictionary, file = "dev/data/built/scales_variables_modules.qsm")
qs::qload("dev/data/built/scales_variables_modules.qsm")

# Postal codes ------------------------------------------------------------

save_postal_codes(census_scales$DA$ID, overwrite = FALSE)


# Map zoom levels ---------------------------------------------------------

map_zoom_levels <- map_zoom_levels_create_all(
  scales_sequences = scales_sequences,
  zoom_levels = list(first = 0, CT = 11, DA = 13, building = 16,
                     grd300 = 10, grd120 = 11, grd60 = 12, grd30 = 13))

map_zoom_levels_save(data_folder = "data/", map_zoom_levels = map_zoom_levels)


# Tilesets ----------------------------------------------------------------

tileset_upload_all(map_zoom_levels = map_zoom_levels,
                   inst_prefix = inst_prefix,
                   username = "curbcut",
                   access_token = .cc_mb_token)

# tileset_upload_ndvi(map_zoom_levels = map_zoom_levels,
#                     regions = base_polygons$regions,
#                     inst_prefix = inst_prefix,
#                     username = "curbcut",
#                     access_token = .cc_mb_token,
#                     crs = crs)


# Add possible regions to modules -----------------------------------------

scales_variables_modules <- pages_regions(svm = scales_variables_modules,
                                          regions_dictionary = regions_dictionary,
                                          inst_prefix = inst_prefix)


# Place explorer page ----------------------------------------------------

avail_scale_combinations <- scales_sequences[!grepl("grd", scales_sequences)]
avail_scale_combinations <- sapply(avail_scale_combinations, paste0, collapse = "_")


# Add the place explorer in the modules dataframe
scales_variables_modules$modules <-
  add_module(modules = scales_variables_modules$modules,
             id = "place_explorer",
             theme = "Explorer",
             nav_title = "Place explorer",
             title_text_title = "Place explorer",
             title_text_main = paste0(
               "Select a location by entering a postal code or clicking on the map to ",
               "see how it compares to the rest of the region across a variety of sust",
               "ainability indicators."
             ),
             title_text_extra = paste0(
               "<p>The data in the Place Explorer is taken from other Curbcut pages with ",
               "the exception of <a href = 'https://www.canuedata.ca/tmp/CANUE_METADATA",
               "_NO2LUR_A_YY.pdf'>Air pollution</a>."
             ),
             metadata = FALSE,
             dataset_info = "",
             avail_scale_combinations = avail_scale_combinations,
             regions = regions_dictionary$region)


# Produce colours ---------------------------------------------------------

colours_dfs <- cc.buildr::build_colours()

# Add natural inf data colours
colours_dfs$viridis_25 <-
  tibble::tibble(group = as.character(26:50),
                 fill = scales::viridis_pal()(25))
qs::qsave(colours_dfs, "data/colours_dfs.qs")


# Write stories -----------------------------------------------------------

# # # TKTK MAKE SURE YOU HAVE THIS VERSION OF LEAFLET, IF NOT THE MAPS IN THE HTML
# # # DOCUMENTS WON'T BE INTERACTIVES:
# # devtools::install_github("dmurdoch/leaflet@crosstalk4")
# stories <- build_stories()
# qs::qsave(stories, file = "data/stories.qs")
# stories_create_tileset(stories = stories,
#                        inst_prefix = inst_prefix,
#                        username = "curbcut",
#                        access_token = .cc_mb_token)
# cc.buildr::resize_image(folder = "www/stories/photos/", max_size_in_MB = 1)


# # Add Montréal stories
# scales_variables_modules$modules <-
#   scales_variables_modules$modules |>
#   add_module(
#     id = "stories",
#     theme = "Urban life",
#     nav_title = "Montreal stories",
#     title_text_title = "Montreal stories",
#     title_text_main = paste0(
#       "Explore narrative case studies about specific urban sustainability and ",
#       "planning issues in the Montreal region."),
#     title_text_extra = paste0(
#       "<p>These narrative case studies are written by the Curbcut team and its contributors."),
#     metadata = FALSE,
#     dataset_info = ""
#   )


# Translation -------------------------------------------------------------

variables <- scales_variables_modules$variables
modules <- scales_variables_modules$modules
source("dev/translation/build_translation.R", encoding = "utf-8")


# Save variables ----------------------------------------------------------

qs::qsave(scales_variables_modules$variables, file = "data/variables.qs")


# Save QS data ------------------------------------------------------------

save_all_scales_qs(scales_dictionary = scales_dictionary)


# Save other global data --------------------------------------------------

qs::qsave(census_variables, file = "data/census_variables.qs")

# For compare, only keep the large brackets of age
scales_variables_modules$modules$var_right <- lapply(
  scales_variables_modules$modules$var_right, \(x) {
    if (is.null(x)) return(NULL)
    not_age <- x[!grepl("^age_", x)]
    age <- x[grepl("^age_", x)]

    age_keep <- age[age %in% c("age_0_14", "age_15_64", "age_65_plus")]

    c(not_age, age_keep)
  })

qs::qsave(scales_variables_modules$modules, file = "data/modules.qs")
tictoc::toc()

# Create DYKs -------------------------------------------------------------

library(tidyverse)
translation_df <- qs::qread("data/translation_df.qs")
vars_dyk <- dyk_prep(svm = scales_variables_modules, scales_dictionary = scales_dictionary)
dyk <- dyk_uni(vars_dyk,
               svm = scales_variables_modules,
               translation_df = translation_df,
               langs = c("en", "fr"),
               scales_dictionary = scales_dictionary)
# dyk <- rbind(dyk, dyk_delta(vars_dyk, scales_variables_modules))
# dyk <- rbind(dyk, dyk_bivar(vars_dyk, scales_variables_modules))
qs::qsave(dyk, "data/dyk.qs")


# Home page ---------------------------------------------------------------

invisible(lapply(list.files("dev/data_import", full.names = TRUE), source))
cc.buildr::convert_svg_to_ico("#A3B0D1")
home_page(modules = modules, translation_df = translation_df,
          c_city_svg = "www/landing/c-laval.svg")


# Place explorer content creation -----------------------------------------

# Should be done once the data is saved
future::plan(future::multisession(), workers = 8)

# pe_main_card_data <- placeex_main_card_data(scales_dictionary = scales_dictionary,
#                                             DA_table = census_scales$DA,
#                                             region_DA_IDs = census_scales$DA$ID,
#                                             crs = crs,
#                                             regions_dictionary = regions_dictionary,
#                                             inst_prefix = inst_prefix,
#                                             first_scales = unique(sapply(scales_sequences, `[[`, 1)))
# qs::qsave(pe_main_card_data, file = "data/pe_main_card_data.qs")
pe_main_card_data <- qs::qread("data/pe_main_card_data.qs")


library(curbcut)
translation_df <- qs::qread("data/translation_df.qs")
placeex_main_card_rmd(pe_main_card_data = pe_main_card_data,
                      regions_dictionary = regions_dictionary,
                      scales_dictionary = scales_dictionary,
                      lang = c("en", "fr"),
                      tileset_prefix = inst_prefix,
                      mapbox_username = "curbcut",
                      rev_geocode_from_localhost = TRUE,
                      overwrite = FALSE,
                      scales_sequences = scales_sequences)

# Save the place explorer files, which serves as a 'does it exist' for `curbcut`
pe_docs <- list.files("www/place_explorer/", full.names = TRUE)
qs::qsave(pe_docs, "data/pe_docs.qs")


# Deploy app --------------------------------------------------------------

# renv::activate()
# heroku_deploy("cc-montreal-centraide") # Centraide
# heroku_deploy("cc-montreal-dev") # Development
# heroku_deploy("cc-montreal") # Production

