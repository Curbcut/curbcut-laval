## BUILD AND APPEND ACCESS DATA ################################################

build_and_append_laval_access <- function(scales_variables_modules,
                                          DA_table,
                                          traveltimes,
                                          scales_sequences,
                                          crs,
                                          overwrite = FALSE,
                                          inst_prefix) {

  # Read and prepare data ---------------------------------------------------

  ## Lieux et édifices municipaux
  laval_lieux <- read.csv("dev/data/laval/lieux.csv") |> tibble::as_tibble()
  laval_lieux <- laval_lieux[!is.na(laval_lieux$longitude) | !is.na(laval_lieux$latitude), ]
  laval_lieux <- sf::st_as_sf(laval_lieux, coords = c("longitude", "latitude"), crs = 4326)

  laval_lieux <- sapply(unique(laval_lieux$type.commun), \(x) {
    laval_lieux[laval_lieux$type.commun == x, "geometry"]
  }, simplify = FALSE, USE.NAMES = TRUE)

  names(laval_lieux) <- gsub(" ", "_", names(laval_lieux))
  names(laval_lieux) <- iconv(names(laval_lieux), to = "ASCII//TRANSLIT")
  names(laval_lieux) <- tolower(names(laval_lieux))
  names(laval_lieux) <- gsub("'", "", names(laval_lieux))

  # # Add point data to DA ----------------------------------------------------
  #
  # point_DA <- accessibility_point_per_DA(point_data = laval_lieux,
  #                                        DA_table = census_scales$DA,
  #                                        crs = crs)
  #
  #
  # # Add access to point data by time intervals ------------------------------
  #
  # data <- accessibility_add_intervals(point_per_DA = point_DA,
  #                                     traveltimes = traveltimes)
  # qs::qsave(data, "dev/data/built/laval_access_data.qs")
  data <- qs::qread("dev/data/built/laval_access_data.qs")

  # 2024 data
  names(data)[2:ncol(data)] <- paste0(names(data)[2:ncol(data)], "_2024")

  # Get list of data variables ----------------------------------------------

  average_vars <- names(data)[!grepl("ID$", names(data))]
  additive_vars <- c()
  vars <- c(average_vars, additive_vars)

  # Interpolate data to all possible scales ---------------------------------

  # In the case where the dataset is already aggregated to a census scale,
  # use the `interpolate_from_census_geo` function.
  names(data)[1] <- "DA_ID"
  data_interpolated <-
    interpolate_from_census_geo(
      data = data,
      base_scale = "DA",
      all_scales = scales_variables_modules$scales,
      weight_by = "population",
      average_vars = average_vars,
      additive_vars = additive_vars,
      crs = crs,
      overwrite = overwrite,
      time_regex = "_\\d{4}$",
      inst_prefix = inst_prefix
    )


  # Data tibble -------------------------------------------------------------

  time_regex <- "_\\d{4}$"
  unique_var <- gsub(time_regex, "", average_vars)
  unique_var <- gsub("_\\d{1,2}$", "", unique_var)
  unique_var <- unique(unique_var)

  # Construct the breaks_var list (take the breaks from a 20 minutes traject)
  breaks_var <- lapply(unique_var, paste0, "_20_2024")
  names(breaks_var) <- unique_var

  data_construct(scales_data = data_interpolated$scales,
                 unique_var = unique_var,
                 time_regex = time_regex,
                 schema = list(time = gsub("^_", "", time_regex),
                               transportationtime = "_\\d{1,2}"),
                 breaks_var = breaks_var,
                 inst_prefix = inst_prefix)


  # Types and parent vectors ------------------------------------------------

  parent_strings <- rep(list("population"), length(unique_var))
  names(parent_strings) <- unique_var

  types <- rep(list("avg"), length(unique_var))
  names(types) <- unique_var


  # Variable measurements ----------------------------------------------------

  var_measurement <- data.frame(
    scale = data_interpolated$avail_scale,
    measurement = rep("scalar", length(data_interpolated$avail_scale))
  )

  var_measurement$measurement[var_measurement$scale == "DA"] <- "ordinal"

  # Variables table ---------------------------------------------------------

  dict <- tibble::tibble(var = "parc", title = "Parc", short = "Parc")
  dict <- tibble::add_row(dict, var = "patinoire_exterieure", title = "Patinoires extérieures", short = "Patinoires")
  dict <- tibble::add_row(dict, var = "centre_communautaire", title = "Centres communautaires", short = "Centres")
  dict <- tibble::add_row(dict, var = "poste_de_police_de_quartier", title = "Postes de police de quartier", short = "Police")
  dict <- tibble::add_row(dict, var = "piscine_exterieure", title = "Piscines extérieures", short = "Piscines")
  dict <- tibble::add_row(dict, var = "piscine_interieure", title = "Piscines extérieures", short = "Piscines")
  dict <- tibble::add_row(dict, var = "edifice_municipal", title = "Édifices municipales", short = "Édifices")
  dict <- tibble::add_row(dict, var = "station_de_metro", title = "Stations de métro", short = "Métro")
  dict <- tibble::add_row(dict, var = "centre_sportif", title = "Centres sportifs", short = "Sports")
  dict <- tibble::add_row(dict, var = "salle_de_spectacle", title = "Salles de spectacle", short = "Spectacles")
  dict <- tibble::add_row(dict, var = "centres_et_galeries_dart", title = "Centres et galeries d'art", short = "Art")
  dict <- tibble::add_row(dict, var = "belvedere", title = "Belvédères", short = "Belvédères")
  dict <- tibble::add_row(dict, var = "bureau_municipal_lavallois", title = "Bureaux municipaux de Laval", short = "Bureaux")
  dict <- tibble::add_row(dict, var = "halte", title = "Haltes", short = "Haltes")
  dict <- tibble::add_row(dict, var = "autres", title = "Autres", short = "Autres")
  dict <- tibble::add_row(dict, var = "bibliotheque", title = "Bibliothèques", short = "Bibliothèques")
  dict <- tibble::add_row(dict, var = "gare", title = "Gares", short = "Gares")
  dict <- tibble::add_row(dict, var = "arena", title = "Arénas", short = "Arénas")
  dict <- tibble::add_row(dict, var = "berge", title = "Berges", short = "Berges")
  dict <- tibble::add_row(dict, var = "caserne_de_pompiers", title = "Casernes de pompiers", short = "Casernes")
  dict <- tibble::add_row(dict, var = "jeux_deau", title = "Jeux d'eau", short = "Eau")
  dict <- tibble::add_row(dict, var = "bois", title = "Bois", short = "Bois")


  new_variables <- lapply(unique_var, \(var) {
    current_dict <- dict[sapply(dict$var, grepl, var), ]

    mode <- (\(x) {
      if (grepl("_car_", var)) {
        return("car")
      }
      if (grepl("_foot_", var)) {
        return("walking")
      }
      if (grepl("_bicycle_", var)) {
        return("bicycle")
      }
      if (grepl("_transit_opwe_", var)) {
        return("public transit on off-peak weekend days")
      }
      if (grepl("_transit_pwe_", var)) {
        return("public transit on peak weekend days")
      }
      if (grepl("_transit_nwd_", var)) {
        return("public transit on weekdays at night")
      }
      if (grepl("_transit_nwe_", var)) {
        return("public transit on weekends at night")
      }
      if (grepl("_transit_opwd_", var)) {
        return("public transit on off-peak weekdays")
      }
      if (grepl("_transit_pwd_", var)) {
        return("public transit on peak weekdays")
      }
    })()

    time <- "__transportationtime__"

    var_title <- stringr::str_to_sentence(paste0(current_dict$title, " accessible by ", mode))
    var_short <- stringr::str_to_sentence(current_dict$short)

    mode_text <- (\(x) {
      if (mode == "car") {
        return("drive")
      }
      if (mode == "walking") {
        return("walk")
      }
      if (mode == "bicycle") {
        return("bike ride")
      }
      if (grepl("public transit", mode)) {
        return(gsub("public transit", "transit journey", mode))
      }
    })()
    explanation <- paste0(
      "the number of ", tolower(current_dict$title),
      " a resident can reach within a ", time, "-minute ", mode_text
    )
    exp_q5 <- paste0(
      "a resident has access to, on average, _X_ ", tolower(current_dict$title), " within a ",
      time, "-minute ", mode_text
    )

    # Cut timing out of the mode
    mode <- stringr::str_extract(mode, "(^car$)|(^walking$)|(^bicycle$)|(^public transit)")

    theme <- "lieux et édifices municipaux"
    group_name <- paste("Access to", theme)
    group_diff <- list(
      "Mode of transport" = stringr::str_to_sentence(mode),
      "Transportation time" = which(1:60 %% 5 == 0)
    )

    if (grepl("_transit_", var)) {
      timing <- (\(x) {
        if (grepl("_transit_opwe_", var)) {
          return("Weekend traffic off-peak")
        }
        if (grepl("_transit_pwe_", var)) {
          return("Weekend traffic peak")
        }
        if (grepl("_transit_nwd_", var)) {
          return("Weekday night")
        }
        if (grepl("_transit_nwe_", var)) {
          return("Weekend night")
        }
        if (grepl("_transit_opwd_", var)) {
          return("Weekday traffic off-peak")
        }
        if (grepl("_transit_pwd_", var)) {
          return("Weekday traffic peak")
        }
      })()
      group_diff <- c(group_diff, list("Timing" = timing))
    }

    # Additional group_diff
    val <- if (grepl("_total$", var)) "All" else stringr::str_to_sentence(current_dict$title)

    group_diff <- c(group_diff, list("Types de lieux/édifices" = val))

    add_variable(
      variables = scales_variables_modules$variables,
      var_code = var,
      type = "avg",
      var_title = var_title,
      var_short = var_short,
      explanation = explanation,
      exp_q5 = exp_q5,
      group_name = group_name,
      group_diff = group_diff,
      parent_vec = "population",
      theme = "Transport",
      private = FALSE,
      pe_include = FALSE,
      dates = "2024",
      avail_scale = data_interpolated$avail_scale,
      source = "Ville de Laval",
      interpolated = data_interpolated$interpolated_ref,
      rankings_chr = c(
        "exceptionally sparse", "unusually sparse",
        "just about average", "unusually dense",
        "exceptionally dense"
      ),
      var_measurement = var_measurement
    ) |>
      (\(x) x[nrow(x), ])()
  })

  variables <- rbind(scales_variables_modules$variables, Reduce(rbind, new_variables))


  # Possible sequences ------------------------------------------------------

  avail_scale_combinations <-
    get_avail_scale_combinations(scales_sequences = scales_sequences,
                                 avail_scales = data_interpolated$avail_scale)


  # Return ------------------------------------------------------------------

  return(list(
    scales = scales_variables_modules$scales,
    variables = variables,
    modules = if (exists("modules")) modules else scales_variables_modules$modules,
    data = data
  ))

}
