vars <- variables$var_code[grepl("^housingdev_", variables$var_code)]

dw_vars <- grep("_dw_", vars, value = TRUE)
im_vars <- grep("_im_", vars, value = TRUE)

dw_dict <- c(Unifamilial = "single",
             `Semi-détaché` = "semi",
             `En rangée` = "row",
             `Appartment` = "apt",
             `Tout` = "total")
im_dict <- c(`Marché inconnu` = "unkown",
             `Marché des propriétaires occupants` = "homeowner",
             `Marché de la location` = "rental",
             `Marché des condominiums` = "condo",
             `Marché des logements coopératifs` = "coop",
             `Tout` = "total")

translation_housingdev <-
  lapply(dw_vars, \(var) {

    # Dwelling type
    type <- stringr::str_extract(var, "dw_.*$")
    type <- gsub("dw_", "", type)
    group_dw <- names(dw_dict)[which(type == dw_dict)]
    group_dw <- stringr::str_to_sentence(group_dw)

    # exp_q5
    exp_q5  <-  tolower(paste("étaient des logements de type", group_dw))
    if (group_dw == "All") exp_q5 <- "étaient réparties dans tous les types de logement"

    # Development type
    group_dev <- (\(x) {
      if (grepl("_s_", var)) {
        return("de mises en chantier")
      }
      if (grepl("_c_", var)) {
        return("d'achèvements")
      }
      if (grepl("_au_", var)) {
        return("d'unités écoulées")
      }
    })(var)

    explanation <- tolower(paste(
      "le nombre", group_dev, "de logements de type", group_dw
    ))
    explanation_nodet <- tolower(paste(
      "nombre", group_dev, "de logements de type", group_dw
    ))
    # if (explanation == "the count of all housing absorbed units")
    #   explanation <- "the count of all absorbed housing units"

    # Create short title
    short <- group_dw

    # Create group_name
    group_name <- (\(x) {
      if (grepl("_s_", var)) {
        return("Mises en chantier")
      }
      if (grepl("_c_", var)) {
        return("Achèvements")
      }
      if (grepl("_au_", var)) {
        return("Unités écoulées")
      }
    })(var)

    # Create title and explanation
    title <- paste(group_name, "de logements de type", tolower(group_dw))
    title <- gsub("de type tout", "de tous types", title)

    # Create group_diff
    group_diff <- list(
      "Dimension" = "Type de logement",
      "Type de logement" = group_dw
    )

    # Construct le table
    tibble(en = variables$var_title[variables$var_code == var],
           fr = title) |>
      add_row(en = variables$var_short[variables$var_code == var],
              fr = short) |>
      add_row(en = variables$explanation[variables$var_code == var],
              fr = explanation) |>
      add_row(en = variables$exp_q5[variables$var_code == var],
              fr = exp_q5) |>
      add_row(en = variables$explanation_nodet[variables$var_code == var],
              fr = explanation_nodet)

  }) |> (\(x) Reduce(rbind, x))()

translation_housingdev_2 <-
  lapply(im_vars, \(var) {

    # ALSO MUST BE ADDED
    parent_vec <- (\(x) {
      # Bedroom types
      if (grepl("_s_", var)) {
        return("housingdev_starts")
      }
      # Year of construction
      if (grepl("_c_", var)) {
        return("housingdev_completions")
      }
      if (grepl("_au_", var)) {
        return("housingdev_absorbedunits")
      }
    })(var)

    # Dwelling type
    type <- stringr::str_extract(var, "im_.*$")
    type <- gsub("im_", "", type)
    group_dw <- names(im_dict)[which(type == im_dict)]
    group_dw <- stringr::str_to_sentence(group_dw)

    # exp_q5
    exp_q5 = tolower(paste("étaient destinés au", group_dw))
    if (group_dw == "All") exp_q5 <- "ont été répartis entre les différents marchés visés"

    # Development type
    group_dev <- (\(x) {
      if (grepl("_s_", var)) {
        return("de mises en chantier")
      }
      if (grepl("_c_", var)) {
        return("d'achèvements")
      }
      if (grepl("_au_", var)) {
        return("d'unités écoulées")
      }
    })(var)

    # Create title and explanation
    explanation <- tolower(paste(
      "le nombre", group_dev, "destinés au", group_dw
    ))
    explanation_nodet <- tolower(paste(
      "nombre", group_dev, "destinés au", group_dw
    ))

    # Create short title
    short <- gsub("(Marché )|(Marché des )|(Marché de la )|(Marché des logements )",
                  "", group_dw) |>
      stringr::str_to_sentence()

    # Create group_name
    group_name <- (\(x) {
      if (grepl("_s_", var)) {
        return("Mises en chantier")
      }
      if (grepl("_c_", var)) {
        return("Achèvements")
      }
      if (grepl("_au_", var)) {
        return("Unités écoulées")
      }
    })(var)

    # Create title and explanation
    title <- paste(group_name, "sur le", tolower(group_dw))
    if (group_dw == "Tout") title <- (\(x) {
      if (grepl("_s_", var)) {
        return("Toutes les mises en chantier")
      }
      if (grepl("_c_", var)) {
        return("Tous les achèvements")
      }
      if (grepl("_au_", var)) {
        return("Toutes les unités écoulées")
      }
    })(var)

    # Create group_diff
    group_diff <- list(
      "Dimension" = "Marché visé",
      "Marché visé" = group_dw
    )

    tibble(en = variables$var_title[variables$var_code == var],
           fr = title) |>
      add_row(en = variables$var_short[variables$var_code == var],
              fr = short) |>
      add_row(en = variables$explanation[variables$var_code == var],
              fr = explanation) |>
      add_row(en = variables$exp_q5[variables$var_code == var],
              fr = exp_q5) |>
      add_row(en = variables$explanation_nodet[variables$var_code == var],
              fr = explanation_nodet)

  }) |> (\(x) Reduce(rbind, x))()


translation_housingdev <- rbind(translation_housingdev, translation_housingdev_2) |>
  tibble::add_row(en = "Housing starts",
                  fr = "Mises en chantier") |>
  tibble::add_row(en = "Starts",
                  fr = "Mises en chantier de logements") |>
  tibble::add_row(en = "the count of housing starts",
                  fr = "le nombre de mises en chantier de logements") |>
  tibble::add_row(en = "count of housing starts",
                  fr = "nombre de mises en chantier de logements") |>
  tibble::add_row(en = "Housing completions",
                  fr = "Achèvements de logements") |>
  tibble::add_row(en = "Completions",
                  fr = "Achèvements") |>
  tibble::add_row(en = "the count of housing completions",
                  fr = "le nombre d'achèvements de logement") |>
  tibble::add_row(en = "count of housing completions",
                  fr = "nombre d'achèvements de logement") |>
  tibble::add_row(en = "Absorbed housing units",
                  fr = "Unités de logements écoulées") |>
  tibble::add_row(en = "Absorbed",
                  fr = "Unités écoulées") |>
  tibble::add_row(en = "the count of absorbed housing units",
                  fr = "le nombre d'unités de logements écoulées") |>
  tibble::add_row(en = "count of absorbed housing units",
                  fr = "nombre d'unités de logements écoulées")

