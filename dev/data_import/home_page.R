## HOME PAGE TIBBLES ###########################################################

home_page <- function(modules, stories, translation_df, data_path = "data/", port = 9999,
                      c_city_svg) {
  ### READ ?cc.landing::landing_input DOCUMENTATION TO CONSTRUCT CORRECTLY
  # EACH OF THESE OBJECTS.

  # Encode images to base64 for the input
  base64 <- function(x) {
    # Read the JPG image as raw binary data
    image_data <- readBin(x, "raw", file.info(x)$size)

    # Encode the image data to base64
    paste0("data:image/jpeg;base64,", base64enc::base64encode(image_data))
  }

  # Tibble for the discover section -----------------------------------------

  # Function to remove all HTML tags from a given string
  remove_html_tags <- function(input_vector) {
    # Use gsub to replace all HTML tags with an empty string
    output_vector <- gsub("<[^>]*>", "", input_vector)
    return(output_vector)
  }

  # Pages from the modules
  disc_modules <- modules[c("id", "title_text_title", "title_text_main", "theme")]
  names(disc_modules) <- c("id", "en", "preview_en", "theme")
  disc_modules$img <- sprintf("%s.png", disc_modules$id)
  disc_modules$theme <- gsub(" .*", "", disc_modules$theme) |> tolower()
  disc_modules$preview_fr <- sapply(disc_modules$preview_en, curbcut::cc_t, lang = "fr", USE.NAMES = FALSE)
  disc_modules$preview_fr <- remove_html_tags(disc_modules$preview_fr)
  disc_modules$preview_en <- remove_html_tags(disc_modules$preview_en)
  disc_modules$fr <- sapply(disc_modules$en, curbcut::cc_t, lang = "fr", USE.NAMES = FALSE)
  disc_modules <- disc_modules[c("id", "img", "theme", "en", "fr", "preview_en", "preview_fr")]
  disc_modules$type <- "page"
  disc_modules$select_id <- NA_character_
  disc_modules$var_left <- NA
  disc_modules$var_right <- NA
  disc_modules$page <- NA
  disc_modules$date <- NA
  disc_modules$scale <- NA

  # disc_stories <- disc_stories[c("id", "img", "theme", "en", "fr", "preview_en", "preview_fr", "type", "select_id")]

  # DYK for discovers
  dyk <- qs::qread("data/dyk.qs")
  dyk_discover_fun <- function(theme, page, var_right = " ", var_left = NULL,
                               date, type, region = NULL, scale = NULL, en, fr) {

    # Filter page, var_right, type
    this <- dyk[dyk$module == page & dyk$var_right == var_right & dyk$dyk_type == type, ]

    # Filter var_left if supplied
    if (!is.null(var_left)) {
      this <- dyk[dyk$var_left == var_left, ]
    }

    # Filter date over the list column
    this <- this[unlist(sapply(this$date, identical, as.character(date))), ]

    # Filter region and scale. Use of `identical` for when scale is NA.
    if (is.null(region) & is.null(scale)) {
    this <- this[this$region == this$region[1] & unlist(sapply(this$scale, identical, this$scale[1])), ]
    } else {
      this <- this[this$region == region & this$scale == scale, ]
    }

    # Grab last row
    this <- this[nrow(this), ]

    # Discover columns
    this$id <- sprintf("%s_dyk1", page)
    this$img <- sprintf("%s.png", this$id)
    this$theme <- theme
    this$en <- en
    this$fr <- fr
    names(this)[names(this) == "dyk_text_en"] <- "preview_en"
    names(this)[names(this) == "dyk_text_fr"] <- "preview_fr"
    names(this)[names(this) == "module"] <- "page"
    names(this)[names(this) == "select_ID"] <- "select_id"
    this$type <- "dyk"

    # Return
    this[c("id", "img", "theme", "en", "fr", "preview_en", "preview_fr", "type",
           "page", "var_left", "var_right", "select_id", "date", "scale")]
  }

  disc_dyk <-
    tibble::tibble() |>
    rbind(dyk_discover_fun(theme = "health",
                           page = "alp",
                           var_right = "housing_single_detached",
                           date = 2021,
                           type = "compare",
                           scale = "secteur",
                           region = "city",
                           en = "Dense, walkable neighbourhoods",
                           fr = "Quartiers denses et accessibles à pied")) |>
    rbind(dyk_discover_fun(theme = "climate",
                           page = "lst",
                           var_right = " ",
                           date = 2021,
                           type = "lowest",
                           region = "city",
                           scale = "secteur",
                           en = "The coolest town in the region",
                           fr = "La ville la plus fraîche de la région")) |>
    rbind(dyk_discover_fun(theme = "ecology",
                           page = "ndvi",
                           var_right = "housing_tenant",
                           date = 2021,
                           scale = "secteur",
                           region = "city",
                           type = "compare",
                           en = "Tenants lack green space",
                           fr = "Les locataires manquent d'espaces verts")) |>
    rbind(dyk_discover_fun(theme = "housing",
                           page = "housing",
                           var_left = "housing_rent",
                           date = c("1996", "2021"),
                           type = "change",
                           en = "Skyrocketing housing costs",
                           fr = "La flambée des prix du logement")) |>
    rbind(dyk_discover_fun(theme = "transport",
                           page = "canbics",
                           var_right = " ",
                           date = c("2021"),
                           type = "highest",
                           region = "city",
                           scale = "secteur",
                           en = "The best bikelanes",
                           fr = "Les meilleures pistes cyclables"))

  discover_cards <- rbind(disc_modules, disc_dyk)

  if (length(unique(discover_cards$id)) != nrow(discover_cards)) {
    stop("Discover cards do not have unique ids")
  }


  # Tibble for team members -------------------------------------------------
  team_cards <- tibble::tibble(
    id = c("davidw", "kevinm", "maxbdb", "dominiqueb"),
    img = c(
      "www/landing/team/david_wachsmuth.jpeg",
      "www/landing/team/kevin_manaugh.jpg",
      "www/landing/team/maxime_belanger_de_blois.jpg",
      "www/landing/team/dominique_boulet.jpg"
    ),
    name = c("David Wachsmuth", "Kevin Manaugh", "Maxime Bélanger De Blois", "Dominique Boulet"),
    role_en = c("Co-founder & Co-CEO", "Co-founder & Co-CEO", "Head of Technology and Data", "Qualitative Research Lead"),
    role_fr = c("Co-fondateur et co-PDG", "Co-fondateur et co-PDG", "Responsable technologie et données", "Responsable de la recherche qualitative"),
    # bio_en = c("David is one of the world’s leading experts on the impacts of short-term rental platforms, such as Airbnb, on cities around the world and consults widely with municipalities and community organizations on designing appropriate regulations. In addition to his work at Curbcut, David is the Canada Research Chair in Urban Governance at McGill University, where he is also an Associate Professor in the School of Urban Planning.",
    #            "Kevin is one of the leading experts on the intersection between urban transport systems and social and environmental justice. In addition to his work at Curbcut, Kevin is also an associate professor at McGill University jointly appointed in the Department of Geography and the Bieler School of Environment.",
    #            "Maxime is a skilled, resourceful and forward-thinking data scientist, adept at developing and transforming intricate datasets into actionable intelligence. With a master's degree in Urban Planning from McGill University, his extensive understanding of data analysis and geovisualization enables him to extract valuable insights and provide innovative solutions.",
    #            "Dominique is driven to create qualitative work that complements quantitative information. She has a master’s degree in Urban Planning from McGill University and a master’s degree in Anthropology from Aarhus University, Copenhagen."),
    # bio_fr = c("David est l'un des plus grands experts mondiaux sur les impacts des plateformes de location à court terme, telles que Airbnb, sur les villes du monde entier et consulte largement les municipalités et les organisations communautaires sur la conception de réglementations appropriées. En plus de son travail chez Curbcut, David est titulaire de la Chaire de recherche du Canada en gouvernance urbaine à l'Université McGill, où il est également professeur associé à l'École d'urbanisme.",
    #            "Kevin est l'un des principaux experts de l'intersection entre les systèmes de transport urbain et la justice sociale et environnementale. En plus de son travail chez Curbcut, Kevin est également professeur associé à l'Université McGill, nommé conjointement au département de géographie et à l'école d'environnement Bieler.",
    #            "Maxime est un scientifique de données habile, ingénieux et avant-gardiste, capable de concevoir et de transformer des ensembles de données complexes en renseignements exploitables. Diplômé d'une maîtrise en urbanisme de l'Université McGill, sa connaissance approfondie de l'analyse des données et de la géovisualisation lui permet d'extraire des informations précieuses et de proposer des solutions innovantes.",
    #            "Dominique est motivée par la nécessité de produire des études qualitatives qui complètent les informations quantitatives. Elle est titulaire d'une maîtrise en urbanisme de l'Université McGill et d'une maîtrise en anthropologie de l'Université d'Aarhus, à Copenhague."),
    theme = c("housing", "transport", "health", "urban")
  )

  team_cards$img <- sapply(team_cards$img, base64)

  # Character vector for contributors ---------------------------------------
  contributors <- c(
    "Cloé St-Hilaire",
    "Emma Ezvan",
    "Daniela Rodriguez",
    "Connor Cordingley",
    "Robin Basalaev-Binder",
    "Josh Medicoff",
    "Philip Bligh",
    "Lauren Rosenthal"
  )


  # Tibble for collaborators ------------------------------------------------
  collabs <- tibble::tibble(
    id = c("laval", "MSSI", "centraide"),
    img = c(
      "www/landing/collab/laval-logo.png",
      "www/landing/collab/mcgill-logo.png",
      "www/landing/collab/centraide-logo.png"
    ),
    name = c("City of Laval", "The McGill Sustainability Systems Initiative", "Centraide")
  )

  collabs$img <- sapply(collabs$img, base64)


  # Save home page information as qsm ---------------------------------------
  if (!exists("data_path")) data_path <- "data/"

  qs::qsavem(c_city_svg, discover_cards,
             team_cards, contributors, collabs,
             file = paste0(data_path, "home_page.qsm")
  )

  # Filter out missing photos and warn!
  present_img <- sprintf("%s.png", discover_cards$id) %in% list.files("www/landing/discover/")
  missing_img <- discover_cards[!present_img, ]

  # Allow for the user to take images of the discover cards content
  if (nrow(missing_img) != 0) {
    tryCatch(cc.buildr::discover_cards_screenshots(port = port,
                                        discover_cards = discover_cards,
                                        only_ids = missing_img$id),
             error = function(e) {
               discover_cards$img <- sapply(discover_cards$img, base64)

               qs::qsavem(c_city_svg, news_cards, discover_cards,
                          team_cards, contributors, collabs,
                          file = paste0(data_path, "home_page.qsm")
               )
             })
  }

  discover_cards$img <- sapply(paste0("www/landing/discover/", discover_cards$img), base64)

  qs::qsavem(c_city_svg, discover_cards,
             team_cards, contributors, collabs,
             file = paste0(data_path, "home_page.qsm")
  )
}
