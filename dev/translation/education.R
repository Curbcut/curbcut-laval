
# Go over every possible variable
vars <- variables$var_code[grepl("^edu_", variables$var_code) &
                             grepl("(_count$)|(_pct$)", variables$var_code)]

# translation_age <- tibble::tibble(en = "Aged 85 and above (%)",
#                                   fr = "Agés de plus de 85 ans (%)") |>
#   tibble::add_row(en = "are aged 85 and above",
#                   fr = "sont âgés de plus de 85 ans") |>
#   tibble::add_row(en = "the percentage of the population aged 85 and above",
#                   fr = "le pourcentage de la population âgée de plus de 85 ans") |>
#   tibble::add_row(en = "percentage of the population aged 85 and above",
#                   fr = "pourcentage de la population âgée de plus de 85 ans")


vartitles <- cc.data::census_vectors_education[1:5, c("var_code", "var_title", "explanation", "exp_q5")]
vartitles$var_code <- gsub("edu_", "", vartitles$var_code)
vartitles$var_title <- gsub(" \\(\\%\\)$", "", vartitles$var_title)
vartitles$var_short <- c("No degree", "Secondary", "College", "Uni. below bachelor",
                         "Bachelor+")

# Translate var title
vartitles$var_title[
  vartitles$var_title == "No certificate, diploma or degree"
] <- "Aucun certificat, diplôme ou grade"
vartitles$var_title[
  vartitles$var_title == "Secondary school diploma or equivalent"
] <- "Diplôme d'études secondaires ou équivalent"
vartitles$var_title[
  vartitles$var_title == "College, CEGEP or other non-university certificate or diploma"
] <- "Certificat ou diplôme d'un collège, d'un cégep ou d'un autre établissement non universitaire"
vartitles$var_title[
  vartitles$var_title == "University certificate or diploma below bachelor level"
] <- "Certificat ou diplôme universitaire inférieur au baccalauréat"
vartitles$var_title[
  vartitles$var_title == "Bachelor and above"
] <- "Baccalauréat et plus"

# Translate explanation
vartitles$explanation <-
  sapply(vartitles$explanation, \(x) translation_variables$fr[translation_variables$en %in% x],
         USE.NAMES = FALSE)
vartitles$explanation <-
  gsub("le pourcentage de la population âgée de 15 ans et plus ", "", vartitles$explanation)

# Translate exp_q5
vartitles$exp_q5 <- paste("sont", vartitles$explanation)

# Translate var_short
vartitles$var_short[
  vartitles$var_short == "No degree"
] <- "Sans diplôme"
vartitles$var_short[
  vartitles$var_short == "Secondary"
] <- "Secondaire"
vartitles$var_short[
  vartitles$var_short == "College"
] <- "CEGEP"
vartitles$var_short[
  vartitles$var_short == "Uni. below bachelor"
] <- "Uni. sous bacc."
vartitles$var_short[
  vartitles$var_short == "Bachelor+"
] <- "Baccalauréat+"

translation_education <- lapply(vars, \(u_var) {

      pct <- grepl("_pct$", u_var)
      var <- gsub("_pct|_count", "", u_var)

      v <- gsub("edu_", "", u_var)
      vals <- stringr::str_extract_all(v, paste0(vartitles$var_code, collapse = "|"))[[1]]

      title <- (\(x) {
        out <- if (length(vals) == 1) {
          (\(y) {
            vartitles$var_title[vartitles$var_code == vals]
          })()
        } else {
          start <- vartitles$var_title[vartitles$var_code == vals[[1]]]
          end <- vartitles$var_title[vartitles$var_code == vals[[2]]]
          if (vals[[2]] == "bachelor_above") return(sprintf("%s ou tout autre niveau d'éducation plus élevé", start))

          sprintf("%s, jusqu'à %s", start, end)
        }

        if (pct) out <- paste(out, "(%)")
        out})()

      short <- (\(x) {
        if (length(vals) == 1) return(vartitles$var_short[vartitles$var_code == vals])
        start <- vartitles$var_short[vartitles$var_code == vals[[1]]]
        end <- vartitles$var_short[vartitles$var_code == vals[[2]]]

        if (vals[[2]] == "bachelor_above") return(sprintf("%s+", start))

        sprintf("%s - %s", start, end)
      })()

      explanation <- (\(x) {
        beg <- if (pct) "pourcentage" else "nombre"
        beg <- sprintf("le %s d'individus âgés de 15 ans et plus", beg)
        if (length(vals) == 1) {
          return(sprintf("%s %s", beg, vartitles$explanation[vartitles$var_code == vals]))
        }
        start <- vartitles$explanation[vartitles$var_code == vals[[1]]]
        end <- vartitles$explanation[vartitles$var_code == vals[[2]]]
        if (vals[[2]] == "bachelor_above") return(sprintf("%s %s ou tout autre niveau d'éducation plus élevé", beg, start))

        sprintf("%s %s, jusqu'à %s", beg, start, gsub("holding ", "", end))
      })()

      explanation_nodet <- (\(x) {
        beg <- if (pct) "pourcentage" else "nombre"
        beg <- sprintf("%s d'individus âgés de 15 ans et plus", beg)
        if (length(vals) == 1) {
          return(sprintf("%s %s", beg, vartitles$explanation[vartitles$var_code == vals]))
        }
        start <- vartitles$explanation[vartitles$var_code == vals[[1]]]
        end <- vartitles$explanation[vartitles$var_code == vals[[2]]]
        if (vals[[2]] == "bachelor_above") return(sprintf("%s %s ou tout autre niveau d'éducation plus élevé", beg, start))

        sprintf("%s %s, jusqu'à %s", beg, start, gsub("holding ", "", end))
      })()

      exp_q5 <- (\(x) {
        if (length(vals) == 1) {
          return(vartitles$exp_q5[vartitles$var_code == vals])
        }
        start <- vartitles$exp_q5[vartitles$var_code == vals[[1]]]
        end <- vartitles$exp_q5[vartitles$var_code == vals[[2]]]
        if (vals[[2]] == "bachelor_above") return(sprintf("%s ou tout autre niveau d'éducation plus élevé", start))

        out <- sprintf("%s jusqu'à %s", start, gsub("titulaires ", "", end))
        gsub("jusqu'à sont titulaires d'", "jusqu'à ", out)
      })()


  # Construct the table
  tibble(en = variables$var_title[variables$var_code == u_var],
         fr = title) |>
    add_row(en = variables$var_short[variables$var_code == u_var],
            fr = short) |>
    add_row(en = variables$explanation[variables$var_code == u_var],
            fr = explanation) |>
    add_row(en = variables$exp_q5[variables$var_code == u_var],
            fr = exp_q5) |>
    add_row(en = variables$explanation_nodet[variables$var_code == u_var],
            fr = explanation_nodet)

}) |> (\(x) Reduce(rbind, x))()

