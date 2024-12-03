
vars <- variables$var_code[grepl("^household_size_agg_", variables$var_code)]


translation_householdsize <-
  lapply(vars, \(u_var) {

    pct <- grepl("_pct$", u_var)
    var <- gsub("_pct|_count", "", u_var)

    vals <- unlist(stringr::str_extract_all(u_var, "\\d"))

    title <- (\(x) {
      out <- if (length(vals) == 1) {
        (\(y) {
          if (vals == "4") return("Ménage de 4 individus ou plus")
          if (vals == "1") return("Ménage d'un seul individu")
          return(sprintf("Ménage de %s individus", vals))
        })()
      } else {
        start <- vals[[1]]
        end <- vals[[2]]
        if (end == "4") {
          if (start == "1") {
            sprintf("Ménage d'un seul individu ou plus")
          } else {
            sprintf("Ménage de %s individus ou plus", start)
          }
        } else {
          sprintf("Ménage de %s à %s individus", start, end)
        }
      }


      if (pct) out <- paste(out, "(%)")
      out
    })()

    short <- (\(x) {
      if (length(vals) == 1) {
        if (vals == "4") return("4+ individus")
        if (vals == "1") return("1 individu")
        return(sprintf("%s individus", vals))
      }
      start <- vals[[1]]
      end <- vals[[2]]
      if (end == "4") return({
        sprintf("%s+ individus", start)
      })

      sprintf("%s-%s individus", start, end)
    })()

    explanation <- (\(x) {
      beg <- if (pct) "pourcentage de ménages" else "nombre de ménages"
      if (length(vals) == 1) {
        if (vals == "4") return(sprintf("le %s occupés par 4 individus ou plus", beg))
        if (vals == "1") return(sprintf("le %s occupés par un seul individu", beg))
        return(sprintf("le %s occupés par %s individus", beg, vals))
      }
      start <- vals[[1]]
      end <- vals[[2]]
      if (end == "4") return({
        if (start == "1") {
          sprintf("le %s occupés par un seul individu ou plus", beg)
        } else {
          sprintf("le %s occupés par %s individus ou plus", beg, start)
        }
      })

      sprintf("le %s occupés par %s à %s individus", beg, start, end)
    })()

    explanation_nodet <- (\(x) {
      beg <- if (pct) "pourcentage de ménages" else "nombre de ménages"
      if (length(vals) == 1) {
        if (vals == "4") return(sprintf("%s occupés par 4 individus ou plus", beg))
        if (vals == "1") return(sprintf("%s occupés par un seul individu", beg))
        return(sprintf("%s occupés par %s individus", beg, vals))
      }
      start <- vals[[1]]
      end <- vals[[2]]
      if (end == "4") return({
        if (start == "1") {
          sprintf("%s occupés par un seul individu ou plus", beg)
        } else {
          sprintf("%s occupés par %s individus ou plus", beg, start)
        }
      })

      sprintf("%s occupés par %s à %s individus", beg, start, end)
    })()

    exp_q5 <- (\(x) {
      if (length(vals) == 1) {
        if (vals == "4") return(sprintf("sont occupés par 4 individus ou plus"))
        if (vals == "1") return(sprintf("sont occupés par un seul individu"))
        return(sprintf("sont occupés par %s individus", vals))
      }
      start <- vals[[1]]
      end <- vals[[2]]
      if (end == "4") return({
        if (start == "1") {
          sprintf("sont occupés par un seul individu ou plus")
        } else {
          sprintf("sont occupés par %s individus ou plus", start)
        }
      })

      sprintf("sont occupés par %s à %s individus", start, end)
    })()

    # Construct le table
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
