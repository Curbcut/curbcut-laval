### CUSTOM INFO TABLES #########################################################

translation_custom_pages <- 
  tibble(en = character(),
         fr = character()) |> 
  # Natural infrastructure
  add_row(en = paste0("<p>Natural infrastructure represents approximately 25% ",
                      "of the territory of the Montreal region. Preserving {sl",
                      "ider}% of the overall territory as natural infrastructu",
                      "re means that {conservation_pct}% of the natural infras",
                      "tructure of the region would be protected.</p><p>This l",
                      "evel of protection allows for the conservation of {floo",
                      "d} of the runoff reduction, {biodiversity} of the biodi",
                      "versity conservation, and {heat_island} of the urban he",
                      "at island reduction effects provided by natural infrast",
                      "ructure in the region.</p>"), 
          fr = paste0("<p>Les infrastructures naturelles représentent environ ",
                      "25 % du territoire de la région de Montréal. En préserv",
                      "ant {slider} % du territoire global en tant qu'infrastru",
                      "cture naturelle, {conservation_pct} % de l'infrastructur",
                      "e naturelle de la région serait protégée.</p>",
                      "<p>Ce niveau de protection permet de conserver {flood}",
                      " de la réduction du ruissellement, {biodiversity} de",
                      " la conservation de la biodiversité et {heat_island} ",
                      "des effets de réduction des îlots de chaleur urbains fo",
                      "urnis par les infrastructures naturelles de la région.<",
                      "/p>")) |> 
  add_row(en = paste0("<p>Natural infrastructure represents approximately 25% of the ",
                      "territory of the Montreal region. Biodiversity-related natural ", 
                      "infrastructure functions include:</p><ul>{c_bio}{habitat_qual}",
                      "{habitat_con}{favorable_cc}</ul>"), 
          fr = paste0("<p>Les infrastructures naturelles représentent environ ",
                      "25 % du territoire de la région de Montréal. Les foncti",
                      "ons des infrastructures naturelles liées à la biodivers",
                      "ité sont les suivantes :</p><ul>{c_bio}{habitat_qual}",
                      "{habitat_con}{favorable_cc}</ul>")) |> 
  add_row(en = paste0("<p>Natural infrastructure represents approximately 25% of the ",
                      "territory of the Montreal region. Flood-related natural ", 
                      "infrastructure functions include:</p><ul>{c_flood}{flood}</ul>"), 
          fr = paste0("<p>Les infrastructures naturelles représentent environ ", 
                      "25 % du territoire de la région de Montréal. Les foncti", 
                      "ons des infrastructures naturelles liées aux inondation", 
                      "s sont les suivantes :</p><ul>{c_flood}{flood}</ul>")) |> 
  add_row(en = paste0("<p>Natural infrastructure represents approximately 25% of the ",
                      "territory of the Montreal region. Heat-related natural ", 
                      "infrastructure functions include:</p><ul>{c_heat}{heat}{cool}</ul>"), 
          fr = paste0("<p>Les infrastructures naturelles représentent environ ", 
                      "25 % du territoire de la région de Montréal. Les foncti", 
                      "ons des infrastructures naturelles liées à la températu", 
                      "re sont les suivantes:</p><ul>{c_heat}{heat}{cool}</ul>")) |> 
  add_row(en = "Biodiversity",
          fr = "Biodiversité") |> 
  add_row(en = "Heat island",
          fr = "Îlot de chaleur") |> 
  add_row(en = "Heat island",
          fr = "Îlot de chaleur") |> 
  add_row(en = "Theme",
          fr = "Thème") |> 
  add_row(en = "Amount of territory to protect",
          fr = "Quantité de territoire à protéger") |> 
  add_row(en = "Custom priorities",
          fr = "Priorités personnalisées") |> 
  add_row(en = "Biodiversity conservation",
          fr = "Conservation de la biodiversité") |> 
  add_row(en = "Heat island reduction",
          fr = "Réduction des îlots de chaleur") |> 
  add_row(en = "Flood prevention",
          fr = "Prévention des inondations") |> 
  add_row(en = "Not important",
          fr = "Pas important") |> 
  add_row(en = "Somewhat important",
          fr = "Peu important") |> 
  add_row(en = "Important",
          fr = "Important") |> 
  add_row(en = "Very important",
          fr = "Très important") |> 
  add_row(en = "Extremely important",
          fr = "Extrêmement important") |> 
  add_row(en = "Amount protected",
          fr = "Montant protégé") |> 
  add_row(en = "Share of Montreal area",
          fr = "Part de la région de Montréal") |> 
  add_row(en = "Flood risk areas",
          fr = "Zones à risque d'inondation") |> 
  
  # Green alley
  add_row(en = "Borough summary",
          fr = "Résumé de l'arrondissement") |> 
  add_row(en = "Green alleys per square kilometer",
          fr = "Ruelles vertes par kilomètre carré") |>
  add_row(en = "Green alleys per 1,000 residents",
          fr = "Ruelles vertes pour 1 000 habitants") |>
  add_row(en = "{participate} out of 19 Montreal boroughs have a green alley program.",
          fr = "{participate} des 19 arrondissements de Montréal ont un programme de ruelles vertes.") |>
  add_row(en = "{borough} does not have a green alley program.",
          fr = "{borough} n'a pas de programme de ruelles vertes.") |>
  add_row(en = "Our team visited {visited} of the {all_nb} green alleys in Montreal. We classified {green} ({green_per}) as 'green', {community} ({community_per}) as 'community', {mixed} ({mixed_per}) as 'mixed' green and community, and {unmaintained} ({unmaintained_per}) as 'unmaintained'.",
          fr = "Notre équipe a visité {visited} des {all_nb} ruelles vertes de Montréal. Nous avons classé {green} ({green_per}) comme 'verte', {community} ({community_per}) comme 'communautaire', {mixed} ({mixed_per}) comme 'mixte' verte et communautaire, et {unmaintained} ({unmaintained_per}) comme 'non-entretenue'.") |>
  add_row(en = "Green alley inaugurations",
          fr = "Inaugurations de la ruelle verte") |>
  add_row(en = "Green",
          fr = "Verte") |>
  add_row(en = "Community",
          fr = "Communautaire") |>
  add_row(en = "Mixed",
          fr = "Mixte") |>
  add_row(en = "Unmaintained",
          fr = "Non-entretenue") |>
  add_row(en = "Visited green alleys type",
          fr = "Type de ruelles vertes visitées") |>
  add_row(en = "Curbcut’s observational data",
          fr = "Les données d'observation de Curbcut") |> 
  
  # Climate
  add_row(en = "Climate",
          fr = "Climat") |> 
  add_row(en = "View with grids",
          fr = "Vue avec maillage") |> 
  
  # Crash
  add_row(en = "Heatmap",
          fr = "Carte thermique") |> 
  add_row(en = "Crash locations",
          fr = "Emplacement des collisions")