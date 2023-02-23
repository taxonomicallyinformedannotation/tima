#' @title Process annotations
#'
#' @description This function weights and eventually complements initial annotations.
#'
#' @param library Library to be used to perform MS1 annotation
#' @param str_2D_3D File containing 2D and 3D structures
#' @param str_met File containing structures metadata
#' @param str_nam File containing structures names
#' @param str_tax_cla File containing Classyfire taxonomy
#' @param str_tax_npc File containing NPClassifier taxonomy
#' @param name Name of the adducts
#' @param annotations Prepared annotations file
#' @param taxa Prepared taxed features file
#' @param edges Prepared edges file
#' @param output Output file
#' @param candidates_initial Number of initial candidates to keep
#' @param candidates_final Number of final candidates to keep
#' @param weight_spectral Weight for the spectral score
#' @param weight_chemical Weight for the biological score
#' @param weight_biological Weight for the chemical consistency score
#' @param score_chemical_pathway Score for a `pathway` match (should be lower than `superclass`)
#' @param score_chemical_superclass Score for a `superclass` match (should be lower than `class`)
#' @param score_chemical_class Score for a `class` match (should be the highest)
#' @param score_biological_domain Score for a `domain` match (should be lower than `kingdom`)
#' @param score_biological_kingdom Score for a `kingdom` match (should be lower than `phylum`)
#' @param score_biological_phylum Score for a `phylum` match (should be lower than `class`)
#' @param score_biological_class Score for a `class` match (should be lower than `order`)
#' @param score_biological_order Score for a `order` match (should be lower than `infraorder`)
#' @param score_biological_infraorder Score for a `infraorder` match (should be lower than `order`)
#' @param score_biological_family Score for a `family` match (should be lower than `subfamily`)
#' @param score_biological_subfamily Score for a `subfamily` match (should be lower than `family`)
#' @param score_biological_tribe Score for a `tribe` match (should be lower than `subtribe`)
#' @param score_biological_subtribe Score for a `subtribe` match (should be lower than `genus`)
#' @param score_biological_genus Score for a `genus` match (should be lower than `subgenus`)
#' @param score_biological_subgenus Score for a `subgenus` match (should be lower than `species`)
#' @param score_biological_species Score for a `species` match (should be lower than `subspecies`)
#' @param score_biological_subspecies Score for a `subspecies` match (should be lower than `variety`)
#' @param score_biological_variety Score for a `variety` match (should be the highest)
#' @param ms_mode MS ionization mode. Should be 'pos' or 'neg'
#' @param annotate Boolean. Perform MS1 annotation completion or not
#' @param tolerance_ppm Tolerance in ppm for MS1 annotation
#' @param tolerance_rt Tolerance in retention time (minute) for adducts attribution
#' @param force Force parameters. Use it at your own risk
#' @param adducts_list Adducts list
#' @param minimal_ms1_bio Minimal biological score to keep MS1 based annotation
#' @param minimal_ms1_chemo Minimal chemical score to keep MS1 based annotation
#' @param ms1_only Boolean. Keep only MS1 annotations
#' @param parameters Params
#'
#' @return NULL
#'
#' @export
#'
#' @importFrom crayon green
#' @importFrom dplyr across arrange bind_rows distinct filter select
#' @importFrom dplyr left_join matches mutate mutate_all mutate_if
#' @importFrom readr cols read_delim write_delim
#' @importFrom yaml write_yaml
#'
#' @seealso annotate_ms1 weight_bio weight_chemo
#'
#' @examples NULL
process_annotations <-
  function(library = paths$data$interim$libraries$merged$keys,
           org_tax_ott = paths$data$interim$libraries$merged$organisms$taxonomies$ott,
           str_2D_3D = paths$data$interim$libraries$merged$structures$dd_ddd,
           str_met = paths$data$interim$libraries$merged$structures$metadata,
           str_nam = paths$data$interim$libraries$merged$structures$names,
           str_tax_cla = paths$data$interim$libraries$merged$structures$taxonomies$classyfire,
           str_tax_npc = paths$data$interim$libraries$merged$structures$taxonomies$npc,
           name = params$files$libraries$adducts$processed,
           annotations = params$files$annotations$filled,
           taxa = params$files$taxa$processed,
           edges = params$files$networks$spectral$edges$processed,
           output = params$files$annotations$processed,
           candidates_initial = params$annotations$candidates$initial,
           candidates_final = params$annotations$candidates$final,
           weight_spectral = params$weights$global$spectral,
           weight_chemical = params$weights$global$chemical,
           weight_biological = params$weights$global$biological,
           score_chemical_pathway = params$weights$chemical$pathway,
           score_chemical_superclass = params$weights$chemical$superclass,
           score_chemical_class = params$weights$chemical$class,
           score_biological_domain = params$weights$biological$domain,
           score_biological_kingdom = params$weights$biological$kingdom,
           score_biological_phylum = params$weights$biological$phylum,
           score_biological_class = params$weights$biological$class,
           score_biological_order = params$weights$biological$order,
           score_biological_infraorder = params$weights$biological$infraorder,
           score_biological_family = params$weights$biological$family,
           score_biological_subfamily = params$weights$biological$subfamily,
           score_biological_tribe = params$weights$biological$tribe,
           score_biological_subtribe = params$weights$biological$subtribe,
           score_biological_genus = params$weights$biological$genus,
           score_biological_subgenus = params$weights$biological$subgenus,
           score_biological_species = params$weights$biological$species,
           score_biological_subspecies = params$weights$biological$subspecies,
           score_biological_variety = params$weights$biological$variety,
           ms_mode = params$ms$polarity,
           annotate = params$annotations$ms1$annotate,
           tolerance_ppm = params$ms$tolerances$mass$ppm$ms1,
           tolerance_rt = params$ms$tolerances$rt$minutes,
           adducts_list = params$ms$adducts,
           adducts_masses_list = paths$inst$extdata$adducts,
           neutral_losses_list = paths$inst$extdata$neutral_losses,
           minimal_ms1_bio = params$annotations$ms1$thresholds$biological,
           minimal_ms1_chemo = params$annotations$ms1$thresholds$chemical,
           # TODO ADD CONDITION,
           ms1_only = params$annotations$ms1only,
           force = params$options$force,
           parameters = params) {
    stopifnot("Your library file does not exist." = file.exists(library))
    ## TODO add name
    stopifnot("Your annotations file does not exist." = file.exists(annotations))
    stopifnot("Your taxa file does not exist." = file.exists(taxa))
    stopifnot("Your edges file does not exist." = file.exists(edges))
    stopifnot("Your ms_mode parameter must be 'pos' or 'neg'" = ms_mode %in% c("pos", "neg"))
    stopifnot("Your ms_annotate parameter must be 'true' or 'false'" = annotate %in% c(TRUE, FALSE))
    if (force == FALSE) {
      stopifnot("Your ppm tolerance must be lower or equal to 20" = tolerance_ppm <=
        20)
      stopifnot("Your rt tolerance must be lower or equal to 0.1" = tolerance_rt <=
        0.1)
    }
    ## TODO for later on
    # stopifnot(
    #  adducts ...
    # )

    paths <<- parse_yaml_paths()
    params <<- parameters

    vars <- ls(all.names = TRUE)
    for (i in 1:length(vars)) {
      assign(vars[i], get(vars[i]), envir = .GlobalEnv)
    }

    log_debug(x = "... files ...")
    log_debug(x = "... annotations")
    annotation_table_ms2 <<-
      readr::read_delim(
        file = annotations,
        col_types = readr::cols(.default = "c")
      ) |>
      dplyr::mutate_all(list(~ gsub(
        pattern = "\\|",
        replacement = " or ",
        x = .x
      ))) |>
      dplyr::mutate(dplyr::across(
        c(feature_id, component_id, rt, mz, score_input, mz_error),
        as.numeric
      )) |>
      dplyr::distinct() |>
      dplyr::arrange(feature_id)

    log_debug(x = "... metadata_table_biological_annotation")
    taxed_features_table <<- readr::read_delim(
      file = taxa,
      col_types = readr::cols(.default = "c")
    ) |>
      dplyr::mutate(dplyr::across(feature_id, as.numeric)) |>
      dplyr::mutate_if(is.logical, as.character)

    taxed_features_table[is.na(taxed_features_table)] <- "ND"

    log_debug(x = "... edges table")
    edges_table <<- readr::read_delim(file = edges)

    log_debug(x = "... structure-organism pairs table")
    structure_organism_pairs_table <<-
      readr::read_delim(
        file = library,
        col_types = readr::cols(.default = "c")
      ) |>
      dplyr::left_join(readr::read_delim(
        file = str_2D_3D,
        col_types = readr::cols(.default = "c")
      )) |>
      dplyr::left_join(readr::read_delim(
        file = str_met,
        col_types = readr::cols(.default = "c")
      )) |>
      dplyr::left_join(readr::read_delim(
        file = str_nam,
        col_types = readr::cols(.default = "c")
      )) |>
      dplyr::left_join(readr::read_delim(
        file = str_tax_cla,
        col_types = readr::cols(.default = "c")
      )) |>
      dplyr::left_join(readr::read_delim(
        file = str_tax_npc,
        col_types = readr::cols(.default = "c")
      )) |>
      dplyr::left_join(readr::read_delim(
        file = org_tax_ott,
        col_types = readr::cols(.default = "c")
      )) |>
      dplyr::filter(!is.na(structure_exact_mass)) |>
      dplyr::mutate(dplyr::across(c(
        "structure_exact_mass",
        "structure_xlogp"
      ), as.numeric)) |>
      round_reals() |>
      dplyr::mutate(dplyr::across(
        dplyr::matches("taxonomy.*_0"),
        ~ tidyr::replace_na(.x, "notClassified")
      ))

    if (ms1_only == TRUE) {
      log_debug(x = "Erasing MS2 results")
      annotation_table_ms2 <<-
        annotation_table_ms2 |>
        dplyr::mutate(
          structure_inchikey_2D = NA_character_,
          score_input = NA_real_,
          library = NA_character_,
          mz_error = NA_real_
        )
    }

    if (annotate == TRUE) {
      log_debug("... single charge adducts table")
      if (ms_mode == "pos") {
        adduct_file <- paths$data$interim$adducts$pos
      }
      if (ms_mode == "neg") {
        adduct_file <- paths$data$interim$adducts$neg
      }

      adductsTable <<- readr::read_delim(file = adduct_file)

      log_debug("... adducts masses for in source dimers and multicharged")
      adductsMassTable <<-
        readr::read_delim(file = adducts_masses_list)

      log_debug("... neutral lossses")
      neutral_losses_table <<-
        readr::read_delim(file = neutral_losses_list)

      adductsM <<- adductsMassTable$mass
      names(adductsM) <<- adductsMassTable$adduct

      if (ms_mode == "pos") {
        adduct_db_file <<-
          file.path(
            paths$data$interim$adducts$path,
            paste0(name, "_pos.tsv.gz")
          )
      }
      if (ms_mode == "neg") {
        adduct_db_file <<-
          file.path(
            paths$data$interim$adducts$path,
            paste0(name, "_neg.tsv.gz")
          )
      }

      ## TODO remove this dirty fix
      adduct_db_file <- adduct_db_file |>
        gsub(
          pattern = "NA_pos.tsv.gz",
          replacement = "library_pos.tsv.gz",
          fixed = TRUE
        ) |>
        gsub(
          pattern = "NA_neg.tsv.gz",
          replacement = "library_neg.tsv.gz",
          fixed = TRUE
        )

      log_debug(x = "... exact masses for MS1 annotation")
      structure_exact_mass_table <<-
        readr::read_delim(file = adduct_db_file) |>
        dplyr::filter(exact_mass %in% structure_organism_pairs_table[["structure_exact_mass"]])

      log_debug(x = "performing MS1 annotation")
      annotation_table_ms1 <<- annotation_table_ms2 |>
        annotate_ms1(
          structureExactMassTable = structure_exact_mass_table,
          structureOrganismPairsTable = structure_organism_pairs_table,
          adducts = unlist(adducts_list[[ms_mode]]),
          neutralLosses = neutral_losses_table,
          msMode = ms_mode,
          tolerancePpm = tolerance_ppm,
          toleranceRt = tolerance_rt,
          candidatesInitial = candidates_initial
        )

      annotation_table_ms1 |>
        decorate_ms1()
    } else {
      annotation_table_ms1 <<- table_ms2_annotations
      annotate_non_ms1(candidatesInitial = candidates_initial)
    }

    log_debug(x = "adding biological organism metadata")
    annotation_table_ms1_taxed <<- annotation_table_ms1 |>
      dplyr::left_join(taxed_features_table)

    log_debug(x = "performing taxonomically informed scoring")
    annotation_table_weighted_bio <<- annotation_table_ms1_taxed |>
      weight_bio(
        structureOrganismPairsTable = structure_organism_pairs_table,
        weightSpectral = weight_spectral,
        weightBiological = weight_biological,
        scoreBiologicalDomain = score_biological_domain,
        scoreBiologicalKingdom = score_biological_kingdom,
        scoreBiologicalPhylum = score_biological_phylum,
        scoreBiologicalClass = score_biological_class,
        scoreBiologicalOrder = score_biological_order,
        scoreBiologicalFamily = score_biological_family,
        scoreBiologicalTribe = score_biological_tribe,
        scoreBiologicalGenus = score_biological_genus,
        scoreBiologicalSpecies = score_biological_species,
        scoreBiologicalVariety = score_biological_variety
      )

    annotation_table_weighted_bio |>
      decorate_bio(
        sc_kin = score_biological_kingdom,
        sc_phy = score_biological_phylum,
        sc_cla = score_biological_class,
        sc_ord = score_biological_order,
        sc_fam = score_biological_family,
        sc_gen = score_biological_genus,
        sc_spe = score_biological_species,
        sc_var = score_biological_variety
      )

    log_debug(x = "cleaning taxonomically informed results and preparing for chemically informed scoring")
    annotation_table_weighted_bio_cleaned <<-
      annotation_table_weighted_bio |>
      clean_bio(
        edgesTable = edges_table,
        aNnOtAtE = annotate,
        candidatesInitial = candidates_initial,
        minimalMs1Bio = minimal_ms1_bio
      )

    log_debug(x = "performing chemically informed scoring")
    annotation_table_weighted_chemo <<-
      annotation_table_weighted_bio_cleaned |>
      weight_chemo(
        weightSpectral = weight_spectral,
        weightBiological = weight_biological,
        weightChemical = weight_chemical,
        scoreChemicalPathway = score_chemical_pathway,
        scoreChemicalSuperclass = score_chemical_superclass,
        scoreChemicalClass = score_chemical_class
      )

    annotation_table_weighted_chemo |>
      decorate_chemo(
        sc_pat = score_chemical_pathway,
        sc_sup = score_chemical_superclass,
        sc_cla = score_chemical_class
      )

    log_debug(x = "cleaning for export")
    results <<- annotation_table_weighted_chemo |>
      clean_chemo(
        structureOrganismPairsTable = structure_organism_pairs_table,
        candidatesFinal = candidates_final,
        minimalMs1Bio = minimal_ms1_bio,
        minimalMs1Chemo = minimal_ms1_chemo
      )

    log_debug(x = "Exporting ...")
    time <- format(Sys.time(), "%y%m%d_%H%M%OS")
    dir_time <- file.path(paths$data$processed$path, time)
    final_output <- file.path(
      dir_time,
      output
    )
    export_params(
      directory = dir_time,
      step = "process_annotations"
    )
    export_output(
      x = results,
      file = final_output
    )

    return(final_output)
  }
