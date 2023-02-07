#' @title Process annotations
#'
#' @description This function weights and eventually complements initial annotations.
#'
#' @param library Library to be used to perform MS1 annotation
#' @param name Name of the adducts
#' @param gnps GNPS annotation results
#' @param isdb ISDB annotation results
#' @param sirius SIRIUS annotation results
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
#' @param ms_level 1
#' @param annotate Boolean. Perform MS1 annotation completion or not
#' @param tolerance_ppm Tolerance in ppm for MS1 annotation
#' @param tolerance_rt Tolerance in retention time (minute) for adducts attribution
#' @param force Force parameters. Use it at your own risk
#' @param adducts_list Adducts list
#' @param minimal_ms1_bio Minimal biological score to keep MS1 based annotation
#' @param minimal_ms1_chemo Minimal chemical score to keep MS1 based annotation
#' @param ms1_only Boolean. Keep only MS1 annotations
#'
#' @return NULL
#'
#' @export
#'
#' @importFrom crayon green
#' @importFrom dplyr across arrange bind_rows distinct filter select left_join
#' @importFrom dplyr mutate mutate_all mutate_if
#' @importFrom readr read_delim write_delim
#' @importFrom yaml write_yaml
#'
#' @seealso annotate_ms1 weight_bio weight_chemo
#'
#' @examples NULL
process_annotations <- function(library = params$library,
                                name = params$name,
                                gnps = params$annotation$gnps,
                                isdb = params$annotation$isdb,
                                sirius = params$annotation$sirius,
                                taxa = params$taxa,
                                edges = params$edges,
                                output = params$output,
                                candidates_initial = params$top_k$initial,
                                candidates_final = params$top_k$final,
                                weight_spectral = params$weights$spectral,
                                weight_chemical = params$weights$chemical,
                                weight_biological = params$weights$biological,
                                score_chemical_pathway = params$scores$chemical$pathway,
                                score_chemical_superclass = params$scores$chemical$superclass,
                                score_chemical_class = params$scores$chemical$class,
                                score_biological_domain = params$scores$biological$domain,
                                score_biological_kingdom = params$scores$biological$kingdom,
                                score_biological_phylum = params$scores$biological$phylum,
                                score_biological_class = params$scores$biological$class,
                                score_biological_order = params$scores$biological$order,
                                score_biological_infraorder = params$scores$biological$infraorder,
                                score_biological_family = params$scores$biological$family,
                                score_biological_subfamily = params$scores$biological$subfamily,
                                score_biological_tribe = params$scores$biological$tribe,
                                score_biological_subtribe = params$scores$biological$subtribe,
                                score_biological_genus = params$scores$biological$genus,
                                score_biological_subgenus = params$scores$biological$subgenus,
                                score_biological_species = params$scores$biological$species,
                                score_biological_subspecies = params$scores$biological$subspecies,
                                score_biological_variety = params$scores$biological$variety,
                                ms_mode = params$ms$mode,
                                ms_level = params$ms$level,
                                annotate = params$ms$annotate,
                                tolerance_ppm = params$ms$tolerance$ppm,
                                tolerance_rt = params$ms$tolerance$rt,
                                adducts_list = params$ms$adducts,
                                minimal_ms1_bio = params$scores$biological$minimal,
                                minimal_ms1_chemo = params$scores$chemical$minimal,
                                ms1_only = params$ms$ms1only,
                                force = params$force) {
  stopifnot("Your library file does not exist." = file.exists(library))
  ## TODO add name
  stopifnot("Your GNPS file does not exist." = if (length(gnps) == 0) {
    TRUE
  } else {
    file.exists(gnps)
  })
  stopifnot("Your ISDB file does not exist." = if (length(isdb) == 0) {
    TRUE
  } else {
    file.exists(isdb)
  })
  stopifnot("Your SIRIUS file does not exist." = if (length(sirius) == 0) {
    TRUE
  } else {
    file.exists(sirius)
  })
  stopifnot("Your taxa file does not exist." = file.exists(taxa))
  stopifnot("Your edges file does not exist." = file.exists(edges))
  stopifnot("Your ms_mode parameter must be 'pos' or 'neg'" = ms_mode %in% c("pos", "neg"))
  stopifnot("Your ms_level parameter must be 1" = ms_level == 1)
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

  vars <- ls(all.names = TRUE)
  for (i in 1:length(vars)) {
    assign(vars[i], get(vars[i]), envir = .GlobalEnv)
  }

  log_debug(x = "... files ...")
  log_debug(x = "... annotations")
  metadata_table_spectral_annotation <<- lapply(
    X = c(gnps, isdb, sirius),
    FUN = function(x) {
      readr::read_delim(
        file = x,
        col_types = "c"
      ) |>
        dplyr::mutate_all(list(~ gsub(
          pattern = "\\|",
          replacement = " or ",
          x = .x
        ))) |>
        dplyr::mutate(dplyr::across(feature_id, as.numeric))
    }
  ) |>
    dplyr::bind_rows() |>
    dplyr::distinct() |>
    dplyr::arrange(feature_id)

  log_debug(x = "... metadata_table_biological_annotation")
  taxed_features_table <<- readr::read_delim(
    file = taxa,
    col_types = "c"
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
      col_types = "c"
    ) |>
    dplyr::filter(!is.na(structure_exact_mass)) |>
    dplyr::mutate(dplyr::across(c(
      "structure_exact_mass",
      "structure_xlogp"
    ), as.numeric)) |>
    ## COMMENT AR: else some redundancy because of reals
    dplyr::mutate(structure_xlogp = round(structure_xlogp, digits = 5)) |>
    dplyr::mutate(dplyr::across(
      tidyr::matches("taxonomy"),
      ~ tidyr::replace_na(.x, "notClassified")
    ))

  if (ms1_only == TRUE) {
    log_debug(x = "Erasing MS2 results")
    metadata_table_spectral_annotation <<-
      metadata_table_spectral_annotation |>
      dplyr::mutate(
        inchikey_2D = NA,
        score_input = NA,
        library = NA,
        mz_error = NA
      ) |>
      dplyr::mutate(dplyr::across(
        c(inchikey_2D, score_input, library, mz_error),
        as.character
      ))
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
      readr::read_delim(file = paths$data$source$adducts)

    log_debug("... neutral lossses")
    neutral_losses_table <<-
      readr::read_delim(file = paths$data$source$neutral_losses)

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

    log_debug(x = "... exact masses for MS1 annotation")
    structure_exact_mass_table <<-
      readr::read_delim(file = adduct_db_file) |>
      dplyr::filter(exact_mass %in% structure_organism_pairs_table[["structure_exact_mass"]])

    log_debug(x = "performing MS1 annotation")
    annotation_table_ms1 <<- annotate_ms1()

    decorate_ms1()
  } else {
    annotation_table_ms1 <<-
      annotate_non_ms1(annotationTable = metadata_table_spectral_annotation)
  }

  log_debug(x = "adding biological organism metadata")
  annotation_table_ms1_taxed <<-
    dplyr::left_join(annotation_table_ms1, taxed_features_table) |>
    dplyr::left_join(
      metadata_table_spectral_annotation |>
        dplyr::distinct(
          inchikey_2D,
          smiles_2D,
          structure_taxonomy_npclassifier_01pathway,
          structure_taxonomy_npclassifier_02superclass,
          structure_taxonomy_npclassifier_03class
        )
    )

  log_debug(x = "performing taxonomically informed scoring")
  annotation_table_weighted_bio <<- weight_bio()

  decorate_bio()

  log_debug(x = "cleaning taxonomically informed results and preparing for chemically informed scoring")
  annotation_table_weighted_bio_cleaned <<- clean_bio()

  log_debug(x = "performing chemically informed scoring")
  annotation_table_weighted_chemo <<- weight_chemo()

  decorate_chemo()

  log_debug(x = "cleaning for export")
  results <<- clean_chemo()

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
}
