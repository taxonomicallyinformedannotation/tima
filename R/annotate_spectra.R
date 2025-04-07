#' @title Annotate spectra
#'
#' @description This function annotates spectra
#'
#' @details It takes two files as input.
#'    A query file that will be matched against a library file.
#'
#' @include calculate_entropy_and_similarity.R
#' @include get_params.R
#' @include get_spectra_ids.R
#' @include harmonize_adducts.R
#' @include import_spectra.R
#'
#' @param input Query file containing spectra. Currently an '.mgf' file
#' @param libraries Libraries containing spectra to match against.
#'    Can be '.mgf' or '.sqlite' (Spectra formatted)
#' @param polarity MS polarity. Must be 'pos' or 'neg'.
#' @param output Output file.
#' @param method Similarity method
#' @param threshold Minimal similarity to report
#' @param ppm Relative ppm tolerance to be used
#' @param dalton Absolute Dalton tolerance to be used
#' @param qutoff Intensity under which ms2 fragments will be removed.
#' @param approx Perform matching without precursor match
#'
#' @return NULL
#'
#' @export
#'
#' @examples
#' \dontrun{
#' copy_backbone()
#' go_to_cache()
#' get_file(
#'   url = get_default_paths()$urls$examples$spectra_mini,
#'   export = get_params(step = "annotate_spectra")$files$spectral$raw
#' )
#' get_file(
#'   url = get_default_paths()$urls$examples$spectral_lib_mini$with_rt,
#'   export = get_default_paths()$data$source$libraries$spectra$exp$with_rt
#' )
#' annotate_spectra(
#'   libraries = get_default_paths()$data$source$libraries$spectra$exp$with_rt
#' )
#' unlink("data", recursive = TRUE)
#' }
annotate_spectra <- function(
  input = get_params(step = "annotate_spectra")$files$spectral$raw,
  libraries = get_params(step = "annotate_spectra")$files$libraries$spectral,
  polarity = get_params(step = "annotate_spectra")$ms$polarity,
  output = get_params(
    step = "annotate_spectra"
  )$files$annotations$raw$spectral$spectral,
  method = get_params(
    step = "annotate_spectra"
  )$similarities$methods$annotations,
  threshold = get_params(
    step = "annotate_spectra"
  )$similarities$thresholds$annotations,
  ppm = get_params(step = "annotate_spectra")$ms$tolerances$mass$ppm$ms2,
  dalton = get_params(step = "annotate_spectra")$ms$tolerances$mass$dalton$ms2,
  qutoff = get_params(step = "annotate_spectra")$ms$thresholds$ms2$intensity,
  approx = get_params(step = "annotate_spectra")$annotations$ms2approx
) {
  stopifnot("Your input file does not exist." = file.exists(input))
  stopifnot("Polarity must be 'pos' or 'neg'." = polarity %in% c("pos", "neg"))
  ## Check if libraries exist
  stopifnot(
    "Library file(s) do(es) not exist" = all(
      purrr::map(.x = libraries, .f = file.exists) |>
        unlist()
    )
  )

  ## Not checking for ppm and Da limits, everyone is free.
  if (length(libraries) > 1) {
    libraries <- libraries[grepl(polarity, libraries, fixed = TRUE)]
  }

  logger::log_trace("Loading spectra")
  spectra <- input |>
    import_spectra(
      cutoff = qutoff,
      dalton = dalton,
      polarity = polarity,
      ppm = ppm
    )

  df_empty <- data.frame(
    feature_id = NA,
    candidate_spectrum_entropy = NA,
    candidate_adduct = NA,
    candidate_library = NA,
    candidate_spectrum_id = NA,
    candidate_structure_error_mz = NA,
    candidate_structure_name = NA,
    candidate_structure_inchikey_connectivity_layer = NA,
    candidate_structure_smiles_no_stereo = NA,
    candidate_structure_molecular_formula = NA,
    candidate_structure_exact_mass = NA,
    candidate_structure_xlogp = NA,
    candidate_score_similarity = NA,
    candidate_count_similarity_peaks_matched = NA
  )

  if (length(spectra) > 0) {
    logger::log_trace("Loading spectral libraries")
    spectral_library <- unlist(libraries) |>
      purrr::map(
        .f = import_spectra,
        cutoff = 0,
        dalton = dalton,
        polarity = polarity,
        ppm = ppm,
        sanitize = FALSE,
        combine = FALSE
      ) |>
      purrr::map(
        .f = Spectra::applyProcessing,
        BPPARAM = BiocParallel::SerialParam()
      )
    spectral_library <- spectral_library |>
      purrr::map(.f = function(spectra) {
        spectra <- spectra[
          !spectra@backend@peaksData |>
            purrr::map(.f = is.null) |>
            purrr::map(.f = any) |>
            as.character() |>
            as.logical()
        ]
      }) |>
      Spectra::concatenateSpectra()

    logger::log_info("Annotating using following libraries")
    library_stats <- spectral_library |>
      Spectra::spectraData() |>
      data.frame() |>
      tidytable::filter(!is.na(library)) |>
      tidytable::group_by(library) |>
      tidytable::add_count(name = "spectra") |>
      tidytable::distinct(inchikey_connectivity_layer, .keep_all = TRUE) |>
      tidytable::add_count(name = "unique_connectivities") |>
      tidytable::ungroup() |>
      tidytable::select(library, spectra, unique_connectivities) |>
      tidytable::distinct() |>
      ## temporary fix
      tidytable::mutate(
        unique_connectivities = tidytable::case_when(
          library == "ISDB - Wikidata" ~ spectra,
          ## solved now (using <10.5281/zenodo.11566051> instead of <10.5281/zenodo.11193898>)
          # library == "gnps" ~ NA_integer_,
          TRUE ~ unique_connectivities
        )
      ) |>
      tidytable::arrange(
        spectra |>
          tidytable::desc()
      )
    logger::log_info(
      "\n{paste(capture.output(print.data.frame(library_stats, row.names = FALSE)), collapse = '\n')}"
    )

    query_precursors <- spectra@backend@spectraData$precursorMz
    query_spectra <- spectra@backend@peaksData
    query_ids <- spectra |>
      get_spectra_ids()
    rm(spectra)

    ## Fix needed
    lib_precursors <- spectral_library@backend@spectraData |>
      tidytable::transmute(
        precursor = tidytable::coalesce(tidytable::across(tidyselect::any_of(
          c("precursorMz", "precursor_mz")
        )))
      ) |>
      tidytable::pull()
    minimal <- pmin(
      lib_precursors - dalton,
      lib_precursors * (1 - (10^-6 * ppm))
    )
    maximal <- pmax(
      lib_precursors + dalton,
      lib_precursors * (1 + (10^-6 * ppm))
    )

    if (approx == FALSE) {
      logger::log_trace("Reducing library size")
      df_3 <- dplyr::inner_join(
        tidytable::tidytable(minimal, maximal, lib_precursors),
        tidytable::tidytable(val = unique(query_precursors)),
        by = dplyr::join_by(minimal <= val, maximal >= val)
      ) |>
        tidytable::distinct(minimal, .keep_all = TRUE)

      spectral_library <-
        spectral_library[lib_precursors %in% df_3$lib_precursors]

      ## Fix needed
      lib_precursors <- spectral_library@backend@spectraData |>
        tidytable::transmute(
          precursor = tidytable::coalesce(tidytable::across(tidyselect::any_of(
            c("precursorMz", "precursor_mz")
          )))
        ) |>
        tidytable::pull()
      minimal <- pmin(
        lib_precursors - dalton,
        lib_precursors * (1 - (10^-6 * ppm))
      )
      maximal <- pmax(
        lib_precursors + dalton,
        lib_precursors * (1 + (10^-6 * ppm))
      )
      rm(df_3)
    }

    lib_ids <- seq_along(spectral_library)
    lib_spectra <- spectral_library@backend@peaksData
    safety <- lib_spectra[purrr::map(.x = lib_spectra, .f = length) != 0]
    if (length(safety) != 0) {
      logger::log_trace("Annotating")
      df_final <-
        calculate_entropy_and_similarity(
          lib_ids = lib_ids,
          lib_precursors = lib_precursors,
          lib_spectra = lib_spectra,
          query_ids = query_ids,
          query_precursors = query_precursors,
          query_spectra = query_spectra,
          method = method,
          dalton = dalton,
          ppm = ppm,
          threshold = threshold,
          approx = approx
        ) |>
        tidytable::as_tidytable()

      lib_adduct <- spectral_library@backend@spectraData$adduct
      if (is.null(lib_adduct)) {
        lib_adduct <- rep(NA_character_, length(spectral_library))
      }
      # lib_collision_energy <- spectral_library@backend@spectraData$collision_energy
      # if (is.null(lib_collision_energy)) {
      #   lib_collision_energy <- rep(NA_character_, length(spectral_library))
      # }
      lib_inchikey <- spectral_library@backend@spectraData$inchikey
      if (is.null(lib_inchikey)) {
        lib_inchikey <- rep(NA_character_, length(spectral_library))
      }
      lib_inchikey2D <-
        spectral_library@backend@spectraData$inchikey_2D
      if (is.null(lib_inchikey2D)) {
        lib_inchikey2D <- rep(NA_character_, length(spectral_library))
      }
      lib_smiles <- spectral_library@backend@spectraData$smiles
      if (is.null(lib_smiles)) {
        lib_smiles <- rep(NA_character_, length(spectral_library))
      }
      lib_smiles2D <- spectral_library@backend@spectraData$smiles_2D
      if (is.null(lib_smiles2D)) {
        lib_smiles2D <- rep(NA_character_, length(spectral_library))
      }
      lib_library <- spectral_library@backend@spectraData$library
      if (is.null(lib_library)) {
        lib_library <- rep(NA_character_, length(spectral_library))
      }
      lib_mf <- spectral_library@backend@spectraData$formula
      if (is.null(lib_mf)) {
        lib_mf <- rep(NA_character_, length(spectral_library))
      }
      lib_mass <- spectral_library@backend@spectraData$exactmass
      if (is.null(lib_mass)) {
        lib_mass <- rep(NA_real_, length(spectral_library))
      }
      lib_name <- spectral_library@backend@spectraData$name
      if (is.null(lib_name)) {
        lib_name <- rep(NA_character_, length(spectral_library))
      }
      # lib_splash <- spectral_library@backend@spectraData$splash
      # if (is.null(lib_splash)) {
      #   lib_splash <- rep(NA_character_, length(spectral_library))
      # }
      lib_spectrum_id <- spectral_library@backend@spectraData$spectrum_id
      if (is.null(lib_spectrum_id)) {
        lib_spectrum_id <- rep(NA_character_, length(spectral_library))
      }
      lib_xlogp <- spectral_library@backend@spectraData$xlogp
      if (is.null(lib_xlogp)) {
        lib_xlogp <- rep(NA_real_, length(spectral_library))
      }
      rm(spectral_library)

      df_meta <- tidytable::tidytable(
        "target_id" = lib_ids,
        "target_adduct" = lib_adduct,
        "target_spectrum_id" = lib_spectrum_id,
        "target_inchikey" = lib_inchikey,
        "target_inchikey_connectivity_layer" = lib_inchikey2D,
        "target_smiles" = lib_smiles,
        "target_smiles_no_stereo" = lib_smiles2D,
        "target_library" = lib_library,
        "target_formula" = lib_mf,
        "target_exactmass" = lib_mass,
        "target_name" = lib_name,
        "target_xlogp" = lib_xlogp,
        "target_precursorMz" = lib_precursors
      )
      df_meta <- df_meta |>
        harmonize_adducts(
          adducts_colname = "target_adduct",
          adducts_translations = adducts_translations
        )
      rm(lib_precursors)

      df_final$candidate_spectrum_entropy <- as.numeric(
        df_final$candidate_spectrum_entropy
      )
      df_final$candidate_score_similarity <- as.numeric(
        df_final$candidate_score_similarity
      )
      df_final$candidate_count_similarity_peaks_matched <- as.integer(
        df_final$candidate_count_similarity_peaks_matched
      )

      df_final <- df_final |>
        tidytable::left_join(df_meta) |>
        tidytable::select(-target_id)

      df_final <- df_final |>
        tidytable::mutate(
          candidate_structure_error_mz = target_precursorMz - precursorMz,
          candidate_structure_inchikey_connectivity_layer = tidytable::if_else(
            condition = is.na(target_inchikey_connectivity_layer),
            true = target_inchikey |>
              gsub(
                pattern = "-.*",
                replacement = "",
                perl = TRUE
              ),
            false = target_inchikey_connectivity_layer
          ),
          candidate_structure_smiles_no_stereo = tidytable::coalesce(
            target_smiles_no_stereo,
            target_smiles
          )
        ) |>
        tidytable::select(tidyselect::any_of(
          c(
            "feature_id",
            "candidate_adduct" = "target_adduct",
            "candidate_library" = "target_library",
            "candidate_spectrum_id" = "target_spectrum_id",
            "candidate_structure_error_mz",
            "candidate_structure_name" = "target_name",
            "candidate_structure_inchikey_connectivity_layer",
            "candidate_structure_smiles_no_stereo",
            "candidate_structure_molecular_formula" = "target_formula",
            "candidate_structure_exact_mass" = "target_exactmass",
            "candidate_structure_xlogp" = "target_xlogp",
            "candidate_spectrum_entropy",
            "candidate_score_similarity",
            "candidate_count_similarity_peaks_matched"
          )
        ))

      ## COMMENT AR: Not doing it because of thresholding
      ## df_final[is.na(df_final)] <- 0

      logger::log_trace("Filtering results above threshold only")
      df_final <- df_final |>
        tidytable::filter(candidate_score_similarity >= threshold) |>
        tidytable::arrange(candidate_score_similarity |> tidytable::desc()) |>
        ## keep only the best result (per library for now)
        tidytable::distinct(
          feature_id,
          candidate_library,
          candidate_structure_inchikey_connectivity_layer,
          .keep_all = TRUE
        )

      logger::log_info(
        nrow(
          df_final |>
            ## else doesn't work if some are empty
            tidytable::distinct(
              candidate_structure_inchikey_connectivity_layer,
              candidate_structure_smiles_no_stereo
            )
        ),
        " Candidates were annotated on ",
        nrow(
          df_final |>
            tidytable::distinct(feature_id)
        ),
        " features, with at least ",
        threshold,
        " similarity score."
      )
      if (nrow(df_final) == 0) {
        logger::log_warn(
          "No spectra were matched, returning an empty dataframe"
        )
        df_final <- df_empty
      }
    } else {
      logger::log_warn(
        "No spectra left in the library, returning an empty dataframe"
      )
      df_final <- df_empty
    }
    rm(
      query_precursors,
      query_spectra,
      query_ids,
      minimal,
      maximal
    )
  } else {
    logger::log_warn(
      "No spectra matched the given polarity, returning an empty dataframe"
    )
    df_final <- df_empty
  }

  export_params(
    parameters = get_params(step = "annotate_spectra"),
    step = "annotate_spectra"
  )
  export_output(x = df_final, file = output[[1]])
  rm(df_final)

  return(output[[1]])
}
