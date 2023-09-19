#' @title Annotate spectra
#'
#' @description This function annotates spectra
#'
#' @details It takes two files as input.
#'    A query file that will be matched against a library file.
#'
#' @include import_spectra.R
#'
#' @param input Query file containing spectra. Currently an '.mgf' file
#' @param library Library containing spectra to match against.
#'    Can be '.mgf' or '.sqlite' (Spectra formatted)
#' @param polarity MS polarity. Must be 'pos' or 'neg'.
#' @param output Output file.
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
#' @examples NULL
annotate_spectra <- function(input = get_params(step = "annotate_spectra")$files$spectral$raw,
                             library = get_params(step = "annotate_spectra")$files$libraries$spectral$exp,
                             polarity = get_params(step = "annotate_spectra")$ms$polarity,
                             output = get_params(step = "annotate_spectra")$files$annotations$raw$spectral,
                             threshold =
                               get_params(step = "annotate_spectra")$annotations$thresholds$ms2$similarity,
                             ppm = get_params(step = "annotate_spectra")$ms$tolerances$mass$ppm$ms2,
                             dalton = get_params(step = "annotate_spectra")$ms$tolerances$mass$dalton$ms2,
                             qutoff = get_params(step = "annotate_spectra")$ms$thresholds$ms2$intensity,
                             approx = get_params(step = "annotate_spectra")$annotations$ms2approx) {
  stopifnot("Your input file does not exist." = file.exists(input))
  stopifnot("Polarity must be 'pos' or 'neg'." = polarity %in% c("pos", "neg"))
  ## Check if library file(s) exists
  stopifnot(
    "Library file(s) do(es) not exist" = rep(TRUE, length(unlist(library))) == lapply(X = unlist(library), FUN = file.exists)
  )

  ## Not checking for ppm and Da limits, everyone is free.

  if (length(library) > 1) {
    library <- library[grepl(polarity, library)]
  }

  log_debug("Loading spectra...")
  spectra <- input |>
    import_spectra() |>
    Spectra::filterPrecursorCharge(z = if (polarity == "pos") {
      c(1, 2, 3)
    } else {
      c(-1, -2, -3)
    })

  df_empty <- data.frame(
    feature_id = NA,
    candidate_spectrum_entropy = NA,
    candidate_library = NA,
    candidate_structure_error_mz = NA,
    candidate_structure_name = NA,
    candidate_structure_inchikey_no_stereo = NA,
    candidate_structure_smiles_no_stereo = NA,
    candidate_structure_molecular_formula = NA,
    candidate_structure_exact_mass = NA,
    candidate_structure_xlogp = NA,
    candidate_score_similarity = NA,
    candidate_count_similarity_peaks_matched = NA
  )

  if (length(spectra) > 0) {
    log_debug("Loading spectral library")
    spectral_library <- unlist(library) |>
      lapply(FUN = import_spectra) |>
      Spectra::concatenateSpectra() |>
      sanitize_spectra() |>
      Spectra::addProcessing(remove_above_precursor(),
        spectraVariables = c("precursorMz")
      ) |>
      Spectra::addProcessing(normalize_peaks()) |>
      Spectra::applyProcessing()

    log_debug("Applying initial intensity filter to query spectra")
    spectra <- spectra |>
      sanitize_spectra() |>
      Spectra::filterIntensity(intensity = c(qutoff, Inf)) |>
      Spectra::addProcessing(remove_above_precursor(),
        spectraVariables = c("precursorMz")
      ) |>
      Spectra::addProcessing(normalize_peaks()) |>
      Spectra::applyProcessing()

    query_precursors <- spectra@backend@spectraData$precursorMz
    query_spectra <- spectra@backend@peaksData
    ## TODO find a way to have consistency in spectrum IDs
    query_ids <- spectra@backend@spectraData$acquisitionNum
    if (is.null(query_ids)) {
      query_ids <- spectra@backend@spectraData$spectrum_id
    }

    if (approx == FALSE) {
      log_debug("Reducing library size...")
      lib_precursors <-
        spectral_library@backend@spectraData$precursorMz

      minimal <- pmin(
        lib_precursors - dalton,
        lib_precursors * (1 - (10^-6 * ppm))
      )
      maximal <- pmax(
        lib_precursors + dalton,
        lib_precursors * (1 + (10^-6 * ppm))
      )

      df_3 <- dplyr::inner_join(
        tidytable::tidytable(minimal, maximal, lib_precursors),
        tidytable::tidytable(val = unique(query_precursors)),
        by = dplyr::join_by(
          minimal < val,
          maximal > val
        )
      ) |>
        tidytable::distinct(minimal, .keep_all = TRUE)

      spectral_library <-
        spectral_library[lib_precursors %in% df_3$lib_precursors]
      rm(df_3)
    }

    lib_precursors <-
      spectral_library@backend@spectraData$precursorMz
    minimal <- pmin(
      lib_precursors - dalton,
      lib_precursors * (1 - (10^-6 * ppm))
    )
    maximal <- pmax(
      lib_precursors + dalton,
      lib_precursors * (1 + (10^-6 * ppm))
    )

    lib_id <- seq_along(spectral_library)
    spectral_library$spectrum_id <- lib_id
    lib_spectra <- spectral_library@backend@peaksData
    safety <- lib_spectra[lapply(X = lib_spectra, length) != 0]
    if (length(safety) != 0) {
      calculate_entropy_score <-
        function(spectra,
                 spectral_library,
                 dalton,
                 ppm) {
          calculate_score_and_create_inner_list <-
            function(spectrum,
                     precursor,
                     spectral_lib,
                     query_ids,
                     query_spectra,
                     lib_id,
                     minimal,
                     maximal,
                     daz = dalton,
                     ppmz = ppm) {
              indices <- minimal <= precursor & precursor <= maximal
              spectral_lib <- lib_spectra[indices]

              inner_list <-
                lapply(
                  X = seq_along(spectral_lib),
                  FUN = function(index,
                                 sp = spectrum,
                                 spectra = query_spectra,
                                 lib = spectral_lib,
                                 dalton = daz,
                                 ppm = ppmz) {
                    score <- msentropy::calculate_entropy_similarity(
                      peaks_a = spectra[[sp]],
                      peaks_b = lib[[index]],
                      min_mz = 0,
                      max_mz = 5000,
                      noise_threshold = 0,
                      ms2_tolerance_in_da = dalton,
                      ms2_tolerance_in_ppm = ppm,
                      max_peak_num = -1,
                      clean_spectra = TRUE
                    )
                    ## more efficient to do it when creating edges
                    # entropy_query <- spectra[[sp]] |>
                    #   msentropy::calculate_spectral_entropy()
                    entropy_target <- lib[[index]] |>
                      msentropy::calculate_spectral_entropy()

                    ## number of matched peaks (only Da for now)
                    matched <- sum(
                      abs(
                        outer(
                          X = spectra[[sp]],
                          Y = lib[[index]],
                          FUN = "-"
                        )
                      ) <= dalton
                    )

                    tidytable::tidytable(
                      "feature_id" = query_ids[[spectrum]],
                      # "feature_spectrum_entropy" = entropy_query,
                      "precursorMz" = precursor,
                      "target_id" = lib_id[indices][[index]],
                      "candidate_spectrum_entropy" = entropy_target,
                      "candidate_score_similarity" = as.numeric(score),
                      "candidate_count_similarity_peaks_matched" = matched
                    )
                  }
                )

              return(inner_list)
            }

          log_debug("Performing spectral comparison")
          outer_list <-
            pbapply::pblapply(
              X = seq_along(spectra),
              FUN = function(spectrum, qp = query_precursors) {
                precursor <- qp[spectrum]
                calculate_score_and_create_inner_list(
                  spectrum = spectrum,
                  precursor = precursor,
                  spectral_lib = lib_spectra,
                  query_ids = query_ids,
                  query_spectra = query_spectra,
                  lib_id = lib_id,
                  minimal = minimal,
                  maximal = maximal
                )
              }
            ) |>
            tidytable::bind_rows()

          return(outer_list)
        }

      df_final <-
        calculate_entropy_score(
          spectra = spectra,
          spectral_library = spectral_library,
          dalton = dalton,
          ppm = ppm
        ) |>
        tidytable::as_tidytable()
      rm(spectra)

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
      lib_xlogp <- spectral_library@backend@spectraData$xlogp
      if (is.null(lib_xlogp)) {
        lib_xlogp <- rep(NA_real_, length(spectral_library))
      }
      rm(spectral_library)

      df_meta <- tidytable::tidytable(
        "target_id" = lib_id,
        "target_inchikey" = lib_inchikey,
        "target_inchikey_no_stereo" = lib_inchikey2D,
        "target_smiles" = lib_smiles,
        "target_smiles_no_stereo" = lib_smiles2D,
        "target_library" = lib_library,
        "target_formula" = lib_mf,
        "target_exactmass" = lib_mass,
        "target_name" = lib_name,
        "target_xlogp" = lib_xlogp,
        "target_precursorMz" = lib_precursors
      )
      rm(lib_precursors)
      df_final <- df_final |>
        tidytable::left_join(df_meta) |>
        tidytable::select(-target_id)

      df_final <- df_final |>
        tidytable::mutate(
          candidate_structure_error_mz = target_precursorMz - precursorMz,
          candidate_structure_inchikey_no_stereo = ifelse(
            test = is.na(target_inchikey_no_stereo),
            yes = target_inchikey |>
              gsub(
                pattern = "-.*",
                replacement = ""
              ),
            no = target_inchikey_no_stereo
          ),
          candidate_structure_smiles_no_stereo = tidytable::coalesce(
            target_smiles_no_stereo,
            target_smiles
          )
        ) |>
        tidytable::select(tidytable::any_of(
          c(
            "feature_id",
            "candidate_library" = "target_library",
            "candidate_structure_error_mz",
            "candidate_structure_name" = "target_name",
            "candidate_structure_inchikey_no_stereo",
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

      log_debug("Filtering results above threshold only...")
      df_final <- df_final |>
        tidytable::filter(candidate_score_similarity >= threshold) |>
        tidytable::arrange(tidytable::desc(candidate_score_similarity)) |>
        ## keep only the best result (per library for now)
        tidytable::distinct(
          feature_id,
          candidate_library,
          candidate_structure_inchikey_no_stereo,
          .keep_all = TRUE
        )

      log_debug(
        nrow(
          df_final |>
            ## else doesn't work if some are empty
            tidytable::distinct(
              candidate_structure_inchikey_no_stereo,
              candidate_structure_smiles_no_stereo
            )
        ),
        "Candidates were annotated on",
        nrow(df_final |> tidytable::distinct(feature_id)),
        "features, with at least",
        threshold,
        "similarity score."
      )
      if (nrow(df_final) == 0) {
        log_debug("No spectra were matched, returning an empty dataframe")
        df_final <- df_empty
      }
    } else {
      log_debug("No spectra left in the library,
              returning an empty dataframe")
      df_final <- df_empty
    }
    rm(query_precursors, query_spectra, query_ids, minimal, maximal)
  } else {
    log_debug("No spectra matched the given polarity,
              returning an empty dataframe")
    df_final <- df_empty
  }

  export_params(parameters = get_params(step = "annotate_spectra"), step = "annotate_spectra")
  export_output(x = df_final, file = output[[1]])
  rm(df_final)

  return(output[[1]])
}
