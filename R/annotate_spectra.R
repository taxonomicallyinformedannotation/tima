utils::globalVariables(
  c(
    "acquisitionNum",
    "count_peaks_matched",
    "feature_id",
    "params",
    "precursorMz",
    "presence_ratio",
    "score",
    "SLAW_ID",
    "structure_inchikey_2D",
    "structure_smiles_2D",
    "target_id",
    "target_inchikey",
    "target_inchikey_2D",
    "target_precursorMz",
    "target_rtime",
    "target_smiles",
    "target_smiles_2D",
    "val"
  )
)

#' @title Annotate spectra
#'
#' @description This function annotates spectra
#'
#' @details It takes two files as input.
#'    A query file that will be matched against a library file.
#'
#' @include export_output.R
#' @include export_params.R
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
#' @param parameters Params
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
annotate_spectra <- function(input = params$files$spectral$raw,
                             library = params$files$libraries$spectral$exp,
                             polarity = params$ms$polarity,
                             output = params$files$annotations$raw$spectral,
                             threshold =
                               params$annotations$ms2$thresholds$similarity,
                             ppm = params$ms$tolerances$mass$ppm$ms2,
                             dalton = params$ms$tolerances$mass$dalton$ms2,
                             qutoff = params$ms$intensity$thresholds$ms2,
                             approx = params$annotations$ms2$approx,
                             parameters = params) {
  stopifnot("Your input file does not exist." = file.exists(input))
  stopifnot("Polarity must be 'pos' or 'neg'." = polarity %in% c("pos", "neg"))
  ## Check if library file(s) exists
  stopifnot(
    "Library file(s) do(es) not exist" =
      rep(TRUE, length(unlist(library))) ==
        lapply(X = unlist(library), file.exists)
  )

  ## Not checking for ppm and Da limits, everyone is free.

  params <<- parameters
  if (length(library) > 1) {
    library <- library[[polarity]]
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
    error_mz = NA,
    structure_name = NA,
    structure_inchikey_2D = NA,
    structure_smiles_2D = NA,
    structure_molecular_formula = NA,
    structure_exact_mass = NA,
    structure_xlogp = NA,
    score = NA,
    reverse_score = NA,
    presence_ratio = NA,
    count_peaks_matched = NA
  )

  if (length(spectra) > 0) {
    log_debug("Loading spectral library")
    spectral_library <- lapply(library, import_spectra) |>
      Spectra::concatenateSpectra() |>
      sanitize_spectra() |>
      Spectra::addProcessing(remove_above_precursor(),
        spectraVariables = c("precursorMz")
      ) |>
      Spectra::addProcessing(normalize_peaks()) |>
      Spectra::applyProcessing()

    ## COMMENT (AR): TODO Maybe implement sanitization of the spectra?
    ## Can be very slow otherwise

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

      df_1 <- tidytable::tidytable(minimal, maximal, lib_precursors)
      df_2 <- tidytable::tidytable(val = unique(query_precursors))

      df_3 <- dplyr::inner_join(df_1,
        df_2,
        by = dplyr::join_by(
          minimal < val,
          maximal > val
        )
      ) |>
        tidytable::distinct(minimal, .keep_all = TRUE)

      spectral_library <-
        spectral_library[lib_precursors %in% df_3$lib_precursors]
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

    lib_id <- spectral_library@backend@spectraData$spectrum_id
    lib_spectra <- spectral_library@backend@peaksData

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

                  if (score >= 0.1) {
                    list(
                      "feature_id" = query_ids[[spectrum]],
                      "precursorMz" = precursor,
                      "target_id" = lib_id[indices][[index]],
                      "score" = as.numeric(score),
                      "count_peaks_matched" = NA_integer_,
                      "reverse_score" = NA_real_,
                      "presence_ratio" = NA_real_
                    )
                  }
                }
              ) |>
              Filter(f = Negate(is.null)) |>
              dplyr::bind_rows()

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
          dplyr::bind_rows()

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
    lib_name <- spectral_library@backend@spectraData$name
    if (is.null(lib_name)) {
      lib_name <- rep(NA_character_, length(spectral_library))
    }
    lib_mf <- spectral_library@backend@spectraData$formula
    if (is.null(lib_mf)) {
      lib_mf <- rep(NA_character_, length(spectral_library))
    }
    lib_mass <- spectral_library@backend@spectraData$exactmass
    if (is.null(lib_mass)) {
      lib_mass <- rep(NA_real_, length(spectral_library))
    }
    lib_xlogp <- spectral_library@backend@spectraData$xlogp
    if (is.null(lib_xlogp)) {
      lib_xlogp <- rep(NA_real_, length(spectral_library))
    }

    df_meta <- tidytable::tidytable(
      "target_id" = lib_id,
      "target_inchikey" = lib_inchikey,
      "target_inchikey_2D" = lib_inchikey2D,
      "target_smiles" = lib_smiles,
      "target_smiles_2D" = lib_smiles2D,
      "target_name" = lib_name,
      "target_formula" = lib_mf,
      "target_exactmass" = lib_mass,
      "target_xlogp" = lib_xlogp,
      "target_precursorMz" = lib_precursors
    )
    df_final <- df_final |>
      tidytable::left_join(df_meta) |>
      tidytable::select(-target_id)

    df_final <- df_final |>
      tidytable::rowwise() |>
      tidytable::mutate(
        error_mz = target_precursorMz - precursorMz,
        structure_inchikey_2D = ifelse(
          test = is.na(target_inchikey_2D),
          yes = target_inchikey |>
            gsub(
              pattern = "-.*",
              replacement = ""
            ),
          no = target_inchikey_2D
        ),
        structure_smiles_2D = tidytable::coalesce(
          target_smiles_2D,
          target_smiles
        )
      ) |>
      tidytable::select(tidytable::any_of(
        c(
          "feature_id",
          "error_mz",
          "structure_name" = "target_name",
          "structure_inchikey_2D",
          "structure_smiles_2D",
          "structure_molecular_formula" = "target_formula",
          "structure_exact_mass" = "target_exactmass",
          "structure_xlogp" = "target_xlogp",
          "score",
          "reverse_score",
          "presence_ratio",
          "count_peaks_matched"
        )
      ))

    ## COMMENT AR: Not doing it because of thresholding
    ## df_final[is.na(df_final)] <- 0

    log_debug("Filtering results above threshold only...")
    df_final <- df_final |>
      tidytable::filter(score >= threshold)

    log_debug(
      nrow(
        df_final |>
          ## else doesn't work if some are empty
          tidytable::distinct(structure_inchikey_2D, structure_smiles_2D)
      ),
      "Candidates were annotated on",
      nrow(df_final |>
        tidytable::distinct(feature_id)),
      "features, with at least",
      threshold,
      "similarity score."
    )
    if (nrow(df_final) == 0) {
      log_debug("No spectra were matched, returning an empty dataframe")
      df_final <- df_empty
    }
  } else {
    log_debug("No spectra matched the given polarity,
              returning an empty dataframe")
    df_final <- df_empty
  }

  export_params(step = "annotate_spectra")
  export_output(x = df_final, file = output[[1]])

  return(output[[1]])
}
