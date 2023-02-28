#' @title Process spectra
#'
#' @description This function performs spectral matching.
#'
#' @details It takes two files as input. A query file that will be matched against a library file.
#'    Multiple comparison distances are available
#'    ('gnps', 'navdist','ndotproduct','neuclidean', 'nspectraangle' (See MsCoreUtils for details)).
#'    Number of matched peaks and their ratio are also available (See MatchForwardReverseParam for details).
#'    Parallel processing is also made available.
#'
#' @param input Query file containing spectra. Currently an '.mgf' file
#' @param library Library containing spectra to match against. Can be '.mgf' or '.sqlite' (Spectra formatted)
#' @param polarity MS polarity. Must be 'pos' or 'neg'.
#' @param output Output file.
#' @param method Method to be used to perform spectral comparison
#' @param threshold Minimal similarity to report
#' @param ppm Relative ppm tolerance to be used
#' @param dalton Absolute Dalton tolerance to be used
#' @param npeaks Absolute minimum number of peaks to be matched
#' @param rpeaks Relative minimum number of peaks to be matched
#' @param condition Condition to be fulfilled. Either 'OR' or 'AND' (mass and peaks minima).
#' @param qutoff Intensity under which ms2 fragments will be removed previous to comparison.
#' @param parallel Boolean. Process in parallel
#' @param parameters Params
#'
#' @return NULL
#'
#' @export
#'
#' @importFrom BiocParallel MulticoreParam SerialParam
#' @importFrom dplyr full_join left_join select
#' @importFrom MetaboAnnotation CompareSpectraParam MatchForwardReverseParam matchSpectra
#' @importFrom MsCoreUtils gnps navdist ndotproduct neuclidean nspectraangle
#' @importFrom Spectra filterIntensity joinPeaksGnps
#'
#' @examples NULL
process_spectra <- function(input = params$files$spectral$raw,
                            library = params$files$libraries$spectral,
                            polarity = params$ms$polarity,
                            output = params$files$annotations$raw$spectral,
                            method = params$annotations$ms2$method,
                            threshold = params$annotations$ms2$thresholds$similarity,
                            ppm = params$ms$tolerances$mass$ppm$ms2,
                            dalton = params$ms$tolerances$mass$dalton$ms2,
                            npeaks = params$annotations$ms2$thresholds$peaks$absolute,
                            rpeaks = params$annotations$ms2$thresholds$peaks$ratio,
                            condition = params$annotations$ms2$thresholds$condition,
                            qutoff = params$ms$intensity$thresholds$ms2,
                            parallel = params$options$parallel,
                            fast = params$options$fast,
                            approx = params$annotations$ms2$approx,
                            parameters = params) {
  stopifnot("Your input file does not exist." = file.exists(input))
  stopifnot("Polarity must be 'pos' or 'neg'." = polarity %in% c("pos", "neg"))
  params <<- parameters
  if (length(library) > 1) {
    library <- library[[polarity]]
  }
  stopifnot("Your library file does not exist." = file.exists(library))
  stopifnot(
    "Your similarity is not supported, supported similarities are 'gnps', 'navdist', 'ndotproduct', 'neuclidean', 'nspectraangle'" = method %in%
      c(
        "gnps",
        "navdist",
        "ndotproduct",
        "neuclidean",
        "nspectraangle"
      )
  )
  ## Not checking for ppm and Da limits, everyone is free.
  par <- if (parallel) {
    BiocParallel::MulticoreParam()
  } else {
    BiocParallel::SerialParam()
  }

  log_debug("Loading spectra...")
  spectra <- input |>
    import_spectra() |>
    Spectra::filterPrecursorCharge(z = if (polarity == "pos") {
      c(1, 2, 3)
    } else {
      c(-1, -2, -3)
    })

  if (length(spectra) > 0) {
    log_debug("Loading spectral library")
    ## COMMENT (AR): TODO Try HDF5 formatted?
    spectral_library <- library |>
      import_spectra()

    sim_fun <- switch(
      EXPR = method,
      "gnps" = MsCoreUtils::gnps,
      "navdist" = MsCoreUtils::navdist,
      "ndotproduct" = MsCoreUtils::ndotproduct,
      "neuclidean" = MsCoreUtils::neuclidean,
      "nspectraangle" = MsCoreUtils::nspectraangle
    )

    ## COMMENT (AR): TODO export as param, same for precursor
    if (fast) {
      params_sim <- MetaboAnnotation::CompareSpectraParam(
        ppm = ppm,
        tolerance = dalton,
        MAPFUN = Spectra::joinPeaksGnps,
        FUN = sim_fun,
        requirePrecursor = ifelse(
          test = approx,
          yes = FALSE,
          no = TRUE
        ),
        THRESHFUN = function(x) {
          which(x >= threshold)
        },
        BPPARAM = par
      )
    } else {
      params_sim <- MetaboAnnotation::MatchForwardReverseParam(
        ppm = ppm,
        tolerance = dalton,
        MAPFUN = Spectra::joinPeaksGnps,
        FUN = sim_fun,
        requirePrecursor = ifelse(
          test = approx,
          yes = FALSE,
          no = TRUE
        ),
        THRESHFUN = function(x) {
          which(x >= threshold)
        },
        BPPARAM = par
      )
    }

    ## COMMENT (AR): TODO Maybe implement some safety sanitization of the spectra?
    ## Can be very slow otherwise

    log_debug("Applying initial intensity filter to query spectra")
    spectra <- spectra |>
      Spectra::filterIntensity(intensity = c(qutoff, Inf))

    log_debug("Performing spectral comparison")
    log_debug(
      "If you do not need the number/ratio of matched peaks,
    computation can be much faster by setting the parameter fast to TRUE"
    )
    log_debug(
      "If you need it, the score threshold greatly impacts speed.
    A higher threshold will lead to faster results."
    )
    matches_sim <- MetaboAnnotation::matchSpectra(
      query = spectra,
      target = spectral_library,
      param = params_sim
    )

    df_full <- matches_sim@matches |>
      dplyr::rename(msms_score = score)

    if (condition == "AND") {
      df_full <- df_full |>
        dplyr::filter(msms_score >= threshold &
          matched_peaks_count >= npeaks &
          presence_ratio >= rpeaks)
    }

    ## COMMENT AR: Not doing it because of thresholding
    # df_full[is.na(df_full)] <- 0

    spectral_library_extracted <-
      spectral_library[spectral_library@backend@spectraIds %in% matches_sim@matches[["target_idx"]]] |>
      extract_spectra()
    target_idx <- spectral_library_extracted$spectrum_id
    short_inchikey <- spectral_library_extracted$name
    smiles <- spectral_library_extracted$smiles
    molecular_formula <- spectral_library_extracted$formula
    exact_mass <- spectral_library_extracted$exactmass

    df_meta <- data.frame(
      target_idx,
      short_inchikey,
      smiles,
      molecular_formula,
      exact_mass
    )

    spectra_ids <- spectra |>
      extract_spectra() |>
      dplyr::mutate(query_idx = dplyr::row_number()) |>
      dplyr::distinct(feature_id = acquisitionNum, query_idx)

    df_final <- df_full |>
      dplyr::left_join(df_meta) |>
      dplyr::left_join(spectra_ids) |>
      dplyr::select(-target_idx, -query_idx) |>
      dplyr::relocate(feature_id, .before = msms_score)

    log_debug(
      nrow(df_final |>
        dplyr::distinct(short_inchikey)),
      "Candidates were annotated on",
      nrow(df_final |>
        dplyr::distinct(feature_id)),
      "features, with at least",
      threshold,
      "similarity score",
      condition,
      "at least",
      npeaks,
      "(absolute)",
      condition,
      rpeaks,
      "(relative) matched peaks."
    )
    if (nrow(df_final) == 0) {
      log_debug("No spectra were matched, returning an empty dataframe")
      df_final <-
        data.frame(
          feature_id = NA,
          msms_score = NA,
          reverse_score = NA,
          presence_ratio = NA,
          matched_peaks_count = NA,
          short_inchikey = NA,
          smiles = NA,
          molecular_formula = NA,
          exact_mass = NA
        )
    }
  } else {
    log_debug("No spectra matched the given polarity, returning an empty dataframe")
    df_final <-
      data.frame(
        feature_id = NA,
        msms_score = NA,
        reverse_score = NA,
        presence_ratio = NA,
        matched_peaks_count = NA,
        short_inchikey = NA,
        smiles = NA,
        molecular_formula = NA,
        exact_mass = NA
      )
  }

  export_params(step = "process_spectra")
  export_output(x = df_final, file = output[[1]])

  return(output[[1]])
}
