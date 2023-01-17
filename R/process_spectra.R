#' @title Process spectra
#'
#' @description This function performs spectral matching.
#'
#' @details It takes two files as input. A query file that will be matched against a library file.
#'    Multiple comparison distances are available
#'    ('gnps', 'navdist','ndotproduct','neuclidean', 'nspectraangle' (See MsCoreUtils for details)).
#'    Number of matched peaks and their ratio are also available.
#'    Parallel processing is also made available.
#'
#' @param input Query file containing spectra. Currently an '.mgf' file
#' @param library Library containing spectra to match against. Can be '.mgf' or '.sqlite' (Spectra formatted)
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
#'
#' @return NULL
#'
#' @export
#'
#' @importFrom BiocParallel MulticoreParam SerialParam
#' @importFrom dplyr full_join left_join select
#' @importFrom MetaboAnnotation CompareSpectraParam matchSpectra
#' @importFrom MsCoreUtils gnps navdist ndotproduct neuclidean nspectraangle
#' @importFrom Spectra filterIntensity
#'
#' @examples NULL
process_spectra <- function(input = params$input,
                            library = params$library,
                            output = params$output,
                            method = params$ms$similarity$method,
                            threshold = params$ms$similarity$threshold,
                            ppm = params$ms$tolerance$ppm,
                            dalton = params$ms$tolerance$dalton,
                            npeaks = params$ms$peaks$absolute,
                            rpeaks = params$ms$peaks$ratio,
                            condition = params$ms$condition,
                            qutoff = params$qutoff,
                            parallel = params$parallel) {
  stopifnot("Your input file does not exist." = file.exists(input))
  if (file.exists(library |>
    gsub(
      pattern = ".mgf",
      replacement = ".sqlite",
      fixed = TRUE
    ))) {
    library <- library |>
      gsub(
        pattern = ".mgf",
        replacement = ".sqlite",
        fixed = TRUE
      )
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

  log_debug("Loading spectra")
  spectra <- input |>
    import_spectra()

  log_debug("Loading spectral library (Can take long)")
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

  params_sim <- MetaboAnnotation::CompareSpectraParam(
    ppm = ppm,
    tolerance = dalton,
    FUN = sim_fun,
    requirePrecursor = FALSE,
    THRESHFUN = function(x) {
      which(x >= threshold)
    },
    BPPARAM = par
  )

  params_left <- MetaboAnnotation::CompareSpectraParam(
    ppm = ppm,
    tolerance = dalton,
    FUN = function(x, y, ...) {
      nrow(x)
    },
    type = "left",
    requirePrecursor = FALSE,
    BPPARAM = par
  )

  params_right <- MetaboAnnotation::CompareSpectraParam(
    ppm = ppm,
    tolerance = dalton,
    FUN = function(x, y, ...) {
      nrow(x)
    },
    type = "right",
    requirePrecursor = FALSE,
    BPPARAM = par
  )

  params_inner <- MetaboAnnotation::CompareSpectraParam(
    ppm = ppm,
    tolerance = dalton,
    FUN = function(x, y, ...) {
      nrow(x)
    },
    type = "inner",
    requirePrecursor = FALSE,
    THRESHFUN = function(x) {
      which(x >= npeaks)
    },
    BPPARAM = par
  )

  ## COMMENT (AR): TODO Maybe implement some safety sanitization of the spectra?
  ## Can be very slow otherwise

  log_debug("Applying initial intensity filter to query spectra")
  spectra <- spectra |>
    Spectra::filterIntensity(intensity = c(qutoff, Inf))

  log_debug("Performing spectral comparison")
  matches_sim <- MetaboAnnotation::matchSpectra(
    query = spectra,
    target = spectral_library,
    param = params_sim
  )
  df_similarity <- matches_sim@matches |>
    dplyr::rename(msms_score = score)

  log_debug("Counting fragments")
  peaks_query <- MetaboAnnotation::matchSpectra(
    query = spectra,
    target = spectral_library,
    param = params_left
  )
  peaks_target <- MetaboAnnotation::matchSpectra(
    query = spectra,
    target = spectral_library,
    param = params_right
  )
  df_count <- peaks_query@matches |>
    dplyr::rename(peaks_query = score) |>
    dplyr::full_join(peaks_target@matches |>
      dplyr::rename(peaks_target = score)) |>
    dplyr::rowwise() |>
    dplyr::mutate(peaks = max(peaks_query, peaks_target)) |>
    dplyr::select(-peaks_query, -peaks_target)

  log_debug("Performing fragments comparison")
  matches_inner <- MetaboAnnotation::matchSpectra(
    query = spectra,
    target = spectral_library,
    param = params_inner
  )
  df_peaks <- matches_inner@matches |>
    dplyr::rename(peaks_abs = score) |>
    dplyr::inner_join(df_count) |>
    dplyr::mutate(peaks_rel = peaks_abs / peaks) |>
    dplyr::select(-peaks)

  if (condition == "AND") {
    df_peaks <- df_peaks |>
      dplyr::filter(peaks_rel >= rpeaks)
  }

  log_debug("Formatting results")
  df_full <- df_similarity |>
    dplyr::full_join(df_peaks)

  if (condition == "AND") {
    df_full <- df_full |>
      dplyr::filter(msms_score >= threshold &
        peaks_abs >= npeaks &
        peaks_rel >= rpeaks)
  }

  ## COMMENT AR: Not doing it because of thresholding
  # df_full[is.na(df_full)] <- 0

  spectral_library_extracted <- spectral_library |>
    extract_spectra()
  target_idx <- seq_along(1:length(spectral_library_extracted$name))
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

  df_final <- df_full |>
    dplyr::left_join(df_meta) |>
    dplyr::rename(feature_id = query_idx) |>
    dplyr::select(-target_idx)

  log_debug(
    nrow(df_final),
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
  
  export_output(x = df_final, file = output)
}
