#' @title Process spectra
#'
#' @description TODO
#'
#' @param input TODO
#' @param library TODO
#' @param output TODO
#' @param method TODO
#' @param threshold TODO
#' @param ppm TODO
#' @param dalton TODO
#' @param npeaks TODO
#' @param rpeaks TODO
#' @param condition TODO
#' @param quickmode TODO
#' @param parallel TODO
#'
#' @return NULL
#'
#' @export
#'
#' @importFrom BiocParallel MulticoreParam SerialParam
#' @importFrom dplyr full_join left_join select
#' @importFrom MetaboAnnotation CompareSpectraParam matchSpectra
#' @importFrom MsCoreUtils gnps navdist ndotproduct neuclidean nspectraangle
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
                            quickmode = params$quickmode,
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
    requirePrecursor = TRUE,
    THRESHFUN = function(x) {
      which(x >= threshold)
    }
  )

  params_abs <- MetaboAnnotation::CompareSpectraParam(
    ppm = ppm,
    tolerance = dalton,
    FUN = .ms2_matching_peaks,
    requirePrecursor = TRUE,
    THRESHFUN = function(x) {
      which(x >= npeaks)
    }
  )

  params_rel <- MetaboAnnotation::CompareSpectraParam(
    ppm = ppm,
    tolerance = dalton,
    FUN = .ms2_matching_peaks_fraction,
    requirePrecursor = TRUE,
    THRESHFUN = function(x) {
      which(x >= rpeaks)
    },
    BPPARAM = par
  )

  ## COMMENT (AR): TODO Maybe implement some safety sanitization of the spectra?
  ## Can be very slow otherwise

  log_debug("Performing spectral comparison")
  matches_sim <- MetaboAnnotation::matchSpectra(
    query = spectra,
    target = spectra,
    param = params_sim
  )

  if (quickmode != TRUE) {
    log_debug("Performing peak matching (long)")
    matches_abs <- MetaboAnnotation::matchSpectra(
      query = spectra,
      target = spectra,
      param = params_abs
    )
    matches_rel <- MetaboAnnotation::matchSpectra(
      query = spectra,
      target = spectra,
      param = params_rel
    )
  }

  log_debug("Formatting results")
  df_similarity <- matches_sim@matches |>
    dplyr::select(
      feature_id = query_idx,
      target_id = target_idx,
      msms_score = score
    )

  if (quickmode != TRUE) {
    df_similarity <- df_similarity |>
      dplyr::full_join(
        matches_abs@matches |>
          dplyr::select(
            feature_id = query_idx,
            target_id = target_idx,
            peaks_abs = score
          )
      ) |>
      dplyr::full_join(
        matches_rel@matches |>
          dplyr::select(
            feature_id = query_idx,
            target_id = target_idx,
            peaks_rel = score
          )
      )
  }

  if (condition == "AND") {
    df_similarity <- df_similarity |>
      dplyr::filter(msms_score >= threshold &
        peaks_abs >= npeaks &
        peaks_rel >= rpeaks)
  }

  spectral_library_extracted <- spectral_library |>
    extract_spectra()
  target_id <- seq_along(1:length(spectral_library_extracted$name))
  short_inchikey <- spectral_library_extracted$name
  smiles <- spectral_library_extracted$smiles
  molecular_formula <- spectral_library_extracted$formula
  exact_mass <- spectral_library_extracted$exactmass

  df_meta <- data.frame(
    target_id,
    short_inchikey,
    smiles,
    molecular_formula,
    exact_mass
  )

  df_final <- df_similarity |>
    dplyr::left_join(df_meta)

  export_output(x = df_final, file = output)
}
