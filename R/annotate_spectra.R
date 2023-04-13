utils::globalVariables(
  c(
    "acquisitionNum",
    "feature_id",
    "matched_peaks_count",
    "precursorMz",
    "presence_ratio",
    "rtime",
    "score",
    "SLAW_ID",
    "structure_inchikey_2D",
    "structure_smiles_2D",
    "target_inchikey",
    "target_inchikey_2D",
    "target_precursorMz",
    "target_rtime",
    "target_smiles",
    "target_smiles_2D"
  )
)

#' @title Annotate spectra
#'
#' @description This function annotates spectra
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
#' @param fast Boolean. Do it fast
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
  # Check if library file(s) exists
  stopifnot(
    "Library file(s) do(es) not exist" =
      rep(TRUE, length(unlist(library))) ==
        lapply(X = unlist(library), file.exists)
  )
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

  params <<- parameters
  if (length(library) > 1) {
    library <- library[[polarity]]
  }

  par <- if (parallel) {
    if (.Platform$OS.type == "windows") {
      BiocParallel::SnowParam(progressbar = TRUE)
    } else {
      BiocParallel::MulticoreParam(progressbar = TRUE)
    }
  } else {
    BiocParallel::SerialParam(progressbar = TRUE)
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
    spectral_library <- lapply(library, import_spectra) |>
      Spectra::concatenateSpectra()

    sim_fun <- switch(
      EXPR = method,
      "gnps" = MsCoreUtils::gnps,
      "navdist" = MsCoreUtils::navdist,
      "ndotproduct" = MsCoreUtils::ndotproduct,
      "neuclidean" = MsCoreUtils::neuclidean,
      "nspectraangle" = MsCoreUtils::nspectraangle
    )

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

    log_debug("Collecting results")
    df_final <- matches_sim |>
      MetaboAnnotation::matchedData() |>
      data.frame()

    isSlaw <- "SLAW_ID" %in% colnames(df_final)

    df_final <- df_final |>
      dplyr::rowwise() |>
      dplyr::mutate(
        feature_id = ifelse(
          test = isSlaw,
          yes = as.numeric(SLAW_ID),
          no = as.numeric(acquisitionNum)
        ),
        ## Working in minutes
        rt_error = (target_rtime - rtime) / 60,
        mz_error = target_precursorMz - precursorMz,
        structure_inchikey_2D = ifelse(
          test = is.na(target_inchikey_2D),
          yes = target_inchikey |>
            gsub(
              pattern = "-.*",
              replacement = ""
            ),
          no = target_inchikey_2D
        ),
        structure_smiles_2D = dplyr::coalesce(target_smiles_2D, target_smiles)
      ) |>
      dplyr::select(dplyr::any_of(
        c(
          "feature_id",
          "mz_error",
          "rt_error",
          "structure_name" = "target_name",
          # "structure_inchikey" = "target_inchikey",
          "structure_inchikey_2D",
          # "structure_smiles" = "target_smiles",
          "structure_smiles_2D",
          "structure_molecular_formula" = "target_formula",
          "structure_exact_mass" = "target_exactmass",
          "structure_xlogp" = "target_xlogp",
          "score",
          "reverse_score",
          "presence_ratio",
          "matched_peaks_count"
        )
      )) |>
      dplyr::filter(!is.na(score))

    ## COMMENT AR: Not doing it because of thresholding
    # df_final[is.na(df_final)] <- 0

    if (condition == "AND") {
      df_final <- df_final |>
        dplyr::filter(score >= threshold &
          matched_peaks_count >= npeaks &
          presence_ratio >= rpeaks)
    }

    log_debug(
      nrow(
        df_final |>
          ## else doesn't work if some are empty
          dplyr::distinct(structure_inchikey_2D, structure_smiles_2D)
      ),
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
          mz_error = NA,
          rt_error = NA,
          structure_name = NA,
          # structure_inchikey = NA,
          structure_inchikey_2D = NA,
          # structure_smiles = target_smiles,
          structure_smiles_2D = NA,
          structure_molecular_formula = NA,
          structure_exact_mass = NA,
          structure_xlogp = NA,
          score = NA,
          reverse_score = NA,
          presence_ratio = NA,
          matched_peaks_count = NA
        )
    }
  } else {
    log_debug("No spectra matched the given polarity, returning an empty dataframe")
    df_final <-
      data.frame(
        feature_id = NA,
        mz_error = NA,
        rt_error = NA,
        structure_name = NA,
        # structure_inchikey = NA,
        structure_inchikey_2D = NA,
        # structure_smiles = target_smiles,
        structure_smiles_2D = NA,
        structure_molecular_formula = NA,
        structure_exact_mass = NA,
        structure_xlogp = NA,
        score = NA,
        reverse_score = NA,
        presence_ratio = NA,
        matched_peaks_count = NA
      )
  }

  export_params(step = "annotate_spectra")
  export_output(x = df_final, file = output[[1]])

  return(output[[1]])
}
