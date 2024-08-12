import::from(msentropy, calculate_spectral_entropy, .into = environment())
import::from(pbapply, pblapply, .into = environment())
import::from(tidyfst, rn_col, .into = environment())
import::from(tidytable, any_of, .into = environment())
import::from(tidytable, bind_rows, .into = environment())
import::from(tidytable, coalesce, .into = environment())
import::from(tidytable, distinct, .into = environment())
import::from(tidytable, everything, .into = environment())
import::from(tidytable, filter, .into = environment())
import::from(tidytable, full_join, .into = environment())
import::from(tidytable, mutate, .into = environment())
import::from(tidytable, select, .into = environment())
import::from(tidytable, tidytable, .into = environment())

#' @title Create edges spectra
#'
#' @description This function create edges
#'    based on fragmentation spectra similarity
#'
#' @importFrom msentropy calculate_spectral_entropy
#' @importFrom pbapply pblapply
#' @importFrom tidyfst rn_col
#' @importFrom tidytable any_of
#' @importFrom tidytable bind_rows
#' @importFrom tidytable coalesce
#' @importFrom tidytable distinct
#' @importFrom tidytable everything
#' @importFrom tidytable filter
#' @importFrom tidytable full_join
#' @importFrom tidytable mutate
#' @importFrom tidytable select
#' @importFrom tidytable tidytable
#'
#' @include create_edges.R
#' @include get_params.R
#' @include import_spectra.R
#'
#' @param input Query file containing spectra. Currently an '.mgf' file
#' @param output Output file.
#' @param name_source Name of the source features column
#' @param name_target Name of the target features column
#' @param threshold Minimal similarity to report
#' @param ppm Relative ppm tolerance to be used
#' @param dalton Absolute Dalton tolerance to be used
#' @param qutoff Intensity under which ms2 fragments will be removed.
#'
#' @return The path to the created spectral edges
#'
#' @export
#'
#' @examples NULL
create_edges_spectra <- function(input = get_params(step = "create_edges_spectra")$files$spectral$raw,
                                 output = get_params(step = "create_edges_spectra")$files$networks$spectral$edges$raw,
                                 name_source = get_params(step = "create_edges_spectra")$names$source,
                                 name_target = get_params(step = "create_edges_spectra")$names$target,
                                 threshold = get_params(step = "create_edges_spectra")$annotations$thresholds$ms2$similarity$edges,
                                 ppm = get_params(step = "create_edges_spectra")$ms$tolerances$mass$ppm$ms2,
                                 dalton = get_params(step = "create_edges_spectra")$ms$tolerances$mass$dalton$ms2,
                                 qutoff = get_params(step = "create_edges_spectra")$ms$thresholds$ms2$intensity) {
  stopifnot("Your input file does not exist." = file.exists(input))
  ## Not checking for ppm and Da limits, everyone is free.

  log_debug("Loading spectra...")
  spectra <- input |>
    import_spectra(
      cutoff = qutoff,
      dalton = dalton,
      ppm = ppm
    )
  if (length(spectra) > 1) {
    log_debug("Performing spectral comparison")
    log_debug("As we do not limit the precursors delta,
      expect a (relatively) long processing time.")
    log_debug("Take yourself a break, you deserve it.")
    nspecz <- length(spectra)
    precz <- spectra$precursorMz
    fragz <- spectra@backend@peaksData

    edges <-
      pblapply(
        X = 1:(nspecz - 1),
        FUN = create_edges,
        frags = fragz,
        precs = precz,
        nspecs = nspecz,
        ms2_tolerance = dalton,
        ppm_tolerance = ppm,
        threshold = threshold
      ) |>
      bind_rows()
    rm(precz)

    log_debug("Calculating features' entropy")
    entropy <- pblapply(
      X = seq_along(1:nspecz),
      FUN = function(x, peaks = fragz) {
        return(peaks[[x]] |> calculate_spectral_entropy())
      }
    )
    log_debug("Calculating features' number of peaks")
    npeaks <- pblapply(
      X = seq_along(1:nspecz),
      FUN = function(x, peaks = fragz) {
        return(peaks[[x]] |> length())
      }
    )
    rm(nspecz, fragz)

    edges <- edges |>
      select(
        !!as.name(name_source) := "feature_id",
        !!as.name(name_target) := "target_id",
        everything()
      )

    ## ISSUE see #148 find a way to have consistency in spectrum IDs
    idz <- spectra@backend@spectraData$acquisitionNum
    rm(spectra)
    edges <- edges |>
      mutate(name_source = idz[name_source], name_target = idz[name_target])
    entropy_df <- tidytable(entropy) |>
      rn_col(var = name_source) |>
      mutate(
        name_source = idz[name_source],
        feature_spectrum_entropy = as.character(entropy),
        feature_spectrum_peaks = as.character(npeaks)
      ) |>
      mutate(!!as.name(name_source) := as.integer(!!as.name(name_source))) |>
      distinct(
        !!as.name(name_source),
        feature_spectrum_entropy,
        feature_spectrum_peaks
      )
    rm(entropy, npeaks, idz)

    edges <- edges |>
      select(any_of(
        c(
          name_source,
          name_target,
          "candidate_score_similarity" = "score",
          "candidate_count_similarity_peaks_matched"
        )
      ))

    edges <- edges |>
      filter(candidate_score_similarity >= threshold)

    edges <- edges |>
      full_join(entropy_df) |>
      mutate(!!as.name(name_target) := coalesce(!!as.name(name_target), !!as.name(name_source)))
    rm(entropy_df)
  } else {
    log_debug("No spectra were found, returning an empty dataframe instead")
    edges <- tidytable(
      !!as.name(name_source) := NA,
      "feature_spectrum_entropy" = NA,
      "feature_spectrum_peaks" = NA,
      !!as.name(name_target) := NA,
      "candidate_score_similarity" = NA,
      "candidate_count_similarity_peaks_matched" = NA
    )
  }

  try(expr = {
    export_params(
      parameters = get_params(step = "create_edges_spectra"),
      step = "create_edges_spectra"
    )
  }, silent = TRUE)
  export_output(x = edges, file = output[[1]])
  rm(edges)

  return(output[[1]])
}
