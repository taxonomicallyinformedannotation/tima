#' @title Calculate entropy score
#'
#' @description This function applies similarity calculation to a list of
#'        spectra to obtain entropy scores
#'
#' @param lib_ids Lib Ids
#' @param lib_precursors Lib precursors
#' @param lib_spectra Lib spectra
#' @param query_ids Query Ids
#' @param query_precursors Query precursors
#' @param query_spectra Query spectra
#' @param dalton Dalton
#' @param ppm Ppm
#' @param threshold Threshold
#' @param approx Approx
#'
#' @return NULL
#'
#' @examples NULL
calculate_entropy_score <- function(lib_ids,
                                    lib_precursors,
                                    lib_spectra,
                                    query_ids,
                                    query_precursors,
                                    query_spectra,
                                    dalton,
                                    ppm,
                                    threshold,
                                    approx) {
  p <- progressr::progressor(along = seq_along(query_spectra))
  results <- furrr::future_map(
    .options = furrr::furrr_options(seed = TRUE),
    .x = seq_along(query_spectra),
    .f = function(spectrum_idx) {
      p()
      current_spectrum <- query_spectra[[spectrum_idx]]
      current_precursor <- query_precursors[spectrum_idx]
      current_id <- query_ids[spectrum_idx]

      if (approx == FALSE) {
        minimum <- min(current_precursor - dalton, current_precursor * (1 - (10^
          -6 * ppm)))
        maximum <- max(current_precursor + dalton, current_precursor * (1 + (10^
          -6 * ppm)))
        val_ind <- lib_precursors >= minimum &
          lib_precursors <= maximum

        filtered_lib_spectra <- lib_spectra[val_ind]
        filtered_lib_ids <- lib_ids[val_ind]
      } else {
        filtered_lib_spectra <- lib_spectra
        filtered_lib_ids <- lib_ids
      }

      if (length(filtered_lib_ids) != 0) {
        similarities <- vapply(X = seq_along(filtered_lib_spectra), function(lib_idx) {
          lib_spectrum <- filtered_lib_spectra[[lib_idx]]
          score <- msentropy::calculate_entropy_similarity(
            peaks_a = current_spectrum,
            peaks_b = lib_spectrum,
            min_mz = 0,
            max_mz = 5000,
            noise_threshold = 0,
            ms2_tolerance_in_da = dalton,
            ms2_tolerance_in_ppm = ppm,
            max_peak_num = -1,
            clean_spectra = TRUE
          )
          entropy_target <- msentropy::calculate_spectral_entropy(lib_spectrum)
          ## number of matched peaks (only Da for now)
          matched_peaks <- sum(apply(abs(
            outer(
              X = current_spectrum[, 1],
              Y = lib_spectrum[, 1],
              FUN = "-"
            )
          ) <= dalton, 2, any))

          list(
            score = as.numeric(score),
            entropy = entropy_target,
            matched = matched_peaks
          )
        }, FUN.VALUE = list(
          score = numeric(1),
          entropy = numeric(1),
          matched = integer(1)
        ))

        if (any(similarities[1, ] >= threshold)) {
          valid_indices <- which(similarities[1, ] >= threshold)

          if (length(valid_indices) > 0) {
            tidytable::tidytable(
              feature_id = current_id,
              precursorMz = current_precursor,
              target_id = filtered_lib_ids[valid_indices],
              candidate_spectrum_entropy = similarities[2, valid_indices],
              candidate_score_similarity = similarities[1, valid_indices],
              candidate_count_similarity_peaks_matched = similarities[3, valid_indices]
            )
          } else {
            NULL
          }
        } else {
          NULL
        }
      } else {
        NULL
      }
    }
  )

  if (all(sapply(results, is.null))) {
    tidytable::tidytable(
      feature_id = NA_integer_,
      precursorMz = NA_complex_,
      target_id = NA_integer_,
      candidate_spectrum_entropy = NA_complex_,
      candidate_score_similarity = NA_complex_,
      candidate_count_similarity_peaks_matched = NA_integer_,
    )
  } else {
    tidytable::bind_rows(results[!sapply(results, is.null)])
  }
}
