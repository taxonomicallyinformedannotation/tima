#' @title Calculate entropy score
#'
#' @description This function applies similarity calculation to a list of
#'        spectra to obtain entropy scores
#'
#' @include calculate_similarity.R
#'
#' @param lib_ids Lib Ids
#' @param lib_precursors Lib precursors
#' @param lib_spectra Lib spectra
#' @param query_ids Query Ids
#' @param query_precursors Query precursors
#' @param query_spectra Query spectra
#' @param method Method
#' @param dalton Dalton
#' @param ppm Ppm
#' @param threshold Threshold
#' @param approx Approx
#'
#' @return NULL
#'
#' @examples NULL
calculate_entropy_and_similarity <- function(lib_ids,
                                             lib_precursors,
                                             lib_spectra,
                                             query_ids,
                                             query_precursors,
                                             query_spectra,
                                             method,
                                             dalton,
                                             ppm,
                                             threshold,
                                             approx) {
  results <- purrr::map(
    .x = seq_along(query_spectra),
    .f = function(spectrum_idx) {
      current_spectrum <- query_spectra[[spectrum_idx]]
      current_precursor <- query_precursors[spectrum_idx]
      current_id <- query_ids[spectrum_idx]

      # If not approximating, filter the library to only spectra within tolerance
      if (approx == FALSE) {
        val_ind <- lib_precursors >= min(current_precursor - dalton, current_precursor * (1 - (10^-6 * ppm))) &
          lib_precursors <= max(current_precursor + dalton, current_precursor * (1 + (10^-6 * ppm)))

        lib_spectra_sub <- lib_spectra[val_ind]
        lib_precursors_sub <- lib_precursors[val_ind]
        lib_ids_sub <- lib_ids[val_ind]
      } else {
        lib_spectra_sub <- lib_spectra
        lib_precursors_sub <- lib_precursors
        lib_ids_sub <- lib_ids
      }

      if (length(lib_ids_sub) != 0) {
        similarities <- vapply(
          X = seq_along(lib_spectra_sub),
          FUN = function(lib_idx) {
            lib_spectrum <- lib_spectra_sub[[lib_idx]]
            score <- calculate_similarity(
              method = method,
              query_spectrum = current_spectrum,
              target_spectrum = lib_spectrum,
              query_precursor = current_precursor,
              target_precursor = lib_precursors_sub[[lib_idx]],
              dalton = dalton,
              ppm = ppm
            )
            entropy_target <- msentropy::calculate_spectral_entropy(lib_spectrum)
            # Count the number of peaks in the query that have a match (by Dalton tolerance)
            matched_peaks <- sum(apply(abs(
              outer(X = current_spectrum[, 1], Y = lib_spectrum[, 1], FUN = "-")
            ) <= dalton, 2, any))
            list(
              score = as.numeric(score),
              entropy = entropy_target,
              matched = matched_peaks
            )
          },
          FUN.VALUE = list(
            score = numeric(1),
            entropy = numeric(1),
            matched = integer(1)
          )
        )

        if (any(similarities[1, ] >= threshold)) {
          valid_indices <- which(similarities[1, ] >= threshold)

          if (length(valid_indices) > 0) {
            tidytable::tidytable(
              feature_id = current_id,
              precursorMz = current_precursor,
              target_id = lib_ids_sub[valid_indices],
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
      precursorMz = NA_real_,
      target_id = NA_integer_,
      candidate_spectrum_entropy = NA_real_,
      candidate_score_similarity = NA_real_,
      candidate_count_similarity_peaks_matched = NA_integer_
    )
  } else {
    tidytable::bind_rows(results[!sapply(results, is.null)])
  }
}
