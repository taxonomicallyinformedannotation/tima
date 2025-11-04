#' @title Calculate entropy score
#'
#' @description This function calculates spectral entropy and similarity scores
#'     by comparing query spectra against library spectra. Uses entropy-based
#'     similarity measures to match MS2 fragmentation patterns.
#'
#' @include calculate_similarity.R
#'
#' @param lib_ids Character vector of library spectrum IDs
#' @param lib_precursors Numeric vector of library precursor m/z values
#' @param lib_spectra List of library spectra (each a matrix of mz/intensity)
#' @param query_ids Character vector of query spectrum IDs
#' @param query_precursors Numeric vector of query precursor m/z values
#' @param query_spectra List of query spectra (each a matrix of mz/intensity)
#' @param method Character string similarity method to use
#' @param dalton Numeric absolute mass tolerance in Daltons
#' @param ppm Numeric relative mass tolerance in ppm
#' @param threshold Numeric minimum similarity threshold (0-1)
#' @param approx Logical whether to perform approximate matching without
#'     precursor mass filtering
#'
#' @return Data frame with spectrum IDs, entropy scores, and similarity scores
#'
#' @examples NULL
calculate_entropy_and_similarity <- function(
  lib_ids,
  lib_precursors,
  lib_spectra,
  query_ids,
  query_precursors,
  query_spectra,
  method,
  dalton,
  ppm,
  threshold,
  approx
) {
  # Validate inputs
  if (
    length(lib_ids) != length(lib_spectra) ||
      length(lib_ids) != length(lib_precursors)
  ) {
    stop("lib_ids, lib_precursors, and lib_spectra must have the same length")
  }

  if (
    length(query_ids) != length(query_spectra) ||
      length(query_ids) != length(query_precursors)
  ) {
    stop(
      "query_ids, query_precursors, and query_spectra must have the same length"
    )
  }

  if (!is.numeric(dalton) || dalton <= 0) {
    stop("dalton must be a positive number")
  }

  if (!is.numeric(ppm) || ppm <= 0) {
    stop("ppm must be a positive number")
  }

  if (!is.numeric(threshold) || threshold < 0 || threshold > 1) {
    stop("threshold must be between 0 and 1")
  }

  if (!is.logical(approx)) {
    stop("approx must be logical (TRUE/FALSE)")
  }

  logger::log_info(
    "Calculating entropy and similarity for ",
    length(query_spectra),
    " spectra"
  )
  logger::log_debug(
    "Parameters - Method: ",
    method,
    ", Dalton: ",
    dalton,
    ", PPM: ",
    ppm
  )

  results <- purrr::map(
    .progress = TRUE,
    .x = seq_along(query_spectra),
    .f = function(spectrum_idx) {
      current_spectrum <- query_spectra[[spectrum_idx]]
      current_precursor <- query_precursors[spectrum_idx]
      current_id <- query_ids[spectrum_idx]

      # If not approximating, filter the library to only spectra within tolerance
      if (approx == FALSE) {
        val_ind <- lib_precursors >=
          min(
            current_precursor - dalton,
            current_precursor * (1 - (1E-6 * ppm))
          ) &
          lib_precursors <=
            max(
              current_precursor + dalton,
              current_precursor * (1 + (1E-6 * ppm))
            )

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
            entropy_target <- msentropy::calculate_spectral_entropy(
              lib_spectrum
            )
            # Count the number of peaks in the query that have a match
            .count_matched_peaks_hybrid <- function(
              query_mz,
              lib_mz,
              dalton,
              ppm,
              threshold = 1e5
            ) {
              n_query <- length(query_mz)
              n_lib <- length(lib_mz)

              # Use matrix method if the total comparison count is small
              if (n_query * n_lib <= threshold) {
                diff_matrix <- abs(outer(query_mz, lib_mz, FUN = "-"))
                tolerances_matrix <- outer(
                  pmax(dalton, ppm * query_mz * 1E-6),
                  rep(1, n_lib)
                )
                match_matrix <- diff_matrix <= tolerances_matrix
                return(sum(apply(match_matrix, 1, any)))
              } else {
                # Use binary search method for large comparisons
                lib_mz <- sort(lib_mz)
                matched_count <- 0

                for (mz in query_mz) {
                  tolerance <- max(dalton, ppm * mz * 1E-6)
                  lower_bound <- mz - tolerance
                  upper_bound <- mz + tolerance

                  low_idx <- findInterval(lower_bound, lib_mz)
                  high_idx <- findInterval(
                    upper_bound,
                    lib_mz,
                    rightmost.closed = TRUE
                  )

                  if (
                    high_idx >= low_idx + 1 ||
                      (low_idx > 0 &&
                        lib_mz[low_idx] >= lower_bound &&
                        lib_mz[low_idx] <= upper_bound)
                  ) {
                    matched_count <- matched_count + 1
                  }
                }

                return(matched_count)
              }
            }

            query_mz <- current_spectrum[, 1]
            lib_mz <- lib_spectrum[, 1]
            matched_peaks <- .count_matched_peaks_hybrid(
              query_mz,
              lib_mz,
              dalton,
              ppm
            )

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
              candidate_count_similarity_peaks_matched = similarities[
                3,
                valid_indices
              ]
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
