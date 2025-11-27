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
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Calculate entropy-based similarity
#' results <- calculate_entropy_and_similarity(
#'   lib_ids = library_ids,
#'   lib_precursors = library_mz,
#'   lib_spectra = library_spectra_list,
#'   query_ids = feature_ids,
#'   query_precursors = feature_mz,
#'   query_spectra = feature_spectra_list,
#'   method = "entropy",
#'   dalton = 0.01,
#'   ppm = 10,
#'   threshold = 0.7,
#'   approx = FALSE
#' )
#' }
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
  assert_choice(method, VALID_SIMILARITY_METHODS, "method")
  if (
    length(lib_ids) != length(lib_spectra) ||
      length(lib_ids) != length(lib_precursors)
  ) {
    stop(
      "lib_ids, lib_precursors, and lib_spectra must have the same length",
      call. = FALSE
    )
  }
  if (
    length(query_ids) != length(query_spectra) ||
      length(query_ids) != length(query_precursors)
  ) {
    stop(
      "query_ids, query_precursors, and query_spectra must have the same length",
      call. = FALSE
    )
  }
  assert_scalar_numeric(dalton, "dalton", min = 0, max = Inf)
  assert_scalar_numeric(ppm, "ppm", min = 0, max = Inf)
  assert_scalar_numeric(threshold, "threshold", min = 0, max = 1)
  assert_flag(approx, "approx")

  logger::log_info(
    "Calculating entropy and similarity for {length(query_spectra)} spectra"
  )
  logger::log_debug(
    "Parameters - Method: {method}, Dalton: {dalton}, PPM: {ppm}"
  )

  # Detect if running in a subprocess (callr/targets environment)
  # Disable progress bar to avoid subprocess issues
  show_progress <- interactive() && !isTRUE(getOption("knitr.in.progress"))

  count_matched_peaks <- function(query_mz, lib_mz, dalton, ppm) {
    if (length(query_mz) == 0 || length(lib_mz) == 0) {
      return(0L)
    }

    # Sort library m/z for binary search
    lib_mz_sorted <- sort(lib_mz)

    # Vectorized approach: calculate tolerances for all query peaks at once
    tolerances <- pmax(dalton, ppm * query_mz * 1E-6)

    # Use findInterval for fast binary search
    matched_count <- sum(vapply(
      seq_along(query_mz),
      function(i) {
        mz <- query_mz[i]
        tol <- tolerances[i]
        lower_bound <- mz - tol
        upper_bound <- mz + tol

        # Binary search for matching peaks
        low_idx <- findInterval(lower_bound, lib_mz_sorted)
        high_idx <- findInterval(
          upper_bound,
          lib_mz_sorted,
          rightmost.closed = TRUE
        )

        # Check if any peaks fall within tolerance
        as.integer(high_idx > low_idx)
      },
      FUN.VALUE = integer(1)
    ))

    return(matched_count)
  }

  results <- purrr::map(
    .progress = show_progress,
    .x = seq_along(query_spectra),
    # TODO
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
          # TODO
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

            # Count matched peaks using pre-defined function
            query_mz <- current_spectrum[, 1]
            lib_mz <- lib_spectrum[, 1]
            matched_peaks <- count_matched_peaks(
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
