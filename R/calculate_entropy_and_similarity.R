# Helper Functions ----

#' Count matched peaks between query and library spectra
#' @keywords internal
.count_matched_peaks <- function(query_mz, lib_mz, dalton, ppm) {
  if (length(query_mz) == 0 || length(lib_mz) == 0) {
    return(0L)
  }

  # Sort library m/z for binary search
  lib_mz_sorted <- sort(lib_mz)

  # Vectorized approach: calculate tolerances for all query peaks at once
  tolerances <- pmax(dalton, ppm * query_mz * 1E-6)

  # Use findInterval for fast binary search - fully vectorized
  lower_bounds <- query_mz - tolerances
  upper_bounds <- query_mz + tolerances
  low_idx <- findInterval(lower_bounds, lib_mz_sorted)
  high_idx <- findInterval(upper_bounds, lib_mz_sorted, rightmost.closed = TRUE)

  sum(high_idx > low_idx)
}

#' @title Calculate entropy score
#'
#' @description This function calculates spectral entropy and similarity scores
#'     by comparing query spectra against library spectra. Uses entropy-based
#'     similarity measures to match MS2 fragmentation patterns.
#'
#' @include calculate_similarity.R
#' @include validations_utils.R
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
  ctx <- log_operation(
    "calculate_entropy_similarity",
    n_library = length(lib_ids),
    n_query = length(query_ids),
    method = method
  )

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

  log_info(
    "Calculating entropy and similarity for %d spectra",
    length(query_ids)
  )
  log_debug(
    "Method: %s, PPM: %f2, Dalton: %f2",
    method,
    ppm,
    dalton
  )

  # Pre-calculate length once for efficiency
  n_queries <- length(query_ids)

  # Pre-flight: sample a few spectra to detect unsanitized input.
  # If any are found, sanitize the entire list once before the hot loop.
  .ensure_sanitized <- function(spectra_list, label, dalton, ppm) {
    n <- length(spectra_list)
    if (n == 0L) {
      return(spectra_list)
    }
    sample_idx <- unique(c(
      1L,
      as.integer(seq(1L, n, length.out = min(20L, n)))
    ))
    needs_fix <- FALSE
    for (idx in sample_idx) {
      sp <- spectra_list[[idx]]
      if (is.matrix(sp) && nrow(sp) >= 2L) {
        if (!is_spectrum_sanitized(sp, tolerance = dalton, ppm = ppm)) {
          needs_fix <- TRUE
          break
        }
      }
    }
    if (needs_fix) {
      log_warn(
        paste0(
          "Unsanitized %s spectra detected. ",
          "Sanitizing %d spectra in-place before scoring. ",
          "Consider using import_spectra(sanitize = TRUE) upstream."
        ),
        label,
        n
      )
      spectra_list <- lapply(spectra_list, function(sp) {
        if (
          is.matrix(sp) &&
            nrow(sp) >= 2L &&
            !is_spectrum_sanitized(sp, tolerance = dalton, ppm = ppm)
        ) {
          sanitize_spectrum_matrix(sp, tolerance = dalton, ppm = ppm)
        } else {
          sp
        }
      })
    }
    spectra_list
  }
  query_spectra <- .ensure_sanitized(query_spectra, "query", dalton, ppm)
  lib_spectra <- .ensure_sanitized(lib_spectra, "library", dalton, ppm)

  # Use closures to avoid passing large objects through function arguments.
  # R's lexical scoping means the closure captures references to
  # lib_spectra, lib_precursors, lib_ids, query_spectra, etc. from the
  # parent environment without copying them on each iteration.
  results <- lapply(
    X = seq_along(query_spectra),
    FUN = function(spectrum_idx) {
      current_spectrum <- query_spectra[[spectrum_idx]]
      current_precursor <- query_precursors[spectrum_idx]
      current_id <- query_ids[spectrum_idx]

      # Filter library spectra by precursor mass if not approximating
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

      if (length(lib_ids_sub) == 0) {
        return(NULL)
      }

      # Calculate similarities
      use_gnps <- (method == "gnps")
      q_mz <- current_spectrum[, 1L]

      similarities <- vapply(
        X = seq_along(lib_spectra_sub),
        FUN = function(lib_idx) {
          lib_spectrum <- lib_spectra_sub[[lib_idx]]

          if (use_gnps) {
            # Single fused .Call — join + score in C, O(n+m) chain-DP
            res <- .Call(
              "gnps_compute",
              current_spectrum,
              lib_spectrum,
              current_precursor,
              lib_precursors_sub[[lib_idx]],
              dalton,
              ppm
            )
            score <- res[["score"]]
          } else {
            score <- calculate_similarity(
              method = method,
              query_spectrum = current_spectrum,
              target_spectrum = lib_spectrum,
              query_precursor = current_precursor,
              target_precursor = lib_precursors_sub[[lib_idx]],
              dalton = dalton,
              ppm = ppm
            )
          }

          entropy_target <- msentropy::calculate_spectral_entropy(lib_spectrum)
          matched <- .count_matched_peaks(q_mz, lib_spectrum[, 1L], dalton, ppm)

          list(
            score = as.numeric(score),
            entropy = entropy_target,
            matched = matched
          )
        },
        FUN.VALUE = list(
          score = numeric(1),
          entropy = numeric(1),
          matched = integer(1)
        )
      )

      # Filter by threshold
      if (any(similarities[1, ] >= threshold)) {
        valid_indices <- which(similarities[1, ] >= threshold)

        if (length(valid_indices) > 0) {
          return(
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
          )
        }
      }

      NULL
    }
  )

  # Log progress summary
  log_info("Processed %d / %d queries", n_queries, n_queries)

  if (all(vapply(X = results, FUN = is.null, FUN.VALUE = logical(1)))) {
    result <- tidytable::tidytable(
      feature_id = NA_integer_,
      precursorMz = NA_real_,
      target_id = NA_integer_,
      candidate_spectrum_entropy = NA_real_,
      candidate_score_similarity = NA_real_,
      candidate_count_similarity_peaks_matched = NA_integer_
    )
  } else {
    result <- tidytable::bind_rows(
      results[!vapply(X = results, FUN = is.null, FUN.VALUE = logical(1))]
    )
  }

  log_complete(ctx, n_comparisons = nrow(result))

  result
}
