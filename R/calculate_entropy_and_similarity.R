# Helper Functions ----

#' Count matched peaks between query and library spectra
#' @keywords internal
.count_matched_peaks <- function(query_mz, lib_mz, dalton, ppm) {
  if (length(query_mz) == 0 || length(lib_mz) == 0) {
    return(0L)
  }

  # Avoid re-sorting spectra that are already sanitized and m/z-sorted.
  lib_mz_sorted <- if (is.unsorted(lib_mz, na.rm = FALSE)) {
    sort(lib_mz)
  } else {
    lib_mz
  }

  # Calculate tolerances for all query peaks at once
  tolerances <- pmax(dalton, ppm * query_mz * 1E-6)

  # Use findInterval for fast binary search
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
#' @param lib_ids [character] Character vector of library spectrum IDs
#' @param lib_precursors [numeric] Numeric vector of library precursor m/z
#'     values
#' @param lib_spectra [list] List of library spectra (each a matrix of
#'     mz/intensity)
#' @param query_ids [character] Character vector of query spectrum IDs
#' @param query_precursors [numeric] Numeric vector of query precursor m/z
#'     values
#' @param query_spectra [list] List of query spectra (each a matrix of
#'     mz/intensity)
#' @param method [character] Character string similarity method to use
#' @param dalton [numeric] Numeric absolute mass tolerance in Daltons
#' @param ppm [numeric] Numeric relative mass tolerance in ppm
#' @param threshold [numeric] Numeric minimum similarity threshold (0-1)
#' @param approx [logical] Logical whether to perform approximate matching
#'     without
#'     precursor mass filtering
#' @param compute_forward_reverse [logical] Compute forward/reverse GNPS scores
#'     for retained candidates. Defaults to TRUE for entropy and FALSE for
#'     other methods.
#' @param compute_entropy [logical] Compute per-spectrum entropy values for the
#'     returned candidates. Set to FALSE to skip the entropy pass when the
#'     caller only needs similarity and forward/reverse scores.
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
  approx,
  query_adducts = NULL,
  lib_adducts = NULL,
  compute_forward_reverse = NULL,
  compute_entropy = TRUE
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
    cli::cli_abort(
      "lib_ids, lib_precursors, and lib_spectra must have the same length",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }
  if (
    length(query_ids) != length(query_spectra) ||
      length(query_ids) != length(query_precursors)
  ) {
    cli::cli_abort(
      "query_ids, query_precursors, and query_spectra must have the same length",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }
  assert_scalar_numeric(dalton, "dalton", min = 0, max = Inf)
  assert_scalar_numeric(ppm, "ppm", min = 0, max = Inf)
  assert_scalar_numeric(threshold, "threshold", min = 0, max = 1)
  assert_flag(approx, "approx")
  if (!is.null(compute_forward_reverse)) {
    assert_flag(compute_forward_reverse, "compute_forward_reverse")
  }
  assert_flag(compute_entropy, "compute_entropy")
  compute_forward_reverse <- compute_forward_reverse %||% (method == "entropy")

  if (!is.null(query_adducts) && length(query_adducts) != length(query_ids)) {
    cli::cli_abort(
      "query_adducts must have the same length as query_ids",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }
  if (!is.null(lib_adducts) && length(lib_adducts) != length(lib_ids)) {
    cli::cli_abort(
      "lib_adducts must have the same length as lib_ids",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }
  if (!is.null(query_adducts)) {
    query_adducts <- as.character(query_adducts)
  }
  if (!is.null(lib_adducts)) {
    lib_adducts <- as.character(lib_adducts)
  }

  log_info(
    "Calculating entropy and similarity for %d spectra",
    length(query_ids)
  )
  log_debug(
    "Method: %s, PPM: %.2f, Dalton: %.2f",
    method,
    ppm,
    dalton
  )

  # Pre-calculate length once for efficiency
  n_queries <- length(query_ids)

  n_query <- length(query_spectra)
  n_lib <- length(lib_spectra)
  query_checked <- rep(FALSE, n_query)
  lib_checked <- rep(FALSE, n_lib)
  query_sanitized <- logical(n_query)
  lib_sanitized <- logical(n_lib)
  lib_entropy <- rep(NA_real_, n_lib)

  ensure_query_ready <- function(idx) {
    if (!query_checked[[idx]]) {
      sp <- query_spectra[[idx]]
      if (
        is.matrix(sp) &&
          nrow(sp) >= 2L &&
          !is_spectrum_sanitized(sp, tolerance = dalton, ppm = ppm)
      ) {
        query_spectra[[idx]] <<- sanitize_spectrum_matrix(
          sp,
          tolerance = dalton,
          ppm = ppm
        )
        query_sanitized[[idx]] <<- TRUE
      }
      query_checked[[idx]] <<- TRUE
    }
    query_spectra[[idx]]
  }

  ensure_lib_ready <- function(idx) {
    if (!lib_checked[[idx]]) {
      sp <- lib_spectra[[idx]]
      if (
        is.matrix(sp) &&
          nrow(sp) >= 2L &&
          !is_spectrum_sanitized(sp, tolerance = dalton, ppm = ppm)
      ) {
        lib_spectra[[idx]] <<- sanitize_spectrum_matrix(
          sp,
          tolerance = dalton,
          ppm = ppm
        )
        lib_sanitized[[idx]] <<- TRUE
      }
      if (
        isTRUE(compute_entropy) &&
          is.matrix(lib_spectra[[idx]]) &&
          nrow(lib_spectra[[idx]]) > 0L &&
          ncol(lib_spectra[[idx]]) >= 2L
      ) {
        lib_entropy[[idx]] <<- msentropy::calculate_spectral_entropy(
          lib_spectra[[idx]]
        )
      }
      lib_checked[[idx]] <<- TRUE
    }
    lib_spectra[[idx]]
  }

  # Progress and local function alias used in the hot loop closure
  progress_counter <- 0L
  call_gnps <- gnps_chain_dp_wrapper
  use_gnps <- (method == "gnps")

  has_adduct_metadata <- !is.null(query_adducts) && !is.null(lib_adducts)
  adduct_cache <- new.env(parent = emptyenv())
  get_neutral_precursor <- function(precursor, adduct) {
    if (
      !is.finite(precursor) ||
        is.na(precursor) ||
        is.na(adduct) ||
        !nzchar(adduct)
    ) {
      return(NA_real_)
    }
    key <- paste0(
      adduct,
      "|",
      format(precursor, scientific = FALSE, digits = 15)
    )
    if (exists(key, envir = adduct_cache, inherits = FALSE)) {
      return(get(key, envir = adduct_cache, inherits = FALSE))
    }
    converted <- convert_precursor_to_neutral_if_possible(
      precursors = precursor,
      adducts = adduct
    )
    converted_value <- if (
      length(converted) == 1L &&
        is.finite(converted) &&
        converted > 0
    ) {
      converted
    } else {
      NA_real_
    }
    assign(key, converted_value, envir = adduct_cache)
    converted_value
  }

  results <- lapply(
    X = seq_along(query_spectra),
    FUN = function(spectrum_idx) {
      progress_counter <<- progress_counter + 1L
      if (progress_counter %% 500L == 0L) {
        log_info("Processed %d / %d queries", progress_counter, n_queries)
      }

      current_spectrum <- ensure_query_ready(spectrum_idx)
      current_precursor <- query_precursors[spectrum_idx]
      current_id <- query_ids[spectrum_idx]

      # Filter library spectra by precursor mass if not approximating
      if (!approx) {
        val_ind <- (lib_precursors >=
          min(
            current_precursor - dalton,
            current_precursor * (1 - (1E-6 * ppm))
          )) &
          (lib_precursors <=
            max(
              current_precursor + dalton,
              current_precursor * (1 + (1E-6 * ppm))
            ))
        lib_indices_sub <- which(val_ind)
      } else {
        lib_indices_sub <- seq_along(lib_spectra)
      }

      if (length(lib_indices_sub) == 0) {
        return(NULL)
      }

      # Calculate similarities with pre-allocated vectors to reduce overhead.
      n_candidates <- length(lib_indices_sub)
      # Keep invalid/degenerate candidates as NA so they are never selected.
      scores <- rep(NA_real_, n_candidates)
      entropies <- rep(NA_real_, n_candidates)
      matched_counts <- integer(n_candidates)
      compute_entropy_for_candidates <- isTRUE(compute_entropy)
      scores_forward <- rep(NA_real_, n_candidates)
      scores_reverse <- rep(NA_real_, n_candidates)
      similarity_space <- rep("precursor_mz", n_candidates)
      query_precursor_values <- rep(NA_real_, n_candidates)
      target_precursor_values <- rep(NA_real_, n_candidates)
      q_mz <- current_spectrum[, 1L]
      current_query_adduct <- if (!is.null(query_adducts)) {
        query_adducts[[spectrum_idx]]
      } else {
        NA_character_
      }

      for (pos_idx in seq_len(n_candidates)) {
        lib_idx <- lib_indices_sub[[pos_idx]]
        lib_spectrum <- ensure_lib_ready(lib_idx)

        if (
          !is.matrix(lib_spectrum) ||
            nrow(lib_spectrum) == 0L ||
            ncol(lib_spectrum) < 2L
        ) {
          next
        }

        target_precursor <- lib_precursors[[lib_idx]]
        target_query_adduct <- if (!is.null(lib_adducts)) {
          lib_adducts[[lib_idx]]
        } else {
          NA_character_
        }
        query_precursor_value <- current_precursor
        target_precursor_value <- target_precursor
        space_label <- "precursor_mz"
        if (
          has_adduct_metadata &&
            !is.na(current_query_adduct) &&
            nzchar(current_query_adduct) &&
            !is.na(target_query_adduct) &&
            nzchar(target_query_adduct) &&
            !identical(current_query_adduct, target_query_adduct)
        ) {
          query_neutral <- get_neutral_precursor(
            precursor = current_precursor,
            adduct = current_query_adduct
          )
          target_neutral <- get_neutral_precursor(
            precursor = target_precursor,
            adduct = target_query_adduct
          )
          if (
            is.finite(query_neutral) &&
              is.finite(target_neutral) &&
              query_neutral > 0 &&
              target_neutral > 0
          ) {
            query_precursor_value <- query_neutral
            target_precursor_value <- target_neutral
            space_label <- "neutral_M"
          }
        }

        query_precursor_values[[pos_idx]] <- query_precursor_value
        target_precursor_values[[pos_idx]] <- target_precursor_value

        score_forward <- NA_real_
        score_reverse <- NA_real_
        matched_count <- NA_integer_

        if (use_gnps) {
          res <- call_gnps(
            current_spectrum,
            lib_spectrum,
            query_precursor_value,
            target_precursor_value,
            dalton,
            ppm,
            matchedPeaksCount = TRUE
          )
          score <- as.numeric(res[[1L]])
          matched_count <- as.integer(res[[2L]])
          score_forward <- as.numeric(res[[3L]])
          score_reverse <- as.numeric(res[[4L]])
        } else {
          score <- as.numeric(calculate_similarity(
            method = method,
            query_spectrum = current_spectrum,
            target_spectrum = lib_spectrum,
            query_precursor = query_precursor_value,
            target_precursor = target_precursor_value,
            dalton = dalton,
            ppm = ppm
          ))
          matched_count <- if (score >= threshold) {
            .count_matched_peaks(
              q_mz,
              lib_spectrum[, 1L],
              dalton,
              ppm
            )
          } else {
            NA_integer_
          }
          if (score >= threshold && isTRUE(compute_forward_reverse)) {
            fwd_rev <- call_gnps(
              current_spectrum,
              lib_spectrum,
              query_precursor_value,
              target_precursor_value,
              dalton,
              ppm,
              matchedPeaksCount = TRUE
            )
            score_forward <- as.numeric(fwd_rev[[3L]])
            score_reverse <- as.numeric(fwd_rev[[4L]])
          }
        }

        scores[[pos_idx]] <- score
        matched_counts[[pos_idx]] <- matched_count
        scores_forward[[pos_idx]] <- score_forward
        scores_reverse[[pos_idx]] <- score_reverse
        similarity_space[[pos_idx]] <- space_label
        if (compute_entropy_for_candidates) {
          entropies[[pos_idx]] <- lib_entropy[[lib_idx]]
        }
      }

      valid_indices <- which(!is.na(scores) & scores >= threshold)

      if (length(valid_indices) > 0L) {
        row_scores_forward <- scores_forward[valid_indices]
        row_scores_reverse <- scores_reverse[valid_indices]

        return(
          tidytable::tidytable(
            feature_id = rep(current_id, length(valid_indices)),
            precursorMz = rep(current_precursor, length(valid_indices)),
            target_id = lib_ids[lib_indices_sub[valid_indices]],
            candidate_spectrum_entropy = if (compute_entropy_for_candidates) {
              entropies[valid_indices]
            } else {
              rep(NA_real_, length(valid_indices))
            },
            candidate_score_similarity = scores[valid_indices],
            candidate_score_similarity_forward = row_scores_forward,
            candidate_score_similarity_reverse = row_scores_reverse,
            candidate_count_similarity_peaks_matched = matched_counts[
              valid_indices
            ],
            .similarity_space = similarity_space[valid_indices]
          )
        )
      }

      NULL
    }
  )

  if (any(query_sanitized)) {
    log_warn(
      "Sanitized %d/%d query spectra on-demand before similarity scoring.",
      sum(query_sanitized),
      n_query
    )
  }
  if (any(lib_sanitized)) {
    log_warn(
      "Sanitized %d/%d library spectra on-demand before similarity scoring.",
      sum(lib_sanitized),
      n_lib
    )
  }

  # Log progress summary
  log_info("Processed %d / %d queries", n_queries, n_queries)

  if (all(vapply(X = results, FUN = is.null, FUN.VALUE = logical(1)))) {
    result <- tidytable::tidytable(
      feature_id = NA_integer_,
      precursorMz = NA_real_,
      target_id = NA_integer_,
      candidate_spectrum_entropy = NA_real_,
      candidate_score_similarity = NA_real_,
      candidate_score_similarity_forward = NA_real_,
      candidate_score_similarity_reverse = NA_real_,
      candidate_count_similarity_peaks_matched = NA_integer_,
      .similarity_space = NA_character_
    )
  } else {
    result <- tidytable::bind_rows(
      results[!vapply(X = results, FUN = is.null, FUN.VALUE = logical(1))]
    )
  }

  log_complete(ctx, n_comparisons = nrow(result))

  result
}
