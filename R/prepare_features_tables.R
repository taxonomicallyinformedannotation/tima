#' @title Prepare features table
#'
#' @description Prepares LC-MS feature tables by standardizing column names,
#' filtering to top-intensity samples per feature, and formatting for
#' downstream analysis. Supports multiple formats (mzmine, SLAW, SIRIUS).
#'
#' @include get_params.R
#' @include rts_utils.R
#' @include logs_utils.R
#' @include validations_utils.R
#' @include safe_fread.R
#' @include export_params.R
#' @include export_output.R
#' @include validations_params.R
#'
#' @param features [character] Path to raw features file (CSV/TSV).
#' @param output [character] Path where prepared features should be saved.
#' @param candidates [integer] Number of top-intensity samples to retain per
#' feature (default: from params; recommended <=5 to balance data size and
#'     coverage).
#' @param name_adduct [character] Name of the adduct column in input.
#' @param name_features [character] Name of the feature ID column in input.
#' @param name_rt [character] Name of the retention time column in input.
#' @param name_mz [character] Name of the m/z column in input.
#'
#' @return character(1) Path to the prepared feature table (invisibly).
#'
#' @family preparation
#'
#' @export
#'
#' @examples
#' \dontrun{
#' copy_backbone()
#' go_to_cache()
#' get_file(
#'   url = get_default_paths()$urls$examples$features,
#'   export = get_params(step = "prepare_features_tables")$files$features$raw
#' )
#' prepare_features_tables()
#' unlink("data", recursive = TRUE)
#' }
prepare_features_tables <- function(
  features = get_params(step = "prepare_features_tables")$files$features$raw,
  output = get_params(
    step = "prepare_features_tables"
  )$files$features$prepared,
  candidates = get_params(
    step = "prepare_features_tables"
  )$annotations$canidates$samples,
  name_adduct = get_params(step = "prepare_features_tables")$names$adduct,
  name_features = get_params(step = "prepare_features_tables")$names$features,
  name_rt = get_params(step = "prepare_features_tables")$names$rt$features,
  name_mz = get_params(step = "prepare_features_tables")$names$precursor
) {
  ctx <- log_operation(
    "prepare_features_tables",
    input = features,
    candidates = candidates
  )

  # Input Validation ----
  .validate_prepare_features_inputs(
    features = features,
    output = output,
    candidates = candidates,
    name_adduct = name_adduct,
    name_features = name_features,
    name_rt = name_rt,
    name_mz = name_mz
  )

  # Load Features Table ----
  log_debug("Retaining top %d intensity samples per feature", candidates)

  features_raw <- .load_features_file(features)
  log_debug("Loaded %d features", nrow(features_raw))

  # Resolve configured column names against common aliases (e.g. mzTab fields).
  resolved_names <- .resolve_feature_column_names(
    tbl = features_raw,
    name_features = name_features,
    name_rt = name_rt,
    name_mz = name_mz,
    name_adduct = name_adduct
  )
  name_features <- resolved_names$name_features
  name_rt <- resolved_names$name_rt
  name_mz <- resolved_names$name_mz
  name_adduct <- resolved_names$name_adduct

  # Format and Filter ----
  features_selected <- .select_intensity_columns(
    features_raw,
    name_features = name_features,
    name_rt = name_rt,
    name_mz = name_mz,
    name_adduct = name_adduct
  )

  # Normalize RT to minutes
  features_selected <- .normalize_rt_column(
    features_selected,
    name_rt = name_rt
  )

  # Standardize column names (remove format-specific prefixes/suffixes)
  features_std <- .standardize_column_names(features_selected)

  # Filter to top intensity samples per feature
  features_prepared <- .filter_top_intensity_samples(
    features_std,
    candidates = candidates,
    name_features = name_features,
    name_rt = name_rt,
    name_mz = name_mz,
    name_adduct = name_adduct
  )

  log_info("Prepared %d feature-sample pairs", nrow(features_prepared))

  # Export Results ----
  log_complete(ctx, n_features = nrow(features_prepared))

  export_params(
    parameters = get_params(step = "prepare_features_tables"),
    step = "prepare_features_tables"
  )
  export_output(x = features_prepared, file = output)
  rm(features_prepared)

  invisible(output)
}

# --- Internal Helpers -------------------------------------------------------

#' Validate inputs for prepare_features_tables
#' @keywords internal
.validate_prepare_features_inputs <- function(
  features,
  output,
  candidates,
  name_adduct,
  name_features,
  name_rt,
  name_mz
) {
  # Validate file paths
  validate_character(features, param_name = "features", allow_empty = FALSE)
  validate_file_exists(
    path = features,
    file_type = "features file",
    param_name = "features"
  )
  validate_character(output, param_name = "output", allow_empty = FALSE)

  # Validate candidates (allow NULL or integer in range)
  if (!is.null(candidates)) {
    if (
      !is.numeric(candidates) || length(candidates) != 1L || is.na(candidates)
    ) {
      cli::cli_abort(
        "candidates must be a single numeric value or NULL",
        class = c("tima_validation_error", "tima_error"),
        call = NULL
      )
    }
    if (candidates < 1L || candidates > 100L) {
      cli::cli_abort(
        "candidates must be between 1 and 100, got {.val {candidates}}",
        class = c("tima_validation_error", "tima_error"),
        call = NULL
      )
    }
  }

  # Validate column name parameters (all must be single character strings)
  col_params <- list(
    name_adduct = name_adduct,
    name_features = name_features,
    name_rt = name_rt,
    name_mz = name_mz
  )
  # Validate all parameters
  Map(.validate_column_name_param, col_params, names(col_params))

  invisible(NULL)
}

#' Validate column name parameter
#' @keywords internal
#' @noRd
.validate_column_name_param <- function(val, param_name) {
  if (!is.character(val) || length(val) != 1L || is.na(val) || !nzchar(val)) {
    msg <- sprintf(
      "`%s` must be a single non-empty character string",
      param_name
    )
    cli::cli_abort(
      msg,
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }
}

#' Load features file with error handling
#' @keywords internal
.load_features_file <- function(features) {
  tbl <- safe_fread(
    file = features,
    file_type = "features table",
    na.strings = c("", "NA"),
    colClasses = "character"
  )

  if (nrow(tbl) == 0L) {
    cli::cli_abort(
      c(
        "features file is empty",
        "x" = features
      ),
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  tbl
}

#' Select intensity columns and metadata (auto-detect format)
#' @keywords internal
.select_intensity_columns <- function(
  tbl,
  name_features,
  name_rt,
  name_mz,
  name_adduct
) {
  core_cols <- c(name_features, name_rt, name_mz, name_adduct)

  all_cols <- colnames(tbl)
  is_core <- all_cols %in% core_cols
  is_intensity <- !is_core &
    (grepl(" Peak area", all_cols, fixed = TRUE) |
      grepl(":area", all_cols, fixed = TRUE) |
      grepl("quant_", all_cols, fixed = TRUE) |
      grepl(" Peak height", all_cols, fixed = TRUE)) &
    !grepl("quant_peaktable", all_cols, fixed = TRUE)

  if (any(is_intensity)) {
    # Order matches original tidyselect: core_cols first, then Peak area, :area, quant_, Peak height
    core_present <- intersect(core_cols, all_cols)
    int_present <- unique(c(
      all_cols[is_intensity & grepl(" Peak area", all_cols, fixed = TRUE)],
      all_cols[is_intensity & grepl(":area", all_cols, fixed = TRUE)],
      all_cols[is_intensity & grepl("quant_", all_cols, fixed = TRUE)],
      all_cols[is_intensity & grepl(" Peak height", all_cols, fixed = TRUE)]
    ))
    keep_cols <- c(core_present, int_present)
    selected <- tidytable::as_tidytable(as.list(tbl)[keep_cols])
  } else {
    core_present <- intersect(core_cols, all_cols)
    candidate_cols <- setdiff(all_cols, core_cols)

    is_numeric_like <- function(x) {
      vals <- if (is.character(x)) x else as.character(x)
      vals <- vals[!is.na(vals) & nzchar(vals)]
      if (length(vals) == 0L) {
        return(FALSE)
      }
      ratio_numeric <- mean(!is.na(suppressWarnings(as.numeric(vals))))
      ratio_numeric >= 0.8
    }

    numeric_like_cols <- candidate_cols[vapply(
      X = as.list(tbl)[candidate_cols],
      FUN = is_numeric_like,
      FUN.VALUE = logical(1L)
    )]

    if (length(numeric_like_cols) > 0L) {
      log_info(
        "No standard intensity patterns found; using %d numeric-like column(s) as intensities",
        length(numeric_like_cols)
      )
      keep_cols <- c(core_present, numeric_like_cols)
      selected <- tidytable::as_tidytable(as.list(tbl)[keep_cols])
    } else {
      selected <- tidytable::as_tidytable(as.list(tbl)[core_present])
    }
  }

  # When both Peak area and Peak height are present for the same samples,
  # prefer Peak area and drop Peak height duplicates
  sel_cols <- colnames(selected)
  area_cols <- sel_cols[endsWith(sel_cols, " Peak area")]
  height_cols <- sel_cols[endsWith(sel_cols, " Peak height")]

  if (length(area_cols) > 0L && length(height_cols) > 0L) {
    # Derive sample stems by stripping suffixes
    area_stems <- substr(area_cols, 1L, nchar(area_cols) - 10L)
    height_stems <- substr(height_cols, 1L, nchar(height_cols) - 12L)
    redundant_heights <- height_cols[height_stems %in% area_stems]

    if (length(redundant_heights) > 0L) {
      log_info(
        "Both Peak area and Peak height detected for %d sample(s); using Peak area",
        length(redundant_heights)
      )
      keep_after <- setdiff(sel_cols, redundant_heights)
      selected <- tidytable::as_tidytable(as.list(selected)[keep_after])
    }
  }

  selected
}

#' Normalize RT column to minutes (auto-detect unit if possible)
#' @keywords internal
.normalize_rt_column <- function(tbl, name_rt) {
  if (name_rt %in% colnames(tbl)) {
    tbl[[name_rt]] <- normalize_rt_to_minutes(
      tbl[[name_rt]],
      unit = "auto",
      quiet = FALSE
    )
  }
  tbl
}

#' Standardize column names by removing format-specific prefixes/suffixes
#' @keywords internal
.standardize_column_names <- function(tbl) {
  # Pattern replacements: format markers -> clean names
  replacements <- c(
    " Peak area" = "",
    ":area" = "",
    "datafile:" = "",
    "quant_" = "",
    " Peak height" = ""
  )

  # Sequential replacements
  col_names_clean <- Reduce(
    function(names_vec, pattern) {
      stri_replace_all_fixed(
        str = names_vec,
        pattern = pattern,
        replacement = replacements[[pattern]],
        vectorize_all = FALSE
      )
    },
    names(replacements),
    init = colnames(tbl)
  )

  # Safety guard: if stripping suffixes produced duplicate names (e.g. both

  # Peak area and Peak height survived), keep the first occurrence (area is
  # selected before height by .select_intensity_columns)
  dup_idx <- duplicated(col_names_clean)
  if (any(dup_idx)) {
    dup_names <- unique(col_names_clean[dup_idx])
    log_warn(
      "Duplicate column names after standardization (%s); keeping first occurrence (Peak area preferred)",
      paste(dup_names, collapse = ", ")
    )
    tbl <- tbl[, !dup_idx, with = FALSE]
    col_names_clean <- col_names_clean[!dup_idx]
  }

  colnames(tbl) <- col_names_clean
  tbl
}

#' Filter to top N intensity samples per feature
#' @keywords internal
.filter_top_intensity_samples <- function(
  tbl,
  candidates,
  name_features,
  name_rt,
  name_mz,
  name_adduct
) {
  candidates <- if (is.null(candidates)) 1L else as.integer(candidates)
  candidates <- max(candidates, 1L)

  core_cols <- c(name_features, name_rt, name_mz, name_adduct)
  intensity_cols <- setdiff(colnames(tbl), core_cols)
  if (length(intensity_cols) == 0L) {
    empty_cols <- c("feature_id", "sample")
    empty_tbl <- data.frame(
      feature_id = character(0),
      sample = character(0),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    if (name_rt %in% names(tbl)) {
      empty_tbl$rt <- vector("list", 0L)
    }
    if (name_mz %in% names(tbl)) {
      empty_tbl$mz <- vector("list", 0L)
    }
    if (name_adduct %in% names(tbl)) {
      empty_tbl$adduct <- vector("list", 0L)
    }
    return(empty_tbl)
  }

  feature_values <- tbl[[name_features]]
  feature_ids <- as.character(feature_values)
  feature_order <- suppressWarnings(as.numeric(feature_ids))
  feature_order_is_numeric <- !all(is.na(feature_order))

  tbl_names <- names(tbl)
  extra_cols <- c(
    if (name_rt %in% tbl_names) "rt",
    if (name_mz %in% tbl_names) "mz",
    if (name_adduct %in% tbl_names) "adduct"
  )

  max_keep <- min(candidates, length(intensity_cols))
  if (max_keep < 1L) {
    return(tidytable::tidytable())
  }

  n_feat <- nrow(tbl)
  if (n_feat > 0L && anyDuplicated(feature_ids) == 0L) {
    n_samp <- length(intensity_cols)
    num_mat <- matrix(NA_real_, nrow = n_feat, ncol = n_samp)
    for (j in seq_len(n_samp)) {
      col <- tbl[[intensity_cols[[j]]]]
      num_mat[, j] <- if (is.double(col)) {
        col
      } else {
        suppressWarnings(as.numeric(col))
      }
    }

    work_mat <- num_mat
    work_mat[is.na(work_mat)] <- -Inf
    work_mat[work_mat <= 0] <- -Inf

    if (max_keep <= 16L) {
      row_idx_list <- vector("list", max_keep)
      col_idx_list <- vector("list", max_keep)
      val_list <- vector("list", max_keep)
      rk_list <- vector("list", max_keep)
      active_rows <- seq_len(n_feat)

      for (k in seq_len(max_keep)) {
        n_act <- length(active_rows)
        if (n_act == 0L) {
          break
        }
        sub_m <- if (n_act == n_feat) {
          work_mat
        } else {
          work_mat[active_rows, , drop = FALSE]
        }
        best_c <- max.col(sub_m, ties.method = "first")
        lin_idx <- (best_c - 1L) * n_act + seq_len(n_act)
        best_v <- sub_m[lin_idx]
        keep <- is.finite(best_v)
        if (!any(keep)) {
          break
        }
        kept_r <- active_rows[keep]
        kept_c <- best_c[keep]
        row_idx_list[[k]] <- kept_r
        col_idx_list[[k]] <- kept_c
        val_list[[k]] <- best_v[keep]
        rk_list[[k]] <- rep.int(k, length(kept_r))
        if (k < max_keep) {
          work_mat[(kept_c - 1L) * n_feat + kept_r] <- -Inf
          active_rows <- kept_r
        }
      }
      rm(work_mat)

      kept_r_all <- unlist(row_idx_list, use.names = FALSE)
      if (length(kept_r_all) == 0L) {
        empty_tbl <- data.frame(
          feature_id = character(0),
          sample = character(0),
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
        if ("rt" %in% extra_cols) {
          empty_tbl$rt <- vector("list", 0L)
        }
        if ("mz" %in% extra_cols) {
          empty_tbl$mz <- vector("list", 0L)
        }
        if ("adduct" %in% extra_cols) {
          empty_tbl$adduct <- vector("list", 0L)
        }
        return(empty_tbl)
      }
      kept_c_all <- unlist(col_idx_list, use.names = FALSE)
      kept_v_all <- unlist(val_list, use.names = FALSE)
      kept_rk_all <- unlist(rk_list, use.names = FALSE)
    } else {
      valid_pos <- which(is.finite(work_mat))
      if (length(valid_pos) == 0L) {
        empty_tbl <- data.frame(
          feature_id = character(0),
          sample = character(0),
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
        if ("rt" %in% extra_cols) {
          empty_tbl$rt <- vector("list", 0L)
        }
        if ("mz" %in% extra_cols) {
          empty_tbl$mz <- vector("list", 0L)
        }
        if ("adduct" %in% extra_cols) {
          empty_tbl$adduct <- vector("list", 0L)
        }
        return(empty_tbl)
      }
      r_id <- (valid_pos - 1L) %% n_feat + 1L
      c_id <- (valid_pos - 1L) %/% n_feat + 1L
      v_val <- work_mat[valid_pos]
      rm(work_mat)
      ord_v <- order(r_id, -v_val, c_id, method = "radix")
      r_id <- r_id[ord_v]
      c_id <- c_id[ord_v]
      v_val <- v_val[ord_v]
      n_v <- length(r_id)
      new_g <- if (n_v > 1L) c(TRUE, r_id[-1L] != r_id[-n_v]) else TRUE
      rk <- seq_len(n_v) - cummax(ifelse(new_g, seq_len(n_v), 1L)) + 1L
      keep_rk <- which(rk <= max_keep)
      kept_r_all <- r_id[keep_rk]
      kept_c_all <- c_id[keep_rk]
      kept_v_all <- v_val[keep_rk]
      kept_rk_all <- rk[keep_rk]
    }

    if (feature_order_is_numeric) {
      ord_final <- order(
        feature_order[kept_r_all],
        feature_ids[kept_r_all],
        kept_rk_all
      )
    } else {
      ord_final <- order(feature_ids[kept_r_all], kept_rk_all)
    }

    kept_r_all <- kept_r_all[ord_final]
    kept_c_all <- kept_c_all[ord_final]
    kept_v_all <- kept_v_all[ord_final]

    u_r <- unique(kept_r_all)
    n_u_r <- length(u_r)
    samp_ord <- order(intensity_cols)
    mat_t_sorted <- t(num_mat[u_r, samp_ord, drop = FALSE])
    rm(num_mat)
    dim(mat_t_sorted) <- NULL
    grp_fac <- structure(
      rep(seq_len(n_u_r), each = n_samp),
      levels = character(n_u_r),
      class = "factor"
    )
    intensity_u_list <- split.default(mat_t_sorted, grp_fac)
    rm(mat_t_sorted, grp_fac)
    names(intensity_u_list) <- NULL
    r_to_u <- fastmatch::fmatch(kept_r_all, u_r)

    out <- data.frame(
      feature_id = feature_ids[kept_r_all],
      sample = intensity_cols[kept_c_all],
      intensity = kept_v_all,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    if ("rt" %in% extra_cols) {
      out$rt <- tbl[[name_rt]][kept_r_all]
    }
    if ("mz" %in% extra_cols) {
      out$mz <- tbl[[name_mz]][kept_r_all]
    }
    if ("adduct" %in% extra_cols) {
      out$adduct <- tbl[[name_adduct]][kept_r_all]
    }
    out$intensity_all <- intensity_u_list[r_to_u]

    keep_cols <- intersect(
      c(
        "feature_id",
        "rt",
        "mz",
        "adduct",
        "sample",
        "intensity",
        "intensity_all"
      ),
      names(out)
    )
    out <- out[, keep_cols, drop = FALSE]
    row.names(out) <- NULL
    return(out)
  }

  tbl_df <- as.data.frame(tbl)
  long_tbl <- tidytable::as_tidytable(tbl_df) |>
    tidytable::pivot_longer(
      cols = tidyselect::all_of(intensity_cols),
      names_to = "sample",
      values_to = "value"
    ) |>
    tidytable::mutate(
      feature_id = as.character(.data[[name_features]]),
      value_num = suppressWarnings(as.numeric(value))
    )

  # Collect all unfiltered intensities per feature before top-N filtering
  # CRITICAL: Sort by sample to ensure consistent ordering across all features
  # This way feature_1's [intensity_s1, intensity_s2, ...] aligns with
  # feature_2's [intensity_s1, intensity_s2, ...] for proper correlation
  all_intensities <- long_tbl |>
    tidytable::arrange(feature_id, sample) |>
    tidytable::summarise(
      intensity_all = list(value_num),
      .by = feature_id
    )

  # For top-N filtering, only use non-NA, non-zero values (presence-based ranking)
  long_tbl_ranked <- long_tbl |>
    tidytable::filter(!is.na(value_num) & value_num > 0)

  # Apply top-N filtering based on non-zero values
  long_tbl_filtered <- long_tbl_ranked |>
    tidytable::mutate(
      rank = rank(-value_num, ties.method = "first"),
      .by = feature_id
    ) |>
    tidytable::filter(rank <= max_keep) |>
    tidytable::arrange(feature_id, rank)

  if (nrow(long_tbl_filtered) == 0L) {
    empty_tbl <- data.frame(
      feature_id = character(0),
      sample = character(0),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    if ("rt" %in% extra_cols) {
      empty_tbl$rt <- vector("list", 0L)
    }
    if ("mz" %in% extra_cols) {
      empty_tbl$mz <- vector("list", 0L)
    }
    if ("adduct" %in% extra_cols) {
      empty_tbl$adduct <- vector("list", 0L)
    }
    return(empty_tbl)
  }

  out <- data.frame(
    feature_id = long_tbl_filtered$feature_id,
    sample = long_tbl_filtered$sample,
    intensity = long_tbl_filtered$value_num,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  if ("rt" %in% extra_cols) {
    out$rt <- long_tbl_filtered[[name_rt]]
  }
  if ("mz" %in% extra_cols) {
    out$mz <- long_tbl_filtered[[name_mz]]
  }
  if ("adduct" %in% extra_cols) {
    out$adduct <- long_tbl_filtered[[name_adduct]]
  }

  # Add all-intensities column (one list per feature)
  out <- out |>
    tidytable::left_join(all_intensities, by = "feature_id") |>
    as.data.frame()

  if (feature_order_is_numeric) {
    order_idx <- order(
      feature_order[match(out$feature_id, feature_ids)],
      as.character(out$feature_id)
    )
  } else {
    order_idx <- order(as.character(out$feature_id))
  }
  out <- out[order_idx, , drop = FALSE]

  keep_cols <- intersect(
    c(
      "feature_id",
      "rt",
      "mz",
      "adduct",
      "sample",
      "intensity",
      "intensity_all"
    ),
    names(out)
  )
  out <- out[, keep_cols, drop = FALSE]

  row.names(out) <- NULL
  out
}

#' Resolve feature-table column names from configured values or aliases
#' @keywords internal
.resolve_feature_column_names <- function(
  tbl,
  name_features,
  name_rt,
  name_mz,
  name_adduct
) {
  cols <- colnames(tbl)

  resolve_one <- function(
    requested,
    aliases,
    required = TRUE,
    label = requested
  ) {
    if (requested %in% cols) {
      return(requested)
    }

    candidates <- unique(c(aliases, requested))
    matched <- candidates[candidates %in% cols]
    if (length(matched) > 0L) {
      chosen <- matched[[1L]]
      log_warn(
        "Column '%s' not found; using '%s' for %s",
        requested,
        chosen,
        label
      )
      return(chosen)
    }

    if (required) {
      cli::cli_abort(
        c(
          sprintf("Required column '%s' is missing", requested),
          i = sprintf("Tried aliases: %s", paste(candidates, collapse = ", ")),
          i = sprintf("Available columns: %s", paste(cols, collapse = ", "))
        ),
        class = c("tima_validation_error", "tima_error"),
        call = NULL
      )
    }

    requested
  }

  list(
    name_features = resolve_one(
      requested = name_features,
      aliases = c("feature_id", "row ID", "SML_ID", "SMF_ID", "id"),
      required = TRUE,
      label = "feature IDs"
    ),
    name_rt = resolve_one(
      requested = name_rt,
      aliases = c(
        "rt",
        "row retention time",
        "retention_time_in_minutes",
        "retention_time_in_seconds"
      ),
      required = FALSE,
      label = "retention time"
    ),
    name_mz = resolve_one(
      requested = name_mz,
      aliases = c("mz", "row m/z", "exp_mass_to_charge", "precursor_mz"),
      required = TRUE,
      label = "precursor m/z"
    ),
    name_adduct = resolve_one(
      requested = name_adduct,
      aliases = c("adduct", "best ion", "adduct_ion", "adduct_ions"),
      required = FALSE,
      label = "adduct"
    )
  )
}
