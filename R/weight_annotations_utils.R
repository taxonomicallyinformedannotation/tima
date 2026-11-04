#' Validate Inputs for weight_annotations
#'
#' @description Internal helper to validate all input parameters for
#'     weight_annotations.
#' Checks file existence, data types, ranges, and logical consistency of
#'     weights.
#'     Stops execution with informative messages if any condition is violated.
#'
#' @include add_xrefs_to_annotations.R
#' @include constants.R
#' @include safe_fread.R
#' @include validations_utils.R
#'
#' @param library Character, path to library file (required).
#' @param components Character, path to components file (required).
#' @param edges Character, path to edges file (required).
#' @param taxa Character, path to taxa file (required).
#' @param annotations Character vector, paths to annotation files (required).
#' @param str_stereo Character, path to stereo structures file (optional).
#' @param org_tax_ott Character, path to organism taxonomy file (optional).
#' @param canopus Character, path to canopus file (optional).
#' @param formula Character, path to formula file (optional).
#' @param minimal_ms1_condition Character, "OR" or "AND".
#' @param weight_spectral Numeric (0-Inf), weight for spectral score.
#' @param weight_chemical Numeric (0-Inf), weight for chemical score.
#' @param weight_biological Numeric (0-Inf), weight for biological score.
#' @param minimal_consistency Numeric (0-1), min consistency threshold.
#' @param minimal_ms1_bio Numeric (0-1), min biological score for MS1.
#' @param minimal_ms1_chemo Numeric (0-1), min chemical score for MS1.
#' @param ms1_only Logical, filter for MS1-only annotations.
#' @param compounds_names Logical, include compound names.
#' @param high_evidence Logical, also restrict the full results table to
#'     high-evidence candidates (the filtered and mini tables always are).
#' @param remove_ties Logical, remove tied candidates.
#' @param summarize Logical, summarize to one row per feature.
#' @param force Logical, force execution (override warnings).
#' @param candidates_neighbors Integer >= 1, neighbors to keep.
#' @param candidates_final Integer >= 1, final candidates to keep.
#'
#' @return NULL (invisible). Stops execution if validation fails.
#' @keywords internal
validate_weight_annotations_inputs <- function(
  library,
  components,
  edges,
  taxa,
  annotations,
  str_stereo = NULL,
  org_tax_ott = NULL,
  canopus = NULL,
  formula = NULL,
  minimal_ms1_condition,
  weight_spectral,
  weight_chemical,
  weight_biological,
  minimal_consistency,
  minimal_ms1_bio,
  minimal_ms1_chemo,
  ms1_only,
  compounds_names,
  high_evidence,
  remove_ties,
  summarize,
  force,
  candidates_neighbors,
  candidates_final
) {
  # Validate required file existence
  required_files <- list(
    library = library,
    components = components,
    edges = edges,
    taxa = taxa
  )
  missing_required <- Filter(function(x) !file.exists(x), required_files)
  if (length(missing_required) > 0L) {
    cli::cli_abort(
      c(
        "Required file(s) not found",
        "x" = paste(
          names(missing_required),
          missing_required,
          sep = " at ",
          collapse = "; "
        )
      ),
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  # Validate annotation files
  missing_annotations <- annotations[!file.exists(annotations)]
  if (length(missing_annotations) > 0L) {
    cli::cli_abort(
      c(
        "Annotation file(s) not found",
        "x" = paste(missing_annotations, collapse = ", ")
      ),
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  # Validate optional files (log warnings only)
  optional_files <- list(
    str_stereo = str_stereo,
    org_tax_ott = org_tax_ott,
    canopus = canopus,
    formula = formula
  )
  for (name in names(optional_files)) {
    path <- optional_files[[name]]
    if (!is.null(path) && !file.exists(path)) {
      log_warn("Optional file '%s' not found at: %s", name, path)
    }
  }

  # Validate minimal_ms1_condition
  assert_choice(minimal_ms1_condition, c("OR", "AND"), "minimal_ms1_condition")

  # Validate weights: must be numeric, non-negative, sum to ~1
  weights <- c(
    spectral = weight_spectral,
    chemical = weight_chemical,
    biological = weight_biological
  )
  if (!all(is.numeric(weights)) || anyNA(weights)) {
    cli::cli_abort(
      "All weights (spectral, chemical, biological) must be numeric and non-NA",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }
  if (any(weights < 0)) {
    cli::cli_abort(
      c(
        "All weights must be non-negative",
        "x" = paste(names(weights), "=", weights, collapse = ", ")
      ),
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }
  weight_sum <- sum(weights)
  if (abs(weight_sum - 1) > WEIGHT_SUM_TOLERANCE) {
    cli::cli_abort(
      c(
        "Weights must sum to 1",
        "x" = paste0(
          "tolerance=",
          WEIGHT_SUM_TOLERANCE,
          "; sum=",
          signif(weight_sum, 6),
          " (",
          paste(names(weights), weights, sep = "=", collapse = ", "),
          ")"
        )
      ),
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  # Validate score thresholds (0-1)
  assert_scalar_numeric(
    minimal_consistency,
    "minimal_consistency",
    min = 0,
    max = 1
  )
  assert_scalar_numeric(minimal_ms1_bio, "minimal_ms1_bio", min = 0, max = 1)
  assert_scalar_numeric(
    minimal_ms1_chemo,
    "minimal_ms1_chemo",
    min = 0,
    max = 1
  )

  # Validate logical flags
  assert_flag(ms1_only, "ms1_only")
  assert_flag(compounds_names, "compounds_names")
  assert_flag(high_evidence, "high_evidence")
  assert_flag(remove_ties, "remove_ties")
  assert_flag(summarize, "summarize")
  assert_flag(force, "force")

  # Validate candidate counts
  assert_positive_integer(candidates_neighbors, "candidates_neighbors")
  assert_positive_integer(candidates_final, "candidates_final")

  invisible(NULL)
}

#' Load and Prepare Annotation Tables
#'
#' @description Internal helper to load, combine, and filter annotation tables.
#'     Reads multiple annotation files, binds rows, and optionally filters to
#' retain only MS1-based annotations (those without MS2 similarity or SIRIUS CSI
#'     scores).
#'
#' @param annotations [character] Character vector of file paths to annotation
#'     files.
#' @param ms1_only [logical] Logical; if TRUE, keep only annotations where both
#'     `candidate_score_similarity` and `candidate_score_sirius_csi` are NA.
#'
#' @return Data frame with combined (and optionally filtered) annotations.
#' @keywords internal
load_annotation_tables <- function(annotations, ms1_only) {
  log_debug("Loading %d annotation file(s)", length(annotations))
  annotation_table <- tryCatch(
    if (length(annotations) == 1L) {
      .safe_fread_selected_columns(
        file = annotations[[1L]],
        file_type = "annotation file 1",
        selector = select_weight_annotations_working_columns
      )
    } else {
      tbl_list <- Map(
        function(x, y) {
          .safe_fread_selected_columns(
            file = x,
            file_type = paste0("annotation file ", y),
            selector = select_weight_annotations_working_columns
          )
        },
        annotations,
        seq_along(annotations)
      )
      res <- tidytable::as_tidytable(data.table::rbindlist(
        tbl_list,
        use.names = TRUE,
        fill = TRUE
      ))
      rm(tbl_list)
      res
    },
    error = function(e) {
      cli::cli_abort(
        c(
          "Failed to load annotation files",
          "x" = conditionMessage(e)
        ),
        class = c("tima_runtime_error", "tima_error"),
        call = NULL
      )
    }
  )

  if (ms1_only) {
    n_before <- nrow(annotation_table)
    nms_ann <- names(annotation_table)
    keep_mask <- rep(TRUE, n_before)
    if ("candidate_score_similarity" %in% nms_ann) {
      keep_mask <- keep_mask &
        is.na(annotation_table$candidate_score_similarity)
    }
    if ("candidate_score_sirius_csi" %in% nms_ann) {
      keep_mask <- keep_mask &
        is.na(annotation_table$candidate_score_sirius_csi)
    }
    if ("candidate_score_sirius_confidence" %in% nms_ann) {
      conf_num <- .as_numeric_dedup(
        annotation_table$candidate_score_sirius_confidence
      )
      keep_mask <- keep_mask & (is.na(conf_num) | conf_num == 0)
      rm(conf_num)
    }
    if (!all(keep_mask)) {
      keep_idx <- which(keep_mask)
      rm(keep_mask)
      annotation_table <- annotation_table[keep_idx, ]
      rm(keep_idx)
    } else {
      rm(keep_mask)
    }

    log_debug(
      "MS1-only filter: %d -> %d rows",
      n_before,
      nrow(annotation_table)
    )
  }

  if (nrow(annotation_table) == 0L) {
    log_warn("No annotations remaining after loading/filtering")
  }

  annotation_table
}

#' Load Structure-Organism Pairs
#'
#' @description Internal helper to load the main library table and sequentially
#' left-join optional supplemental tables (stereo structures, organism
#'     taxonomy).
#'     Returns a combined data frame suitable for biological scoring.
#'
#' @param library [character] Character, path to main library file (required).
#' @param str_stereo [character] Character, path to stereo structures file
#'     (optional; NULL allowed).
#' @param org_tax_ott [character] Character, path to organism taxonomy file
#'     (optional; NULL allowed).
#' @param needed_structures [character] Optional character vector of candidate
#'     structure connectivity layers (`structure_inchikey_connectivity_layer`)
#'     to filter structure-organism pairs during loading.
#'
#' @return Data frame with structure-organism pairs, including any joined
#'     metadata.
#' @keywords internal
load_structure_organism_pairs <- function(
  library,
  str_stereo,
  org_tax_ott,
  needed_structures = NULL
) {
  log_debug("Loading library from: %s", library)
  library_table <- tryCatch(
    .safe_fread_selected_columns(
      file = library,
      file_type = "structure library",
      selector = select_structure_organism_pair_loading_columns
    ),
    error = function(e) {
      cli::cli_abort(
        c(
          "Failed to load library file",
          "x" = conditionMessage(e)
        ),
        class = c("tima_runtime_error", "tima_error"),
        call = NULL
      )
    }
  )

  if (!is.null(needed_structures)) {
    needed_structures <- unique(needed_structures[!is.na(needed_structures)])
  }

  supp_files <- list(str_stereo, org_tax_ott)
  supp_names <- c("stereochemistry", "organism taxonomy")
  supp_tables <- Map(
    function(path, name) {
      if (is.null(path) || !file.exists(path)) {
        return(tidytable::tidytable())
      }
      .safe_fread_selected_columns(
        file = path,
        file_type = name,
        selector = select_structure_organism_pair_loading_columns
      )
    },
    supp_files,
    supp_names
  )

  if (
    length(needed_structures) > 0L &&
      nrow(supp_tables[[1L]]) > 0L &&
      "structure_inchikey_connectivity_layer" %in% names(supp_tables[[1L]])
  ) {
    keep_st <- which(
      !is.na(fastmatch::fmatch(
        supp_tables[[1L]]$structure_inchikey_connectivity_layer,
        needed_structures
      ))
    )
    supp_tables[[1L]] <- supp_tables[[1L]][keep_st, ]
  }

  joined <- tidytable::as_tidytable(library_table)
  rm(library_table)
  for (i in seq_along(supp_tables)) {
    tbl <- supp_tables[[i]]
    supp_tables[[i]] <- tidytable::tidytable()
    if (nrow(tbl) == 0L) {
      if (
        i == 1L &&
          length(needed_structures) > 0L &&
          "structure_inchikey_connectivity_layer" %in% names(joined)
      ) {
        keep_j <- which(
          !is.na(fastmatch::fmatch(
            joined$structure_inchikey_connectivity_layer,
            needed_structures
          ))
        )
        joined <- joined[keep_j, ]
      }
      next
    }
    by_cols <- intersect(names(joined), names(tbl))
    if (
      length(by_cols) == 1L &&
        anyDuplicated(tbl[[by_cols]]) == 0L &&
        !anyNA(tbl[[by_cols]])
    ) {
      key_col <- by_cols[[1L]]
      m_idx <- fastmatch::fmatch(joined[[key_col]], tbl[[key_col]])
      new_cols <- setdiff(names(tbl), key_col)
      for (col in new_cols) {
        data.table::set(joined, j = col, value = tbl[[col]][m_idx])
      }
    } else if (
      length(by_cols) == 2L &&
        anyDuplicated(tbl[[by_cols[[1L]]]]) == 0L &&
        !anyNA(tbl[[by_cols[[1L]]]])
    ) {
      k1 <- by_cols[[1L]]
      k2 <- by_cols[[2L]]
      m_idx <- fastmatch::fmatch(joined[[k1]], tbl[[k1]])
      v1 <- joined[[k2]]
      v2 <- tbl[[k2]][m_idx]
      ok_k2 <- !is.na(m_idx) &
        ((is.na(v1) & is.na(v2)) | (!is.na(v1) & !is.na(v2) & v1 == v2))
      m_idx[!ok_k2] <- NA_integer_
      new_cols <- setdiff(names(tbl), by_cols)
      for (col in new_cols) {
        data.table::set(joined, j = col, value = tbl[[col]][m_idx])
      }
    } else if (length(by_cols) > 0L) {
      joined <- tidytable::left_join(x = joined, y = tbl, by = by_cols)
    }
    rm(tbl)

    if (
      i == 1L &&
        length(needed_structures) > 0L &&
        "structure_inchikey_connectivity_layer" %in% names(joined)
    ) {
      keep_j <- which(
        !is.na(fastmatch::fmatch(
          joined$structure_inchikey_connectivity_layer,
          needed_structures
        ))
      )
      joined <- joined[keep_j, ]
    }
  }
  rm(supp_tables)
  select_structure_organism_pair_columns(joined)
}

#' Load Edges Table with Neighbor Filtering
#'
#' @description Internal helper to load the edges file and retain only the top N
#'     neighbors (by similarity score) for each feature. This reduces complexity
#'     for downstream annotation propagation and scoring.
#'
#' @param edges [character] Character, path to edges file.
#' @param candidates_neighbors [integer] Integer, number of top neighbors to
#'     keep per feature.
#'
#' @return Data frame with filtered edges (top `candidates_neighbors` per
#'     `feature_source`).
#' @keywords internal
load_edges_table <- function(edges, candidates_neighbors) {
  log_debug("Loading edges from: %s", edges)
  edges_table <- tryCatch(
    .safe_fread_selected_columns(
      file = edges,
      file_type = "spectral edges table",
      selector = .select_edges_working_columns
    ),
    error = function(e) {
      cli::cli_abort(
        c(
          "Failed to load edges file",
          "x" = conditionMessage(e)
        ),
        class = c("tima_runtime_error", "tima_error"),
        call = NULL
      )
    }
  )

  n_before <- nrow(edges_table)
  if (n_before > 0L && "feature_source" %in% names(edges_table)) {
    # Columns are read as character; rank on the numeric value, not the text
    sim_num <- if ("candidate_score_similarity" %in% names(edges_table)) {
      .as_numeric_dedup(edges_table$candidate_score_similarity)
    } else {
      rep(NA_real_, n_before)
    }
    src_col <- edges_table$feature_source
    u_src <- unique(src_col)
    src_id <- fastmatch::fmatch(src_col, u_src)
    ord <- order(src_id, -sim_num, na.last = TRUE, method = "radix")
    s_ord <- src_id[ord]
    counts <- tabulate(s_ord, nbins = length(u_src))
    present <- which(counts > 0L)
    c_pres <- counts[present]
    starts <- cumsum(c(1L, c_pres[-length(c_pres)]))
    keep_counts <- pmin(c_pres, as.integer(candidates_neighbors))
    keep_idx <- ord[sequence(keep_counts, from = starts)]
    edges_table <- edges_table[keep_idx, , drop = FALSE]
  }

  log_debug(
    "Edges filtered to top %d neighbors/feature: %d -> %d rows",
    candidates_neighbors,
    n_before,
    nrow(edges_table)
  )
  edges_table
}

.safe_fread_selected_columns <- function(file, file_type, selector) {
  header <- safe_fread(
    file = file,
    file_type = paste0(file_type, " header"),
    na.strings = c("", "NA"),
    colClasses = "character",
    nrows = 0L
  )
  selected <- names(selector(header))
  if (length(selected) == 0L) {
    return(safe_fread(
      file = file,
      file_type = file_type,
      na.strings = c("", "NA"),
      colClasses = "character"
    ))
  }

  safe_fread(
    file = file,
    file_type = file_type,
    na.strings = c("", "NA"),
    colClasses = "character",
    select = selected
  )
}

.select_edges_working_columns <- function(df) {
  tidytable::select(
    df,
    tidyselect::any_of(c(
      "feature_source",
      "feature_target",
      "candidate_score_similarity",
      "feature_spectrum_entropy",
      "feature_spectrum_peaks"
    ))
  )
}

#' Log Annotation Statistics
#'
#' @description Internal helper to log annotation counts by library.
#'
#' @param annotation_table [data.frame] Data frame with annotations
#'
#' @return NULL (logs statistics as side effect)
#' @keywords internal
log_annotation_stats <- function(annotation_table) {
  ik_col <- annotation_table$candidate_structure_inchikey_connectivity_layer
  keep <- which(!is.na(ik_col))
  if (length(keep) == 0L) {
    annotation_stats <- tidytable::tidytable(
      candidate_library = character(0L),
      n = integer(0L),
      Pct = character(0L)
    )
  } else {
    fid_col <- annotation_table$feature_id[keep]
    lib_col <- annotation_table$candidate_library[keep]
    ik_sub <- ik_col[keep]

    u_fids <- unique(fid_col)
    u_libs <- sort(unique(lib_col), na.last = TRUE)
    u_iks <- unique(ik_sub)

    fid_id <- fastmatch::fmatch(fid_col, u_fids)
    lib_id <- fastmatch::fmatch(lib_col, u_libs)
    ik_id <- fastmatch::fmatch(ik_sub, u_iks)

    n_libs <- max(1L, length(u_libs))
    n_iks <- max(1L, length(u_iks))
    max_combos <- as.double(length(u_fids)) * n_libs * n_iks
    if (max_combos < .Machine$integer.max) {
      combo_key <- ((fid_id - 1L) * n_libs + (lib_id - 1L)) * n_iks + ik_id
      first_occ <- !duplicated(combo_key)
    } else {
      combo_key <- ((as.numeric(fid_id - 1L) * n_libs + (lib_id - 1L)) *
        n_iks) +
        ik_id
      first_occ <- fastmatch::fmatch(combo_key, combo_key) ==
        seq_along(combo_key)
    }
    lib_counts <- tabulate(lib_id[first_occ], nbins = length(u_libs))
    ord_l <- order(lib_counts, decreasing = TRUE)
    annotation_stats <- tidytable::tidytable(
      candidate_library = u_libs[ord_l],
      n = lib_counts[ord_l]
    )
    annotation_stats <- add_percentage_column(
      annotation_stats,
      count_col = "n",
      out_col = "Pct"
    )
  }

  log_info(
    "\n%s",
    paste(
      utils::capture.output(print.data.frame(
        x = annotation_stats,
        row.names = FALSE
      )),
      collapse = "\n"
    )
  )

  invisible(NULL)
}

.dedup_ordered_tuple_indices <- function(
  annotation_table,
  dedup_cols,
  ord1,
  bases_override = NULL
) {
  n_dc <- length(dedup_cols)
  ids_dc <- vector("list", n_dc)
  bases_dc <- numeric(n_dc)
  for (i in seq_len(n_dc)) {
    dc_v <- annotation_table[[dedup_cols[[i]]]]
    u_dc <- unique(dc_v)
    id_i <- if (anyNA(u_dc)) {
      match(dc_v, u_dc)
    } else {
      fastmatch::fmatch(dc_v, u_dc)
    }
    ids_dc[[i]] <- id_i[ord1]
    bases_dc[[i]] <- as.double(max(1L, length(u_dc)))
    rm(dc_v, u_dc, id_i)
  }
  if (!is.null(bases_override)) {
    bases_dc <- as.double(bases_override)
  }
  total_combos <- prod(bases_dc)
  if (total_combos < .Machine$integer.max) {
    k1 <- ids_dc[[1L]]
    if (n_dc >= 2L) {
      for (i in 2:n_dc) {
        bi <- as.integer(bases_dc[[i]])
        k1 <- (k1 - 1L) * bi + ids_dc[[i]]
      }
    }
    keep1_mask <- !duplicated(k1)
    rm(k1, ids_dc)
    keep1_idx <- ord1[keep1_mask]
    rm(keep1_mask)
  } else if (total_combos < 4503599627370496) {
    k1 <- as.double(ids_dc[[1L]])
    if (n_dc >= 2L) {
      for (i in 2:n_dc) {
        bi <- bases_dc[[i]]
        k1 <- (k1 - 1) * bi + ids_dc[[i]]
      }
    }
    keep1_mask <- fastmatch::fmatch(k1, k1) == seq_along(k1)
    rm(k1, ids_dc)
    keep1_idx <- ord1[keep1_mask]
    rm(keep1_mask)
  } else {
    rm(ids_dc)
    dedup_sub <- tidytable::as_tidytable(
      as.list(annotation_table)[dedup_cols]
    )[ord1, ]
    keep1_idx <- ord1[!duplicated(dedup_sub)]
    rm(dedup_sub)
  }
  keep1_idx
}

#' Rearrange Annotation Tables
#'
#' @description Internal helper to reorganize and merge annotation tables.
#'
#' @include columns_utils.R
#' @include logs_utils.R
#' @include get_default_paths.R
#' @include get_params.R
#' @include export_params.R
#' @include export_output.R
#' @include assert_utils.R
#'
#' @param annotation_table [data.frame] Data frame with annotations
#' @param formula_table [data.frame] Data frame with formula data
#' @param canopus_table [data.frame] Data frame with canopus data
#' @param edges_table [data.frame] Data frame with edges
#'
#' @return Data frame with rearranged annotations
#' @keywords internal
rearrange_annotations <- function(
  annotation_table,
  formula_table,
  canopus_table,
  edges_table,
  consume_input = FALSE
) {
  model <- columns_model()
  ann_names <- names(annotation_table)

  # Table 1: deduplicated annotations (all sources, spectra + structure cols)
  t1_cols <- intersect(
    c(
      model$features_columns,
      model$candidates_calculated_columns,
      model$candidates_spectra_columns,
      model$candidates_structures_columns
    ),
    ann_names
  )

  # Pre-extract SIRIUS structure-level rows BEFORE consuming/subsetting
  # annotation_table so we do not need to retain the full input table in memory
  t2_cols <- intersect(
    c(
      "feature_id",
      "candidate_structure_inchikey_connectivity_layer",
      model$candidates_sirius_str_columns
    ),
    ann_names
  )
  sirius_new_cols <- setdiff(t2_cols, t1_cols)
  csi_idx <- if ("candidate_score_sirius_csi" %in% ann_names) {
    which(!is.na(annotation_table$candidate_score_sirius_csi))
  } else {
    integer(0L)
  }
  sirius_protos <- vector("list", length(sirius_new_cols))
  names(sirius_protos) <- sirius_new_cols
  for (col in sirius_new_cols) {
    sirius_protos[[col]] <- annotation_table[[col]][NA_integer_]
  }
  sirius_sub <- NULL
  if (
    length(csi_idx) > 0L &&
      length(sirius_new_cols) > 0L &&
      all(
        c(
          "feature_id",
          "candidate_structure_inchikey_connectivity_layer"
        ) %in%
          t1_cols
      )
  ) {
    sirius_keep_cols <- unique(c(
      "feature_id",
      "candidate_structure_inchikey_connectivity_layer",
      sirius_new_cols
    ))
    sirius_sub <- vector("list", length(sirius_keep_cols))
    names(sirius_sub) <- sirius_keep_cols
    for (col in sirius_keep_cols) {
      sirius_sub[[col]] <- annotation_table[[col]][csi_idx]
    }
  }

  n_in <- nrow(annotation_table)
  sim_num <- if ("candidate_score_similarity" %in% ann_names) {
    .as_numeric_dedup(annotation_table$candidate_score_similarity)
  } else {
    rep(NA_real_, n_in)
  }

  dedup_cols <- intersect(
    c(
      "feature_id",
      "candidate_structure_inchikey_no_stereo",
      "candidate_structure_inchikey_connectivity_layer"
    ),
    t1_cols
  )
  if (
    length(dedup_cols) == 3L &&
      identical(
        annotation_table$candidate_structure_inchikey_no_stereo,
        annotation_table$candidate_structure_inchikey_connectivity_layer
      )
  ) {
    dedup_cols <- c(
      "feature_id",
      "candidate_structure_inchikey_connectivity_layer"
    )
  }

  if (n_in <= 1L || length(dedup_cols) == 0L) {
    if (isTRUE(consume_input) && data.table::is.data.table(annotation_table)) {
      drop_cols <- setdiff(names(annotation_table), t1_cols)
      if (length(drop_cols) > 0L) {
        data.table::set(annotation_table, j = drop_cols, value = NULL)
      }
      annotation_table_merged <- tidytable::as_tidytable(
        as.list(annotation_table)[t1_cols]
      )
    } else {
      annotation_table_merged <- data.table::copy(
        tidytable::as_tidytable(as.list(annotation_table)[t1_cols])
      )
    }
    if ("candidate_score_similarity" %in% names(annotation_table_merged)) {
      data.table::set(
        annotation_table_merged,
        j = "candidate_score_similarity",
        value = sim_num
      )
    }
  } else {
    ord1 <- order(-sim_num, na.last = TRUE, method = "radix")
    keep1_idx <- .dedup_ordered_tuple_indices(
      annotation_table = annotation_table,
      dedup_cols = dedup_cols,
      ord1 = ord1
    )
    rm(ord1)

    if (isTRUE(consume_input) && data.table::is.data.table(annotation_table)) {
      drop_cols <- setdiff(names(annotation_table), t1_cols)
      if (length(drop_cols) > 0L) {
        data.table::set(annotation_table, j = drop_cols, value = NULL)
      }
      if (identical(keep1_idx, seq_len(n_in))) {
        annotation_table_merged <- tidytable::as_tidytable(
          as.list(annotation_table)[t1_cols]
        )
        if ("candidate_score_similarity" %in% names(annotation_table_merged)) {
          data.table::set(
            annotation_table_merged,
            j = "candidate_score_similarity",
            value = sim_num
          )
        }
      } else {
        merged_list <- vector("list", length(t1_cols))
        names(merged_list) <- t1_cols
        for (col in t1_cols) {
          if (col == "candidate_score_similarity") {
            data.table::set(annotation_table, j = col, value = NULL)
            merged_list[[col]] <- sim_num[keep1_idx]
          } else {
            col_v <- annotation_table[[col]]
            data.table::set(annotation_table, j = col, value = NULL)
            merged_list[[col]] <- col_v[keep1_idx]
            rm(col_v)
          }
        }
        annotation_table_merged <- tidytable::as_tidytable(merged_list)
        rm(merged_list)
      }
    } else {
      annotation_table_merged <- tidytable::as_tidytable(
        as.list(annotation_table)[t1_cols]
      )[keep1_idx, ]
      if ("candidate_score_similarity" %in% names(annotation_table_merged)) {
        data.table::set(
          annotation_table_merged,
          j = "candidate_score_similarity",
          value = sim_num[keep1_idx]
        )
      }
    }
    rm(keep1_idx)
  }
  rm(sim_num)

  # Step 1: Merge SIRIUS structure-level scores into deduplicated annotations
  # by (feature_id, candidate_structure_inchikey_connectivity_layer) without
  # copying the entire multi-million-row annotation table via full_join.
  if (length(csi_idx) == 0L) {
    n_rows <- nrow(annotation_table_merged)
    for (col in sirius_new_cols) {
      val <- if (col == "candidate_score_sirius_csi") {
        rep.int(NA_real_, n_rows)
      } else {
        rep.int(sirius_protos[[col]], n_rows)
      }
      data.table::set(annotation_table_merged, j = col, value = val)
    }
  } else if (!is.null(sirius_sub)) {
    csi_num <- .as_numeric_dedup(sirius_sub[["candidate_score_sirius_csi"]])
    rel_ord2 <- order(-csi_num, na.last = TRUE, method = "radix")
    f2 <- sirius_sub[["feature_id"]][rel_ord2]
    ik2 <- sirius_sub[[
      "candidate_structure_inchikey_connectivity_layer"
    ]][rel_ord2]
    u_f2 <- unique(f2)
    u_ik2 <- unique(ik2)
    f2_id <- if (anyNA(u_f2)) {
      match(f2, u_f2)
    } else {
      fastmatch::fmatch(f2, u_f2)
    }
    ik2_id <- if (anyNA(u_ik2)) {
      match(ik2, u_ik2)
    } else {
      fastmatch::fmatch(ik2, u_ik2)
    }
    n_ik2 <- max(1L, length(u_ik2))
    use_int2 <- as.double(length(u_f2)) * n_ik2 < .Machine$integer.max
    k2 <- if (use_int2) {
      (f2_id - 1L) * n_ik2 + ik2_id
    } else {
      as.double(f2_id - 1L) * n_ik2 + ik2_id
    }
    keep2_mask <- if (use_int2) {
      !duplicated(k2)
    } else {
      fastmatch::fmatch(k2, k2) == seq_along(k2)
    }
    keep2_rel <- rel_ord2[keep2_mask]
    k2_u <- k2[keep2_mask]

    m_f <- if (anyNA(u_f2)) {
      match(annotation_table_merged$feature_id, u_f2)
    } else {
      fastmatch::fmatch(annotation_table_merged$feature_id, u_f2)
    }
    m_ik <- if (anyNA(u_ik2)) {
      match(
        annotation_table_merged$candidate_structure_inchikey_connectivity_layer,
        u_ik2
      )
    } else {
      fastmatch::fmatch(
        annotation_table_merged$candidate_structure_inchikey_connectivity_layer,
        u_ik2
      )
    }
    both_ok <- !is.na(m_f) & !is.na(m_ik)
    m_pos <- rep.int(NA_integer_, nrow(annotation_table_merged))
    if (any(both_ok)) {
      k_m <- if (use_int2) {
        (m_f[both_ok] - 1L) * n_ik2 + m_ik[both_ok]
      } else {
        as.double(m_f[both_ok] - 1L) * n_ik2 + m_ik[both_ok]
      }
      m_pos[both_ok] <- fastmatch::fmatch(k_m, k2_u)
    }
    csi_u <- csi_num[keep2_rel]
    for (col in sirius_new_cols) {
      val <- if (col == "candidate_score_sirius_csi") {
        csi_u[m_pos]
      } else {
        sirius_sub[[col]][keep2_rel][m_pos]
      }
      data.table::set(annotation_table_merged, j = col, value = val)
    }
  }
  rm(sirius_sub, sirius_protos)

  # Step 2: Add formula-level SIRIUS scores.
  # left_join (not full_join) so that formula entries without a matching
  # annotation row do NOT create phantom rows with NA connectivity_layer.
  formula_new_cols <- setdiff(
    names(formula_table),
    names(annotation_table_merged)
  )
  formula_join_by <- intersect(
    c("feature_id", "candidate_structure_molecular_formula"),
    intersect(names(formula_table), names(annotation_table_merged))
  )
  if (
    length(formula_new_cols) > 0L &&
      length(formula_join_by) > 0L &&
      nrow(formula_table) > 0L
  ) {
    did_fast_formula <- FALSE
    if (length(formula_join_by) == 1L) {
      k_col <- formula_join_by[[1L]]
      fk <- formula_table[[k_col]]
      if (anyDuplicated(fk) == 0L) {
        m_for <- if (anyNA(fk)) {
          match(annotation_table_merged[[k_col]], fk)
        } else {
          fastmatch::fmatch(annotation_table_merged[[k_col]], fk)
        }
        for (col in formula_new_cols) {
          data.table::set(
            annotation_table_merged,
            j = col,
            value = formula_table[[col]][m_for]
          )
        }
        did_fast_formula <- TRUE
      }
    } else if (length(formula_join_by) == 2L) {
      c1 <- formula_join_by[[1L]]
      c2 <- formula_join_by[[2L]]
      f_c1 <- formula_table[[c1]]
      f_c2 <- formula_table[[c2]]
      u1 <- unique(f_c1)
      u2 <- unique(f_c2)
      id1 <- if (anyNA(u1)) match(f_c1, u1) else fastmatch::fmatch(f_c1, u1)
      id2 <- if (anyNA(u2)) match(f_c2, u2) else fastmatch::fmatch(f_c2, u2)
      n2 <- max(1L, length(u2))
      use_int_f <- as.double(length(u1)) * n2 < .Machine$integer.max
      kf <- if (use_int_f) {
        (id1 - 1L) * n2 + id2
      } else {
        as.double(id1 - 1L) * n2 + id2
      }
      if (anyDuplicated(kf) == 0L) {
        m1 <- if (anyNA(u1)) {
          match(annotation_table_merged[[c1]], u1)
        } else {
          fastmatch::fmatch(annotation_table_merged[[c1]], u1)
        }
        m2 <- if (anyNA(u2)) {
          match(annotation_table_merged[[c2]], u2)
        } else {
          fastmatch::fmatch(annotation_table_merged[[c2]], u2)
        }
        both_f <- !is.na(m1) & !is.na(m2)
        m_for <- rep.int(NA_integer_, nrow(annotation_table_merged))
        if (any(both_f)) {
          km <- if (use_int_f) {
            (m1[both_f] - 1L) * n2 + m2[both_f]
          } else {
            as.double(m1[both_f] - 1L) * n2 + m2[both_f]
          }
          m_for[both_f] <- fastmatch::fmatch(km, kf)
        }
        for (col in formula_new_cols) {
          data.table::set(
            annotation_table_merged,
            j = col,
            value = formula_table[[col]][m_for]
          )
        }
        did_fast_formula <- TRUE
      }
    }
    if (!did_fast_formula) {
      annotation_table_merged <- tidytable::left_join(
        annotation_table_merged,
        formula_table |>
          tidytable::select(
            tidyselect::any_of(c(formula_join_by, formula_new_cols))
          ),
        by = formula_join_by
      )
    }
  }

  # Step 3: Add CANOPUS predictions (feature-level).
  # left_join (not full_join) so that CANOPUS entries for features
  # without annotations do NOT create phantom rows.
  canopus_new_cols <- setdiff(
    names(canopus_table),
    names(annotation_table_merged)
  )
  if (length(canopus_new_cols) > 0L && nrow(canopus_table) > 0L) {
    if (!anyDuplicated(canopus_table$feature_id)) {
      m_can <- fastmatch::fmatch(
        annotation_table_merged$feature_id,
        canopus_table$feature_id
      )
      for (col in canopus_new_cols) {
        data.table::set(
          annotation_table_merged,
          j = col,
          value = canopus_table[[col]][m_can]
        )
      }
    } else {
      annotation_table_merged <- tidytable::left_join(
        annotation_table_merged,
        canopus_table |>
          tidytable::select(
            tidyselect::any_of(c("feature_id", canopus_new_cols))
          ),
        by = "feature_id"
      )
    }
  }

  # Step 4: Add edge metadata (feature-level).
  edge_new_cols <- setdiff(
    c("feature_spectrum_entropy", "feature_spectrum_peaks"),
    names(annotation_table_merged)
  )
  if (length(edge_new_cols) > 0L && nrow(edges_table) > 0L) {
    first_src <- !duplicated(edges_table$feature_source)
    m_edge <- fastmatch::fmatch(
      annotation_table_merged$feature_id,
      edges_table$feature_source[first_src]
    )
    first_idx <- which(first_src)
    for (col in intersect(edge_new_cols, names(edges_table))) {
      data.table::set(
        annotation_table_merged,
        j = col,
        value = edges_table[[col]][first_idx][m_edge]
      )
    }
  }

  annotation_table_merged
}

#' Export Results to Files
#'
#' @description Internal helper to export all result tiers and parameters.
#'
#' @param results_list [list] List with full, filtered, mini data frames
#' @param output [character] Character, base output filename
#' @param pattern [character] Character, pattern for directory naming
#'
#' @return Named character vector with output file paths
#' @keywords internal
export_results <- function(results_list, output, pattern) {
  time <- format(Sys.time(), "%Y%m%d_%H%M%S")
  dir_time <- file.path(
    get_default_paths()$data$processed$path,
    paste0(time, "_", pattern)
  )

  final_output <- file.path(dir_time, output)
  final_output_filtered <- file.path(
    dir_time,
    gsub(output, pattern = ".tsv", replacement = "_filtered.tsv", fixed = TRUE)
  )
  final_output_mini <- file.path(
    dir_time,
    gsub(output, pattern = ".tsv", replacement = "_mini.tsv", fixed = TRUE)
  )

  # Export parameters
  export_params(
    parameters = get_params(step = "prepare_params"),
    directory = dir_time,
    step = "prepare_params"
  )
  export_params(
    parameters = get_params(step = "prepare_params_advanced"),
    directory = dir_time,
    step = "prepare_params_advanced"
  )

  # Export results
  export_output(x = results_list$mini, file = final_output_mini)
  export_output(x = results_list$filtered, file = final_output_filtered)
  export_output(x = results_list$full, file = final_output)
  rm(results_list)

  c(
    "filtered" = final_output_filtered,
    "full" = final_output
  )
}
