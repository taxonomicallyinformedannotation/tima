#' @title Read mzTab-M and export TIMA-compatible files
#'
#' @description Parses mzTab-M plain-text files (v2.0.0–M) and exports
#'   TIMA-ready feature, optional spectra (MGF), and optional metadata tables.
#'
#' @details Two spectrum export modes are supported:
#' \describe{
#'   \item{Embedded spectra (masster COM MGF)}{When the mzTab-M file contains
#'     masster-style `COM\tMGH` / `COM\tMGF` lines, real MS2 spectra are
#'     extracted.  Each entry carries a `FEATURE_ID=` field so that
#'     [get_spectra_ids()] can map edges back to feature IDs.}
#'   \item{Proxy MGF}{When no embedded spectra are found, a proxy MGF is
#'     generated with one dummy entry per feature (using the precursor m/z as
#'     the sole peak).  Each entry carries both `TITLE=` and `FEATURE_ID=` set
#'     to the feature identifier.}
#' }
#'
#' @include export_output.R
#' @include mztab_parser.R
#' @include mztab_validate.R
#' @include validations_utils.R
#'
#' @param input `character(1)` Path to an mzTab-M file (`.mztab` or `.json`
#'   for rmzTabM / progenesis / MetaboScape JSON layouts).
#' @param output_features `character(1)` Output path for the feature table
#'   (`.tsv` or `.csv`).
#' @param output_spectra `character(1) | NULL` Output path for the MGF spectra
#'   file.  `NULL` (default) skips spectral export.
#' @param output_metadata `character(1) | NULL` Output path for the metadata
#'   table.  `NULL` (default) skips metadata export.
#' @param name_features `character(1)` Name for the feature identifier column
#'   in the output feature table.
#' @param name_rt `character(1)` Name for the retention time column (minutes).
#' @param name_mz `character(1)` Name for the precursor m/z column.
#' @param name_adduct `character(1)` Name for the adduct column.
#' @param strict `logical(1)` If `TRUE`, enforce stricter SME required-column
#'   checks during validation.
#'
#' @return Named list with paths: `$features` (always set), `$spectra` and
#'   `$metadata` (NULL when the corresponding output argument is NULL or the
#'   export step is skipped).
#'
#' @family preparation
#' @export
read_mztab <- function(
  input,
  output_features,
  output_spectra = NULL,
  output_metadata = NULL,
  name_features = "feature_id",
  name_rt = "rt",
  name_mz = "mz",
  name_adduct = "adduct",
  strict = FALSE
) {
  validate_character(input, param_name = "input")
  validate_file_exists(input, file_type = "mzTab-M file", param_name = "input")
  validate_character(output_features, param_name = "output_features")
  if (!is.null(output_spectra)) {
    validate_character(output_spectra, param_name = "output_spectra")
  }
  if (!is.null(output_metadata)) {
    validate_character(output_metadata, param_name = "output_metadata")
  }
  validate_character(name_features, param_name = "name_features")
  validate_character(name_rt, param_name = "name_rt")
  validate_character(name_mz, param_name = "name_mz")
  validate_character(name_adduct, param_name = "name_adduct")
  assert_flag(strict, "strict")

  ctx <- log_operation("read_mztab", input = basename(input))

  tabs <- read_mztab_tables(input)
  validate_mztab_tables(tabs, strict = strict)

  features <- .mztab_to_features(
    tabs = tabs,
    name_features = name_features,
    name_rt = name_rt,
    name_mz = name_mz,
    name_adduct = name_adduct
  )

  export_output(features, output_features)

  spectra_path <- NULL
  spectra_source <- "none"
  if (!is.null(output_spectra)) {
    # Try to extract embedded masster-style COM MGF spectra first
    embedded <- .extract_embedded_mgf(input, features, name_features)
    if (!is.null(embedded)) {
      create_dir(output_spectra)
      writeLines(embedded, output_spectra)
      spectra_path <- output_spectra
      spectra_source <- "embedded"
    } else {
      spectra_path <- .write_proxy_mgf(
        features,
        output_spectra,
        name_features,
        name_mz
      )
      spectra_source <- "proxy"
    }
  }

  metadata_path <- NULL
  if (!is.null(output_metadata)) {
    metadata <- .mztab_metadata_table(tabs$metadata)
    export_output(metadata, output_metadata)
    metadata_path <- output_metadata
  }

  log_complete(
    ctx,
    n_features = nrow(features),
    spectra = spectra_source,
    has_metadata = !is.null(metadata_path)
  )

  list(
    features = output_features,
    spectra = spectra_path,
    metadata = metadata_path
  )
}

#' Build assay-to-sample-name mapping from metadata
#' @keywords internal
.mztab_assay_sample_map <- function(metadata_df) {
  if (nrow(metadata_df) == 0L) {
    return(stats::setNames(character(0), character(0)))
  }

  kv <- stats::setNames(metadata_df$value, metadata_df$key)

  # sample[N] -> sample name
  sample_keys <- grep("^sample\\[([0-9]+)\\]$", names(kv), value = TRUE)
  sample_names <- stats::setNames(
    kv[sample_keys],
    sample_keys
  )

  # assay[N] -> assay name
  assay_key_pat <- "^assay\\[([0-9]+)\\]$"
  assay_keys <- grep(assay_key_pat, names(kv), value = TRUE)
  assay_names <- stats::setNames(kv[assay_keys], assay_keys)

  # assay[N]-sample_ref -> sample[M]
  assay_ref_keys <- grep(
    "^assay\\[([0-9]+)\\]-sample_ref$",
    names(kv),
    value = TRUE
  )
  assay_sample <- stats::setNames(kv[assay_ref_keys], assay_ref_keys)

  if (length(assay_keys) == 0L) {
    return(stats::setNames(character(0), character(0)))
  }

  abundance_cols <- sub("^assay", "abundance_assay", assay_keys)
  sample_refs <- unname(assay_sample[paste0(assay_keys, "-sample_ref")])
  sample_labels <- unname(sample_names[sample_refs])
  assay_labels <- unname(assay_names[assay_keys])

  out_vals <- ifelse(
    !is.na(sample_labels) & nzchar(sample_labels),
    sample_labels,
    assay_labels
  )
  keep <- !is.na(out_vals) & nzchar(out_vals)
  stats::setNames(out_vals[keep], abundance_cols[keep])
}

#' @keywords internal
.mztab_to_features <- function(
  tabs,
  name_features,
  name_rt,
  name_mz,
  name_adduct
) {
  src <- if (nrow(tabs$sml) > 0L) tabs$sml else tabs$smf
  n <- nrow(src)

  smf <- tabs$smf
  smf_lookup <- NULL
  if (nrow(smf) > 0L && "SMF_ID" %in% names(smf)) {
    smf_ids <- as.character(smf$SMF_ID)
    smf_lookup <- list(
      mz = stats::setNames(as.character(smf$exp_mass_to_charge), smf_ids),
      rt_s = if ("retention_time_in_seconds" %in% names(smf)) {
        stats::setNames(as.character(smf$retention_time_in_seconds), smf_ids)
      } else {
        NULL
      },
      rt_m = if ("retention_time_in_minutes" %in% names(smf)) {
        stats::setNames(as.character(smf$retention_time_in_minutes), smf_ids)
      } else {
        NULL
      },
      adduct = if ("adduct_ion" %in% names(smf)) {
        stats::setNames(as.character(smf$adduct_ion), smf_ids)
      } else {
        NULL
      }
    )
  }

  first_ref <- function(x) {
    if (is.na(x) || !nzchar(x)) {
      return(NA_character_)
    }
    refs <- strsplit(x, "|", fixed = TRUE)[[1L]]
    refs <- refs[nzchar(refs) & refs != "null"]
    if (length(refs) == 0L) NA_character_ else refs[[1L]]
  }

  smf_ref <- if ("SMF_ID_REFS" %in% names(src)) {
    vapply(as.character(src$SMF_ID_REFS), first_ref, FUN.VALUE = character(1L))
  } else {
    rep(NA_character_, n)
  }

  pick_with_smf <- function(primary, smf_field = NULL) {
    vals <- if (!is.na(primary) && primary %in% names(src)) {
      as.character(src[[primary]])
    } else {
      rep(NA_character_, n)
    }
    vals[vals %in% c("", "null", "NA", "N/A")] <- NA_character_
    if (
      !is.null(smf_lookup) &&
        !is.null(smf_field) &&
        !is.null(smf_lookup[[smf_field]])
    ) {
      smf_vals <- unname(smf_lookup[[smf_field]][smf_ref])
      idx <- is.na(vals) |
        (
          # Also replace zero m/z placeholders from SML with SMF values
          smf_field == "mz" &
            !is.na(suppressWarnings(as.numeric(vals))) &
            suppressWarnings(as.numeric(vals)) == 0
        )
      vals[idx] <- smf_vals[idx]
    }
    vals
  }

  id_col <- if ("SML_ID" %in% names(src)) "SML_ID" else "SMF_ID"

  mz_vals <- pick_with_smf(
    primary = if ("exp_mass_to_charge" %in% names(src)) {
      "exp_mass_to_charge"
    } else if ("theoretical_mass_to_charge" %in% names(src)) {
      "theoretical_mass_to_charge"
    } else if ("theoretical_neutral_mass" %in% names(src)) {
      "theoretical_neutral_mass"
    } else {
      NA_character_
    },
    smf_field = "mz"
  )

  rt_secs_vals <- pick_with_smf(
    primary = if ("retention_time_in_seconds" %in% names(src)) {
      "retention_time_in_seconds"
    } else {
      NA_character_
    },
    smf_field = "rt_s"
  )
  rt_mins_vals <- pick_with_smf(
    primary = if ("retention_time_in_minutes" %in% names(src)) {
      "retention_time_in_minutes"
    } else {
      NA_character_
    },
    smf_field = "rt_m"
  )

  adduct_vals <- pick_with_smf(
    primary = if ("adduct_ions" %in% names(src)) {
      "adduct_ions"
    } else if ("adduct" %in% names(src)) {
      "adduct"
    } else {
      NA_character_
    },
    smf_field = "adduct"
  )

  out <- data.frame(row.names = seq_len(n))
  out[[name_features]] <- as.character(src[[id_col]])
  out[[name_mz]] <- suppressWarnings(as.numeric(mz_vals))

  rt_vals_sec <- suppressWarnings(as.numeric(rt_secs_vals))
  rt_vals <- rt_vals_sec / 60
  rt_vals_min <- suppressWarnings(as.numeric(rt_mins_vals))
  idx_missing_rt <- is.na(rt_vals)
  rt_vals[idx_missing_rt] <- rt_vals_min[idx_missing_rt]
  out[[name_rt]] <- rt_vals
  out[[name_adduct]] <- adduct_vals

  abundance_cols <- grep("^abundance_assay\\[", names(src), value = TRUE)
  if (length(abundance_cols) > 0L) {
    src_abundance <- src[, ..abundance_cols]
    out[abundance_cols] <- lapply(src_abundance, as.numeric)

    # Rename abundance_assay[N] columns to sample names when available
    assay_map <- .mztab_assay_sample_map(tabs$metadata)
    rename_mask <- names(out) %in% names(assay_map)
    names(out)[rename_mask] <- assay_map[names(out)[rename_mask]]
  }

  # ── Pass-through: preserve optional mzTab columns ────────────────────────
  # Columns already mapped to TIMA names or managed above.
  core_mapped <- c(
    id_col,
    "SMF_ID_REFS",
    "SML_ID_REFS",
    "SME_ID_REFS",
    "exp_mass_to_charge",
    "theoretical_mass_to_charge",
    "theoretical_neutral_mass",
    "retention_time_in_seconds",
    "retention_time_in_minutes",
    "adduct_ion",
    "adduct_ions",
    abundance_cols
  )
  # Annotation columns from SML worth retaining alongside feature data.
  sml_annotation_cols <- c(
    "database_identifier",
    "chemical_formula",
    "smiles",
    "inchi",
    "chemical_name",
    "uri",
    "adduct_ions",
    "reliability",
    "best_id_confidence_measure",
    "best_id_confidence_value"
  )
  # opt_* columns: any optional column the upstream tool added.
  opt_cols <- grep("^opt_", names(src), value = TRUE)

  pass_cols <- setdiff(
    union(
      intersect(sml_annotation_cols, names(src)),
      opt_cols
    ),
    c(core_mapped, names(out))
  )
  if (length(pass_cols) > 0L) {
    pass_data <- as.data.frame(src)[, pass_cols, drop = FALSE]
    out[pass_cols] <- pass_data
  }

  out
}

#' Parse the human-readable name from an mzTab CV term string.
#'
#' CV terms in mzTab-M have the format: `[cv_label, accession, name, value]`.
#' This helper returns the third (name) element, trimmed of whitespace.
#'
#' @param x Character vector of CV term strings.
#' @return Character vector of extracted names (NA where parsing fails).
#' @keywords internal
.parse_mztab_cv_name <- function(x) {
  vapply(
    x,
    function(term) {
      if (is.na(term) || !nzchar(term)) {
        return(NA_character_)
      }
      term <- trimws(term)

      # Plain text fallback (some producers do not use mzTab Param syntax).
      if (!grepl("^\\[.*\\]$", term)) {
        return(term)
      }

      inner <- sub("^\\[", "", term)
      inner <- sub("\\]$", "", inner)
      parts <- .mztab_split_param_fields(inner)
      if (length(parts) < 3L) {
        return(NA_character_)
      }

      name <- trimws(parts[[3L]])
      if (!nzchar(name)) NA_character_ else name
    },
    FUN.VALUE = character(1L),
    USE.NAMES = FALSE
  )
}

#' Build a sample-ID to species-name map from mzTab MTD rows.
#'
#' Scans for `sample[N]-species[M]` keys and returns a named character vector
#' mapping `sample[N]` to the first parsed species name for that sample.
#'
#' @param metadata_df Tidytable with `key` and `value` columns from MTD section.
#' @return Named character vector: names = sample IDs, values = species names.
#' @keywords internal
.mztab_sample_species_map <- function(metadata_df) {
  species_rows <- metadata_df[
    grepl(
      "^sample\\[[0-9]+\\]-(species|organism)(\\[[0-9]+\\])?$",
      metadata_df$key
    ),
  ]
  if (nrow(species_rows) == 0L) {
    return(stats::setNames(character(0), character(0)))
  }
  species_rows$sample_id <- sub(
    "-(species|organism)(\\[[0-9]+\\])?$",
    "",
    species_rows$key
  )
  species_rows$species_name <- .parse_mztab_cv_name(species_rows$value)

  valid <- !is.na(species_rows$species_name) & nzchar(species_rows$species_name)
  species_rows <- species_rows[valid, ]
  if (nrow(species_rows) == 0L) {
    return(stats::setNames(character(0), character(0)))
  }

  first_for_sample <- !duplicated(species_rows$sample_id)
  stats::setNames(
    species_rows$species_name[first_for_sample],
    species_rows$sample_id[first_for_sample]
  )
}

#' @keywords internal
.mztab_metadata_table <- function(metadata_df) {
  if (nrow(metadata_df) == 0L) {
    return(tidytable::tidytable())
  }

  # sample[N] rows (no sub-key) give the sample name
  samples <- metadata_df |>
    tidytable::filter(grepl("^sample\\[[0-9]+\\]$", key)) |>
    tidytable::mutate(sample_id = key) |>
    tidytable::select(sample_id, sample_name = value)

  assays <- metadata_df |>
    tidytable::filter(grepl("^assay\\[[0-9]+\\]$", key)) |>
    tidytable::mutate(assay_id = key) |>
    tidytable::select(assay_id, assay_name = value)

  assay_ref <- metadata_df |>
    tidytable::filter(grepl("^assay\\[[0-9]+\\]-sample_ref$", key)) |>
    tidytable::mutate(assay_id = sub("-sample_ref$", "", key)) |>
    tidytable::select(assay_id, sample_id = value)

  # Extract species per sample
  species_map <- .mztab_sample_species_map(metadata_df)

  # Build TIMA-compatible metadata: filename = sample name, ATTRIBUTE_species
  if (nrow(samples) > 0L) {
    samples_with_species <- samples
    samples_with_species$ATTRIBUTE_species <- unname(
      species_map[samples_with_species$sample_id]
    )
    # Replace empty strings / NAs consistently
    samples_with_species$ATTRIBUTE_species[
      is.na(samples_with_species$ATTRIBUTE_species) |
        !nzchar(samples_with_species$ATTRIBUTE_species)
    ] <- NA_character_

    tima_meta <- samples_with_species |>
      tidytable::select(filename = sample_name, ATTRIBUTE_species)

    return(tima_meta)
  }

  # Fallback: try via assay linkage
  out <- assays |>
    tidytable::left_join(assay_ref, by = "assay_id") |>
    tidytable::left_join(samples, by = "sample_id")

  if (nrow(out) > 0L) {
    out$ATTRIBUTE_species <- unname(species_map[out$sample_id])
    out$ATTRIBUTE_species[
      is.na(out$ATTRIBUTE_species) | !nzchar(out$ATTRIBUTE_species)
    ] <- NA_character_
    return(out)
  }

  # Last-resort fallback to a key-value export for non-assay-oriented files.
  metadata_df
}

#' Write a proxy MGF from a features table.
#'
#' Generates one minimal MGF entry per feature, using the precursor m/z as the
#' sole fragment peak.  Each entry includes both `TITLE=` and `FEATURE_ID=` set
#' to the feature identifier so that [get_spectra_ids()] works correctly when
#' computing spectral network edges from mzTab-M-derived inputs.
#'
#' Features with a missing or non-finite m/z are silently skipped.
#'
#' @param features [data.frame] Feature table with at least two columns:
#'   `id_col` (feature identifier) and `mz_col` (precursor m/z).
#' @param file `character(1)` Output file path.
#' @param id_col `character(1)` Name of the feature identifier column.
#' @param mz_col `character(1)` Name of the precursor m/z column.
#' @return `file` (invisibly).
#' @keywords internal
.write_proxy_mgf <- function(features, file, id_col, mz_col) {
  create_dir(file)

  con <- file(file, open = "wt", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)

  for (i in seq_len(nrow(features))) {
    fid <- features[[id_col]][[i]]
    mz <- features[[mz_col]][[i]]
    if (is.na(mz) || !is.finite(mz) || mz <= 0) {
      next
    }

    writeLines(
      c(
        "BEGIN IONS",
        paste0("TITLE=", fid),
        paste0("FEATURE_ID=", fid),
        paste0("PEPMASS=", format(mz, scientific = FALSE, trim = TRUE)),
        "CHARGE=1+",
        paste0(
          format(mz, scientific = FALSE, trim = TRUE),
          " 1"
        ),
        "END IONS",
        ""
      ),
      con
    )
  }

  invisible(file)
}

#' Extract embedded masster-style COM MGF spectra from mzTab-M file
#'
#' Masster encodes MS spectra inside COM lines using a custom tabular format:
#' - `COM\tMGH\t<col1>\t<col2>...` defines the column header row
#' - `COM\tMGF\t<val1>\t<val2>...` defines one spectrum per row
#'
#' @details The function emits a `FEATURE_ID=` field in each MGF entry so that
#'   [get_spectra_ids()] can later map spectra back to TIMA feature IDs.
#'   Feature IDs are resolved in this priority order:
#'   1. Reverse-lookup from `opt_global_mgf_index` column (masster >= 0.5) — the
#'      SML table stores a comma-separated list of `mgf_id` values per feature,
#'      giving the authoritative bi-directional mapping.
#'   2. Parse the `id:<N>` key from the TITLE string (newer masster variants that
#'      embed the feature/scan ID there, e.g.
#'      `"id:42, rt:168.84, mz:293.1762, energy:nan, ..."`)
#'   3. Use `mgf_id` directly when it equals the feature ID (tools that export
#'      a single MGF entry per feature with `mgf_id == SML_ID`).
#'
#' @return Character vector of MGF lines, or NULL if no embedded spectra found.
#' @keywords internal
.extract_embedded_mgf <- function(input, features, id_col) {
  lines <- readLines(input, warn = FALSE, encoding = "UTF-8")
  if (length(lines) > 0L) {
    lines[[1L]] <- sub("^\ufeff", "", lines[[1L]])
  }
  lines <- .normalize_mztab_prefix_sep(lines)

  # Detect COM MGH header (masster-specific)
  mgh_lines <- grep("^COM\t[Mm][Gg][Hh]\t", lines, value = TRUE, perl = TRUE)
  if (length(mgh_lines) == 0L) {
    return(NULL)
  }

  # Parse column names from first MGH line
  mgh_cols <- strsplit(mgh_lines[[1L]], "\t", fixed = TRUE)[[1L]]
  # mgh_cols[1]="COM", mgh_cols[2]="MGH", mgh_cols[3:] = column names
  col_names <- mgh_cols[seq(3L, length(mgh_cols))]

  # Parse COM MGF data rows
  mgf_data_lines <- grep(
    "^COM\t[Mm][Gg][Ff]\t",
    lines,
    value = TRUE,
    perl = TRUE
  )
  if (length(mgf_data_lines) == 0L) {
    return(NULL)
  }

  col_idx <- stats::setNames(seq_along(col_names), col_names)
  get_col <- function(vals, name) {
    i <- col_idx[[name]]
    if (is.null(i) || i > length(vals)) NA_character_ else vals[[i]]
  }

  # ── Build mgf_id → feature_id reverse lookup ─────────────────────────────
  # masster stores a per-feature comma-separated list of mgf_id values in the
  # optional column "opt_global_mgf_index".  We invert this to get the fast
  # mgf_id → SML_ID lookup used inside the per-spectrum loop.
  mgf_to_feature <- NULL
  if (
    id_col %in% names(features) && "opt_global_mgf_index" %in% names(features)
  ) {
    fids <- as.character(features[[id_col]])
    mgf_idx_raw <- as.character(features[["opt_global_mgf_index"]])

    pairs <- unlist(lapply(seq_along(fids), function(i) {
      raw <- mgf_idx_raw[[i]]
      if (is.na(raw) || !nzchar(raw)) {
        return(character(0))
      }
      idxs <- trimws(strsplit(raw, ",", fixed = TRUE)[[1L]])
      idxs <- idxs[nzchar(idxs)]
      if (length(idxs) == 0L) {
        return(character(0))
      }
      stats::setNames(rep(fids[[i]], length(idxs)), idxs)
    }))

    if (length(pairs) > 0L) {
      mgf_to_feature <- pairs
    }
  }

  # ── Build feature_id set for direct mgf_id fallback ─────────────────────
  # Used when there is no opt_global_mgf_index mapping but mgf_id happens to
  # equal the feature ID (single-spectrum-per-feature masster style).
  feature_id_set <- if (id_col %in% names(features)) {
    as.character(features[[id_col]])
  } else {
    character(0)
  }

  out_lines <- character(0)
  for (line in mgf_data_lines) {
    parts <- strsplit(line, "\t", fixed = TRUE)[[1L]]
    # parts[1]="COM", parts[2]="MGF", parts[3:] = values
    vals <- parts[seq(3L, length(parts))]

    mgf_id <- get_col(vals, "mgf_id")
    prec_mz <- suppressWarnings(as.numeric(get_col(vals, "prec_mz")))
    prec_rt <- suppressWarnings(as.numeric(get_col(vals, "prec_rt")))
    ms_level <- get_col(vals, "level")
    title <- get_col(vals, "title")
    spec_mz_raw <- get_col(vals, "spec_mz")
    spec_int_raw <- get_col(vals, "spec_int")

    if (is.na(prec_mz) || prec_mz <= 0) {
      next
    }

    # ── Resolve FEATURE_ID for this spectrum ────────────────────────────────
    # Priority 1: opt_global_mgf_index reverse lookup
    feature_id_val <- if (
      !is.null(mgf_to_feature) && !is.na(mgf_id) && nzchar(mgf_id)
    ) {
      mgf_to_feature[[mgf_id]] # NULL if mgf_id not in map
    } else {
      NULL
    }

    # Priority 2: parse "id:<N>" from TITLE (newer masster, e.g. "id:42, rt:...")
    # Note: matches "id:" but NOT "uid:", "scan_id:", "_id:", etc.
    if (is.null(feature_id_val) && !is.na(title) && nzchar(title)) {
      m <- regmatches(
        title,
        regexpr("(?<![_a-zA-Z])id:(\\d+)", title, perl = TRUE)
      )
      if (length(m) > 0L && nzchar(m)) {
        feature_id_val <- sub(".*id:", "", m)
      }
    }

    # Priority 3: use mgf_id directly when it matches a known feature ID
    if (
      is.null(feature_id_val) &&
        !is.na(mgf_id) &&
        nzchar(mgf_id) &&
        mgf_id %in% feature_id_set
    ) {
      feature_id_val <- mgf_id
    }

    # ── Build TITLE string ───────────────────────────────────────────────────
    entry_title <- if (!is.na(title) && nzchar(title) && title != "null") {
      title
    } else {
      feature_id_val %||%
        if (!is.na(mgf_id) && nzchar(mgf_id)) {
          paste0("mgf_id:", mgf_id)
        } else {
          paste0("mz:", format(prec_mz, scientific = FALSE))
        }
    }

    # ── Parse fragment peaks ─────────────────────────────────────────────────
    mz_parts <- if (
      !is.na(spec_mz_raw) && nzchar(spec_mz_raw) && spec_mz_raw != "null"
    ) {
      strsplit(spec_mz_raw, "|", fixed = TRUE)[[1L]]
    } else {
      character(0)
    }
    int_parts <- if (
      !is.na(spec_int_raw) && nzchar(spec_int_raw) && spec_int_raw != "null"
    ) {
      strsplit(spec_int_raw, "|", fixed = TRUE)[[1L]]
    } else {
      character(0)
    }
    n_peaks <- min(length(mz_parts), length(int_parts))

    # ── Assemble MGF entry ───────────────────────────────────────────────────
    entry <- c(
      "BEGIN IONS",
      paste0("TITLE=", entry_title)
    )
    if (!is.null(feature_id_val)) {
      entry <- c(entry, paste0("FEATURE_ID=", feature_id_val))
    }
    entry <- c(
      entry,
      paste0("PEPMASS=", format(prec_mz, scientific = FALSE, trim = TRUE))
    )
    if (!is.na(prec_rt) && is.finite(prec_rt)) {
      entry <- c(
        entry,
        paste0("RTINSECONDS=", format(prec_rt, scientific = FALSE, trim = TRUE))
      )
    }
    if (!is.na(ms_level) && nzchar(ms_level) && ms_level != "null") {
      entry <- c(entry, paste0("MSLEVEL=", ms_level))
    }

    for (p in seq_len(n_peaks)) {
      mz_p <- suppressWarnings(as.numeric(mz_parts[[p]]))
      int_p <- suppressWarnings(as.numeric(int_parts[[p]]))
      if (
        !is.na(mz_p) && is.finite(mz_p) && !is.na(int_p) && is.finite(int_p)
      ) {
        entry <- c(
          entry,
          paste(
            format(mz_p, scientific = FALSE, trim = TRUE),
            format(int_p, scientific = FALSE, trim = TRUE)
          )
        )
      }
    }

    entry <- c(entry, "END IONS", "")
    out_lines <- c(out_lines, entry)
  }

  if (length(out_lines) == 0L) NULL else out_lines
}
