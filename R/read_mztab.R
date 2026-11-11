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
#' @include logs_utils.R
#' @include adduct_universe.R
#' @include create_dir.R
#' @include assert_utils.R
#' @include validations_params.R
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
#' @concept EDAM:operation_1812 Parsing
#' @concept EDAM:format_3681 mzTab
#' @export
#'
#' @examples
#' \dontrun{
#' # Read mzTab-M file and export TIMA-compatible tables
#' result <- read_mztab(
#'   input = "annotations.mztab",
#'   output_features = "features.tsv",
#'   output_spectra = "spectra.mgf",
#'   output_metadata = "metadata.tsv"
#' )
#' }
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

  tabs <- read_mztab_tables(input, include_raw_lines = !is.null(output_spectra))
  raw_lines <- attr(tabs, ".raw_lines", exact = TRUE)
  attr(tabs, ".raw_lines") <- NULL
  validate_mztab_tables(tabs, strict = strict)

  features <- .mztab_to_features(
    tabs = tabs,
    name_features = name_features,
    name_rt = name_rt,
    name_mz = name_mz,
    name_adduct = name_adduct
  )

  export_output(features, output_features)
  nrows_features <- nrow(features)

  spectra_path <- NULL
  spectra_source <- "none"
  if (!is.null(output_spectra)) {
    # Try to extract embedded masster-style COM MGF spectra first
    embedded <- .extract_embedded_mgf(
      input,
      features,
      name_features,
      lines = raw_lines
    )
    rm(raw_lines)
    if (!is.null(embedded)) {
      rm(features)
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
      rm(features)
      spectra_source <- "proxy"
    }
  }

  metadata_path <- NULL
  if (!is.null(output_metadata)) {
    metadata <- .mztab_metadata_table(tabs$metadata)
    export_output(metadata, output_metadata)
    rm(metadata)
    metadata_path <- output_metadata
  }

  log_complete(
    ctx,
    n_features = nrows_features,
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
  smf_ids <- character(0)
  if (nrow(smf) > 0L && "SMF_ID" %in% names(smf)) {
    smf_ids <- as.character(smf$SMF_ID)
    smf_lookup <- list(
      mz = as.character(smf$exp_mass_to_charge),
      rt_s = if ("retention_time_in_seconds" %in% names(smf)) {
        as.character(smf$retention_time_in_seconds)
      } else {
        NULL
      },
      rt_m = if ("retention_time_in_minutes" %in% names(smf)) {
        as.character(smf$retention_time_in_minutes)
      } else {
        NULL
      },
      adduct = if ("adduct_ion" %in% names(smf)) {
        as.character(smf$adduct_ion)
      } else {
        NULL
      }
    )
  }

  smf_ref <- if ("SMF_ID_REFS" %in% names(src)) {
    raw_refs <- as.character(src$SMF_ID_REFS)
    out_ref <- rep(NA_character_, n)
    valid_r <- which(!is.na(raw_refs) & nzchar(raw_refs) & raw_refs != "null")
    if (length(valid_r) > 0L) {
      rv <- raw_refs[valid_r]
      has_pipe <- grepl("|", rv, fixed = TRUE)
      if (!all(has_pipe)) {
        out_ref[valid_r[!has_pipe]] <- rv[!has_pipe]
      }
      if (any(has_pipe)) {
        pipe_splits <- strsplit(rv[has_pipe], "|", fixed = TRUE)
        out_ref[valid_r[has_pipe]] <- vapply(
          pipe_splits,
          function(refs) {
            refs <- refs[nzchar(refs) & refs != "null"]
            if (length(refs) == 0L) NA_character_ else refs[[1L]]
          },
          character(1L)
        )
      }
    }
    out_ref
  } else {
    rep(NA_character_, n)
  }

  smf_match_idx <- if (!is.null(smf_lookup)) {
    fastmatch::fmatch(smf_ref, smf_ids)
  } else {
    NULL
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
      smf_vals <- smf_lookup[[smf_field]][smf_match_idx]
      idx <- if (smf_field == "mz") {
        # Also replace zero m/z placeholders from SML with SMF values
        vals_num <- suppressWarnings(as.numeric(vals))
        is.na(vals) | (!is.na(vals_num) & vals_num == 0)
      } else {
        is.na(vals)
      }
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
    "best_id_evidence_value"
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
  if (length(x) == 0L) {
    return(character(0))
  }
  x_chr <- if (is.character(x)) x else as.character(x)
  u <- unique(x_chr)
  u_out <- vapply(
    u,
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
  if (length(u) == length(x_chr)) u_out else u_out[fastmatch::fmatch(x_chr, u)]
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
  if (
    nrow(metadata_df) == 0L || !all(c("key", "value") %in% names(metadata_df))
  ) {
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
    out <- out |>
      tidytable::mutate(filename = as.character(assay_name)) |>
      tidytable::select(filename, ATTRIBUTE_species)
    return(out)
  }

  # Last-resort fallback: export raw key/value pairs under the TIMA-compatible
  # schema so no metadata is lost and downstream consumers (which expect
  # `filename`/`ATTRIBUTE_species`) keep working. This shape is not assay- or
  # sample-oriented, so warn that the linkage is absent.
  log_warn(
    "mzTab metadata has no sample or assay linkage; exporting raw key/value pairs"
  )
  metadata_df <- as.data.frame(metadata_df, stringsAsFactors = FALSE)
  tidytable::tidytable(
    filename = as.character(metadata_df$key),
    ATTRIBUTE_species = as.character(metadata_df$value)
  )
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

  valid_idx <- which(
    !is.na(features[[mz_col]]) &
      is.finite(features[[mz_col]]) &
      features[[mz_col]] > 0
  )
  if (length(valid_idx) == 0L) {
    return(invisible(file))
  }

  fids <- as.character(features[[id_col]][valid_idx])
  mz_vals <- as.numeric(features[[mz_col]][valid_idx])
  mz_str <- format(mz_vals, scientific = FALSE, trim = TRUE)

  n_lines <- 8L * length(valid_idx)
  lines <- character(n_lines)
  line_pos <- seq.int(1L, n_lines, by = 8L)
  lines[line_pos] <- "BEGIN IONS"
  lines[line_pos + 1L] <- paste0("TITLE=", fids)
  lines[line_pos + 2L] <- paste0("FEATURE_ID=", fids)
  lines[line_pos + 3L] <- paste0("PEPMASS=", mz_str)
  lines[line_pos + 4L] <- "CHARGE=1+"
  lines[line_pos + 5L] <- paste0(mz_str, " 1")
  lines[line_pos + 6L] <- "END IONS"
  lines[line_pos + 7L] <- ""

  writeLines(lines, con)

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
.extract_embedded_mgf <- function(input, features, id_col, lines = NULL) {
  if (is.null(lines)) {
    lines <- readLines(input, warn = FALSE, encoding = "UTF-8")
    if (length(lines) > 0L) {
      lines[[1L]] <- sub("^\ufeff", "", lines[[1L]])
    }
    lines <- .normalize_mztab_prefix_sep(lines)
  }

  # Detect COM MGH header (masster-specific)
  com_lines <- lines[startsWith(lines, "COM\t")]
  if (length(com_lines) == 0L) {
    return(NULL)
  }
  mgh_lines <- grep(
    "^COM\t[Mm][Gg][Hh]\t",
    com_lines,
    value = TRUE,
    perl = TRUE
  )
  if (length(mgh_lines) == 0L) {
    return(NULL)
  }

  # Parse column names from first MGH line
  mgh_cols <- strsplit(mgh_lines[[1L]], "\t", fixed = TRUE)[[1L]]
  if (length(mgh_cols) < 3L) {
    return(NULL)
  }
  col_names <- mgh_cols[seq(3L, length(mgh_cols))]

  # Parse COM MGF data rows
  mgf_data_lines <- grep(
    "^COM\t[Mm][Gg][Ff]\t",
    com_lines,
    value = TRUE,
    perl = TRUE
  )
  n_spec <- length(mgf_data_lines)
  if (n_spec == 0L) {
    return(NULL)
  }

  # Split all MGF lines at once
  all_parts <- strsplit(mgf_data_lines, "\t", fixed = TRUE)
  part_lens <- lengths(all_parts)
  target_len <- length(col_names) + 2L
  uniform_len <- all(part_lens == target_len)
  mat_parts <- if (uniform_len) {
    matrix(
      unlist(all_parts, use.names = FALSE),
      nrow = target_len,
      ncol = n_spec
    )
  } else {
    NULL
  }

  extract_col_vec <- function(name) {
    idx <- match(name, col_names)
    if (is.na(idx)) {
      return(rep.int(NA_character_, n_spec))
    }
    pos <- idx + 2L
    if (uniform_len && pos <= target_len) {
      return(mat_parts[pos, ])
    }
    out <- rep.int(NA_character_, n_spec)
    ok <- which(part_lens >= pos)
    if (length(ok) > 0L) {
      out[ok] <- vapply(all_parts[ok], `[[`, character(1L), pos)
    }
    out
  }

  prec_mz_raw <- extract_col_vec("prec_mz")
  prec_mz_all <- suppressWarnings(as.numeric(prec_mz_raw))
  valid_spec <- which(!is.na(prec_mz_all) & prec_mz_all > 0)
  n_entries <- length(valid_spec)
  if (n_entries == 0L) {
    return(NULL)
  }

  mgf_id_all <- extract_col_vec("mgf_id")[valid_spec]
  prec_mz <- prec_mz_all[valid_spec]
  prec_rt <- suppressWarnings(
    as.numeric(extract_col_vec("prec_rt")[valid_spec])
  )
  ms_level <- extract_col_vec("level")[valid_spec]
  title <- extract_col_vec("title")[valid_spec]
  spec_mz_raw <- extract_col_vec("spec_mz")[valid_spec]
  spec_int_raw <- extract_col_vec("spec_int")[valid_spec]
  rm(all_parts, mat_parts, prec_mz_raw, prec_mz_all)

  # ── Build mgf_id → feature_id reverse lookup ─────────────────────────────
  # masster stores a per-feature comma-separated list of mgf_id values in the
  # optional column "opt_global_mgf_index". We invert this to get the fast
  # mgf_id → SML_ID lookup across all spectra.
  map_keys <- character(0)
  map_vals <- character(0)
  if (
    id_col %in% names(features) && "opt_global_mgf_index" %in% names(features)
  ) {
    fids <- as.character(features[[id_col]])
    mgf_idx_raw <- as.character(features[["opt_global_mgf_index"]])
    valid_raw <- which(!is.na(mgf_idx_raw) & nzchar(mgf_idx_raw))
    if (length(valid_raw) > 0L) {
      splits <- strsplit(mgf_idx_raw[valid_raw], ",", fixed = TRUE)
      lens <- lengths(splits)
      all_idxs <- trimws(unlist(splits, use.names = FALSE))
      all_fids <- rep.int(fids[valid_raw], lens)
      keep_idx <- nzchar(all_idxs)
      if (any(keep_idx)) {
        all_idxs <- all_idxs[keep_idx]
        all_fids <- all_fids[keep_idx]
        first_u <- !duplicated(all_idxs)
        map_keys <- all_idxs[first_u]
        map_vals <- all_fids[first_u]
      }
    }
  }

  # ── Build feature_id set for direct mgf_id fallback ─────────────────────
  feature_id_set <- if (id_col %in% names(features)) {
    as.character(features[[id_col]])
  } else {
    character(0)
  }

  # ── Resolve FEATURE_ID across all valid spectra ─────────────────────────
  feature_id_val <- rep.int(NA_character_, n_entries)
  valid_mgf_id <- !is.na(mgf_id_all) & nzchar(mgf_id_all)

  # Priority 1: opt_global_mgf_index reverse lookup
  if (length(map_keys) > 0L && any(valid_mgf_id)) {
    idx_p1 <- which(valid_mgf_id)
    m_idx <- fastmatch::fmatch(mgf_id_all[idx_p1], map_keys)
    hit_p1 <- !is.na(m_idx)
    if (any(hit_p1)) {
      feature_id_val[idx_p1[hit_p1]] <- map_vals[m_idx[hit_p1]]
    }
  }

  # Priority 2: parse "id:<N>" from TITLE (newer masster, e.g. "id:42, rt:...")
  # Note: matches "id:" but NOT "uid:", "scan_id:", "_id:", etc.
  need_p2 <- which(is.na(feature_id_val) & !is.na(title) & nzchar(title))
  if (length(need_p2) > 0L) {
    t_sub <- title[need_p2]
    rx <- regexpr("(?<![_a-zA-Z])id:(\\d+)", t_sub, perl = TRUE)
    hit_p2 <- rx > 0L
    if (any(hit_p2)) {
      m_str <- substr(
        t_sub[hit_p2],
        rx[hit_p2] + 3L,
        rx[hit_p2] + attr(rx, "match.length")[hit_p2] - 1L
      )
      feature_id_val[need_p2[hit_p2]] <- m_str
    }
  }

  # Priority 3: use mgf_id directly when it matches a known feature ID
  if (length(feature_id_set) > 0L) {
    need_p3 <- which(is.na(feature_id_val) & valid_mgf_id)
    if (length(need_p3) > 0L) {
      hit_p3 <- !is.na(fastmatch::fmatch(mgf_id_all[need_p3], feature_id_set))
      if (any(hit_p3)) {
        feature_id_val[need_p3[hit_p3]] <- mgf_id_all[need_p3[hit_p3]]
      }
    }
  }

  format_num_vec <- function(x) {
    u <- unique(x)
    s <- as.character(u)
    sci <- grep("[eE]", s)
    if (length(sci) > 0L) {
      s[sci] <- vapply(
        u[sci],
        function(v) format(v, scientific = FALSE, trim = TRUE),
        character(1L)
      )
    }
    if (length(u) == length(x)) s else s[fastmatch::fmatch(x, u)]
  }

  # ── Build TITLE string ───────────────────────────────────────────────────
  valid_title <- !is.na(title) & nzchar(title) & title != "null"
  entry_title <- title
  if (!all(valid_title)) {
    miss_t <- which(!valid_title)
    has_fid <- !is.na(feature_id_val[miss_t])
    if (any(has_fid)) {
      entry_title[miss_t[has_fid]] <- feature_id_val[miss_t[has_fid]]
    }
    still_miss <- miss_t[!has_fid]
    if (length(still_miss) > 0L) {
      has_mid <- valid_mgf_id[still_miss]
      if (any(has_mid)) {
        entry_title[still_miss[has_mid]] <- paste0(
          "mgf_id:",
          mgf_id_all[still_miss[has_mid]]
        )
      }
      no_mid <- still_miss[!has_mid]
      if (length(no_mid) > 0L) {
        entry_title[no_mid] <- paste0("mz:", format_num_vec(prec_mz[no_mid]))
      }
    }
  }

  pepmass_lines <- paste0("PEPMASS=", format_num_vec(prec_mz))
  valid_rt <- !is.na(prec_rt) & is.finite(prec_rt)
  rt_lines <- rep.int(NA_character_, n_entries)
  if (any(valid_rt)) {
    rt_lines[valid_rt] <- paste0(
      "RTINSECONDS=",
      format_num_vec(prec_rt[valid_rt])
    )
  }
  valid_mslevel <- !is.na(ms_level) & nzchar(ms_level) & ms_level != "null"
  mslevel_lines <- rep.int(NA_character_, n_entries)
  if (any(valid_mslevel)) {
    mslevel_lines[valid_mslevel] <- paste0("MSLEVEL=", ms_level[valid_mslevel])
  }
  has_fid_all <- !is.na(feature_id_val)
  fid_lines <- rep.int(NA_character_, n_entries)
  if (any(has_fid_all)) {
    fid_lines[has_fid_all] <- paste0("FEATURE_ID=", feature_id_val[has_fid_all])
  }
  title_lines <- paste0("TITLE=", entry_title)

  # ── Parse and format fragment peaks across all spectra ──────────────────
  valid_mz_raw <- !is.na(spec_mz_raw) &
    nzchar(spec_mz_raw) &
    spec_mz_raw != "null"
  valid_int_raw <- !is.na(spec_int_raw) &
    nzchar(spec_int_raw) &
    spec_int_raw != "null"
  both_raw <- which(valid_mz_raw & valid_int_raw)

  peak_blocks <- vector("list", n_entries)
  if (length(both_raw) > 0L) {
    mz_sp <- strsplit(spec_mz_raw[both_raw], "|", fixed = TRUE)
    int_sp <- strsplit(spec_int_raw[both_raw], "|", fixed = TRUE)
    len_mz <- lengths(mz_sp)
    len_int <- lengths(int_sp)
    n_peaks_vec <- pmin.int(len_mz, len_int)
    mismatch_len <- which(len_mz != n_peaks_vec | len_int != n_peaks_vec)
    if (length(mismatch_len) > 0L) {
      for (idx_m in mismatch_len) {
        np <- n_peaks_vec[[idx_m]]
        mz_sp[[idx_m]] <- mz_sp[[idx_m]][seq_len(np)]
        int_sp[[idx_m]] <- int_sp[[idx_m]][seq_len(np)]
      }
    }
    has_peaks <- n_peaks_vec > 0L
    if (any(has_peaks)) {
      idx_hp <- both_raw[has_peaks]
      np_hp <- n_peaks_vec[has_peaks]
      flat_mz_chr <- unlist(mz_sp[has_peaks], use.names = FALSE)
      flat_int_chr <- unlist(int_sp[has_peaks], use.names = FALSE)
      rm(mz_sp, int_sp)

      flat_mz_num <- suppressWarnings(as.numeric(flat_mz_chr))
      flat_int_num <- suppressWarnings(as.numeric(flat_int_chr))
      rm(flat_mz_chr, flat_int_chr)

      valid_p <- !is.na(flat_mz_num) &
        is.finite(flat_mz_num) &
        !is.na(flat_int_num) &
        is.finite(flat_int_num)

      if (any(valid_p)) {
        spec_grp <- rep.int(seq_along(idx_hp), np_hp)
        if (!all(valid_p)) {
          flat_mz_num <- flat_mz_num[valid_p]
          flat_int_num <- flat_int_num[valid_p]
          spec_grp <- spec_grp[valid_p]
        }
        mz_fmt <- format_num_vec(flat_mz_num)
        int_fmt <- format_num_vec(flat_int_num)
        rm(flat_mz_num, flat_int_num)
        peak_str <- paste(mz_fmt, int_fmt)
        rm(mz_fmt, int_fmt)

        n_hp <- length(idx_hp)
        grp_fac <- structure(
          spec_grp,
          levels = as.character(seq_len(n_hp)),
          class = "factor"
        )
        split_peaks <- split.default(peak_str, grp_fac)
        names(split_peaks) <- NULL
        peak_blocks[idx_hp] <- split_peaks
      }
    }
  }

  # ── Assemble MGF entries ─────────────────────────────────────────────────
  out_entries <- vector("list", n_entries)
  for (i in seq_len(n_entries)) {
    out_entries[[i]] <- c(
      "BEGIN IONS",
      title_lines[[i]],
      if (has_fid_all[[i]]) fid_lines[[i]],
      pepmass_lines[[i]],
      if (valid_rt[[i]]) rt_lines[[i]],
      if (valid_mslevel[[i]]) mslevel_lines[[i]],
      peak_blocks[[i]],
      "END IONS",
      ""
    )
  }

  unlist(out_entries, use.names = FALSE)
}
