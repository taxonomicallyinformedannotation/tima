.mztab_build_metadata_table <- function(
  mztab_id,
  title,
  description,
  software_version,
  ms_run_location,
  ms_run_format = "null",
  ms_run_id_format = "null",
  polarity = NULL,
  instrument = NULL,
  sample_name = NULL,
  publication = NULL,
  contact = NULL,
  xrefs_index = NULL,
  sme_table,
  existing_metadata = NULL
) {
  # Detect whether id_confidence_measure columns exist in SME
  has_bio <- "id_confidence_measure[2]" %in% colnames(sme_table)
  has_chemo <- "id_confidence_measure[3]" %in% colnames(sme_table)
  has_spectral <- "id_confidence_measure[4]" %in% colnames(sme_table)

  add_default <- function(meta, key, value) {
    if (is.list(meta) && all(c("keys", "values") %in% names(meta))) {
      keys <- meta$keys
      values <- meta$values
    } else {
      keys <- character(0)
      values <- character(0)
    }

    if (length(keys) == 0L || !any(keys == key, na.rm = TRUE)) {
      keys <- c(keys, key)
      values <- c(values, value)
    }

    list(keys = keys, values = values)
  }

  meta <- if (is.null(existing_metadata)) {
    list(keys = character(0), values = character(0))
  } else {
    existing_df <- tidytable::as_tidytable(existing_metadata)
    if (all(c("key", "value") %in% colnames(existing_df))) {
      list(
        keys = as.character(existing_df$key),
        values = as.character(existing_df$value)
      )
    } else {
      list(keys = character(0), values = character(0))
    }
  }

  software_version <- if (
    is.null(software_version) || !nzchar(as.character(software_version))
  ) {
    "unknown"
  } else {
    as.character(software_version)
  }
  catalog <- .mztab_schema_catalog(software_version)

  # ── Mandatory header fields ──────────────────────────────────────────────
  meta <- add_default(
    meta,
    "mzTab-version",
    catalog$terms$metadata_defaults[["mzTab-version"]]
  )
  meta <- add_default(
    meta,
    "mzTab-mode",
    catalog$terms$metadata_defaults[["mzTab-mode"]]
  )
  meta <- add_default(
    meta,
    "mzTab-type",
    catalog$terms$metadata_defaults[["mzTab-type"]]
  )
  meta <- add_default(meta, "mzTab-ID", .mztab_escape(mztab_id))
  meta <- add_default(meta, "title", .mztab_escape(title))
  meta <- add_default(meta, "description", .mztab_escape(description))
  meta <- add_default(
    meta,
    "uri[1]",
    catalog$terms$metadata_defaults[["uri[1]"]]
  )

  # ── Controlled vocabulary registry ───────────────────────────────────────
  cv_registry <- catalog$terms$cv_registry
  for (i in seq_along(cv_registry)) {
    entry <- cv_registry[[i]]
    meta <- add_default(meta, paste0("cv[", i, "]-label"), entry$label)
    meta <- add_default(meta, paste0("cv[", i, "]-full_name"), entry$full_name)
    meta <- add_default(meta, paste0("cv[", i, "]-version"), entry$version)
    meta <- add_default(meta, paste0("cv[", i, "]-uri"), entry$uri)
  }

  if (!is.null(publication) && nzchar(publication) && publication != "null") {
    meta <- add_default(meta, "publication[1]", .mztab_escape(publication))
  }

  # ── Software ─────────────────────────────────────────────────────────────
  # TIMA has no PSI-MS accession; the user-parameter Param format is correct.
  meta <- add_default(
    meta,
    "software[1]",
    catalog$terms$metadata_defaults[["software[1]"]]
  )
  # Emit the canonical repository URL as a software setting so consumers can
  # look up algorithm details without relying on free-text fields.
  meta <- add_default(
    meta,
    "software[1]-setting[1]",
    catalog$terms$metadata_defaults[["software[1]-setting[1]"]]
  )

  if (!is.null(instrument) && nzchar(instrument) && instrument != "null") {
    meta <- add_default(meta, "instrument[1]-name", instrument)
  }

  # ── Contact information (optional) ───────────────────────────────────────
  if (is.list(contact) && length(contact) > 0L) {
    if (!is.null(contact$name) && nzchar(contact$name)) {
      meta <- add_default(meta, "contact[1]-name", .mztab_escape(contact$name))
    }
    if (!is.null(contact$email) && nzchar(contact$email)) {
      meta <- add_default(
        meta,
        "contact[1]-email",
        .mztab_escape(contact$email)
      )
    }
    if (!is.null(contact$affiliation) && nzchar(contact$affiliation)) {
      meta <- add_default(
        meta,
        "contact[1]-affiliation",
        .mztab_escape(contact$affiliation)
      )
    }
  }

  # ── ms_run metadata ───────────────────────────────────────────────────────
  meta <- add_default(meta, "ms_run[1]-location", ms_run_location)

  # File format: downstream consumers can parse spectra if they know the format.
  # Acceptable values are CV Params like [MS, MS:1000584, mzML file, ].
  if (
    !is.null(ms_run_format) && ms_run_format != "null" && nzchar(ms_run_format)
  ) {
    meta <- add_default(meta, "ms_run[1]-format", ms_run_format)
  }

  # Native spectrum identifier format.
  if (
    !is.null(ms_run_id_format) &&
      ms_run_id_format != "null" &&
      nzchar(ms_run_id_format)
  ) {
    meta <- add_default(meta, "ms_run[1]-id_format", ms_run_id_format)
  }

  # Scan polarity – use PSI-MS registered accessions.
  if (!is.null(polarity) && nzchar(polarity) && polarity != "null") {
    polarity_param <- switch(
      tolower(polarity),
      positive = catalog$terms$params$polarity$positive,
      negative = catalog$terms$params$polarity$negative,
      NULL
    )
    if (!is.null(polarity_param)) {
      meta <- add_default(meta, "ms_run[1]-scan_polarity", polarity_param)
    }
  }

  # Summary-level experiment design defaults for TIMA exports.
  meta <- add_default(
    meta,
    "quantification_method",
    catalog$terms$params$quantification$label_free
  )
  meta <- add_default(
    meta,
    "small_molecule-quantification_unit",
    catalog$terms$params$quantification$peak_area
  )
  meta <- add_default(
    meta,
    "small_molecule_feature-quantification_unit",
    catalog$terms$params$quantification$peak_area
  )
  meta <- add_default(meta, "sample[1]", catalog$terms$params$sample)
  meta <- add_default(
    meta,
    "sample[1]-description",
    if (!is.null(sample_name) && nzchar(sample_name) && sample_name != "null") {
      .mztab_escape(sample_name)
    } else {
      catalog$terms$metadata_defaults[["sample[1]-description"]]
    }
  )
  meta <- add_default(meta, "assay[1]", "sample[1]")
  meta <- add_default(meta, "assay[1]-ms_run_ref", "ms_run[1]")
  meta <- add_default(
    meta,
    "assay[1]-quantification_reagent",
    catalog$terms$params$assay_quantification_reagent
  )
  # mzTab-M 2.1 introduces study_variable_group and removes
  # study_variable[*]-factors. Emit a minimal default group so the generated
  # file remains explicit and machine-readable for simple one-factor exports.
  meta <- add_default(
    meta,
    "study_variable_group[1]",
    catalog$terms$params$study_variable_group
  )
  meta <- add_default(
    meta,
    "study_variable_group[1]-description",
    catalog$terms$metadata_defaults[["study_variable_group[1]-description"]]
  )
  meta <- add_default(
    meta,
    "study_variable_group[1]-type",
    catalog$terms$metadata_defaults[["study_variable_group[1]-type"]]
  )
  meta <- add_default(
    meta,
    "study_variable_group[1]-datatype",
    catalog$terms$metadata_defaults[["study_variable_group[1]-datatype"]]
  )
  meta <- add_default(
    meta,
    "study_variable[1]-group_ref",
    "study_variable_group[1]"
  )
  meta <- add_default(meta, "study_variable[1]-assay_refs", "assay[1]")
  meta <- add_default(
    meta,
    "study_variable[1]-description",
    catalog$terms$metadata_defaults[["study_variable[1]-description"]]
  )

  # ── database[1]: primary natural-product reference ────────────────────────
  # LOTUS is the primary SOP used by TIMA; no PSI-MS accession exists for it.
  meta <- add_default(
    meta,
    "database[1]",
    catalog$terms$params$default_database
  )
  # The connectivity-layer InChIKey fragment used as database_identifier has no
  # database-specific prefix; null is the correct mzTab-M value.
  meta <- add_default(
    meta,
    "database[1]-prefix",
    catalog$terms$metadata_defaults[["database[1]-prefix"]]
  )
  meta <- add_default(
    meta,
    "database[1]-version",
    catalog$terms$metadata_defaults[["database[1]-version"]]
  )
  meta <- add_default(
    meta,
    "database[1]-uri",
    catalog$terms$metadata_defaults[["database[1]-uri"]]
  )

  # ── Additional databases from xrefs (database[2], [3], …) ────────────────
  db_prefix_info <- .mztab_xref_database_blocks(xrefs_index)
  if (length(db_prefix_info) > 0L) {
    for (i in seq_along(db_prefix_info)) {
      db_n <- i + 1L # database[1] is already LOTUS
      info <- db_prefix_info[[i]]
      meta <- add_default(
        meta,
        paste0("database[", db_n, "]"),
        paste0("[, , ", info$name, ", ]")
      )
      meta <- add_default(
        meta,
        paste0("database[", db_n, "]-prefix"),
        info$prefix
      )
      meta <- add_default(
        meta,
        paste0("database[", db_n, "]-version"),
        "Unknown"
      )
      meta <- add_default(
        meta,
        paste0("database[", db_n, "]-uri"),
        info$uri
      )
    }
  }

  # ── id_confidence_measure declarations (TIMA user-CV namespace) ───────────
  # All four measures use TIMA-prefixed accessions.  No PSI-MS accession covers
  # composite annotation scores for metabolomics; these user parameters follow
  # the mzTab-M convention [cv_label, accession, name, value].
  meta <- add_default(
    meta,
    "id_confidence_measure[1]",
    catalog$terms$metadata_defaults[["id_confidence_measure[1]"]]
  )
  if (has_bio) {
    meta <- add_default(
      meta,
      "id_confidence_measure[2]",
      catalog$terms$metadata_defaults[["id_confidence_measure[2]"]]
    )
  }
  if (has_chemo) {
    meta <- add_default(
      meta,
      "id_confidence_measure[3]",
      catalog$terms$metadata_defaults[["id_confidence_measure[3]"]]
    )
  }
  if (has_spectral) {
    # TIMA:004 – spectral similarity score (cosine or entropy depending on
    # the annotate_spectra() method used).  No single PSI-MS accession covers
    # both cosine and entropy similarity in the metabolomics context.
    meta <- add_default(
      meta,
      "id_confidence_measure[4]",
      catalog$terms$metadata_defaults[["id_confidence_measure[4]"]]
    )
  }
  meta <- add_default(
    meta,
    "small_molecule-identification_reliability",
    catalog$terms$metadata_defaults[[
      "small_molecule-identification_reliability"
    ]]
  )

  # ── Column unit declarations ───────────────────────────────────────────────
  # SMF retention time (UO:0000010 = second)
  meta <- add_default(
    meta,
    "colunit-small_molecule_feature",
    catalog$terms$metadata_defaults[["colunit-small_molecule_feature"]]
  )
  # SML theoretical neutral mass (UO:0000221 = dalton)
  meta <- add_default(
    meta,
    "colunit-small_molecule",
    catalog$terms$metadata_defaults[["colunit-small_molecule"]]
  )
  # SME id_confidence_measure columns are dimensionless scores (0–1).
  # We document them so consumers know the scale without reading the paper.
  meta <- add_default(
    meta,
    "colunit-small_molecule_evidence",
    catalog$terms$metadata_defaults[["colunit-small_molecule_evidence"]]
  )

  tidytable::tidytable(
    key = as.character(meta$keys),
    value = as.character(meta$values)
  )
}

#' Write mzTab-M metadata section
#' @keywords internal
.mztab_write_metadata <- function(con, meta) {
  if (nrow(meta) > 0L) {
    meta$value <- as.character(meta$value)
    meta$value[is.na(meta$value) | meta$value == "NA"] <- "null"
  }

  lines <- paste("MTD", meta$key, meta$value, sep = "\t")
  if (length(lines) > 0L) {
    writeLines(lines, con)
  }
  writeLines("", con)
}

#' Write a single mzTab-M section (header + data rows)
#' @keywords internal
.mztab_write_section <- function(con, header_prefix, row_prefix, tbl) {
  if (nrow(tbl) == 0L) {
    return(invisible(NULL))
  }

  # Drop internal bookkeeping columns (feature_id) before writing
  write_cols <- setdiff(colnames(tbl), c("feature_id"))

  # ── Header line ──
  header_vals <- c(header_prefix, write_cols)
  writeLines(paste(header_vals, collapse = "\t"), con)

  # ── Data rows ──
  tbl_df <- as.data.frame(tbl, stringsAsFactors = FALSE, check.names = FALSE)
  if (length(write_cols) == 0L) {
    rows <- rep(row_prefix, nrow(tbl))
  } else {
    for (col in write_cols) {
      if (!col %in% colnames(tbl_df)) {
        tbl_df[[col]] <- "null"
      }
    }

    row_data <- lapply(write_cols, function(col) {
      v <- tbl_df[[col]]
      v <- as.character(v)
      v[is.na(v) | !nzchar(v)] <- "null"
      v
    })

    row_matrix <- do.call(cbind, row_data)
    row_matrix[is.na(row_matrix)] <- "null"
    rows <- apply(row_matrix, 1L, paste, collapse = "\t")
  }

  if (length(rows) > 0L) {
    writeLines(paste(row_prefix, rows, sep = "\t"), con)
  }
  writeLines("", con)
}

# ── Micro-helpers ─────────────────────────────────────────────────────────────

#' Build one MTD line
#' @keywords internal
.mtd <- function(key, value) {
  paste("MTD", key, value, sep = "\t")
}

#' Convert a source column name into an mzTab optional column name.
#' @keywords internal
.mztab_opt_colname <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x, perl = TRUE)
  x <- gsub("^_+|_+$", "", x, perl = TRUE)
  if (!nzchar(x)) {
    return("opt_global_extra")
  }
  paste0("opt_global_", x)
}
