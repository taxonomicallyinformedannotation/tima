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
    if (nrow(meta) == 0L || !any(meta$key == key, na.rm = TRUE)) {
      tidytable::bind_rows(
        meta,
        tidytable::tidytable(key = key, value = value)
      )
    } else {
      meta
    }
  }

  meta <- if (is.null(existing_metadata)) {
    tidytable::tidytable(key = character(0), value = character(0))
  } else {
    tidytable::as_tidytable(existing_metadata)
  }

  if (!all(c("key", "value") %in% colnames(meta))) {
    meta <- tidytable::tidytable(key = character(0), value = character(0))
  }

  # ── Mandatory header fields ──────────────────────────────────────────────
  meta <- add_default(meta, "mzTab-version", "2.1.0-M")
  meta <- add_default(meta, "mzTab-mode", "Summary")
  meta <- add_default(meta, "mzTab-type", "Identification")
  meta <- add_default(meta, "mzTab-ID", .mztab_escape(mztab_id))
  meta <- add_default(meta, "title", .mztab_escape(title))
  meta <- add_default(meta, "description", .mztab_escape(description))
  meta <- add_default(
    meta,
    "uri[1]",
    "https://github.com/taxonomicallyinformedannotation/tima"
  )

  # ── Controlled vocabulary registry ───────────────────────────────────────
  meta <- add_default(meta, "cv[1]-label", "MS")
  meta <- add_default(meta, "cv[1]-full_name", "PSI-MS controlled vocabulary")
  meta <- add_default(meta, "cv[1]-version", "4.1.11")
  meta <- add_default(
    meta,
    "cv[1]-uri",
    "https://purl.obolibrary.org/obo/ms.obo"
  )

  meta <- add_default(meta, "cv[2]-label", "UO")
  meta <- add_default(meta, "cv[2]-full_name", "Units of Measurement Ontology")
  meta <- add_default(meta, "cv[2]-version", "unknown")
  meta <- add_default(
    meta,
    "cv[2]-uri",
    "https://purl.obolibrary.org/obo/uo.obo"
  )

  meta <- add_default(meta, "cv[3]-label", "STATO")
  meta <- add_default(
    meta,
    "cv[3]-full_name",
    "The Statistical Methods Ontology"
  )
  meta <- add_default(meta, "cv[3]-version", "unknown")
  meta <- add_default(
    meta,
    "cv[3]-uri",
    "https://purl.obolibrary.org/obo/stato.obo"
  )

  # TIMA scores are user-namespace Params; register the namespace explicitly.
  meta <- add_default(meta, "cv[4]-label", "TIMA")
  meta <- add_default(
    meta,
    "cv[4]-full_name",
    "Taxonomically Informed Metabolite Annotation user vocabulary"
  )
  meta <- add_default(meta, "cv[4]-version", as.character(software_version))
  meta <- add_default(
    meta,
    "cv[4]-uri",
    "https://github.com/taxonomicallyinformedannotation/tima"
  )

  if (!is.null(publication) && nzchar(publication) && publication != "null") {
    meta <- add_default(meta, "publication[1]", .mztab_escape(publication))
  }

  # ── Software ─────────────────────────────────────────────────────────────
  # TIMA has no PSI-MS accession; the user-parameter Param format is correct.
  meta <- add_default(
    meta,
    "software[1]",
    paste0("[, , TIMA, ", software_version, "]")
  )
  # Emit the canonical repository URL as a software setting so consumers can
  # look up algorithm details without relying on free-text fields.
  meta <- add_default(
    meta,
    "software[1]-setting[1]",
    "https://github.com/taxonomicallyinformedannotation/tima"
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
      positive = "[MS, MS:1000130, positive scan, ]",
      negative = "[MS, MS:1000129, negative scan, ]",
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
    "[MS, MS:1001834, LC-MS label-free quantitation analysis, ]"
  )
  meta <- add_default(
    meta,
    "small_molecule-quantification_unit",
    "[MS, MS:1001113, peak area, ]"
  )
  meta <- add_default(
    meta,
    "small_molecule_feature-quantification_unit",
    "[MS, MS:1001113, peak area, ]"
  )
  meta <- add_default(meta, "sample[1]", "[, , metabolomics sample, ]")
  meta <- add_default(
    meta,
    "sample[1]-description",
    if (!is.null(sample_name) && nzchar(sample_name) && sample_name != "null") {
      .mztab_escape(sample_name)
    } else {
      "TIMA metabolomics sample"
    }
  )
  meta <- add_default(meta, "assay[1]", "sample[1]")
  meta <- add_default(meta, "assay[1]-ms_run_ref", "ms_run[1]")
  meta <- add_default(
    meta,
    "assay[1]-quantification_reagent",
    "[MS, MS:1002038, unlabeled sample, ]"
  )
  # mzTab-M 2.1 introduces study_variable_group and removes
  # study_variable[*]-factors. Emit a minimal default group so the generated
  # file remains explicit and machine-readable for simple one-factor exports.
  meta <- add_default(
    meta,
    "study_variable_group[1]",
    "[, , annotation_group, ]"
  )
  meta <- add_default(
    meta,
    "study_variable_group[1]-description",
    "TIMA default study variable group"
  )
  meta <- add_default(
    meta,
    "study_variable_group[1]-type",
    "[STATO, STATO:0000252, categorical variable, ]"
  )
  meta <- add_default(meta, "study_variable_group[1]-datatype", "xsd:string")
  meta <- add_default(
    meta,
    "study_variable[1]-group_ref",
    "study_variable_group[1]"
  )
  meta <- add_default(meta, "study_variable[1]-assay_refs", "assay[1]")
  meta <- add_default(
    meta,
    "study_variable[1]-description",
    "TIMA annotation study variable"
  )

  # ── database[1]: primary natural-product reference ────────────────────────
  # LOTUS is the primary SOP used by TIMA; no PSI-MS accession exists for it.
  meta <- add_default(
    meta,
    "database[1]",
    "[, , LOTUS natural-product database, ]"
  )
  # The connectivity-layer InChIKey fragment used as database_identifier has no
  # database-specific prefix; null is the correct mzTab-M value.
  meta <- add_default(meta, "database[1]-prefix", "null")
  meta <- add_default(meta, "database[1]-version", "Unknown")
  meta <- add_default(
    meta,
    "database[1]-uri",
    "https://lotus.naturalproducts.net"
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
    "[TIMA, TIMA:001, TIMA combined annotation score, ]"
  )
  if (has_bio) {
    meta <- add_default(
      meta,
      "id_confidence_measure[2]",
      "[TIMA, TIMA:002, TIMA biological (taxonomic context) score, ]"
    )
  }
  if (has_chemo) {
    meta <- add_default(
      meta,
      "id_confidence_measure[3]",
      "[TIMA, TIMA:003, TIMA chemical (structural consistency) score, ]"
    )
  }
  if (has_spectral) {
    # TIMA:004 – spectral similarity score (cosine or entropy depending on
    # the annotate_spectra() method used).  No single PSI-MS accession covers
    # both cosine and entropy similarity in the metabolomics context.
    meta <- add_default(
      meta,
      "id_confidence_measure[4]",
      "[TIMA, TIMA:004, TIMA spectral similarity score, ]"
    )
  }
  meta <- add_default(
    meta,
    "small_molecule-identification_reliability",
    "[MS, MS:1000932, identification reliability, ]"
  )

  # ── Column unit declarations ───────────────────────────────────────────────
  # SMF retention time (UO:0000010 = second)
  meta <- add_default(
    meta,
    "colunit-small_molecule_feature",
    "retention_time_in_seconds=[UO, UO:0000010, second, ]"
  )
  # SML theoretical neutral mass (UO:0000221 = dalton)
  meta <- add_default(
    meta,
    "colunit-small_molecule",
    "theoretical_neutral_mass=[UO, UO:0000221, dalton, ]"
  )
  # SME id_confidence_measure columns are dimensionless scores (0–1).
  # We document them so consumers know the scale without reading the paper.
  meta <- add_default(
    meta,
    "colunit-small_molecule_evidence",
    paste0(
      "id_confidence_measure[1]=[TIMA, TIMA:001, TIMA combined annotation score, ]"
    )
  )

  meta
}

#' Write mzTab-M metadata section
#' @keywords internal
.mztab_write_metadata <- function(con, meta) {
  if (nrow(meta) > 0L) {
    meta$value <- as.character(meta$value)
    meta$value[is.na(meta$value) | meta$value == "NA"] <- "null"
  }

  lines <- paste("MTD", meta$key, meta$value, sep = "\t")
  writeLines(lines, con)
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

  # ── Data rows (vectorized) ──
  # Convert table to data.frame for efficient column access
  tbl_df <- as.data.frame(tbl, stringsAsFactors = FALSE, check.names = FALSE)

  # Pre-process all columns: replace NA/empty with "null"
  row_data <- lapply(write_cols, function(col) {
    v <- tbl_df[[col]]
    v <- as.character(v)
    v[is.na(v) | !nzchar(v)] <- "null"
    v
  })

  # Build all rows at once (vectorized)
  rows_list <- lapply(seq_len(nrow(tbl)), function(i) {
    row_vals <- vapply(row_data, function(col_vec) col_vec[[i]], character(1L))
    paste(c(row_prefix, row_vals), collapse = "\t")
  })

  writeLines(unlist(rows_list), con)
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
