fixture_dir <- file.path("tests", "testthat", "fixtures", "mztab")
out <- file.path(fixture_dir, "mock_all_cases.mztab")

all_files <- list.files(
  fixture_dir,
  pattern = "\\.(mztab|mzTab)$",
  full.names = TRUE
)
all_files <- setdiff(all_files, out)

if (length(all_files) == 0L) {
  stop("No source mzTab fixtures found in: ", fixture_dir)
}

read_nonempty <- function(path) {
  x <- readLines(path, warn = FALSE, encoding = "UTF-8")
  x[nzchar(x)]
}

collect_metadata <- function(files) {
  preferred <- c(
    "mzTab-version",
    "mzTab-mode",
    "mzTab-type",
    "title",
    "description",
    "publication[1]",
    "software[1]",
    "software[1]-setting[1]",
    "ms_run[1]-location",
    "ms_run[1]-format",
    "ms_run[1]-id_format",
    "ms_run[1]-scan_polarity",
    "sample[1]",
    "sample[1]-species[1]",
    "sample[1]-organism",
    "sample[1]-description",
    "assay[1]",
    "assay[1]-sample_ref",
    "assay[1]-ms_run_ref",
    "study_variable[1]-assay_refs",
    "study_variable[1]-description",
    "database[1]",
    "database[1]-prefix",
    "database[1]-uri",
    "id_confidence_measure[1]",
    "id_confidence_measure[2]",
    "id_confidence_measure[3]",
    "id_confidence_measure[4]",
    "colunit-small_molecule_feature",
    "colunit-small_molecule",
    "colunit-small_molecule_evidence"
  )

  kv <- list()
  for (f in files) {
    for (ln in grep("^MTD\\t", read_nonempty(f), value = TRUE)) {
      parts <- strsplit(ln, "\\t", fixed = FALSE)[[1L]]
      if (length(parts) < 3L) {
        next
      }
      key <- parts[[2L]]
      val <- paste(parts[-c(1L, 2L)], collapse = "\t")
      if (is.null(kv[[key]]) && nzchar(val) && val != "null") {
        kv[[key]] <- val
      }
    }
  }

  keys <- unique(c(preferred, names(kv)))
  rows <- character(0)
  for (k in keys) {
    v <- kv[[k]]
    if (is.null(v)) {
      next
    }
    rows <- c(rows, paste("MTD", k, v, sep = "\t"))
  }

  # Strong defaults for parser coverage if absent in source fixtures.
  fallback <- c(
    "MTD\tmzTab-version\t2.0.0-M",
    "MTD\tmzTab-mode\tSummary",
    "MTD\tmzTab-type\tIdentification",
    "MTD\ttitle\tMock all-cases mzTab",
    "MTD\tms_run[1]-location\tnull",
    "MTD\tsample[1]\tmock_sample_1",
    "MTD\tassay[1]\tsample[1]"
  )
  have <- sub("^MTD\\t([^\\t]+)\\t.*$", "\\1", rows)
  for (ln in fallback) {
    key <- sub("^MTD\\t([^\\t]+)\\t.*$", "\\1", ln)
    if (!key %in% have) {
      rows <- c(rows, ln)
    }
  }

  rows
}

collect_section <- function(files, hdr_prefix, row_prefix, id_col) {
  required <- switch(
    hdr_prefix,
    SMH = c(
      "SML_ID",
      "SMF_ID_REFS",
      "SME_ID_REFS",
      "database_identifier",
      "chemical_formula",
      "smiles",
      "inchi",
      "chemical_name",
      "uri",
      "theoretical_neutral_mass",
      "adduct_ions",
      "reliability",
      "best_id_confidence_measure",
      "best_id_confidence_value"
    ),
    SFH = c(
      "SMF_ID",
      "SML_ID_REFS",
      "exp_mass_to_charge",
      "charge",
      "retention_time_in_seconds"
    ),
    SEH = c(
      "SME_ID",
      "evidence_input_id",
      "database_identifier",
      "chemical_formula",
      "smiles",
      "inchi",
      "chemical_name",
      "uri",
      "derivatized_form",
      "adduct_ion",
      "exp_mass_to_charge",
      "charge",
      "theoretical_mass_to_charge",
      "spectra_ref",
      "identification_method",
      "ms_level",
      "rank",
      "id_confidence_measure[1]"
    ),
    character(0)
  )

  col_order <- required
  values <- list()

  for (f in files) {
    lines <- read_nonempty(f)
    h_idx <- grep(paste0("^", hdr_prefix, "\\t"), lines)[1L]
    if (is.na(h_idx)) {
      next
    }
    cols <- strsplit(lines[[h_idx]], "\\t", fixed = FALSE)[[1L]][-1L]
    r_idx <- grep(paste0("^", row_prefix, "\\t"), lines)
    if (length(r_idx) == 0L) {
      next
    }

    for (i in r_idx) {
      vals <- strsplit(lines[[i]], "\\t", fixed = FALSE)[[1L]][-1L]
      if (length(vals) < length(cols)) {
        vals <- c(vals, rep("null", length(cols) - length(vals)))
      }
      for (j in seq_along(cols)) {
        col <- cols[[j]]
        val <- vals[[j]]
        if (!col %in% col_order) {
          col_order <- c(col_order, col)
        }
        if (
          (is.null(values[[col]]) || values[[col]] == "null") &&
            nzchar(val) &&
            val != "null"
        ) {
          values[[col]] <- val
        }
      }
    }
  }

  col_order <- unique(col_order)
  row <- vapply(
    col_order,
    function(col) {
      v <- values[[col]]
      if (is.null(v) || !nzchar(v)) "null" else v
    },
    character(1L)
  )

  if (id_col %in% names(row)) {
    row[[id_col]] <- "1"
  }
  if ("SMF_ID_REFS" %in% names(row)) {
    row[["SMF_ID_REFS"]] <- "1"
  }
  if ("SME_ID_REFS" %in% names(row)) {
    row[["SME_ID_REFS"]] <- "1"
  }
  if ("SML_ID_REFS" %in% names(row)) {
    row[["SML_ID_REFS"]] <- "1"
  }
  if ("evidence_input_id" %in% names(row)) {
    row[["evidence_input_id"]] <- "1"
  }
  if (
    "identification_method" %in%
      names(row) &&
      row[["identification_method"]] == "null"
  ) {
    row[["identification_method"]] <- "[, , spectral library matching, ]"
  }
  if ("ms_level" %in% names(row) && row[["ms_level"]] == "null") {
    row[["ms_level"]] <- "[MS, MS:1000580, MS2 spectrum, ]"
  }

  list(
    header = paste(c(hdr_prefix, col_order), collapse = "\t"),
    row = paste(c(row_prefix, unname(row[col_order])), collapse = "\t")
  )
}

mtd_lines <- collect_metadata(all_files)
sml <- collect_section(all_files, "SMH", "SML", "SML_ID")
smf <- collect_section(all_files, "SFH", "SMF", "SMF_ID")
sme <- collect_section(all_files, "SEH", "SME", "SME_ID")

lines <- c(
  "COM\tmock fixture aggregating parser/writer edge cases from all source fixtures",
  mtd_lines,
  "COM\tMGH\tmgf_id\tprec_mz\tprec_rt\tlevel\ttitle\tspec_mz\tspec_int\tspec_len",
  "COM\tMGF\t1\t123.45\t90\t2\tmock_embedded\t50|75\t100|200\t2",
  "COM\tCUSTOM\tpost-managed extension row",
  "",
  sml$header,
  sml$row,
  "",
  smf$header,
  smf$row,
  "",
  sme$header,
  sme$row
)

dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
writeLines(lines, out, useBytes = TRUE)

cat("Wrote:", out, "\n")
cat("Source fixtures:", length(all_files), "\n")
