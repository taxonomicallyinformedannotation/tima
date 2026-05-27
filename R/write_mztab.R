#' @title Write TIMA results as mzTab-M
#'
#' @description Exports TIMA weighted-annotation results to mzTab-M 2.1.0
#' plain-text format.  The output is a conformant mzTab-M file containing:
#'
#' * **MTD** – metadata (software, database, instrument, confidence measures,
#'   ms_run, sample, assay, study_variable).
#' * **SMF** – one row per chromatographic feature (feature_id, m/z, RT).
#' * **SME** – one row per identification evidence (candidate annotation).
#' * **SML** – one row per unique compound, linking all associated SMF and SME
#'   rows.
#'
#' @details
#' The function intentionally writes in **Summary** mode because TIMA
#' is an annotation/prioritization tool and does not guarantee complete
#' quantification matrices.  Fields that have no TIMA equivalent (e.g.
#' full InChI, spectra_ref) are written as `null`.
#'
#' TIMA columns are mapped to canonical mzTab fields where a direct
#' equivalent exists; only truly unmapped columns fall back to
#' `opt_global_*` to keep downstream consumers happy:
#'
#' * **SME section** – candidate-level columns with no canonical field
#'   (e.g. SIRIUS subscores, similarity forward/reverse, m/z error)
#'   become `opt_global_*`.  Feature-level columns are **not** repeated
#'   here; they belong to the SMF section.
#' * **SMF section** – all `feature_*` columns beyond `feature_mz` and
#'   `feature_rt` (spectrum entropy, spectrum peaks, predicted taxonomy
#'   class/NPC scores …) are exported as `opt_global_*` in the SMF row,
#'   making them available without polluting SME.
#'
#' ## mzTab-M reliability mapping
#'
#' Reliability levels follow the Metabolomics Standards Initiative (MSI) scale:
#'
#' * **1** – confirmed (score ≥ 0.7 with spectral library evidence)
#' * **2** – probable (score ≥ 0.5; or spectral match, any score)
#' * **3** – putative (score ≥ 0.2)
#' * **4** – unambiguous compound class only (everything else)
#'
#' ## id_confidence_measure columns
#'
#' Four TIMA-specific confidence measures are exported as
#' `id_confidence_measure[1..4]` in the MTD section and as additional
#' columns in the SME section.  All use the `TIMA` user-controlled CV
#' namespace (no PSI-MS accession exists for these composite scores):
#'
#' * `[1]` – `score_final` (combined TIMA score; `TIMA:001`)
#' * `[2]` – `score_biological` (taxonomic score; `TIMA:002`)
#' * `[3]` – `score_chemical` (chemical consistency; `TIMA:003`)
#' * `[4]` – `candidate_score_similarity` (spectral similarity;
#'   `TIMA:004`; omitted when no spectral evidence is present)
#'
#' ## Ontology alignment
#'
#' * `ms_level` uses PSI-MS accessions `MS:1000579` (MS1) and
#'   `MS:1000580` (MS2).
#' * `scan_polarity` uses `MS:1000130` (positive) and `MS:1000129`
#'   (negative) when `polarity` is supplied.
#' * `retention_time_in_seconds` is declared in
#'   `colunit-small_molecule_feature` with UO accession `UO:0000010`.
#' * `theoretical_neutral_mass` is declared with `UO:0000221` (dalton).
#' * `spectra_ref` is formatted as `ms_run[1]:{spectrum_native_id}`.
#' * `instrument[1]` is populated from the `instrument` parameter using a
#'   PSI-MS CV Param when provided.
#' * `quantification_method` is set to
#'   `[MS, MS:1001834, LC-MS label-free quantitation analysis, ]` for
#'   untargeted metabolomics (per PSI-MS ontology).
#' * `assay[1]-quantification_reagent` is set to
#'   `[MS, MS:1002038, unlabeled sample, ]` (no labelling used by TIMA).
#' * `sample[1]` defaults to a metabolite mixture Param; can be overridden
#'   via the `sample_name` parameter.
#' * `publication` emits a formatted citation when `publication` is supplied.
#' * The software entry includes the TIMA repository URL as a
#'   `software[1]-setting` for machine-readable provenance.
#'
#' @include calculate_mass_of_m.R
#' @include get_params.R
#' @include mztab_parser.R
#' @include safe_fread.R
#' @include validations_utils.R
#'
#' @param input `character` Path to TIMA results file produced by
#'   `weight_annotations()`.
#' @param output `character` Destination path for the mzTab-M file
#'   (`.mztab` extension recommended).
#' @param ms_run_location `character` URI/path to the originating raw data
#'   file (used in `MTD ms_run[1]-location`).  Defaults to `"null"`.
#' @param ms_run_format `character` CV Param string for the raw file format,
#'   e.g. `"[MS, MS:1000584, mzML file, ]"`.  Defaults to `"null"` when not
#'   known.
#' @param ms_run_id_format `character` CV Param string for the spectrum
#'   native-ID format, e.g.
#'   `"[MS, MS:1000776, scan number only nativeID format, ]"`.  Defaults to
#'   `"null"`.
#' @param polarity `character | NULL` Scan polarity of the MS run.  One of
#'   `"positive"`, `"negative"`, or `NULL`/`"null"` (unknown / data-dependent).
#'   Used to populate `ms_run[1]-scan_polarity` with the correct PSI-MS CV term
#'   (`MS:1000130` positive, `MS:1000129` negative).
#' @param instrument `character | NULL` CV Param string for the mass spectrometer
#'   model, e.g. `"[MS, MS:1001742, LTQ Orbitrap Velos, ]"`.  When `NULL`
#'   the `instrument[1]` block is omitted.
#' @param sample_name `character | NULL` Free-text sample name written to
#'   `sample[1]-description`.  When `NULL`, a generic metabolomics description
#'   is used.
#' @param publication `character | NULL` Bibliographic reference for the study
#'   (PubMed URL or DOI).  When provided, emitted as `MTD publication[1]`.
#' @param title `character` Free-text study title written to `MTD title`.
#' @param description `character` Free-text experiment description written to
#'   `MTD description`.
#' @param software_version `character` Version string for the software entry.
#'   Defaults to the installed TIMA package version.
#' @param contact `list | NULL` Optional contact information list with fields
#'   `name`, `email`, and optionally `affiliation`.  When supplied, the
#'   corresponding `MTD contact[1]-*` lines are emitted.
#' @param xrefs_file `character | NULL` Optional path to a cross-references TSV
#'   produced by [get_compounds_xrefs()].  When provided, the `uri` fields in
#'   SME/SML are enriched with external database URLs (Wikidata preferred), and
#'   additional `MTD database[n]` blocks are registered for each unique xref
#'   prefix found in the data.
#' @param edges_file `character | NULL` Optional path to an edge table (for
#'   example from `data/interim`). When provided, all edge rows are embedded in
#'   the mzTab text export as `COM` extension lines (`TIMA edges`) so graph
#'   information lives in the same artifact.
#' @param base_mztab `character | NULL` Optional existing mzTab-M file to
#'   complement. Existing SML/SMF/SME rows are preserved and new TIMA rows are
#'   appended without duplication. Non-managed lines/sections are passed through
#'   unchanged.
#'
#' @return Character path to the written mzTab-M file (invisibly).
#'
#' @family annotation
#' @export
#'
#' @examples
#' \dontrun{
#' write_mztab(
#'   input  = "annotations.tsv",
#'   output = "annotations.mztab"
#' )
#' }
write_mztab <- function(
  input = get_params(step = "write_mztab")$files$annotations$processed,
  output = get_params(step = "write_mztab")$files$output$mztab,
  ms_run_location = "null",
  ms_run_format = "null",
  ms_run_id_format = "null",
  polarity = NULL,
  instrument = NULL,
  sample_name = NULL,
  publication = NULL,
  title = "TIMA annotation results",
  description = paste0(
    "Annotation results produced by Taxonomically Informed ",
    "Metabolomics Annotation (TIMA)."
  ),
  software_version = as.character(utils::packageVersion("tima")),
  contact = NULL,
  xrefs_file = NULL,
  edges_file = NULL,
  base_mztab = NULL
) {
  # Some parameter files may accidentally provide character vectors for fields
  # that are semantically scalar. Coerce deterministically to a length-1 value.
  .mztab_scalar_chr <- function(x, param_name, allow_null = TRUE) {
    if (is.null(x)) {
      return(NULL)
    }
    if (!is.character(x)) {
      return(x)
    }
    if (length(x) == 0L) {
      return(if (allow_null) NULL else x)
    }
    if (length(x) > 1L) {
      log_warn(
        "Parameter '%s' received %d values; using the first one",
        param_name,
        length(x)
      )
    }
    x[[1L]]
  }

  input <- .mztab_scalar_chr(input, "input", allow_null = FALSE)
  output <- .mztab_scalar_chr(output, "output", allow_null = FALSE)
  ms_run_location <- .mztab_scalar_chr(ms_run_location, "ms_run_location")
  ms_run_format <- .mztab_scalar_chr(ms_run_format, "ms_run_format")
  ms_run_id_format <- .mztab_scalar_chr(ms_run_id_format, "ms_run_id_format")
  polarity <- .mztab_scalar_chr(polarity, "polarity")
  instrument <- .mztab_scalar_chr(instrument, "instrument")
  sample_name <- .mztab_scalar_chr(sample_name, "sample_name")
  publication <- .mztab_scalar_chr(publication, "publication")
  title <- .mztab_scalar_chr(title, "title", allow_null = FALSE)
  description <- .mztab_scalar_chr(
    description,
    "description",
    allow_null = FALSE
  )
  software_version <- .mztab_scalar_chr(
    software_version,
    "software_version",
    allow_null = FALSE
  )
  xrefs_file <- .mztab_scalar_chr(xrefs_file, "xrefs_file")
  edges_file <- .mztab_scalar_chr(edges_file, "edges_file")
  base_mztab <- .mztab_scalar_chr(base_mztab, "base_mztab")

  validate_character(input, param_name = "input")
  validate_file_exists(
    input,
    file_type = "TIMA results file",
    param_name = "input"
  )
  validate_character(output, param_name = "output")

  # Keep exports beside the annotation input while preserving the requested
  # filename; this avoids writing mzTab to a different pipeline directory.
  output <- file.path(dirname(input), basename(output))

  if (!is.null(instrument)) {
    validate_character(instrument, param_name = "instrument")
  }
  if (!is.null(sample_name)) {
    validate_character(sample_name, param_name = "sample_name")
  }
  if (!is.null(publication)) {
    validate_character(publication, param_name = "publication")
  }
  if (!is.null(edges_file)) {
    validate_character(edges_file, param_name = "edges_file")
    validate_file_exists(
      edges_file,
      file_type = "edge table",
      param_name = "edges_file"
    )
  }

  # Normalise polarity to NULL / "positive" / "negative"
  if (
    !is.null(polarity) &&
      !is.na(polarity) &&
      nzchar(polarity) &&
      polarity != "null"
  ) {
    polarity <- tolower(trimws(polarity))
    if (!polarity %in% c("positive", "negative", "pos", "neg")) {
      log_warn(
        "Unrecognised polarity '%s'; treating as unknown",
        polarity
      )
      polarity <- NULL
    }
    # Expand abbreviations
    if (!is.null(polarity)) {
      polarity <- switch(polarity, pos = "positive", neg = "negative", polarity)
    }
  } else {
    polarity <- NULL
  }

  if (is.null(base_mztab)) {
    base_mztab <- tryCatch(
      get_params(step = "write_mztab")$files$mztab$raw,
      error = function(e) {
        rm(e)
        NULL
      }
    )
  }
  if (!is.null(base_mztab)) {
    validate_character(base_mztab, param_name = "base_mztab")
    validate_file_exists(
      base_mztab,
      file_type = "mzTab-M file",
      param_name = "base_mztab"
    )
  }

  # Resolve xrefs index (InChIKey connectivity layer → list of prefix+id rows)
  xrefs_index <- .mztab_build_xrefs_index(xrefs_file)

  ctx <- log_operation(
    "write_mztab",
    input = basename(input),
    output = basename(output)
  )

  results <- safe_fread(
    file = input,
    file_type = "mztab export input",
    na.strings = c("", "NA", "null", "NULL"),
    colClasses = "character"
  )

  if (nrow(results) == 0L) {
    log_warn("TIMA results table is empty; writing a minimal skeleton mzTab-M")
    results <- tidytable::tidytable(
      feature_id = character(0),
      feature_mz = character(0),
      feature_rt = character(0)
    )
  }

  # ── Build internal tables ─────────────────────────────────────────────────

  # Each row in `results` is one feature × candidate pair.
  # Detect "summarized" form (pipe-separated multi-values) by checking
  # whether score_final contains "|" characters – if so, we expand to one
  # row per candidate so mzTab-M has one SME per evidence item.
  results <- .mztab_expand_summarized(results)

  smf_table <- .mztab_build_smf(results)
  sme_table <- .mztab_build_sme(results, smf_table, xrefs_index)
  sml_table <- .mztab_build_sml(results, smf_table, sme_table, xrefs_index)

  base <- NULL
  passthrough_lines <- character(0)
  if (!is.null(base_mztab)) {
    base <- .mztab_read_base(base_mztab)
    passthrough_lines <- base$passthrough

    merged <- .mztab_merge_tables(
      base = base,
      sml_new = sml_table,
      smf_new = smf_table,
      sme_new = sme_table
    )
    sml_table <- merged$sml
    smf_table <- merged$smf
    sme_table <- merged$sme
  }

  # Keep SMF -> SME evidence links explicit in the exported table.
  smf_table <- .mztab_refresh_smf_sme_refs(smf = smf_table, sme = sme_table)

  # ── Write file ────────────────────────────────────────────────────────────

  metadata_table <- .mztab_build_metadata_table(
    mztab_id = tools::file_path_sans_ext(basename(output)),
    title = title,
    description = description,
    software_version = software_version,
    ms_run_location = ms_run_location,
    ms_run_format = ms_run_format,
    ms_run_id_format = ms_run_id_format,
    polarity = polarity,
    instrument = instrument,
    sample_name = sample_name,
    publication = publication,
    contact = contact,
    xrefs_index = xrefs_index,
    sme_table = sme_table,
    existing_metadata = if (is.null(base)) {
      NULL
    } else {
      base$metadata
    }
  )

  if (.mztab_is_json_path(output)) {
    .mztab_write_json(
      file = output,
      metadata = metadata_table,
      sml_table = sml_table,
      smf_table = smf_table,
      sme_table = sme_table,
      edges_file = edges_file
    )
  } else {
    create_dir(output)
    con <- file(output, open = "wt", encoding = "UTF-8")
    on.exit(close(con), add = TRUE)
    export_comments <- .mztab_build_export_comments(results)
    edge_comments <- .mztab_build_edges_comments(edges_file)

    if (length(passthrough_lines) > 0L) {
      writeLines(passthrough_lines, con)
      if (
        length(passthrough_lines) > 0L &&
          utils::tail(passthrough_lines, 1L) != ""
      ) {
        writeLines("", con)
      }
    }

    if (length(export_comments) > 0L) {
      writeLines(export_comments, con)
      writeLines("", con)
    }

    if (length(edge_comments) > 0L) {
      writeLines(edge_comments, con)
      writeLines("", con)
    }

    .mztab_write_metadata(con = con, meta = metadata_table)
    .mztab_write_section(con, "SMH", "SML", sml_table)
    .mztab_write_section(con, "SFH", "SMF", smf_table)
    .mztab_write_section(con, "SEH", "SME", sme_table)
  }

  log_complete(
    ctx,
    n_sml = nrow(sml_table),
    n_smf = nrow(smf_table),
    n_sme = nrow(sme_table)
  )

  invisible(output)
}

# ── Internal helpers ──────────────────────────────────────────────────────────

#' Safely extract a column or return "null" vector (vectorized utility)
#' @keywords internal
.mztab_pluck <- function(df, col) {
  if (col %in% colnames(df)) {
    as.character(df[[col]])
  } else {
    rep("null", nrow(df))
  }
}

#' Safely extract a column, replacing NA/null with "null" (vectorized utility)
#' @keywords internal
.mztab_pluck_na <- function(df, col) {
  x <- .mztab_pluck(df, col)
  x[is.na(x) | x == "NA" | x == "null" | !nzchar(x)] <- "null"
  x
}

#' Expand summarized (pipe-separated) results to one row per candidate
#' @keywords internal
.mztab_expand_summarized <- function(results) {
  # Heuristic: if any candidate/rank/score column contains "|", the table was
  # written with summarize = TRUE. Expand to long form.
  multi_cols <- colnames(results)[grepl(
    "^candidate|^rank|^score",
    colnames(results),
    perl = TRUE
  )]
  multi_cols <- intersect(multi_cols, colnames(results))

  # Check if any cell contains pipe separator
  has_multivalue <- length(multi_cols) > 0L &&
    any(
      vapply(
        X = multi_cols,
        FUN = function(col) {
          any(
            grepl("|", as.character(results[[col]]), fixed = TRUE),
            na.rm = TRUE
          )
        },
        FUN.VALUE = logical(1L)
      )
    )
  if (!has_multivalue) {
    return(results)
  }

  # safe_fread() may return a data.table; convert once
  results <- as.data.frame(
    results,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  id_cols <- setdiff(colnames(results), multi_cols)

  # Pre-compute the number of values per row (vectorized)
  n_vals_per_row <- vapply(
    seq_len(nrow(results)),
    function(i) {
      max(
        vapply(
          multi_cols,
          function(col) {
            v <- results[[col]][i]
            if (is.na(v) || !nzchar(v)) {
              1L
            } else {
              length(strsplit(v, "|", fixed = TRUE)[[1L]])
            }
          },
          FUN.VALUE = integer(1L)
        )
      )
    },
    FUN.VALUE = integer(1L)
  )

  rows <- lapply(seq_len(nrow(results)), function(i) {
    n_vals <- n_vals_per_row[i]
    if (n_vals <= 1L) {
      return(results[i, , drop = FALSE])
    }

    base <- results[rep(i, n_vals), id_cols, drop = FALSE]
    expanded <- lapply(multi_cols, function(col) {
      v <- results[[col]][i]
      if (is.na(v) || !nzchar(v)) {
        return(rep(NA_character_, n_vals))
      }
      parts <- strsplit(v, "|", fixed = TRUE)[[1L]]
      if (length(parts) < n_vals) {
        parts <- c(parts, rep(NA_character_, n_vals - length(parts)))
      }
      parts[seq_len(n_vals)]
    })
    names(expanded) <- multi_cols
    expanded_df <- as.data.frame(expanded, stringsAsFactors = FALSE)

    cbind(base, expanded_df)[, colnames(results), drop = FALSE]
  })

  tidytable::as_tidytable(tidytable::bind_rows(rows))
}

#' Build SMF (Small Molecule Feature) table – one row per feature_id
#' @keywords internal
.mztab_build_smf <- function(results) {
  # Assign sequential integer SMF_ID to each unique feature_id.
  feature_ids <- unique(results$feature_id[!is.na(results$feature_id)])

  smf <- tidytable::tidytable(
    SMF_ID = seq_along(feature_ids),
    feature_id = feature_ids
  )

  # Canonical feature columns that become proper mzTab-M SMF fields.
  canonical_feat_cols <- c("feature_id", "feature_mz", "feature_rt")

  # All feature_* columns present in results (beyond the canonical three) are
  # surfaced as opt_global_* in the SMF section.  They are feature-level data
  # and must not land in the per-evidence SME section.
  # TIMA summary columns below are also feature-level and must survive export.
  feature_level_passthrough <- intersect(
    c("component_id", "candidates_evaluated", "candidates_best"),
    colnames(results)
  )
  extra_feat_cols <- setdiff(
    c(
      grep("^feature_", colnames(results), value = TRUE),
      feature_level_passthrough
    ),
    canonical_feat_cols
  )

  # Attach mz, rt and any extra feature metadata from the first occurrence per
  # feature_id in results.
  feat_meta_cols <- c(canonical_feat_cols, extra_feat_cols)
  feat_meta <- results |>
    tidytable::distinct(feature_id, .keep_all = TRUE) |>
    tidytable::select(
      tidyselect::any_of(feat_meta_cols)
    )

  smf <- smf |>
    tidytable::left_join(feat_meta, by = "feature_id")

  # Convert RT to seconds for mzTab-M.
  if ("feature_rt" %in% colnames(smf)) {
    smf <- smf |>
      tidytable::mutate(
        retention_time_in_seconds = suppressWarnings(
          as.character(as.numeric(feature_rt) * 60)
        )
      )
  } else {
    smf$retention_time_in_seconds <- "null"
  }

  # Build canonical portion of SMF.
  smf_canonical <- smf |>
    tidytable::transmute(
      SMF_ID = as.character(SMF_ID),
      SML_ID_REFS = NA_character_, # filled later
      SME_ID_REFS = NA_character_, # filled later from SME feature links
      SME_ID_REF_ambiguity_code = "null",
      exp_mass_to_charge = if ("feature_mz" %in% colnames(smf)) {
        feature_mz
      } else {
        "null"
      },
      charge = "null",
      retention_time_in_seconds,
      feature_id = feature_id
    )

  # Append opt_global_* columns for extra feature-level metadata.
  if (length(extra_feat_cols) > 0L) {
    used_names <- colnames(smf_canonical)

    # Build all opt_global column names first (vectorized)
    opt_col_mapping <- lapply(extra_feat_cols, function(col) {
      base_name <- .mztab_opt_colname(col)
      out_name <- base_name
      idx <- 1L
      while (out_name %in% used_names) {
        idx <- idx + 1L
        out_name <- paste0(base_name, "_", idx)
      }
      used_names <<- c(used_names, out_name)
      list(col = col, out_name = out_name)
    })

    # Vectorized extraction and assignment
    for (mapping in opt_col_mapping) {
      col <- mapping$col
      out_name <- mapping$out_name
      val <- if (col %in% colnames(smf)) {
        v <- as.character(smf[[col]])
        v[is.na(v) | !nzchar(v) | v == "NA"] <- "null"
        v
      } else {
        rep("null", nrow(smf_canonical))
      }
      smf_canonical[[out_name]] <- val
    }
  }

  smf_canonical
}

#' Build SME (Small Molecule Evidence) table – one row per evidence
#' @keywords internal
.mztab_build_sme <- function(results, smf_table, xrefs_index = NULL) {
  ann_signal_cols <- unique(c(
    grep("^candidate_", colnames(results), value = TRUE),
    intersect(
      c(
        "rank_initial",
        "rank_final",
        "score_initial",
        "score_biological",
        "score_chemical",
        "score_final",
        "annotation_note"
      ),
      colnames(results)
    )
  ))

  has_annotation <- if (length(ann_signal_cols) == 0L) {
    rep(FALSE, nrow(results))
  } else {
    Reduce(
      `|`,
      lapply(ann_signal_cols, function(col) {
        x <- as.character(results[[col]])
        !(is.na(x) | x == "NA" | x == "null" | x == "NULL" | !nzchar(x))
      })
    )
  }

  if (!any(has_annotation)) {
    # No candidates at all – return a minimal empty SME
    return(tidytable::tidytable(
      SME_ID = character(0),
      evidence_input_id = character(0),
      database_identifier = character(0),
      chemical_formula = character(0),
      smiles = character(0),
      inchi = character(0),
      chemical_name = character(0),
      uri = character(0),
      derivatized_form = character(0),
      adduct_ion = character(0),
      exp_mass_to_charge = character(0),
      charge = character(0),
      theoretical_mass_to_charge = character(0),
      spectra_ref = character(0),
      identification_method = character(0),
      ms_level = character(0),
      rank = character(0),
      "id_confidence_measure[1]" = character(0),
      "id_confidence_measure[2]" = character(0),
      "id_confidence_measure[3]" = character(0),
      "id_confidence_measure[4]" = character(0),
      feature_id = character(0)
    ))
  }

  ann <- results[has_annotation, , drop = FALSE]

  # Map feature_id -> SMF_ID
  id_lookup <- stats::setNames(
    as.character(smf_table$SMF_ID),
    smf_table$feature_id
  )

  # Compute theoretical m/z from neutral mass + adduct when possible.
  # Vectorized approach: process all rows at once
  exact_mass_raw <- suppressWarnings(as.numeric(.mztab_pluck(
    ann,
    "candidate_structure_exact_mass"
  )))
  adduct_raw <- .mztab_pluck(ann, "candidate_adduct")

  theo_mz <- mapply(
    function(m, ads) {
      if (!is.na(m) && m > 0 && !is.na(ads) && ads != "null" && nzchar(ads)) {
        val <- tryCatch(
          calculate_mz_from_mass(neutral_mass = m, adduct_string = ads),
          error = function(...) NA_real_ # mute: harmless calculation error
        )
        if (!is.na(val) && val > 0) {
          return(as.character(round(val, 6)))
        }
      }
      "null"
    },
    exact_mass_raw,
    adduct_raw,
    SIMPLIFY = TRUE,
    USE.NAMES = FALSE
  )

  # Determine ms_level from candidate_library
  lib_col <- .mztab_pluck(ann, "candidate_library")
  charge_from_adduct <- .mztab_charge_from_adduct(.mztab_pluck(
    ann,
    "candidate_adduct"
  ))
  ms_level_val <- tidytable::case_when(
    grepl("MS1|ms1|TIMA MS1", lib_col, ignore.case = TRUE) ~
      "[MS, MS:1000579, MS1 spectrum, ]",
    grepl(
      "MS2|ms2|spectral|gnps|mztab|sirius|mzmine|mzMine",
      lib_col,
      ignore.case = TRUE
    ) ~
      "[MS, MS:1000580, MS2 spectrum, ]",
    TRUE ~ "[MS, MS:1000580, MS2 spectrum, ]"
  )

  # Format spectra_ref as ms_run[1]:{spectrum_native_id}.
  # The mzTab-M spec requires this prefix so downstream tools can locate
  # spectra back to the declared ms_run[1] block.
  raw_spectra_ref <- .mztab_pluck_na(ann, "candidate_spectrum_id")
  spectra_ref_formatted <- ifelse(
    raw_spectra_ref == "null" | is.na(raw_spectra_ref),
    "null",
    paste0("ms_run[1]:", raw_spectra_ref)
  )

  # Resolve URIs from xrefs (prefer Wikidata, then ChEBI, then first available)
  inchikey_col <- .mztab_pluck_na(
    ann,
    "candidate_structure_inchikey_connectivity_layer"
  )
  uri_col <- .mztab_pluck_na(ann, "candidate_structure_uri")
  if (!is.null(xrefs_index) && length(xrefs_index) > 0L) {
    uri_from_xrefs <- vapply(
      X = inchikey_col,
      FUN = function(ik) {
        if (is.na(ik) || ik == "null" || !nzchar(ik)) {
          return("null")
        }
        rows <- xrefs_index[[ik]]
        if (is.null(rows) || nrow(rows) == 0L) {
          return("null")
        }
        .mztab_pick_best_uri(rows)
      },
      FUN.VALUE = character(1L)
    )
    # Override bare "null" from original uri with xref-resolved URIs
    uri_col <- ifelse(
      uri_col == "null" | is.na(uri_col),
      uri_from_xrefs,
      uri_col
    )
  }

  # Prefer prefixed database identifiers (e.g. CHEBI:12345) when xrefs are
  # available; otherwise fall back to the connectivity-layer InChIKey.
  database_identifier_col <- inchikey_col
  if (!is.null(xrefs_index) && length(xrefs_index) > 0L) {
    database_identifier_col <- vapply(
      X = inchikey_col,
      FUN = function(ik) {
        .mztab_pick_best_database_identifier(
          rows = xrefs_index[[ik]],
          fallback = ik
        )
      },
      FUN.VALUE = character(1L)
    )
  }

  sme <- tidytable::tidytable(
    SME_ID = as.character(seq_len(nrow(ann))),
    evidence_input_id = id_lookup[.mztab_pluck(ann, "feature_id")],
    database_identifier = database_identifier_col,
    chemical_formula = .mztab_pluck_na(
      ann,
      "candidate_structure_molecular_formula"
    ),
    smiles = .mztab_pluck_na(ann, "candidate_structure_smiles_no_stereo"),
    inchi = .mztab_pluck_na(ann, "candidate_structure_inchi"),
    chemical_name = .mztab_pluck_na(ann, "candidate_structure_name"),
    uri = uri_col,
    derivatized_form = "null",
    adduct_ion = .mztab_pluck_na(ann, "candidate_adduct"),
    exp_mass_to_charge = .mztab_pluck_na(ann, "feature_mz"),
    charge = charge_from_adduct,
    theoretical_mass_to_charge = theo_mz,
    spectra_ref = spectra_ref_formatted,
    identification_method = .mztab_library_to_identification_method(.mztab_pluck(
      ann,
      "candidate_library"
    )),
    ms_level = ms_level_val,
    rank = .mztab_pluck_na(ann, "rank_final"),
    "id_confidence_measure[1]" = .mztab_pluck_na(ann, "score_final"),
    "id_confidence_measure[2]" = .mztab_pluck_na(ann, "score_biological"),
    "id_confidence_measure[3]" = .mztab_pluck_na(ann, "score_chemical"),
    # [4] spectral similarity – TIMA user-namespace (TIMA:004).
    # No PSI-MS accession covers composite metabolomics similarity scores;
    # using the TIMA user-CV keeps the namespace consistent with measures 1-3.
    "id_confidence_measure[4]" = .mztab_pluck_na(
      ann,
      "candidate_score_similarity"
    ),
    feature_id = .mztab_pluck(ann, "feature_id")
  )

  # Drop [4] column when no spectral similarity data is present to keep the
  # metadata section in sync (declared only when the column is non-trivial).
  if (all(sme[["id_confidence_measure[4]"]] == "null", na.rm = TRUE)) {
    sme[["id_confidence_measure[4]"]] <- NULL
  }

  # Append opt_global_* columns for unmapped candidate-level metadata
  # Columns directly mapped to canonical mzTab SME fields or that belong
  # semantically to the SMF (feature) level rather than SME (evidence) level.
  # Feature-level columns are attached to the SMF section instead – they must
  # not appear as opt_global_* in SME to avoid redundancy and consumer confusion.
  consumed_cols <- c(
    # ── canonical SME mappings ───────────────────────────────────────────────
    "feature_id",
    "candidate_structure_molecular_formula",
    "candidate_structure_smiles_no_stereo",
    "candidate_structure_inchi",
    "candidate_structure_name",
    "candidate_structure_uri",
    "candidate_adduct",
    "feature_mz",
    "candidate_structure_exact_mass",
    "candidate_spectrum_id",
    "candidate_library",
    "rank_final",
    "score_final",
    "score_biological",
    "score_chemical",
    # [4] spectral similarity → id_confidence_measure[4]
    "candidate_score_similarity",
    # ── feature-level columns that belong in SMF, not SME ───────────────────
    # Matched by prefix so future feature_* columns are automatically excluded.
    grep("^feature_", colnames(ann), value = TRUE),
    # TIMA feature-level summary columns exported to SMF as opt_global_*.
    intersect(
      c("component_id", "candidates_evaluated", "candidates_best"),
      colnames(ann)
    )
  )
  # When xrefs drive canonical database_identifier, still preserve the original
  # TIMA InChIKey source column as opt_global_* for full reporting parity.
  if (is.null(xrefs_index) || length(xrefs_index) == 0L) {
    consumed_cols <- c(
      consumed_cols,
      "candidate_structure_inchikey_connectivity_layer"
    )
  }
  extra_cols <- setdiff(colnames(ann), consumed_cols)
  if (length(extra_cols) > 0L) {
    used_names <- colnames(sme)

    # Build all opt_global column names first (vectorized)
    opt_col_mapping <- lapply(extra_cols, function(col) {
      base_name <- .mztab_opt_colname(col)
      out_name <- base_name
      i <- 1L
      while (out_name %in% used_names) {
        i <- i + 1L
        out_name <- paste0(base_name, "_", i)
      }
      used_names <<- c(used_names, out_name)
      list(col = col, out_name = out_name)
    })

    # Vectorized extraction and assignment
    for (mapping in opt_col_mapping) {
      sme[[mapping$out_name]] <- .mztab_pluck_na(ann, mapping$col)
    }
  }

  sme
}

#' Build SML (Small Molecule Summary) table – one row per unique compound
#' @keywords internal
.mztab_build_sml <- function(
  results,
  smf_table,
  sme_table,
  xrefs_index = NULL
) {
  if (nrow(sme_table) == 0L) {
    # No annotations; return skeleton SML (one unannotated row per feature)
    return(tidytable::tidytable(
      SML_ID = as.character(seq_len(nrow(smf_table))),
      SMF_ID_REFS = as.character(smf_table$SMF_ID),
      SME_ID_REFS = "null",
      database_identifier = "null",
      chemical_formula = "null",
      smiles = "null",
      inchi = "null",
      chemical_name = "null",
      uri = "null",
      theoretical_neutral_mass = "null",
      adduct_ions = "null",
      reliability = "null",
      best_id_confidence_measure = "id_confidence_measure[1]",
      best_id_confidence_value = "null"
    ))
  }

  inchikey_col <- .mztab_pluck_na(
    results,
    "candidate_structure_inchikey_connectivity_layer"
  )
  unique_compounds <- unique(inchikey_col[inchikey_col != "null"])

  # Sparse-case fallback when no InChIKey exists: group on SME identity.
  if (length(unique_compounds) == 0L) {
    sme_src <- as.data.frame(
      sme_table,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    key <- ifelse(
      !is.na(sme_src$database_identifier) &
        nzchar(sme_src$database_identifier) &
        sme_src$database_identifier != "null",
      sme_src$database_identifier,
      paste(
        sme_src$chemical_name,
        sme_src$chemical_formula,
        sme_src$adduct_ion,
        sep = "||"
      )
    )

    rows <- lapply(seq_along(unique(key)), function(j) {
      k <- unique(key)[[j]]
      idx <- which(key == k)
      best_scores <- suppressWarnings(as.numeric(sme_src[[
        "id_confidence_measure[1]"
      ]][idx]))
      best_i <- idx[[1L]]
      if (length(best_scores) > 0L && any(is.finite(best_scores))) {
        best_i <- idx[[which.max(best_scores)]]
      }

      feat_ids <- unique(sme_src$feature_id[idx])
      feat_ids <- feat_ids[!is.na(feat_ids) & nzchar(feat_ids)]
      smf_id_refs <- paste(
        smf_table$SMF_ID[smf_table$feature_id %in% feat_ids],
        collapse = "|"
      )
      if (!nzchar(smf_id_refs)) {
        smf_id_refs <- "null"
      }

      sme_id_refs <- paste(unique(sme_src$SME_ID[idx]), collapse = "|")
      if (!nzchar(sme_id_refs)) {
        sme_id_refs <- "null"
      }

      best_score <- suppressWarnings(as.numeric(sme_src[[
        "id_confidence_measure[1]"
      ]][[best_i]]))
      score_val <- if (!is.na(best_score)) {
        as.character(round(best_score, 6))
      } else {
        "null"
      }

      tidytable::tidytable(
        SML_ID = as.character(j),
        SMF_ID_REFS = smf_id_refs,
        SME_ID_REFS = sme_id_refs,
        database_identifier = .mztab_pluck_na(
          sme_src[best_i, , drop = FALSE],
          "database_identifier"
        ),
        chemical_formula = .mztab_pluck_na(
          sme_src[best_i, , drop = FALSE],
          "chemical_formula"
        ),
        smiles = .mztab_pluck_na(sme_src[best_i, , drop = FALSE], "smiles"),
        inchi = .mztab_pluck_na(sme_src[best_i, , drop = FALSE], "inchi"),
        chemical_name = .mztab_pluck_na(
          sme_src[best_i, , drop = FALSE],
          "chemical_name"
        ),
        uri = .mztab_pluck_na(sme_src[best_i, , drop = FALSE], "uri"),
        theoretical_neutral_mass = "null",
        adduct_ions = .mztab_pluck_na(
          sme_src[best_i, , drop = FALSE],
          "adduct_ion"
        ),
        reliability = as.character(.mztab_score_to_reliability(best_score, "")),
        best_id_confidence_measure = "id_confidence_measure[1]",
        best_id_confidence_value = score_val
      )
    })

    return(tidytable::bind_rows(rows))
  }

  score_col <- suppressWarnings(as.numeric(.mztab_pluck_na(
    results,
    "score_final"
  )))

  sml_rows <- lapply(seq_along(unique_compounds), function(j) {
    ckey <- unique_compounds[j]
    idx <- which(inchikey_col == ckey)

    best_score <- suppressWarnings(max(score_col[idx], na.rm = TRUE))
    if (!is.finite(best_score)) {
      best_score <- NA_real_
    }
    best_i <- idx[which.max(score_col[idx])]
    if (length(best_i) == 0L) {
      best_i <- idx[1L]
    }

    row <- results[best_i, , drop = FALSE]

    feat_ids <- unique(results$feature_id[idx])
    feat_ids <- feat_ids[!is.na(feat_ids)]
    smf_id_refs <- paste(
      smf_table$SMF_ID[smf_table$feature_id %in% feat_ids],
      collapse = "|"
    )
    if (!nzchar(smf_id_refs)) {
      smf_id_refs <- "null"
    }

    sme_id_refs_all <- sme_table$SME_ID[sme_table$feature_id %in% feat_ids]
    sme_id_refs_all <- sme_id_refs_all[!is.na(sme_id_refs_all)]
    sme_id_refs <- if (length(sme_id_refs_all) > 0L) {
      paste(unique(sme_id_refs_all), collapse = "|")
    } else {
      "null"
    }

    score_val <- if (!is.na(best_score)) {
      as.character(round(best_score, 6))
    } else {
      "null"
    }
    reliability <- .mztab_score_to_reliability(
      score = best_score,
      library = .mztab_pluck_na(row, "candidate_library")
    )

    tidytable::tidytable(
      SML_ID = as.character(j),
      SMF_ID_REFS = smf_id_refs,
      SME_ID_REFS = sme_id_refs,
      database_identifier = if (
        !is.null(xrefs_index) && length(xrefs_index) > 0L
      ) {
        .mztab_pick_best_database_identifier(
          rows = xrefs_index[[ckey]],
          fallback = ckey
        )
      } else {
        ckey
      },
      chemical_formula = .mztab_pluck_na(
        row,
        "candidate_structure_molecular_formula"
      ),
      smiles = .mztab_pluck_na(row, "candidate_structure_smiles_no_stereo"),
      inchi = .mztab_pluck_na(row, "candidate_structure_inchi"),
      chemical_name = .mztab_pluck_na(row, "candidate_structure_name"),
      uri = {
        # Prefer xrefs-resolved URI over the raw candidate_structure_uri field.
        raw_uri <- .mztab_pluck_na(row, "candidate_structure_uri")
        if (!is.null(xrefs_index) && length(xrefs_index) > 0L) {
          xref_rows <- xrefs_index[[ckey]]
          if (!is.null(xref_rows) && nrow(xref_rows) > 0L) {
            xref_uri <- .mztab_pick_best_uri(xref_rows)
            ifelse(raw_uri == "null" | is.na(raw_uri), xref_uri, raw_uri)
          } else {
            raw_uri
          }
        } else {
          raw_uri
        }
      },
      theoretical_neutral_mass = {
        em <- suppressWarnings(as.numeric(.mztab_pluck_na(
          row,
          "candidate_structure_exact_mass"
        )))
        if (length(em) == 1L && !is.na(em) && is.finite(em)) {
          as.character(round(em, 6))
        } else {
          "null"
        }
      },
      adduct_ions = .mztab_pluck_na(row, "candidate_adduct"),
      reliability = as.character(reliability),
      best_id_confidence_measure = "id_confidence_measure[1]",
      best_id_confidence_value = score_val
    )
  })

  tidytable::bind_rows(sml_rows)
}

#' Read base mzTab while preserving unmanaged lines
#'
#' Collects passthrough content as every non-blank line that does not belong to
#' the managed TIMA sections (`MTD`, `SMH/SML`, `SFH/SMF`, `SEH/SME`). This
#' includes custom extension blocks and comment rows from anywhere in the file.
#' Passthrough lines are emitted before the TIMA-managed metadata section.
#'
#' @keywords internal
.mztab_read_base <- function(path) {
  if (.mztab_is_json_path(path)) {
    tabs <- read_mztab_tables(path)
    return(list(
      metadata = tabs$metadata,
      sml = tabs$sml,
      smf = tabs$smf,
      sme = tabs$sme,
      passthrough = character(0)
    ))
  }

  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  if (length(lines) > 0L) {
    lines[[1L]] <- sub("^\ufeff", "", lines[[1L]])
  }

  lines_norm <- .normalize_mztab_prefix_sep(lines)
  managed <- grepl("^(MTD|SMH|SML|SFH|SMF|SEH|SME)(\\t|$)", lines_norm)

  # Preserve all non-managed non-blank lines in original order, regardless of
  # whether they occur before or after managed sections.
  passthrough_idx <- which(!managed & nzchar(lines))

  list(
    metadata = .parse_mztab_metadata(lines_norm),
    sml = .parse_mztab_section_table(lines_norm, "SMH", "SML"),
    smf = .parse_mztab_section_table(lines_norm, "SFH", "SMF"),
    sme = .parse_mztab_section_table(lines_norm, "SEH", "SME"),
    passthrough = lines[passthrough_idx]
  )
}

#' Merge TIMA-derived tables into a base mzTab
#' @keywords internal
.mztab_merge_tables <- function(base, sml_new, smf_new, sme_new) {
  smf_base <- tidytable::as_tidytable(base$smf)
  sme_base <- tidytable::as_tidytable(base$sme)
  sml_base <- tidytable::as_tidytable(base$sml)

  # Ensure canonical columns exist in base tables before merging.
  smf_base <- .mztab_ensure_cols(
    smf_base,
    c(
      "SMF_ID",
      "SML_ID_REFS",
      "SME_ID_REFS",
      "SME_ID_REF_ambiguity_code",
      "exp_mass_to_charge",
      "charge",
      "retention_time_in_seconds"
    )
  )
  sme_base <- .mztab_ensure_cols(
    sme_base,
    c(
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
      "id_confidence_measure[1]",
      "id_confidence_measure[2]",
      "id_confidence_measure[3]",
      "id_confidence_measure[4]"
    )
  )
  sml_base <- .mztab_ensure_cols(
    sml_base,
    c(
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
    )
  )

  smf_new <- .mztab_ensure_cols(smf_new, colnames(smf_base))
  sme_new <- .mztab_ensure_cols(sme_new, colnames(sme_base))
  sml_new <- .mztab_ensure_cols(sml_new, colnames(sml_base))

  map_smf <- .mztab_merge_smf_rows(smf_base, smf_new)
  smf_all <- map_smf$smf
  smf_id_map <- map_smf$id_map

  map_sme <- .mztab_merge_sme_rows(sme_base, sme_new, smf_id_map)
  sme_all <- map_sme$sme
  sme_id_map <- map_sme$id_map

  map_sml <- .mztab_merge_sml_rows(sml_base, sml_new, smf_id_map, sme_id_map)
  sml_all <- map_sml$sml

  # Back-fill SMF -> SML references from final SML table.
  smf_all <- .mztab_refresh_smf_sml_refs(smf_all, sml_all)

  list(
    sml = sml_all,
    smf = smf_all,
    sme = sme_all
  )
}

#' Ensure columns exist and are character
#' @keywords internal
.mztab_ensure_cols <- function(tbl, cols) {
  tbl <- as.data.frame(tbl, stringsAsFactors = FALSE, check.names = FALSE)
  if (nrow(tbl) == 0L && ncol(tbl) == 0L) {
    tbl <- data.frame(stringsAsFactors = FALSE, check.names = FALSE)
  }
  for (col in cols) {
    if (!col %in% colnames(tbl)) {
      tbl[[col]] <- character(nrow(tbl))
    }
  }
  # Keep optional/unknown columns to avoid information loss in merge mode.
  ordered_cols <- c(cols, setdiff(colnames(tbl), cols))
  tbl <- tbl[, ordered_cols, drop = FALSE]
  tbl[] <- lapply(tbl, function(x) {
    x <- as.character(x)
    x[is.na(x) | !nzchar(x)] <- "null"
    x
  })
  tidytable::as_tidytable(tbl)
}

#' Build stable key for SMF deduplication
#' @keywords internal
.mztab_smf_key <- function(tbl) {
  tbl_df <- as.data.frame(tbl, stringsAsFactors = FALSE, check.names = FALSE)

  opt_cols <- grep("^opt_", colnames(tbl_df), value = TRUE)

  # Vectorized opt signature generation
  opt_sig <- if (length(opt_cols) > 0L) {
    # Combine all opt columns into a matrix for faster processing
    opt_matrix <- as.matrix(tbl_df[, opt_cols, drop = FALSE])
    apply(opt_matrix, 1L, function(x) {
      paste(ifelse(is.na(x) | !nzchar(x), "null", x), collapse = "|")
    })
  } else {
    rep("null", nrow(tbl_df))
  }

  paste(
    tbl_df$exp_mass_to_charge,
    tbl_df$retention_time_in_seconds,
    tbl_df$charge,
    opt_sig,
    sep = "||"
  )
}

#' Build stable key for SME deduplication
#' @keywords internal
.mztab_sme_key <- function(tbl) {
  paste(
    tbl$database_identifier,
    tbl$evidence_input_id,
    tbl$adduct_ion,
    tbl$rank,
    tbl$exp_mass_to_charge,
    tbl$charge,
    tbl$theoretical_mass_to_charge,
    tbl$identification_method,
    tbl$ms_level,
    tbl$chemical_formula,
    tbl$inchi,
    sep = "||"
  )
}

#' Build stable key for SML deduplication
#' @keywords internal
.mztab_sml_key <- function(tbl) {
  primary <- tbl$database_identifier
  fallback <- paste(
    tbl$chemical_name,
    tbl$chemical_formula,
    tbl$adduct_ions,
    sep = "||"
  )
  ifelse(primary == "null" | !nzchar(primary), fallback, primary)
}

#' Merge SMF rows and return id map from new IDs to final IDs
#' @keywords internal
.mztab_merge_smf_rows <- function(smf_base, smf_new) {
  smf_all <- smf_base
  base_keys <- if (nrow(smf_all) > 0L) .mztab_smf_key(smf_all) else character(0)
  id_map <- stats::setNames(character(0), character(0))
  next_id <- .mztab_next_numeric_id(smf_all$SMF_ID)

  # Process all new rows and collect new entries
  new_keys <- .mztab_smf_key(smf_new)
  match_idx <- match(new_keys, base_keys)

  # Map duplicates and collect new rows to add
  new_rows_to_add <- list()
  id_offset <- 0L

  for (i in seq_len(nrow(smf_new))) {
    old_id <- smf_new$SMF_ID[[i]]

    if (!is.na(match_idx[[i]])) {
      # Duplicate found in base
      id_map[[old_id]] <- smf_all$SMF_ID[[match_idx[[i]]]]
    } else {
      # New entry
      row <- smf_new[i, , drop = FALSE]
      new_id <- as.character(next_id + id_offset)
      row$SMF_ID <- new_id
      row$SML_ID_REFS <- "null"
      new_rows_to_add[[length(new_rows_to_add) + 1L]] <- row
      id_map[[old_id]] <- new_id
      id_offset <- id_offset + 1L
    }
  }

  # Batch append all new rows
  if (length(new_rows_to_add) > 0L) {
    smf_all <- tidytable::bind_rows(
      smf_all,
      tidytable::bind_rows(new_rows_to_add)
    )
  }

  list(smf = smf_all, id_map = id_map)
}

#' Merge SME rows and return id map from new IDs to final IDs
#' @keywords internal
.mztab_merge_sme_rows <- function(sme_base, sme_new, smf_id_map) {
  sme_all <- sme_base
  sme_new <- sme_new |>
    tidytable::mutate(
      evidence_input_id = .mztab_remap_ref_ids(evidence_input_id, smf_id_map)
    )

  base_keys <- if (nrow(sme_all) > 0L) .mztab_sme_key(sme_all) else character(0)
  id_map <- stats::setNames(character(0), character(0))
  next_id <- .mztab_next_numeric_id(sme_all$SME_ID)

  # Process all new rows and collect new entries
  new_keys <- .mztab_sme_key(sme_new)
  match_idx <- match(new_keys, base_keys)

  # Map duplicates and collect new rows to add
  new_rows_to_add <- list()
  id_offset <- 0L

  for (i in seq_len(nrow(sme_new))) {
    old_id <- sme_new$SME_ID[[i]]

    if (!is.na(match_idx[[i]])) {
      # Duplicate found in base
      id_map[[old_id]] <- sme_all$SME_ID[[match_idx[[i]]]]
    } else {
      # New entry
      row <- sme_new[i, , drop = FALSE]
      new_id <- as.character(next_id + id_offset)
      row$SME_ID <- new_id
      new_rows_to_add[[length(new_rows_to_add) + 1L]] <- row
      id_map[[old_id]] <- new_id
      id_offset <- id_offset + 1L
    }
  }

  # Batch append all new rows
  if (length(new_rows_to_add) > 0L) {
    sme_all <- tidytable::bind_rows(
      sme_all,
      tidytable::bind_rows(new_rows_to_add)
    )
  }

  list(sme = sme_all, id_map = id_map)
}

#' Merge SML rows and enrich references for duplicates
#' @keywords internal
.mztab_merge_sml_rows <- function(sml_base, sml_new, smf_id_map, sme_id_map) {
  sml_all <- sml_base
  sml_new <- sml_new |>
    tidytable::mutate(
      SMF_ID_REFS = .mztab_remap_ref_ids(SMF_ID_REFS, smf_id_map),
      SME_ID_REFS = .mztab_remap_ref_ids(SME_ID_REFS, sme_id_map)
    )

  base_keys <- if (nrow(sml_all) > 0L) .mztab_sml_key(sml_all) else character(0)
  next_id <- .mztab_next_numeric_id(sml_all$SML_ID)

  # Pre-compute all keys for new rows
  new_keys <- .mztab_sml_key(sml_new)
  match_idx <- match(new_keys, base_keys)

  # Separate duplicates and new entries
  new_rows_to_add <- list()
  id_offset <- 0L

  for (i in seq_len(nrow(sml_new))) {
    idx <- match_idx[[i]]

    if (!is.na(idx)) {
      # Duplicate found - merge references
      sml_all$SMF_ID_REFS[[idx]] <- .mztab_union_ref_ids(
        sml_all$SMF_ID_REFS[[idx]],
        sml_new$SMF_ID_REFS[[i]]
      )
      sml_all$SME_ID_REFS[[idx]] <- .mztab_union_ref_ids(
        sml_all$SME_ID_REFS[[idx]],
        sml_new$SME_ID_REFS[[i]]
      )
      sml_all$adduct_ions[[idx]] <- .mztab_union_ref_ids(
        sml_all$adduct_ions[[idx]],
        sml_new$adduct_ions[[i]]
      )
    } else {
      # New entry
      row <- sml_new[i, , drop = FALSE]
      row$SML_ID <- as.character(next_id + id_offset)
      new_rows_to_add[[length(new_rows_to_add) + 1L]] <- row
      id_offset <- id_offset + 1L
    }
  }

  # Batch append all new rows
  if (length(new_rows_to_add) > 0L) {
    sml_all <- tidytable::bind_rows(
      sml_all,
      tidytable::bind_rows(new_rows_to_add)
    )
  }

  list(sml = sml_all)
}

#' Refresh SMF SML_ID_REFS from SML definitions
#' @keywords internal
.mztab_refresh_smf_sml_refs <- function(smf, sml) {
  if (nrow(smf) == 0L || nrow(sml) == 0L) {
    return(smf)
  }

  # Initialize all references as "null"
  refs <- stats::setNames(rep("null", nrow(smf)), smf$SMF_ID)

  # Vectorized approach: process all SML rows to accumulate references
  for (i in seq_len(nrow(sml))) {
    sml_id <- sml$SML_ID[[i]]
    smf_ids <- .mztab_split_ref_ids(sml$SMF_ID_REFS[[i]])

    # Update all matching SMF_IDs vectorized
    if (length(smf_ids) > 0L) {
      for (sid in smf_ids) {
        if (sid %in% names(refs)) {
          refs[[sid]] <- .mztab_union_ref_ids(refs[[sid]], sml_id)
        }
      }
    }
  }

  smf$SML_ID_REFS <- unname(refs[smf$SMF_ID])
  smf
}

#' Refresh SMF SME_ID_REFS and ambiguity code from SME definitions
#' @keywords internal
.mztab_refresh_smf_sme_refs <- function(smf, sme) {
  if (nrow(smf) == 0L) {
    return(smf)
  }
  if (nrow(sme) == 0L) {
    smf$SME_ID_REFS <- rep("null", nrow(smf))
    smf$SME_ID_REF_ambiguity_code <- rep("null", nrow(smf))
    return(smf)
  }

  if (!"SME_ID_REFS" %in% colnames(smf)) {
    smf$SME_ID_REFS <- "null"
  }
  if (!"SME_ID_REF_ambiguity_code" %in% colnames(smf)) {
    smf$SME_ID_REF_ambiguity_code <- "null"
  }

  smf_df <- as.data.frame(smf, stringsAsFactors = FALSE, check.names = FALSE)
  sme_df <- as.data.frame(sme, stringsAsFactors = FALSE, check.names = FALSE)

  smf_df$SME_ID_REFS <- "null"
  smf_df$SME_ID_REF_ambiguity_code <- "null"

  for (i in seq_len(nrow(smf_df))) {
    fid <- smf_df$feature_id[[i]]
    if (length(fid) > 1L) {
      fid <- fid[[1L]]
    }
    if (length(fid) == 0L) {
      next
    }
    fid <- as.character(fid)
    if (isTRUE(is.na(fid)) || !nzchar(fid)) {
      next
    }
    idx <- which(sme_df$feature_id == fid)
    if (length(idx) == 0L) {
      next
    }

    refs <- unique(as.character(sme_df$SME_ID[idx]))
    refs <- refs[!is.na(refs) & nzchar(refs) & refs != "null"]
    if (length(refs) == 0L) {
      next
    }

    smf_df$SME_ID_REFS[[i]] <- paste(refs, collapse = "|")

    if (length(refs) <= 1L) {
      smf_df$SME_ID_REF_ambiguity_code[[i]] <- "null"
      next
    }

    db_vals <- unique(as.character(sme_df$database_identifier[idx]))
    db_vals <- db_vals[!is.na(db_vals) & nzchar(db_vals) & db_vals != "null"]
    method_vals <- unique(as.character(sme_df$identification_method[idx]))
    method_vals <- method_vals[
      !is.na(method_vals) & nzchar(method_vals) & method_vals != "null"
    ]

    n_db <- length(db_vals)
    n_method <- length(method_vals)
    ambiguity_code <- if (n_db > 1L && n_method > 1L) {
      "3"
    } else if (n_db > 1L) {
      "1"
    } else if (n_method > 1L) {
      "2"
    } else {
      "1"
    }
    smf_df$SME_ID_REF_ambiguity_code[[i]] <- ambiguity_code
  }

  tidytable::as_tidytable(smf_df)
}

#' Split reference list encoded with pipe separator
#' @keywords internal
.mztab_split_ref_ids <- function(x) {
  if (is.na(x) || !nzchar(x) || x == "null") {
    return(character(0))
  }
  vals <- strsplit(x, "|", fixed = TRUE)[[1L]]
  vals <- vals[nzchar(vals) & vals != "null"]
  unique(vals)
}

#' Union two pipe-separated reference lists
#' @keywords internal
.mztab_union_ref_ids <- function(a, b) {
  ids <- unique(c(.mztab_split_ref_ids(a), .mztab_split_ref_ids(b)))
  if (length(ids) == 0L) {
    "null"
  } else {
    paste(ids, collapse = "|")
  }
}

#' Remap pipe-separated references according to id map
#' @keywords internal
.mztab_remap_ref_ids <- function(x, id_map) {
  vapply(
    X = x,
    FUN = function(one) {
      ids <- .mztab_split_ref_ids(one)
      if (length(ids) == 0L) {
        return("null")
      }
      mapped <- vapply(
        X = ids,
        FUN = function(id) {
          if (!is.null(id_map[[id]]) && nzchar(id_map[[id]])) {
            id_map[[id]]
          } else {
            id
          }
        },
        FUN.VALUE = character(1L)
      )
      .mztab_union_ref_ids("null", paste(mapped, collapse = "|"))
    },
    FUN.VALUE = character(1L)
  )
}

#' Find next numeric ID from an ID column
#' @keywords internal
.mztab_next_numeric_id <- function(ids) {
  nums <- suppressWarnings(as.integer(ids))
  nums <- nums[!is.na(nums)]
  if (length(nums) == 0L) 1L else max(nums) + 1L
}

#' Convert a TIMA score to MSI reliability level (1-4)
#'
#' The mapping is intentionally conservative to avoid overstating confidence:
#' * 1 – confirmed by spectral library match AND score >= 0.7
#' * 2 – probable structure; spectral evidence or score >= 0.5
#' * 3 – putative annotation; score >= 0.2
#' * 4 – unambiguous compound class only
#' @keywords internal
.mztab_score_to_reliability <- function(score, library) {
  is_spectral <- grepl(
    "spectral|gnps|sirius|mzmine|mztab",
    library,
    ignore.case = TRUE
  )
  score <- suppressWarnings(as.numeric(score))
  tidytable::case_when(
    is_spectral & !is.na(score) & score >= 0.7 ~ 1L,
    is_spectral | (!is.na(score) & score >= 0.5) ~ 2L,
    !is.na(score) & score >= 0.2 ~ 3L,
    TRUE ~ 4L
  )
}

#' Build mzTab-M metadata table
#'
#' Returns a complete, ontology-aligned metadata table. Key design decisions:
#'
#' * All `id_confidence_measure[n]` entries use the `TIMA` user-CV namespace
#'   (`TIMA:001`–`TIMA:004`).  No PSI-MS accession exists for composite
#'   metabolomics scores.
#' * `ms_level` values in the SME section use `MS:1000579` / `MS:1000580`.
#' * `ms_run[1]-scan_polarity` is populated with `MS:1000130` (positive) or
#'   `MS:1000129` (negative) when `polarity` is known.
#' * When `xrefs_index` contains data, additional `database[n]` blocks are
#'   emitted (n ≥ 2) for every unique xref prefix found in the annotation set.
#' * The TIMA repository URL is registered as `software[1]-setting[1]` for
#'   machine-readable software provenance.
#' * `colunit-small_molecule_feature` declares seconds (UO:0000010).
#' * `colunit-small_molecule` declares daltons (UO:0000221) for the neutral
#'   mass column.
#' * `colunit-small_molecule_evidence` documents each `id_confidence_measure`
#'   column with its TIMA CV Param.
#'
#' @keywords internal
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
  # The connectivity-layer InChIKey used as database_identifier has no
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

#' Build COM provenance lines for non-classical TIMA columns
#' @keywords internal
.mztab_build_export_comments <- function(results, chunk_size = 16L) {
  if (!is.data.frame(results) || ncol(results) == 0L) {
    return(character(0))
  }

  src_cols <- colnames(results)
  canonical_src <- c(
    "feature_id",
    "feature_mz",
    "feature_rt",
    "candidate_structure_inchikey_connectivity_layer",
    "candidate_structure_molecular_formula",
    "candidate_structure_smiles_no_stereo",
    "candidate_structure_inchi",
    "candidate_structure_name",
    "candidate_structure_uri",
    "candidate_adduct",
    "candidate_structure_exact_mass",
    "candidate_spectrum_id",
    "candidate_library",
    "rank_final",
    "score_final",
    "score_biological",
    "score_chemical",
    "candidate_score_similarity"
  )

  non_classical <- setdiff(src_cols, canonical_src)
  if (length(non_classical) == 0L) {
    return(character(0))
  }

  chunks <- split(
    non_classical,
    ceiling(seq_along(non_classical) / max(1L, as.integer(chunk_size)))
  )

  c(
    "COM\tTIMA export: non-classical input columns are preserved as opt_global_* fields in SMF/SME",
    vapply(
      seq_along(chunks),
      function(i) {
        paste0(
          "COM\tTIMA opt columns[",
          i,
          "]\t",
          paste(chunks[[i]], collapse = "|")
        )
      },
      FUN.VALUE = character(1L)
    )
  )
}

#' Build COM lines embedding edges into mzTab text exports
#' @keywords internal
.mztab_build_edges_comments <- function(edges_file, chunk_size = 500L) {
  if (is.null(edges_file) || !is.character(edges_file) || !nzchar(edges_file)) {
    return(character(0))
  }

  edges <- safe_fread(
    file = edges_file,
    file_type = "edge table",
    na.strings = c("", "NA", "null", "NULL"),
    colClasses = "character"
  )
  if (nrow(edges) == 0L) {
    return(c("COM\tTIMA edges\tedges_file present but empty"))
  }

  df <- as.data.frame(edges, stringsAsFactors = FALSE, check.names = FALSE)
  cols <- colnames(df)

  header <- paste0(
    "COM\tTIMA edges\trows=",
    nrow(df),
    "; cols=",
    paste(cols, collapse = "|")
  )

  # Serialize rows as compact JSON objects for robust parsing of arbitrary values.
  row_lines <- vapply(
    seq_len(nrow(df)),
    function(i) {
      payload <- jsonlite::toJSON(
        as.list(df[i, , drop = FALSE]),
        auto_unbox = TRUE,
        null = "null",
        na = "null"
      )
      paste0("COM\tTIMA edge\t", as.character(payload))
    },
    FUN.VALUE = character(1L)
  )

  if (length(row_lines) > as.integer(chunk_size)) {
    log_warn(
      "Embedding %s edges into mzTab COM lines from %s",
      nrow(df),
      basename(edges_file)
    )
  }

  c(header, row_lines)
}

#' @keywords internal
.mztab_is_json_path <- function(path) {
  is.character(path) &&
    length(path) == 1L &&
    grepl("\\.json$", path, ignore.case = TRUE)
}

#' @keywords internal
.mztab_json_cell <- function(x) {
  x <- as.character(x)
  if (length(x) == 0L || is.na(x) || !nzchar(x) || identical(x, "null")) {
    return(NA_character_)
  }
  x
}

#' @keywords internal
.mztab_metadata_to_json <- function(meta) {
  if (nrow(meta) == 0L) {
    return(list())
  }

  keys <- as.character(meta$key)
  vals <- lapply(meta$value, .mztab_json_cell)
  stats::setNames(vals, keys)
}

#' @keywords internal
.mztab_table_to_json_rows <- function(tbl, drop_feature_id = FALSE) {
  if (nrow(tbl) == 0L) {
    return(list())
  }

  df <- as.data.frame(tbl, stringsAsFactors = FALSE, check.names = FALSE)
  if (drop_feature_id && "feature_id" %in% colnames(df)) {
    df <- df[, setdiff(colnames(df), "feature_id"), drop = FALSE]
  }

  lapply(seq_len(nrow(df)), function(i) {
    row <- lapply(df[i, , drop = FALSE], function(v) .mztab_json_cell(v[[1L]]))
    names(row) <- colnames(df)
    row
  })
}

#' @keywords internal
.mztab_write_json <- function(
  file,
  metadata,
  sml_table,
  smf_table,
  sme_table,
  edges_file = NULL
) {
  sml_rows <- .mztab_table_to_json_rows(sml_table)
  smf_rows <- .mztab_table_to_json_rows(smf_table, drop_feature_id = TRUE)
  sme_rows <- .mztab_table_to_json_rows(sme_table, drop_feature_id = TRUE)
  edges_rows <- .mztab_edges_json_rows(edges_file)

  payload <- list(
    metadata = .mztab_metadata_to_json(metadata),
    # Keep snake_case keys for TIMA parser compatibility.
    small_molecule_summary = sml_rows,
    small_molecule_feature = smf_rows,
    small_molecule_evidence = sme_rows,
    # Add camelCase aliases used by rmzTabM JSON examples.
    smallMoleculeSummary = sml_rows,
    smallMoleculeFeature = smf_rows,
    smallMoleculeEvidence = sme_rows
  )

  if (length(edges_rows) > 0L) {
    payload$tima_edges <- edges_rows
  }

  create_dir(file)
  jsonlite::write_json(
    x = payload,
    path = file,
    pretty = TRUE,
    auto_unbox = TRUE,
    na = "null",
    null = "null"
  )

  invisible(file)
}

#' Build JSON edge rows for mzTab JSON export
#' @keywords internal
.mztab_edges_json_rows <- function(edges_file) {
  if (is.null(edges_file) || !is.character(edges_file) || !nzchar(edges_file)) {
    return(list())
  }

  edges <- safe_fread(
    file = edges_file,
    file_type = "edge table",
    na.strings = c("", "NA", "null", "NULL"),
    colClasses = "character"
  )
  if (nrow(edges) == 0L) {
    return(list())
  }

  .mztab_table_to_json_rows(edges)
}

#' Map a TIMA candidate_library string to an mzTab-M Param for identification_method.
#'
#' The mzTab-M spec requires `identification_method` to be a CV Param
#' `[cv_label, accession, name, value]`.  TIMA evidence items originate from
#' different upstream sources; this function maps known patterns to the most
#' appropriate Param.
#'
#' Mapping rationale (PSI-MS accessions where registered; user params otherwise):
#' * **SIRIUS** – in-silico fragmentation tree by SIRIUS/CSI:FingerID.
#'   No single PSI-MS accession covers the full SIRIUS pipeline, so a user
#'   param is used.  `software[n]` should be declared separately.
#' * **GNPS** – spectral library matching on the GNPS platform.
#' * **Spectral / mzTab / MZmine / MS2** – generic spectral library matching.
#' * **MS1 / exact-mass** – mass-based database look-up (no fragmentation).
#' * **Already-formatted Param** – passed through unchanged.
#' * **Anything else** – wrapped as a user parameter carrying the raw library
#'   name.
#'
#' @param lib Character vector of `candidate_library` values.
#' @return Character vector of mzTab Param strings.
#' @keywords internal
.mztab_library_to_identification_method <- function(lib) {
  tidytable::case_when(
    # Pass through values already in Param format.
    grepl("^\\[.*,.*,.*,.*\\]$", trimws(lib)) ~ trimws(lib),

    # SIRIUS – in-silico molecular-formula / fragmentation scoring.
    # No single PSI-MS accession covers the full SIRIUS pipeline.
    grepl("sirius", lib, ignore.case = TRUE) ~
      "[, , SIRIUS in-silico structure elucidation, ]",

    # GNPS spectral networking / library.
    grepl("gnps", lib, ignore.case = TRUE) ~
      "[, , GNPS spectral library matching, ]",

    # MZmine or other spectral-library tools embedded via mzTab.
    grepl("mzmine|mztab", lib, ignore.case = TRUE) ~
      "[, , spectral library matching, ]",

    # Generic MS/MS spectral library.
    grepl("spectral|library|ms2|msms", lib, ignore.case = TRUE) ~
      "[, , spectral library matching, ]",

    # MS1 exact-mass database search (no fragmentation evidence).
    grepl("ms1|exact.mass", lib, ignore.case = TRUE) ~
      "[, , exact mass database search, ]",

    # Unknown / other: embed raw library name as a user parameter.
    is.na(lib) | !nzchar(lib) ~ "[, , unknown identification method, ]",
    TRUE ~ paste0("[, , ", trimws(lib), ", ]")
  )
}

#' Escape tab characters in free-text fields
#' @keywords internal
.mztab_escape <- function(x) {
  gsub("\t", " ", x, fixed = TRUE)
}

#' Derive charge values from adduct notation
#' @keywords internal
.mztab_charge_from_adduct <- function(adduct) {
  vapply(
    X = as.character(adduct),
    FUN = function(a) {
      if (is.na(a) || !nzchar(a) || a == "null") {
        return("null")
      }
      m <- regexec("([0-9]*)([+-])$", a, perl = TRUE)
      reg <- regmatches(a, m)[[1L]]
      if (length(reg) < 3L) {
        return("null")
      }
      mag <- if (nzchar(reg[[2L]])) as.integer(reg[[2L]]) else 1L
      if (is.na(mag)) {
        mag <- 1L
      }
      sign <- if (identical(reg[[3L]], "-")) -1L else 1L
      as.character(sign * mag)
    },
    FUN.VALUE = character(1L)
  )
}

# ── xrefs helpers ─────────────────────────────────────────────────────────────

# Known prefix → (human-readable name, URL template) lookup.
# Sorted by priority: Wikidata > ChEBI > PubChem > HMDB > ChEMBL > others.
.XREF_KNOWN_PREFIXES <- list(
  wikidata = list(
    name = "Wikidata",
    uri = "https://www.wikidata.org/wiki/",
    prefix = "wd"
  ),
  chebi = list(
    name = "Chemical Entities of Biological Interest (ChEBI)",
    uri = "https://www.ebi.ac.uk/chebi/searchId.do?chebiId=CHEBI:",
    prefix = "CHEBI"
  ),
  "pubchem.compound" = list(
    name = "PubChem Compound",
    uri = "https://pubchem.ncbi.nlm.nih.gov/compound/",
    prefix = "CID"
  ),
  hmdb = list(
    name = "Human Metabolome Database (HMDB)",
    uri = "https://hmdb.ca/metabolites/",
    prefix = "HMDB"
  ),
  "chembl.compound" = list(
    name = "ChEMBL",
    uri = "https://www.ebi.ac.uk/chembl/compound_report_card/",
    prefix = "CHEMBL"
  ),
  kegg = list(
    name = "KEGG Compound",
    uri = "https://www.genome.jp/entry/",
    prefix = "C"
  ),
  cas = list(
    name = "CAS Registry",
    uri = "https://commonchemistry.cas.org/detail?cas_rn=",
    prefix = "CAS"
  ),
  lipidmaps = list(
    name = "LIPID MAPS",
    uri = "https://www.lipidmaps.org/databases/lmsd/",
    prefix = "LM"
  ),
  "knapsack" = list(
    name = "KNApSAcK",
    uri = "http://www.knapsackfamily.com/knapsack_core/information.php?word=",
    prefix = "C"
  )
)

#' Build a named list (InChIKey → data.frame) for fast xrefs look-up.
#'
#' Loads the cross-references file from [get_compounds_xrefs()] and indexes it
#' by InChIKey connectivity layer so per-compound look-up is O(1).
#'
#' @param xrefs_file Character file path, or `NULL`.
#' @return Named list of data.frames, or `NULL` when no valid file is provided.
#' @keywords internal
.mztab_build_xrefs_index <- function(xrefs_file) {
  if (
    is.null(xrefs_file) ||
      !nzchar(xrefs_file) ||
      xrefs_file == "null" ||
      !file.exists(xrefs_file)
  ) {
    return(NULL)
  }

  xrefs <- tryCatch(
    safe_fread(xrefs_file),
    error = function(e) {
      log_warn(
        "Failed to load xrefs file '%s': %s",
        basename(xrefs_file),
        conditionMessage(e)
      )
      NULL
    }
  )

  if (is.null(xrefs) || nrow(xrefs) == 0L) {
    return(NULL)
  }

  required_cols <- c("inchikey", "prefix", "id")
  if (!all(required_cols %in% colnames(xrefs))) {
    log_warn(
      "xrefs file missing required columns (%s); skipping xref enrichment",
      paste(setdiff(required_cols, colnames(xrefs)), collapse = ", ")
    )
    return(NULL)
  }

  xrefs_df <- as.data.frame(
    xrefs,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  xrefs_df$inchikey <- as.character(xrefs_df$inchikey)
  xrefs_df$prefix <- as.character(xrefs_df$prefix)
  xrefs_df$id <- as.character(xrefs_df$id)

  # Index by InChIKey connectivity layer (first 14 characters).
  xrefs_df$ik14 <- substr(xrefs_df$inchikey, 1L, 14L)
  split(xrefs_df, xrefs_df$ik14)
}

#' Select the "best" URI from a set of xref rows for a single compound.
#'
#' Priority: Wikidata > ChEBI > PubChem > HMDB > ChEMBL > first available.
#'
#' @param rows data.frame with columns `prefix`, `id`.
#' @return Single character URI string, or `"null"`.
#' @keywords internal
.mztab_pick_best_uri <- function(rows) {
  priority_prefixes <- c(
    "wikidata",
    "chebi",
    "pubchem.compound",
    "hmdb",
    "chembl.compound",
    "kegg",
    "cas",
    "lipidmaps",
    "knapsack"
  )

  for (pfx in priority_prefixes) {
    match_rows <- rows[rows$prefix == pfx, , drop = FALSE]
    if (nrow(match_rows) > 0L) {
      id <- as.character(match_rows$id[[1L]])
      info <- .XREF_KNOWN_PREFIXES[[pfx]]
      if (!is.null(info)) {
        return(paste0(info$uri, id))
      }
    }
  }

  # Fallback: any available prefix
  if (nrow(rows) > 0L) {
    pfx <- rows$prefix[[1L]]
    id <- rows$id[[1L]]
    info <- .XREF_KNOWN_PREFIXES[[pfx]]
    if (!is.null(info)) {
      return(paste0(info$uri, id))
    }
  }

  "null"
}

#' Select the best database_identifier value as prefix:id.
#'
#' Prefers identifiers that align with registered database blocks and
#' identifiers.org-compatible prefixes.
#'
#' @param rows data.frame with columns `prefix`, `id` (or NULL).
#' @param fallback fallback identifier when no mapped xref is available.
#' @return Single database_identifier string.
#' @keywords internal
.mztab_pick_best_database_identifier <- function(rows, fallback = "null") {
  if (is.null(rows) || nrow(rows) == 0L) {
    fb <- as.character(fallback)[[1L]]
    if (is.na(fb) || !nzchar(fb)) "null" else fb
  } else {
    priority_prefixes <- c(
      "chebi",
      "pubchem.compound",
      "hmdb",
      "chembl.compound",
      "kegg",
      "cas",
      "lipidmaps",
      "knapsack",
      "wikidata"
    )

    for (pfx in priority_prefixes) {
      match_rows <- rows[rows$prefix == pfx, , drop = FALSE]
      if (nrow(match_rows) > 0L) {
        id <- as.character(match_rows$id[[1L]])
        info <- .XREF_KNOWN_PREFIXES[[pfx]]
        if (!is.null(info) && !is.na(id) && nzchar(id)) {
          return(paste0(info$prefix, ":", id))
        }
      }
    }

    # Fallback to first available mapped prefix:id.
    for (i in seq_len(nrow(rows))) {
      pfx <- as.character(rows$prefix[[i]])
      id <- as.character(rows$id[[i]])
      info <- .XREF_KNOWN_PREFIXES[[pfx]]
      if (!is.null(info) && !is.na(id) && nzchar(id)) {
        return(paste0(info$prefix, ":", id))
      }
    }

    fb <- as.character(fallback)[[1L]]
    if (is.na(fb) || !nzchar(fb)) "null" else fb
  }
}

#' Derive additional database[n] metadata blocks from xrefs index.
#'
#' Collects all unique `prefix` values found across the entire xrefs index and
#' returns a list of metadata info blocks (name, prefix, uri) in priority order,
#' excluding `wikidata` (handled via URI enrichment) and unknown prefixes.
#'
#' @param xrefs_index Named list of data.frames (output of
#'   [.mztab_build_xrefs_index()]).
#' @return Named list of lists, each with fields `name`, `prefix`, `uri`.
#' @keywords internal
.mztab_xref_database_blocks <- function(xrefs_index) {
  if (is.null(xrefs_index) || length(xrefs_index) == 0L) {
    return(list())
  }

  # Collect all prefixes across all xref rows
  all_prefixes <- unique(unlist(lapply(xrefs_index, function(df) {
    as.character(df$prefix)
  })))

  # Priority order for database[n] registration (wikidata is for URIs only)
  ordered_prefixes <- c(
    "wikidata",
    "chebi",
    "pubchem.compound",
    "hmdb",
    "chembl.compound",
    "kegg",
    "cas",
    "lipidmaps",
    "knapsack"
  )
  selected <- intersect(ordered_prefixes, all_prefixes)

  out <- lapply(selected, function(pfx) {
    info <- .XREF_KNOWN_PREFIXES[[pfx]]
    if (is.null(info)) {
      return(NULL)
    }
    list(
      name = info$name,
      prefix = info$prefix,
      uri = gsub("/$", "", sub(":[^/]*$", "", info$uri))
    )
  })

  Filter(Negate(is.null), out)
}
