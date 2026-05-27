#' @title Write TIMA results as mzTab-M
#'
#' @description Exports TIMA weighted-annotation results to mzTab-M 2.1.0
#' plain-text format.  The output is a compliant mzTab-M file containing:
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
        "Unrecognized polarity '%s'; treating as unknown",
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

  # Resolve xrefs index (connectivity-layer InChIKey only → list of prefix+id rows)
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
    ambiguity_comments <- .mztab_build_ambiguity_comments(results)
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

    if (
      length(ambiguity_comments) > 0L &&
        !any(grepl("^COM\tTIMA ambiguity\t", passthrough_lines))
    ) {
      writeLines(ambiguity_comments, con)
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
