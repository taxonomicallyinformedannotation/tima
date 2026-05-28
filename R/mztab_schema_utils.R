#' mzTab-M schema utilities
#'
#' @description Internal helpers to access the bundled mzTab-M OpenAPI schema
#' and derive lightweight table validation requirements for plain-text parsing.
#'
#' @include logs_utils.R
#' @include validations_utils.R
#' @keywords internal
#' @name mztab_schema_utils
NULL

#' @keywords internal
MZTAB_SECTION_PREFIX <- list(
  metadata = "MTD",
  small_molecule_summary_header = "SMH",
  small_molecule_summary = "SML",
  small_molecule_feature_header = "SFH",
  small_molecule_feature = "SMF",
  small_molecule_evidence_header = "SEH",
  small_molecule_evidence = "SME"
)

#' @keywords internal
MZTAB_REQUIRED_FALLBACK <- list(
  MTD = c("mzTab-version"),
  SML = c("SML_ID"),
  SMF = c("SMF_ID", "exp_mass_to_charge"),
  SME = c(
    "SME_ID",
    "evidence_input_id",
    "database_identifier",
    "exp_mass_to_charge",
    "charge",
    "theoretical_mass_to_charge",
    "spectra_ref",
    "identification_method",
    "ms_level",
    "rank"
  )
)

#' @keywords internal
.get_mztab_schema_path <- function() {
  system.file("contracts", "mzTab_m_openapi.yml", package = "tima")
}

#' @keywords internal
.mztab_path_exists <- function(path) {
  file.exists(path)
}

#' @keywords internal
.mztab_read_yaml <- function(path) {
  yaml::read_yaml(path)
}

#' @keywords internal
.load_mztab_schema <- function() {
  schema_path <- .get_mztab_schema_path()
  if (!nzchar(schema_path) || !.mztab_path_exists(schema_path)) {
    log_warn(
      "Bundled mzTab-M OpenAPI schema not found, using fallback requirements"
    )
    return(NULL)
  }

  tryCatch(
    .mztab_read_yaml(schema_path),
    error = function(e) {
      log_warn(
        "Failed to parse mzTab-M OpenAPI schema: %s",
        conditionMessage(e)
      )
      NULL
    }
  )
}

#' @keywords internal
.extract_schema_required <- function(schema, schema_name) {
  if (is.null(schema)) {
    return(character(0))
  }

  required <- schema$components$schemas[[schema_name]]$required
  if (is.null(required) || length(required) == 0L) {
    return(character(0))
  }

  # Preserve schema case so checks match parsed mzTab headers exactly.
  gsub("-", "_", required, fixed = TRUE)
}

#' @keywords internal
get_mztab_required_columns <- function() {
  schema <- .load_mztab_schema()

  sml_required <- .extract_schema_required(schema, "SmallMoleculeSummary")
  smf_required <- .extract_schema_required(schema, "SmallMoleculeFeature")
  sme_required <- .extract_schema_required(schema, "SmallMoleculeEvidence")

  # Keep keys that appear in plain-text mzTab headers as-is.
  sml_required <- if (length(sml_required) > 0L) {
    unique(sml_required)
  } else {
    MZTAB_REQUIRED_FALLBACK$SML
  }
  smf_required <- if (length(smf_required) > 0L) {
    unique(smf_required)
  } else {
    MZTAB_REQUIRED_FALLBACK$SMF
  }
  sme_required <- if (length(sme_required) > 0L) {
    unique(sme_required)
  } else {
    MZTAB_REQUIRED_FALLBACK$SME
  }

  list(
    MTD = MZTAB_REQUIRED_FALLBACK$MTD,
    SML = sml_required,
    SMF = smf_required,
    SME = sme_required
  )
}
