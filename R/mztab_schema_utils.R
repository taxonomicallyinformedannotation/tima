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
MZTAB_METADATA_REQUIRED_FALLBACK <- c("mzTab-version")

#' @keywords internal
MZTAB_METADATA_RECOMMENDED_FALLBACK <- c("mzTab-mode", "mzTab-type")

#' @keywords internal
MZTAB_CV_REGISTRY_FALLBACK <- list(
  list(
    label = "MS",
    full_name = "PSI-MS controlled vocabulary",
    version = "4.1.11",
    uri = "https://purl.obolibrary.org/obo/ms.obo"
  ),
  list(
    label = "UO",
    full_name = "Units of Measurement Ontology",
    version = "unknown",
    uri = "https://purl.obolibrary.org/obo/uo.obo"
  ),
  list(
    label = "STATO",
    full_name = "The Statistical Methods Ontology",
    version = "unknown",
    uri = "https://purl.obolibrary.org/obo/stato.obo"
  )
)

#' @keywords internal
MZTAB_TERM_FALLBACKS <- list(
  metadata = list(
    mzTab_version = "2.1.0-M",
    mzTab_mode = "Summary",
    mzTab_type = "Identification"
  ),
  params = list(
    polarity = list(
      positive = "[MS, MS:1000130, positive scan, ]",
      negative = "[MS, MS:1000129, negative scan, ]"
    ),
    quantification = list(
      label_free = "[MS, MS:1001834, LC-MS label-free quantitation analysis, ]",
      peak_area = "[MS, MS:1001113, peak area, ]"
    ),
    sample = "[, , metabolomics sample, ]",
    assay_quantification_reagent = "[MS, MS:1002038, unlabeled sample, ]",
    study_variable_group = "[, , annotation_group, ]",
    default_database = "[, , LOTUS natural-product database, ]"
  )
)

#' @keywords internal
.get_mztab_schema_path <- function() {
  system.file("contracts", "mzTab_m_openapi.yml", package = "tima")
}

#' @keywords internal
.mztab_normalize_column_name <- function(x) {
  if (length(x) == 0L) {
    return(character(0))
  }

  out <- trimws(as.character(x))
  out[is.na(out) | !nzchar(out)] <- NA_character_
  out <- gsub("-", "_", out, fixed = TRUE)
  out <- gsub(" ", "_", out, fixed = TRUE)
  out <- gsub("[^A-Za-z0-9]+", "_", out, perl = TRUE)
  out <- gsub("^_+|_+$", "", out, perl = TRUE)
  out <- tolower(out)
  out <- gsub("^sml_id$", "SML_ID", out)
  out <- gsub("^smf_id$", "SMF_ID", out)
  out <- gsub("^sme_id$", "SME_ID", out)
  out <- gsub("^sml_id_refs$", "SML_ID_REFS", out)
  out <- gsub("^smf_id_refs$", "SMF_ID_REFS", out)
  out <- gsub("^sme_id_refs$", "SME_ID_REFS", out)
  out
}

#' @keywords internal
.mztab_default_cv_registry <- function(software_version = NULL) {
  version <- if (is.null(software_version) || !nzchar(as.character(software_version))) {
    "unknown"
  } else {
    as.character(software_version)
  }

  registry <- MZTAB_CV_REGISTRY_FALLBACK
  registry[[length(registry) + 1L]] <- list(
    label = "TIMA",
    full_name = "Taxonomically Informed Metabolite Annotation user vocabulary",
    version = version,
    uri = "https://github.com/taxonomicallyinformedannotation/tima"
  )
  registry
}

#' @keywords internal
.mztab_default_term_catalog <- function(software_version = NULL) {
  list(
    metadata = MZTAB_TERM_FALLBACKS$metadata,
    cv_registry = .mztab_default_cv_registry(software_version),
    params = MZTAB_TERM_FALLBACKS$params
  )
}

#' @keywords internal
.mztab_schema_catalog <- function() {
  schema <- .load_mztab_schema()
  required_columns <- get_mztab_required_columns()

  list(
    schema = schema,
    required_columns = required_columns,
    metadata = list(
      required = MZTAB_METADATA_REQUIRED_FALLBACK,
      recommended = MZTAB_METADATA_RECOMMENDED_FALLBACK
    ),
    terms = .mztab_default_term_catalog()
  )
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

  .mztab_normalize_column_name(required)
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
