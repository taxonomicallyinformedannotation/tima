#' @title Select SIRIUS CANOPUS columns
#'
#' @description Standardize CANOPUS chemical classification columns.
#'
#' @include validations_utils.R
#'
#' @param df [data.frame] Data frame of SIRIUS CANOPUS results.
#' @param sirius_version [character] SIRIUS version ("5" or "6").
#'
#' @return Data frame with standardized CANOPUS columns.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Internal use only - called by prepare_annotations_sirius()
#' standardized <- select_sirius_columns_canopus(
#'   df = canopus_results,
#'   sirius_version = "6"
#' )
#' }
select_sirius_columns_canopus <- function(df, sirius_version) {
  # Input Validation ----
  validate_dataframe(df, param_name = "df")

  # Early exit for empty data
  if (nrow(df) == 0L) {
    log_warn("Empty CANOPUS data frame")
    return(df)
  }

  # Validate SIRIUS version
  if (!sirius_version %in% c("5", "6", 5, 6)) {
    cli::cli_abort(
      "sirius_version must be '5' or '6'",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  df <- .normalize_sirius_aliases(df)

  df <- df |>
    tidytable::mutate(
      feature_id = switch(
        as.character(sirius_version),
        "5" = harmonize_names_sirius(id),
        "6" = mappingFeatureId
      )
    ) |>
    tidytable::select(
      tidyselect::any_of(
        x = c(
          "feature_id",
          "candidate_adduct" = "adduct",
          "candidate_structure_molecular_formula" = "molecularFormula",
          "feature_pred_tax_npc_01pat_val" = "NPC.pathway",
          "feature_pred_tax_npc_01pat_score" = "NPC.pathway.Probability",
          "feature_pred_tax_npc_02sup_val" = "NPC.superclass",
          "feature_pred_tax_npc_02sup_score" = "NPC.superclass.Probability",
          "feature_pred_tax_npc_03cla_val" = "NPC.class",
          "feature_pred_tax_npc_03cla_score" = "NPC.class.Probability",
          # "feature_pred_tax_cla_01kin_val" = "ClassyFire.TODO",
          # "feature_pred_tax_cla_01kin_score" = "ClassyFire.TODO.Probability",
          "feature_pred_tax_cla_02sup_val" = "ClassyFire.superclass",
          "feature_pred_tax_cla_02sup_score" = "ClassyFire.superclass.probability",
          "feature_pred_tax_cla_03cla_val" = "ClassyFire.class",
          "feature_pred_tax_cla_03cla_score" = "ClassyFire.class.Probability",
          # "feature_pred_tax_cla_04sub_val" = "ClassyFire.subclass",
          # "feature_pred_tax_cla_04sub_score"
          # ="ClassyFire.subclass.Probability",
          # "feature_pred_tax_cla_05lev_val" = "ClassyFire.level.5",
          # "feature_pred_tax_cla_05lev_score"
          # ="ClassyFire.level.5.Probability",
          "feature_pred_tax_cla_04dirpar_val" = "ClassyFire.most.specific.class",
          "feature_pred_tax_cla_04dirpar_score" = "ClassyFire.most.specific.class.Probability"
          # tidytable version
          # "feature_pred_tax_npc_01pat_val" = "NPC#pathway",
          # "feature_pred_tax_npc_01pat_score" = "NPC#pathway Probability",
          # "feature_pred_tax_npc_02sup_val" = "NPC#superclass",
          # "feature_pred_tax_npc_02sup_score" = "NPC#superclass Probability",
          # "feature_pred_tax_npc_03cla_val" = "NPC#class",
          # "feature_pred_tax_npc_03cla_score" = "NPC#class Probability",
          # "feature_pred_tax_cla_01kin_val" = "ClassyFire#TODO",
          # "feature_pred_tax_cla_01kin_score" = "ClassyFire#TODO Probability",
          # "feature_pred_tax_cla_02sup_val" = "ClassyFire#superclass",
          # "feature_pred_tax_cla_02sup_score" =
          #   "ClassyFire#superclass probability",
          # "feature_pred_tax_cla_03cla_val" = "ClassyFire#class",
          # "feature_pred_tax_cla_03cla_score" = "ClassyFire#class Probability",
          # "feature_pred_tax_cla_04dirpar_val" =
          #   "ClassyFire#most specific class",
          # "feature_pred_tax_cla_04dirpar_score" =
          #   "ClassyFire#most specific class Probability"
        )
      )
    ) |>
    # tidytable::bind_cols(tidytable::tidytable(
    #   "feature_pred_tax_cla_01kin_val" = NA_character_,
    #   "feature_pred_tax_cla_01kin_score" = 0
    # )) |>
    tidytable::distinct()
  # Normalize SIRIUS adduct spacing (e.g., "[M + H]+" -> "[M+H]+")
  if ("candidate_adduct" %in% names(df)) {
    df[["candidate_adduct"]] <- gsub("\\s+", "", df[["candidate_adduct"]])
  }
  df
}

#' @title Select SIRIUS formula columns
#'
#' @description Standardize SIRIUS formula-level columns.
#'
#' @param df [data.frame] Data frame of SIRIUS formula results.
#' @param sirius_version [character] SIRIUS version ("5" or "6").
#'
#' @return Data frame with standardized formula columns.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Internal use only - called by prepare_annotations_sirius()
#' formulas <- select_sirius_columns_formulas(
#'   df = sirius_formulas,
#'   sirius_version = "6"
#' )
#' }
select_sirius_columns_formulas <- function(df, sirius_version) {
  df <- .normalize_sirius_aliases(df)

  mass_error_ppm <- if ("massErrorPrecursor.ppm." %in% names(df)) {
    as.numeric(df[["massErrorPrecursor.ppm."]])
  } else {
    rep(NA_real_, nrow(df))
  }

  df <- df |>
    tidytable::mutate(
      feature_id = switch(
        as.character(sirius_version),
        "5" = harmonize_names_sirius(id),
        "6" = mappingFeatureId
      )
    ) |>
    tidytable::mutate(
      ## exact_mass is recomputed from SMILES via process_smiles() downstream.
      ## Keep only mass error for scoring.
      candidate_structure_error_mz = as.numeric(ionMass) *
        mass_error_ppm *
        1E-6
    ) |>
    tidytable::select(
      tidyselect::any_of(
        x = c(
          "feature_id",
          "candidate_adduct" = "adduct",
          "candidate_structure_molecular_formula" = "molecularFormula",
          "candidate_structure_error_mz",
          "candidate_score_sirius_zodiac" = "ZodiacScore",
          "candidate_score_sirius_sirius" = "SiriusScore",
          "candidate_score_sirius_tree" = "TreeScore",
          "candidate_score_sirius_isotope" = "IsotopeScore",
          "candidate_score_sirius_intensity" = "explainedIntensity",
          "candidate_count_sirius_peaks_explained" = "numExplainedPeaks"
        )
      )
    )
  # Normalize SIRIUS adduct spacing (e.g., "[M + H]+" -> "[M+H]+")
  if ("candidate_adduct" %in% names(df)) {
    df[["candidate_adduct"]] <- gsub("\\s+", "", df[["candidate_adduct"]])
  }
  df
}

#' @title Select SIRIUS structure columns
#'
#' @description Standardize SIRIUS structure-level columns.
#'
#' @param df [data.frame] Data frame of SIRIUS structure results.
#' @param sirius_version [character] SIRIUS version ("5" or "6").
#'
#' @return Data frame with standardized structure columns.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Internal use only - called by prepare_annotations_sirius()
#' structures <- select_sirius_columns_structures(
#'   df = sirius_structures,
#'   sirius_version = "6"
#' )
#' }
select_sirius_columns_structures <- function(df, sirius_version) {
  df <- df |>
    tidytable::select(
      tidyselect::any_of(
        x = c(
          "feature_id",
          "candidate_adduct" = "adduct",
          "candidate_structure_name" = "name",
          ## SMILES is the single source of truth for structure
          ## identity.  All structural identifiers (InChIKey, formula,
          ## mass, xlogp) are strictly recomputed from SMILES via
          ## process_smiles() downstream.
          "candidate_structure_smiles_no_stereo" = "smiles",
          ## molecularFormula is kept as a SIRIUS join key (to connect
          ## structure candidates with formula-level scores and CANOPUS).
          ## It will be overwritten by the RDKit-derived formula in
          ## recompute_structure_fields_from_smiles() downstream.
          "candidate_structure_molecular_formula" = "molecularFormula",
          # ISSUE see #147
          "candidate_score_sirius_confidence" = switch(
            as.character(sirius_version),
            "5" = "ConfidenceScore",
            "6" = "ConfidenceScoreApproximate"
          ),
          "candidate_score_sirius_csi" = "CSI.FingerIDScore",
          # tidytable version
          # "candidate_score_sirius_csi" = "CSI:FingerIDScore",
          "candidate_score_sirius_msnovelist" = "ModelScore"
        )
      )
    ) |>
    tidytable::distinct() |>
    tidytable::bind_cols(tidytable::tidytable(
      candidate_library = "SIRIUS",
      candidate_structure_tax_npc_01pat = NA_character_,
      candidate_structure_tax_npc_02sup = NA_character_,
      candidate_structure_tax_npc_03cla = NA_character_,
      candidate_structure_tax_cla_chemontid = NA_character_,
      candidate_structure_tax_cla_01kin = NA_character_,
      candidate_structure_tax_cla_02sup = NA_character_,
      candidate_structure_tax_cla_03cla = NA_character_,
      candidate_structure_tax_cla_04dirpar = NA_character_
    ))
  # Normalize SIRIUS adduct spacing (e.g., "[M + H]+" -> "[M+H]+")
  if ("candidate_adduct" %in% names(df)) {
    df[["candidate_adduct"]] <- gsub("\\s+", "", df[["candidate_adduct"]])
  }
  df
}

# Normalize common SIRIUS column aliases across versions/export formats.
.normalize_sirius_aliases <- function(df) {
  alias_map <- list(
    "NPC.pathway" = "NPC#pathway",
    "NPC.pathway.Probability" = "NPC#pathway Probability",
    "NPC.superclass" = "NPC#superclass",
    "NPC.superclass.Probability" = "NPC#superclass Probability",
    "NPC.class" = "NPC#class",
    "NPC.class.Probability" = "NPC#class Probability",
    "ClassyFire.superclass" = "ClassyFire#superclass",
    "ClassyFire.superclass.probability" = "ClassyFire#superclass probability",
    "ClassyFire.class" = "ClassyFire#class",
    "ClassyFire.class.Probability" = "ClassyFire#class Probability",
    "ClassyFire.most.specific.class" = "ClassyFire#most specific class",
    "ClassyFire.most.specific.class.Probability" = "ClassyFire#most specific class Probability",
    "massErrorPrecursor.ppm." = "massErrorPrecursor(ppm)"
  )

  for (target in names(alias_map)) {
    if (target %in% names(df)) {
      next
    }
    src <- alias_map[[target]]
    src <- src[src %in% names(df)]
    if (length(src) > 0L) {
      names(df)[names(df) == src[[1L]]] <- target
    }
  }

  df
}

.get_col_or_na <- function(df, col) {
  if (col %in% names(df)) {
    return(as.character(df[[col]]))
  }
  rep(NA_character_, nrow(df))
}

#' @title Select SIRIUS spectral match columns
#'
#' @description Standardize SIRIUS spectral-match columns (including analog
#'     hits)
#'     into the common annotation model.
#'
#' @param df [data.frame] Data frame of SIRIUS spectral match results.
#' @param sirius_version [character] SIRIUS version ("5" or "6").
#'
#' @return Data frame with standardized spectral-match columns.
#' @keywords internal
select_sirius_columns_spectral <- function(df, sirius_version) {
  validate_dataframe(df, param_name = "df")

  if (nrow(df) == 0L) {
    return(tidytable::tidytable())
  }

  analog_vals <- .get_col_or_na(df, "analogHit")
  analog_logical <- tolower(trimws(analog_vals)) %in% c("true", "t", "1")

  feature_id <- switch(
    as.character(sirius_version),
    "5" = harmonize_names_sirius(.get_col_or_na(df, "id")),
    "6" = .get_col_or_na(df, "mappingFeatureId")
  )

  # Mass deviation: feature precursor (ionMass) minus library reference
  # precursor
  # (referencePrecursorMz), both provided directly by SIRIUS spectral_matches.
  # For direct hits this is the instrument mass error (should be small).
  # For analog hits this is the structural delta-mass (e.g. +14 Da methylation).
  ion_mz <- as.numeric(.get_col_or_na(df, "ionMass"))
  ref_mz <- as.numeric(.get_col_or_na(
    df,
    "referencePrecursorMz"
  ))
  raw_mz_dev <- ion_mz - ref_mz

  tidytable::tidytable(
    feature_id = feature_id,
    candidate_library = tidytable::if_else(
      analog_logical,
      "SIRIUS spectral (analog)",
      "SIRIUS spectral"
    ),
    candidate_adduct = gsub("\\s+", "", .get_col_or_na(df, "referenceAdduct")),
    candidate_structure_error_mz = raw_mz_dev,
    candidate_spectrum_id = .get_col_or_na(df, "referenceSplash"),
    candidate_structure_name = .get_col_or_na(df, "referenceName"),
    ## SMILES is the single source of truth for structure identity.
    ## All structural identifiers (InChIKey, formula, mass, xlogp)
    ## are strictly recomputed from SMILES via process_smiles()
    ## downstream.
    candidate_structure_smiles_no_stereo = .get_col_or_na(
      df,
      "referenceSmiles"
    ),
    candidate_score_similarity = as.numeric(.get_col_or_na(
      df,
      "similarity"
    )),
    candidate_count_similarity_peaks_matched = as.integer(.get_col_or_na(
      df,
      "sharedPeaks"
    )),
    candidate_structure_tax_npc_01pat = NA_character_,
    candidate_structure_tax_npc_02sup = NA_character_,
    candidate_structure_tax_npc_03cla = NA_character_,
    candidate_structure_tax_cla_chemontid = NA_character_,
    candidate_structure_tax_cla_01kin = NA_character_,
    candidate_structure_tax_cla_02sup = NA_character_,
    candidate_structure_tax_cla_03cla = NA_character_,
    candidate_structure_tax_cla_04dirpar = NA_character_
  ) |>
    tidytable::filter(!is.na(feature_id)) |>
    tidytable::distinct()
}
