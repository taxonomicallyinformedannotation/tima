#' @title Select SIRIUS CANOPUS columns
#'
#' @description Selects and standardizes CANOPUS chemical classification
#'     columns from SIRIUS results, mapping SIRIUS-specific column names
#'     to TIMA standard names for downstream processing.
#'
#' @include harmonize_names_sirius.R
#' @include validators.R
#'
#' @param df Data frame with SIRIUS CANOPUS results
#' @param sirius_version SIRIUS version ("5" or "6")
#'
#' @return Data frame with standardized CANOPUS column names
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Standardize CANOPUS columns from SIRIUS v6
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
    logger::log_warn("Empty CANOPUS data frame")
    return(df)
  }

  # Validate SIRIUS version
  if (!sirius_version %in% c("5", "6", 5, 6)) {
    stop(
      "sirius_version must be '5' or '6', got: ",
      sirius_version,
      call. = FALSE
    )
  }

  df <- df |>
    tidytable::mutate(
      feature_id = switch(
        sirius_version,
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
          # "feature_pred_tax_cla_04sub_score" ="ClassyFire.subclass.Probability",
          # "feature_pred_tax_cla_05lev_val" = "ClassyFire.level.5",
          # "feature_pred_tax_cla_05lev_score" ="ClassyFire.level.5.Probability",
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
  return(df)
}

#' @title Select SIRIUS formula columns
#'
#' @description Selects and standardizes SIRIUS formula-level columns,
#'     extracting molecular formula candidates and their scores.
#'
#' @include harmonize_names_sirius.R
#'
#' @param df Data frame with SIRIUS formula results
#' @param sirius_version SIRIUS version ("5" or "6")
#'
#' @return Data frame with standardized formula column names
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Standardize SIRIUS formula columns
#' formulas <- select_sirius_columns_formulas(
#'   df = sirius_formulas,
#'   sirius_version = "6"
#' )
#' }
select_sirius_columns_formulas <- function(df, sirius_version) {
  df <- df |>
    tidytable::mutate(
      feature_id = switch(
        sirius_version,
        "5" = harmonize_names_sirius(id),
        "6" = mappingFeatureId
      )
    ) |>
    tidytable::mutate(
      candidate_structure_exact_mass = as.numeric(ionMass) -
        as.numeric(`massErrorPrecursor.ppm.`) *
          # tidytable version
          # as.numeric(`massErrorPrecursor(ppm)`) *
          as.numeric(ionMass) *
          1E-6,
      candidate_structure_error_mz = as.numeric(ionMass) *
        as.numeric(`massErrorPrecursor.ppm.`) *
        # tidytable version
        # as.numeric(`massErrorPrecursor(ppm)`) *
        1E-6
    ) |>
    tidytable::select(
      tidyselect::any_of(
        x = c(
          "feature_id",
          "candidate_adduct" = "adduct",
          "candidate_structure_molecular_formula" = "molecularFormula",
          "candidate_structure_exact_mass",
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
  return(df)
}

#' @title Select SIRIUS structure columns
#'
#' @description Selects and standardizes SIRIUS structure-level columns,
#'     extracting structure candidates and their CSI:FingerID scores.
#'
#' @param df Data frame with SIRIUS structure results
#' @param sirius_version SIRIUS version ("5" or "6")
#'
#' @return Data frame with standardized structure column names
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Standardize SIRIUS structure columns
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
          "candidate_structure_smiles_no_stereo" = "smiles",
          "candidate_structure_inchikey_connectivity_layer" = "InChIkey2D",
          "candidate_structure_molecular_formula" = "molecularFormula",
          "candidate_structure_xlogp" = "xlogp",
          # ISSUE see #147
          "candidate_score_sirius_confidence" = switch(
            sirius_version,
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
  return(df)
}
