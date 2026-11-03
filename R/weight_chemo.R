# Helper Functions ----

#' Validate that a score is between 0 and 1
#' @keywords internal
#' @noRd
.is_valid_score <- function(score) {
  is.numeric(score) && score >= 0 && score <= 1
}

# Exported Functions ----

#' @title Weight annotations by chemical consistency
#'
#' @description Weights biologically weighted annotations according to their
#'     chemical consistency by comparing chemical taxonomy (ClassyFire,
#'     NPClassifier) across molecular network neighbors. Higher chemical
#'     consistency within network components results in higher chemical scores.
#'     Internal helper for weight_annotations().
#'
#' @details The weights are automatically normalized by dividing by their sum,
#'     so they do NOT need to sum to 1. For example, weights of (1, 1, 1)
#'     produce the same result as (0.33, 0.33, 0.33).
#'
#' @include validations_utils.R
#' @include weights_utils.R
#' @include logs_utils.R
#' @include columns_utils.R
#' @include validations_params.R
#'
#' @param annot_table_wei_bio_clean [data.frame] Data frame with cleaned
#'     biologically weighted annotations
#' @param weight_spectral [numeric] Weight for spectral similarity score (any
#'     positive number)
#' @param weight_biological [numeric] Weight for biological source score (any
#'     positive number)
#' @param weight_chemical [numeric] Weight for chemical consistency score (any
#'     positive number)
#' @param score_chemical_cla_kingdom [numeric] Score for ClassyFire kingdom
#'     match (0-1)
#' @param score_chemical_cla_superclass [numeric] Score for ClassyFire
#'     superclass match (0-1)
#' @param score_chemical_cla_class [numeric] Score for ClassyFire class match
#'     (0-1)
#' @param score_chemical_cla_parent [numeric] Score for ClassyFire parent match
#'     (0-1, highest)
#' @param score_chemical_npc_pathway [numeric] Score for NPC pathway match (0-1)
#' @param score_chemical_npc_superclass [numeric] Score for NPC superclass match
#'     (0-1)
#' @param score_chemical_npc_class [numeric] Score for NPC class match (0-1,
#'     highest)
#'
#' @return Data frame with chemically weighted annotations including chemical
#'     consistency scores and final weighted scores
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Internal use only - called by weight_annotations()
#' weighted <- weight_chemo(
#'   annot_table_wei_bio_clean = bio_weighted,
#'   weight_spectral = 1,
#'   weight_biological = 1,
#'   weight_chemical = 1,
#'   score_chemical_cla_kingdom = 0.1,
#'   score_chemical_cla_superclass = 0.2,
#'   score_chemical_cla_class = 0.3,
#'   score_chemical_cla_parent = 0.4,
#'   score_chemical_npc_pathway = 0.1,
#'   score_chemical_npc_superclass = 0.2,
#'   score_chemical_npc_class = 0.3
#' )
#' }
weight_chemo <- function(
  annot_table_wei_bio_clean,
  weight_spectral,
  weight_biological,
  weight_chemical,
  score_chemical_cla_kingdom,
  score_chemical_cla_superclass,
  score_chemical_cla_class,
  score_chemical_cla_parent,
  score_chemical_npc_pathway,
  score_chemical_npc_superclass,
  score_chemical_npc_class
) {
  ctx <- log_operation(
    "weight_chemo",
    n_input = nrow(annot_table_wei_bio_clean)
  )

  # Input Validation ----
  validate_dataframe(
    annot_table_wei_bio_clean,
    param_name = "annot_table_wei_bio_clean"
  )

  # Validate weights
  validate_weights(
    c(
      weight_spectral,
      weight_biological,
      weight_chemical
    )
  )

  # Validate all chemical score parameters (0-1 range)
  validate_numeric_range(
    score_chemical_cla_kingdom,
    0,
    1,
    param_name = "score_chemical_cla_kingdom"
  )
  validate_numeric_range(
    score_chemical_cla_superclass,
    0,
    1,
    param_name = "score_chemical_cla_superclass"
  )
  validate_numeric_range(
    score_chemical_cla_class,
    0,
    1,
    param_name = "score_chemical_cla_class"
  )
  validate_numeric_range(
    score_chemical_cla_parent,
    0,
    1,
    param_name = "score_chemical_cla_parent"
  )
  validate_numeric_range(
    score_chemical_npc_pathway,
    0,
    1,
    param_name = "score_chemical_npc_pathway"
  )
  validate_numeric_range(
    score_chemical_npc_superclass,
    0,
    1,
    param_name = "score_chemical_npc_superclass"
  )
  validate_numeric_range(
    score_chemical_npc_class,
    0,
    1,
    param_name = "score_chemical_npc_class"
  )

  annot_table_wei_bio_clean <- select_weight_annotations_working_columns(
    annot_table_wei_bio_clean
  )

  # Early Exit ----
  n_annotations <- nrow(annot_table_wei_bio_clean)
  if (n_annotations == 0L) {
    log_warn(
      "Empty annotation table provided, skipping chemical weighting"
    )
    return(annot_table_wei_bio_clean)
  }

  # Validate weights sum to 1 (with tolerance for floating-point precision)
  weight_sum <- weight_spectral + weight_biological + weight_chemical
  if (abs(weight_sum - 1.0) > 0.01) {
    cli::cli_abort(
      "weights must sum to 1.0, got {.val {round(weight_sum, 4)}}",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  # Validate all weights are non-negative
  if (weight_spectral < 0 || weight_biological < 0 || weight_chemical < 0) {
    cli::cli_abort(
      "all weights must be non-negative",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  # Validate chemical scores
  chem_scores <- list(
    cla_kingdom = score_chemical_cla_kingdom,
    cla_superclass = score_chemical_cla_superclass,
    cla_class = score_chemical_cla_class,
    cla_parent = score_chemical_cla_parent,
    npc_pathway = score_chemical_npc_pathway,
    npc_superclass = score_chemical_npc_superclass,
    npc_class = score_chemical_npc_class
  )

  # Validate all scores
  invalid_scores <- !vapply(
    X = chem_scores,
    FUN = .is_valid_score,
    logical(1L)
  )
  if (any(invalid_scores)) {
    invalid_names <- names(chem_scores)[invalid_scores]
    cli::cli_abort(
      c(
        "the following scores must be between 0 and 1",
        "x" = paste0("score_chemical_", invalid_names, collapse = ", ")
      ),
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  log_info(
    "Weighting %d annotations by chemical consistency",
    n_annotations
  )
  log_debug(
    "Weights - Spectral: %.2f, Biological: %.2f, Chemical: %.2f",
    weight_spectral,
    weight_biological,
    weight_chemical
  )

  chemo_levels <- list(
    list(
      cand = "candidate_structure_tax_cla_01kin",
      fval = "feature_pred_tax_cla_01kin_val",
      weight = score_chemical_cla_kingdom,
      out = "score_chemical_1"
    ),
    list(
      cand = "candidate_structure_tax_npc_01pat",
      fval = "feature_pred_tax_npc_01pat_val",
      weight = score_chemical_npc_pathway,
      out = "score_chemical_2"
    ),
    list(
      cand = "candidate_structure_tax_cla_02sup",
      fval = "feature_pred_tax_cla_02sup_val",
      weight = score_chemical_cla_superclass,
      out = "score_chemical_3"
    ),
    list(
      cand = "candidate_structure_tax_npc_02sup",
      fval = "feature_pred_tax_npc_02sup_val",
      weight = score_chemical_npc_superclass,
      out = "score_chemical_4"
    ),
    list(
      cand = "candidate_structure_tax_cla_03cla",
      fval = "feature_pred_tax_cla_03cla_val",
      weight = score_chemical_cla_class,
      out = "score_chemical_5"
    ),
    list(
      cand = "candidate_structure_tax_npc_03cla",
      fval = "feature_pred_tax_npc_03cla_val",
      weight = score_chemical_npc_class,
      out = "score_chemical_6"
    ),
    list(
      cand = "candidate_structure_tax_cla_04dirpar",
      fval = "feature_pred_tax_cla_04dirpar_val",
      weight = score_chemical_cla_parent,
      out = "score_chemical_7"
    )
  )

  # Evaluate stri_detect_token only on unique valid (cand, fval) pairs per level
  # and assign directly onto annot_table_wei_bio_clean — avoids 21-column
  # distinct() and 21-column right_join() copies across millions of rows.
  annot_table_wei_chemo <- annot_table_wei_bio_clean
  rm(annot_table_wei_bio_clean)
  score_chem_max <- numeric(n_annotations)

  for (lvl in chemo_levels) {
    cand_vals <- annot_table_wei_chemo[[lvl$cand]]
    fval <- annot_table_wei_chemo[[lvl$fval]]
    valid_idx <- which(
      !is.na(cand_vals) &
        cand_vals != "notClassified" &
        !is.na(fval)
    )
    col_out <- rep(NA_real_, n_annotations)
    if (length(valid_idx) > 0L) {
      c_sub <- cand_vals[valid_idx]
      f_sub <- fval[valid_idx]
      pair_k <- paste(c_sub, f_sub, sep = "\r")
      u_pos <- which(!duplicated(pair_k))
      u_match <- stri_detect_token(str = f_sub[u_pos], pattern = c_sub[u_pos])
      if (any(u_match)) {
        w_val <- as.numeric(lvl$weight)
        hit <- u_match[fastmatch::fmatch(pair_k, pair_k[u_pos])]
        hit_idx <- valid_idx[hit]
        col_out[hit_idx] <- w_val
        if (w_val > 0) {
          score_chem_max[hit_idx] <- pmax(score_chem_max[hit_idx], w_val)
        }
      }
    }
    annot_table_wei_chemo[[lvl$out]] <- col_out
  }
  annot_table_wei_chemo$score_chemical <- score_chem_max
  rm(chemo_levels, score_chem_max)

  weighted_chemo <- compute_weighted_components(
    annot_table_wei_chemo$score_chemical,
    annot_table_wei_chemo$score_biological,
    annot_table_wei_chemo$candidate_score_pseudo_initial,
    weights = c(weight_chemical, weight_biological, weight_spectral)
  )
  annot_table_wei_chemo$score_weighted_chemo <- weighted_chemo$score

  log_complete(ctx, n_weighted = nrow(annot_table_wei_chemo))

  annot_table_wei_chemo
}
