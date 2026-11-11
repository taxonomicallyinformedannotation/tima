#' @title Weight annotations by chemical consistency
#'
#' @description Weights biologically weighted annotations according to their
#'     chemical consistency by comparing chemical taxonomy (ClassyFire,
#'     NPClassifier) across molecular network neighbors. Higher chemical
#'     consistency within network components results in higher chemical scores.
#'     Internal helper for weight_annotations().
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
#' @param weight_by_pred_score [logical] If `TRUE`, weights predicted chemical
#'     class frequencies by their CANOPUS/CSI prediction probabilities.
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
  score_chemical_npc_class,
  weight_by_pred_score = FALSE
) {
  ctx <- log_operation(
    "weight_chemo",
    n_input = nrow(annot_table_wei_bio_clean)
  )

  validate_dataframe(
    annot_table_wei_bio_clean,
    param_name = "annot_table_wei_bio_clean"
  )
  validate_weights(
    c(
      weight_spectral,
      weight_biological,
      weight_chemical
    )
  )
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

  n_annotations <- nrow(annot_table_wei_bio_clean)
  if (n_annotations == 0L) {
    log_warn(
      "Empty annotation table provided, skipping chemical weighting"
    )
    return(annot_table_wei_bio_clean)
  }

  weight_sum <- weight_spectral + weight_biological + weight_chemical
  if (abs(weight_sum - 1.0) > 0.01) {
    cli::cli_abort(
      "weights must sum to 1.0, got {.val {round(weight_sum, 4)}}",
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
      fscore = "feature_pred_tax_cla_01kin_score",
      weight = score_chemical_cla_kingdom,
      out = "score_chemical_1"
    ),
    list(
      cand = "candidate_structure_tax_npc_01pat",
      fval = "feature_pred_tax_npc_01pat_val",
      fscore = "feature_pred_tax_npc_01pat_score",
      weight = score_chemical_npc_pathway,
      out = "score_chemical_2"
    ),
    list(
      cand = "candidate_structure_tax_cla_02sup",
      fval = "feature_pred_tax_cla_02sup_val",
      fscore = "feature_pred_tax_cla_02sup_score",
      weight = score_chemical_cla_superclass,
      out = "score_chemical_3"
    ),
    list(
      cand = "candidate_structure_tax_npc_02sup",
      fval = "feature_pred_tax_npc_02sup_val",
      fscore = "feature_pred_tax_npc_02sup_score",
      weight = score_chemical_npc_superclass,
      out = "score_chemical_4"
    ),
    list(
      cand = "candidate_structure_tax_cla_03cla",
      fval = "feature_pred_tax_cla_03cla_val",
      fscore = "feature_pred_tax_cla_03cla_score",
      weight = score_chemical_cla_class,
      out = "score_chemical_5"
    ),
    list(
      cand = "candidate_structure_tax_npc_03cla",
      fval = "feature_pred_tax_npc_03cla_val",
      fscore = "feature_pred_tax_npc_03cla_score",
      weight = score_chemical_npc_class,
      out = "score_chemical_6"
    ),
    list(
      cand = "candidate_structure_tax_cla_04dirpar",
      fval = "feature_pred_tax_cla_04dirpar_val",
      fscore = "feature_pred_tax_cla_04dirpar_score",
      weight = score_chemical_cla_parent,
      out = "score_chemical_7"
    )
  )

  # Evaluate token matching only on unique valid (cand, fval) pairs per level
  # and assign directly onto annot_table_wei_bio_clean — avoids 21-column
  # distinct() and 21-column right_join() copies across millions of rows.
  annot_table_wei_chemo <- annot_table_wei_bio_clean
  rm(annot_table_wei_bio_clean)
  df_names <- names(annot_table_wei_chemo)

  lvl_cols <- lapply(chemo_levels, function(lvl) {
    cand_vals <- annot_table_wei_chemo[[lvl$cand]]
    fval <- annot_table_wei_chemo[[lvl$fval]]
    col_out <- rep(NA_real_, n_annotations)
    if (!is.null(cand_vals) && !is.null(fval)) {
      u_f <- unique(fval)
      u_f <- u_f[!is.na(u_f)]
      u_c <- unique(cand_vals)
      u_c <- u_c[!is.na(u_c) & u_c != "notClassified"]
      n_uf <- length(u_f)
      n_uc <- length(u_c)
      if (n_uf > 0L && n_uc > 0L) {
        parts <- strsplit(u_f, "$", fixed = TRUE)
        lens <- lengths(parts)
        toks <- trimws(unlist(parts, use.names = FALSE))
        tok_c_id <- fastmatch::fmatch(toks, u_c)
        ok_tok <- which(!is.na(tok_c_id))
        if (length(ok_tok) > 0L) {
          tok_f_id <- rep.int(seq_len(n_uf), lens)[ok_tok]
          tok_c_id <- tok_c_id[ok_tok]
          f_id <- fastmatch::fmatch(fval, u_f)
          c_id <- fastmatch::fmatch(cand_vals, u_c)
          valid_idx <- which(!is.na(f_id) & !is.na(c_id))
          if (length(valid_idx) > 0L) {
            c_sub_id <- c_id[valid_idx]
            f_sub_id <- f_id[valid_idx]
            match_lut <- logical(n_uc * n_uf)
            match_lut[(tok_c_id - 1L) * n_uf + tok_f_id] <- TRUE
            hit <- match_lut[(c_sub_id - 1L) * n_uf + f_sub_id]
            if (any(hit)) {
              w_val <- as.numeric(lvl$weight)
              hit_idx <- valid_idx[hit]
              if (
                isTRUE(weight_by_pred_score) &&
                  lvl$fscore %in% df_names
              ) {
                sc_raw <- annot_table_wei_chemo[[lvl$fscore]][hit_idx]
                sc_num <- suppressWarnings(as.double(sc_raw))
                ok_sc <- !is.na(sc_num) & is.finite(sc_num)
                w_vec <- rep.int(w_val, length(hit_idx))
                if (any(ok_sc)) {
                  w_vec[ok_sc] <- w_val * pmin(pmax(sc_num[ok_sc], 0), 1)
                }
                col_out[hit_idx] <- w_vec
              } else {
                col_out[hit_idx] <- w_val
              }
            }
          }
        }
      }
    }
    col_out
  })

  out_names <- vapply(chemo_levels, `[[`, character(1L), "out")
  data.table::set(annot_table_wei_chemo, j = out_names, value = lvl_cols)
  score_chem_max <- Reduce(
    function(acc, col_v) {
      ok <- which(!is.na(col_v) & col_v > acc)
      if (length(ok) > 0L) {
        acc[ok] <- col_v[ok]
      }
      acc
    },
    lvl_cols,
    init = numeric(n_annotations)
  )
  data.table::set(
    annot_table_wei_chemo,
    j = "score_chemical",
    value = score_chem_max
  )
  rm(chemo_levels, lvl_cols, out_names, score_chem_max)

  weighted_chemo <- compute_weighted_components(
    annot_table_wei_chemo$score_chemical,
    annot_table_wei_chemo$score_biological,
    annot_table_wei_chemo$candidate_score_pseudo_initial,
    weights = c(weight_chemical, weight_biological, weight_spectral)
  )
  data.table::set(
    annot_table_wei_chemo,
    j = "score_weighted_chemo",
    value = weighted_chemo$score
  )

  log_complete(ctx, n_weighted = nrow(annot_table_wei_chemo))

  annot_table_wei_chemo
}
