#' @title Decorate biological annotation results with statistics
#'
#' @description Logs summary statistics about biologically weighted annotations,
#'     showing how many candidate rows and unique structures were reranked at
#'     each taxonomic level based on organism occurrence data. Validates
#'     required columns and handles empty inputs gracefully. Internal logging
#'     helper for weight_annotations().
#'
#' @include predicates_utils.R
#' @include validations_utils.R
#' @include logs_utils.R
#' @include validations_params.R
#'
#' @param annot_table_wei_bio Data frame with biologically weighted annotations
#' @param score_biological_kingdom Minimum score for kingdom-level matches
#' @param score_biological_phylum Minimum score for phylum-level matches
#' @param score_biological_class Minimum score for class-level matches
#' @param score_biological_order Minimum score for order-level matches
#' @param score_biological_family Minimum score for family-level matches
#' @param score_biological_tribe Minimum score for tribe-level matches
#' @param score_biological_genus Minimum score for genus-level matches
#' @param score_biological_species Minimum score for species-level matches
#' @param score_biological_variety Minimum score for variety-level matches
#' @param score_biological_biota Minimum score for Biota-level matches
#' @param score_biological_domain Optional minimum score for domain-level matches
#' @param score_biological_infraorder Optional minimum score for infraorder-level
#'     matches
#' @param score_biological_subfamily Optional minimum score for subfamily-level
#'     matches
#' @param score_biological_subtribe Optional minimum score for subtribe-level
#'     matches
#' @param score_biological_subgenus Optional minimum score for subgenus-level
#'     matches
#' @param score_biological_subspecies Optional minimum score for subspecies-level
#'     matches
#'
#' @return The input annotation table (unchanged), for pipeline compatibility
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Internal use only - called by weight_annotations() for logging
#' weighted_bio |>
#'   decorate_bio(
#'     score_biological_kingdom = 0.1,
#'     score_biological_phylum = 0.2,
#'     score_biological_class = 0.3,
#'     score_biological_order = 0.4,
#'     score_biological_family = 0.5,
#'     score_biological_tribe = 0.6,
#'     score_biological_genus = 0.7,
#'     score_biological_species = 0.8,
#'     score_biological_variety = 0.9,
#'     score_biological_biota = 1.0
#'   )
#' }
decorate_bio <- function(
  annot_table_wei_bio,
  score_biological_kingdom,
  score_biological_phylum,
  score_biological_class,
  score_biological_order,
  score_biological_family,
  score_biological_tribe,
  score_biological_genus,
  score_biological_species,
  score_biological_variety,
  score_biological_biota,
  score_biological_domain = NULL,
  score_biological_infraorder = NULL,
  score_biological_subfamily = NULL,
  score_biological_subtribe = NULL,
  score_biological_subgenus = NULL,
  score_biological_subspecies = NULL
) {
  ctx <- log_operation(
    "decorate_bio",
    n_annotations = nrow(annot_table_wei_bio)
  )

  validate_dataframe(annot_table_wei_bio, param_name = "annot_table_wei_bio")

  validate_numeric_range(
    score_biological_kingdom,
    0,
    1,
    param_name = "score_biological_kingdom"
  )
  validate_numeric_range(
    score_biological_phylum,
    0,
    1,
    param_name = "score_biological_phylum"
  )
  validate_numeric_range(
    score_biological_class,
    0,
    1,
    param_name = "score_biological_class"
  )
  validate_numeric_range(
    score_biological_order,
    0,
    1,
    param_name = "score_biological_order"
  )
  validate_numeric_range(
    score_biological_family,
    0,
    1,
    param_name = "score_biological_family"
  )
  validate_numeric_range(
    score_biological_tribe,
    0,
    1,
    param_name = "score_biological_tribe"
  )
  validate_numeric_range(
    score_biological_genus,
    0,
    1,
    param_name = "score_biological_genus"
  )
  validate_numeric_range(
    score_biological_species,
    0,
    1,
    param_name = "score_biological_species"
  )
  validate_numeric_range(
    score_biological_variety,
    0,
    1,
    param_name = "score_biological_variety"
  )
  validate_numeric_range(
    score_biological_biota,
    0,
    1.007277,
    param_name = "score_biological_biota"
  )

  optional_scores <- list(
    score_biological_domain = score_biological_domain,
    score_biological_infraorder = score_biological_infraorder,
    score_biological_subfamily = score_biological_subfamily,
    score_biological_subtribe = score_biological_subtribe,
    score_biological_subgenus = score_biological_subgenus,
    score_biological_subspecies = score_biological_subspecies
  )
  for (nm in names(optional_scores)) {
    if (!is.null(optional_scores[[nm]])) {
      validate_numeric_range(optional_scores[[nm]], 0, 1, param_name = nm)
    }
  }

  required_cols <- c(
    "score_biological",
    "candidate_structure_inchikey_connectivity_layer"
  )
  missing_cols <- setdiff(required_cols, names(annot_table_wei_bio))

  if (length(missing_cols) > 0) {
    log_warn(
      "decorate_bio: missing expected columns: %s",
      paste(missing_cols, collapse = ", ")
    )
    return(annot_table_wei_bio)
  }

  # Count total candidates and unique structures at a given score threshold.
  # OPTIMIZATION: Pre-sort the data once by score descending, then for each
  # threshold compute counts via cumulative sum + duplicated() tracking.
  # This replaces filter+distinct passes with a single O(N log N) sort and
  # O(1) lookups per level.

  all_levels <- list(
    domain = score_biological_domain,
    kingdom = score_biological_kingdom,
    phylum = score_biological_phylum,
    class = score_biological_class,
    order = score_biological_order,
    infraorder = score_biological_infraorder,
    family = score_biological_family,
    subfamily = score_biological_subfamily,
    tribe = score_biological_tribe,
    subtribe = score_biological_subtribe,
    genus = score_biological_genus,
    subgenus = score_biological_subgenus,
    species = score_biological_species,
    subspecies = score_biological_subspecies,
    variety = score_biological_variety,
    biota = score_biological_biota
  )
  levels <- Filter(Negate(is.null), all_levels)
  level_vals <- unlist(levels, use.names = FALSE)
  min_level <- min(level_vals, na.rm = TRUE)

  ik <- annot_table_wei_bio$candidate_structure_inchikey_connectivity_layer
  raw_sc <- annot_table_wei_bio$score_biological
  score <- if (is.double(raw_sc)) raw_sc else as.numeric(raw_sc)
  valid_sc <- which(!is.na(score) & score >= min_level)

  if (length(valid_sc) == 0L) {
    count_stats_at_level <- function(min_score) {
      c(n_candidates = 0L, n_unique_structures = 0L)
    }
  } else {
    sc_v <- score[valid_sc]
    ik_v <- ik[valid_sc]
    u_ik <- unique(ik_v)
    ik_id <- fastmatch::fmatch(ik_v, u_ik)
    u_sc <- sort(unique(sc_v))

    if (length(u_sc) <= 64L) {
      sc_id <- fastmatch::fmatch(sc_v, u_sc)
      sc_counts <- tabulate(sc_id, nbins = length(u_sc))
      ord_sc <- order(-sc_id, method = "radix")
      max_sc_id_ik <- sc_id[ord_sc[!duplicated(ik_id[ord_sc])]]
      u_ik_counts_by_sc_id <- tabulate(max_sc_id_ik, nbins = length(u_sc))

      count_stats_at_level <- function(min_score) {
        keep_sc <- u_sc >= min_score
        c(
          n_candidates = as.integer(sum(sc_counts[keep_sc])),
          n_unique_structures = as.integer(sum(u_ik_counts_by_sc_id[keep_sc]))
        )
      }
    } else {
      ord <- order(sc_v, decreasing = TRUE, method = "radix")
      score_sorted <- sc_v[ord]
      first_occurrence <- !duplicated(ik_id[ord])
      cum_unique <- cumsum(first_occurrence)

      count_stats_at_level <- function(min_score) {
        n_candidates <- sum(score_sorted >= min_score)
        n_unique <- if (n_candidates == 0L) 0L else cum_unique[n_candidates]
        c(n_candidates = n_candidates, n_unique_structures = n_unique)
      }
    }
  }

  counts <- lapply(levels, count_stats_at_level)
  label_width <- max(8L, max(nchar(names(levels))))

  fmt_level <- function(level_name) {
    level_counts <- counts[[level_name]]
    sprintf(
      "    %-*s level: %d candidates (%d unique)",
      label_width,
      tools::toTitleCase(level_name),
      level_counts[["n_candidates"]],
      level_counts[["n_unique_structures"]]
    )
  }

  log_info(
    paste(
      c(
        "Taxonomically informed metabolite annotation reranked:",
        vapply(names(levels), fmt_level, character(1L))
      ),
      collapse = "\n"
    )
  )

  log_complete(ctx, n_processed = nrow(annot_table_wei_bio))

  annot_table_wei_bio
}
