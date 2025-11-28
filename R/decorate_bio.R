#' @title Decorate biological annotation results with statistics
#'
#' @description Logs summary statistics about biologically weighted annotations,
#'     showing how many structures were reranked at each taxonomic level based
#'     on organism occurrence data. Validates required columns and handles
#'     empty inputs gracefully. Internal logging helper for weight_annotations().
#'
#' @include validations_utils.R
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
#'     score_biological_variety = 0.9
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
  score_biological_variety
) {
  # Input Validation ----
  validate_dataframe(annot_table_wei_bio, param_name = "annot_table_wei_bio")

  # Validate all score parameters (0-1 range)
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

  # Helper Function ----

  # Count unique structures at a given score threshold
  count_structures_at_level <- function(df, min_score) {
    if (nrow(df) == 0L) {
      return(0L)
    }

    df |>
      tidytable::filter(score_biological >= min_score) |>
      tidytable::distinct(
        candidate_structure_inchikey_connectivity_layer
      ) |>
      nrow()
  }

  # Calculate Counts for Each Taxonomic Level ----

  # Create score list for efficient processing
  levels <- list(
    kingdom = score_biological_kingdom,
    phylum = score_biological_phylum,
    class = score_biological_class,
    order = score_biological_order,
    family = score_biological_family,
    tribe = score_biological_tribe,
    genus = score_biological_genus,
    species = score_biological_species,
    variety = score_biological_variety
  )

  # Calculate all counts
  counts <- vapply(
    levels,
    function(score) count_structures_at_level(annot_table_wei_bio, score),
    integer(1L),
    USE.NAMES = FALSE
  )

  names(counts) <- names(levels)

  # Log Summary Statistics ----

  log_info(
    "Taxonomically informed metabolite annotation reranked:
    Kingdom level: %d structures
    Phylum level:  %d structures
    Class level:   %d structures
    Order level:   %d structures
    Family level:  %d structures
    Tribe level:   %d structures
    Genus level:   %d structures
    Species level: %d structures
    Variety level: %d structures",
    counts["kingdom"],
    counts["phylum"],
    counts["class"],
    counts["order"],
    counts["family"],
    counts["tribe"],
    counts["genus"],
    counts["species"],
    counts["variety"]
  )

  return(annot_table_wei_bio)
}
