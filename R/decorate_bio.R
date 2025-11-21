#' @title Decorate bio
#'
#' @description This function logs summary statistics about biologically weighted
#'     annotations, showing how many structures were reranked at each taxonomic
#'     level based on organism occurrence data. The function validates that the
#'     required columns are present and handles empty inputs gracefully.
#'
#' @param annot_table_wei_bio Data frame containing biologically weighted annotations
#' @param score_biological_kingdom Numeric minimum score for kingdom-level matches
#' @param score_biological_phylum Numeric minimum score for phylum-level matches
#' @param score_biological_class Numeric minimum score for class-level matches
#' @param score_biological_order Numeric minimum score for order-level matches
#' @param score_biological_family Numeric minimum score for family-level matches
#' @param score_biological_tribe Numeric minimum score for tribe-level matches
#' @param score_biological_genus Numeric minimum score for genus-level matches
#' @param score_biological_species Numeric minimum score for species-level matches
#' @param score_biological_variety Numeric minimum score for variety-level matches
#'
#' @return The input annotation table (unchanged), for use in pipelines
#'
#' @examples NULL
decorate_bio <- function(
  annot_table_wei_bio = get("annot_table_wei_bio", envir = parent.frame()),
  score_biological_kingdom = get(
    "score_biological_kingdom",
    envir = parent.frame()
  ),
  score_biological_phylum = get(
    "score_biological_phylum",
    envir = parent.frame()
  ),
  score_biological_class = get(
    "score_biological_class",
    envir = parent.frame()
  ),
  score_biological_order = get(
    "score_biological_order",
    envir = parent.frame()
  ),
  score_biological_family = get(
    "score_biological_family",
    envir = parent.frame()
  ),
  score_biological_tribe = get(
    "score_biological_tribe",
    envir = parent.frame()
  ),
  score_biological_genus = get(
    "score_biological_genus",
    envir = parent.frame()
  ),
  score_biological_species = get(
    "score_biological_species",
    envir = parent.frame()
  ),
  score_biological_variety = get(
    "score_biological_variety",
    envir = parent.frame()
  )
) {
  # Input Validation ----

  required_cols <- c(
    "score_biological",
    "candidate_structure_inchikey_connectivity_layer"
  )
  missing_cols <- setdiff(required_cols, names(annot_table_wei_bio))

  if (length(missing_cols) > 0) {
    logger::log_warn(
      "decorate_bio: missing expected columns: {paste(missing_cols, collapse = ', ')}"
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

  logger::log_info(
    "Taxonomically informed metabolite annotation reranked:\n",
    "  Kingdom level: {counts['kingdom']} structures\n",
    "  Phylum level:  {counts['phylum']} structures\n",
    "  Class level:   {counts['class']} structures\n",
    "  Order level:   {counts['order']} structures\n",
    "  Family level:  {counts['family']} structures\n",
    "  Tribe level:   {counts['tribe']} structures\n",
    "  Genus level:   {counts['genus']} structures\n",
    "  Species level: {counts['species']} structures\n",
    "  Variety level: {counts['variety']} structures"
  )

  return(annot_table_wei_bio)
}
