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
  required_cols <- c(
    "score_biological",
    "candidate_structure_inchikey_connectivity_layer"
  )
  missing_cols <- setdiff(required_cols, names(annot_table_wei_bio))
  if (length(missing_cols) > 0) {
    logger::log_warn(
      "decorate_bio: missing expected columns: %s",
      paste(missing_cols, collapse = ", ")
    )
    return(annot_table_wei_bio)
  }

  # Helper function to count unique structures at a given score threshold
  count_structures_at_level <- function(df, min_score) {
    if (nrow(df) == 0) {
      return(0L)
    }
    df |>
      tidytable::filter(score_biological >= min_score) |>
      tidytable::distinct(candidate_structure_inchikey_connectivity_layer) |>
      nrow()
  }

  # Calculate counts for each taxonomic level
  n_kingdom <- count_structures_at_level(
    annot_table_wei_bio,
    score_biological_kingdom
  )
  n_phylum <- count_structures_at_level(
    annot_table_wei_bio,
    score_biological_phylum
  )
  n_class <- count_structures_at_level(
    annot_table_wei_bio,
    score_biological_class
  )
  n_order <- count_structures_at_level(
    annot_table_wei_bio,
    score_biological_order
  )
  n_family <- count_structures_at_level(
    annot_table_wei_bio,
    score_biological_family
  )
  n_tribe <- count_structures_at_level(
    annot_table_wei_bio,
    score_biological_tribe
  )
  n_genus <- count_structures_at_level(
    annot_table_wei_bio,
    score_biological_genus
  )
  n_species <- count_structures_at_level(
    annot_table_wei_bio,
    score_biological_species
  )
  n_variety <- count_structures_at_level(
    annot_table_wei_bio,
    score_biological_variety
  )

  # Log summary statistics
  logger::log_info(
    "Taxonomically informed metabolite annotation reranked:\n",
    "  Kingdom level: ",
    n_kingdom,
    " structures\n",
    "  Phylum level:  ",
    n_phylum,
    " structures\n",
    "  Class level:   ",
    n_class,
    " structures\n",
    "  Order level:   ",
    n_order,
    " structures\n",
    "  Family level:  ",
    n_family,
    " structures\n",
    "  Tribe level:   ",
    n_tribe,
    " structures\n",
    "  Genus level:   ",
    n_genus,
    " structures\n",
    "  Species level: ",
    n_species,
    " structures\n",
    "  Variety level: ",
    n_variety,
    " structures"
  )

  return(annot_table_wei_bio)
}
