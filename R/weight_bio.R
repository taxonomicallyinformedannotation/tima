#' @title Weight annotations by biological source
#'
#' @description Weights MS annotations according to their biological source by
#'     comparing the taxonomic hierarchy of candidate structures' reported
#'     organisms with the sample's organism taxonomy. Higher taxonomic
#'     similarity results in higher biological scores.
#'     Internal helper for weight_annotations().
#'
#' @details The weights are automatically normalized by dividing by their sum,
#'     so they do NOT need to sum to 1. For example, weights of (1, 1) produce
#'     the same result as (0.5, 0.5).
#'
#' @include transform_score_sirius_csi.R
#' @include validations_utils.R
#' @include weights_utils.R
#'
#' @param annotation_table_taxed Data frame with initial annotations and sample taxonomy
#' @param structure_organism_pairs_table Data frame with structure-organism pairs and taxonomies
#' @param weight_spectral Weight for spectral similarity score (any positive number)
#' @param weight_biological Weight for biological source score (any positive number)
#' @param score_biological_domain Score for domain-level taxonomic match (0-1)
#' @param score_biological_kingdom Score for kingdom-level match (0-1)
#' @param score_biological_phylum Score for phylum-level match (0-1)
#' @param score_biological_class Score for class-level match (0-1)
#' @param score_biological_order Score for order-level match (0-1)
#' @param score_biological_family Score for family-level match (0-1)
#' @param score_biological_tribe Score for tribe-level match (0-1)
#' @param score_biological_genus Score for genus-level match (0-1)
#' @param score_biological_species Score for species-level match (0-1)
#' @param score_biological_variety Score for variety-level match (0-1, highest)
#'
#' @return Data frame with biologically weighted annotations including biological
#'     scores and combined weighted scores
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Internal use only - called by weight_annotations()
#' weighted <- weight_bio(
#'   annotation_table_taxed = annotations,
#'   structure_organism_pairs_table = sop_table,
#'   weight_spectral = 1,      # Will be normalized to 0.5
#'   weight_biological = 1,    # Will be normalized to 0.5
#'   score_biological_domain = 0.01,
#'   score_biological_kingdom = 0.02,
#'   score_biological_phylum = 0.05,
#'   score_biological_class = 0.1,
#'   score_biological_order = 0.2,
#'   score_biological_family = 0.3,
#'   score_biological_tribe = 0.5,
#'   score_biological_genus = 0.7,
#'   score_biological_species = 0.9,
#'   score_biological_variety = 1.0
#' )
#' }
weight_bio <- function(
  annotation_table_taxed,
  structure_organism_pairs_table,
  weight_spectral,
  weight_biological,
  score_biological_domain,
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
  validate_dataframe(
    annotation_table_taxed,
    param_name = "annotation_table_taxed"
  )
  validate_dataframe(
    structure_organism_pairs_table,
    param_name = "structure_organism_pairs_table"
  )

  # Validate weights
  validate_weights(
    c(
      weight_spectral,
      weight_biological
    )
  )

  # Validate all biological score parameters (0-1 range)
  validate_numeric_range(
    score_biological_domain,
    0,
    1,
    param_name = "score_biological_domain"
  )
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

  # Check Data Frames ----

  if (!is.data.frame(structure_organism_pairs_table)) {
    stop("structure_organism_pairs_table must be a data frame")
  }

  # Early exit for empty input
  n_annotations <- nrow(annotation_table_taxed)
  if (n_annotations == 0L) {
    log_warn(
      "Empty annotation table provided, skipping biological weighting"
    )
    return(annotation_table_taxed)
  }

  # Validate weights
  weights <- list(
    spectral = weight_spectral,
    biological = weight_biological
  )

  is_valid_weight <- sapply(weights, function(w) {
    is.numeric(w) && w >= 0 && w <= 1
  })

  if (!all(is_valid_weight)) {
    invalid_weights <- names(weights)[!is_valid_weight]
    stop(
      "Weight(s) must be between 0 and 1: ",
      paste(invalid_weights, collapse = ", ")
    )
  }

  # Validate biological scores
  bio_scores <- list(
    domain = score_biological_domain,
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

  is_valid_score <- sapply(bio_scores, function(s) {
    is.numeric(s) && s >= 0 && s <= 1
  })

  if (!all(is_valid_score)) {
    invalid_scores <- names(bio_scores)[!is_valid_score]
    stop(
      "Biological score(s) must be between 0 and 1: ",
      paste(paste0("score_biological_", invalid_scores), collapse = ", ")
    )
  }

  # Log Processing Information ----

  log_info("Weighting %d annotations by biological source", n_annotations)
  log_debug(
    "Weights - Spectral: %f2, Biological: %f2",
    weight_spectral,
    weight_biological
  )

  # Filter Structure-Organism Pairs ----

  # log_trace("Filtering structure-organism pairs")
  df0 <- structure_organism_pairs_table |>
    tidytable::filter(!is.na(structure_inchikey_connectivity_layer)) |>
    tidytable::filter(!is.na(organism_taxonomy_ottid)) |>
    tidytable::select(
      tidyselect::all_of(
        x = c(
          "candidate_structure_inchikey_connectivity_layer" = "structure_inchikey_connectivity_layer",
          "candidate_organism_name" = "organism_name",
          "candidate_organism_01_domain" = "organism_taxonomy_01domain",
          "candidate_organism_02_kingdom" = "organism_taxonomy_02kingdom",
          "candidate_organism_03_phylum" = "organism_taxonomy_03phylum",
          "candidate_organism_04_class" = "organism_taxonomy_04class",
          "candidate_organism_05_order" = "organism_taxonomy_05order",
          # "candidate_organism_05_1_infraorder" = "organism_taxonomy_05_1infraorder",
          "candidate_organism_06_family" = "organism_taxonomy_06family",
          # "candidate_organism_06_1_subfamily" = "organism_taxonomy_06_1subfamily",
          "candidate_organism_07_tribe" = "organism_taxonomy_07tribe",
          # "candidate_organism_07_1_subtribe" = "organism_taxonomy_07_1subtribe",
          "candidate_organism_08_genus" = "organism_taxonomy_08genus",
          # "candidate_organism_08_1_subgenus" = "organism_taxonomy_08_1subgenus",
          "candidate_organism_09_species" = "organism_taxonomy_09species",
          # "candidate_organism_09_1_subspecies" = "organism_taxonomy_09_1subspecies",
          "candidate_organism_10_varietas" = "organism_taxonomy_10varietas"
        )
      )
    ) |>
    tidytable::distinct() |>
    tidytable::mutate(tidytable::across(
      .cols = tidyselect::where(fn = is.character),
      .fns = ~ tidytable::na_if(x = .x, y = "")
    ))

  log_debug("Filtered to %d structure-organism pairs", nrow(df0))

  # log_trace("Preparing annotation table")
  df1 <- annotation_table_taxed |>
    tidytable::select(
      candidate_structure_inchikey_connectivity_layer,
      sample_organism_name
    ) |>
    tidytable::distinct() |>
    tidytable::left_join(
      y = df0 |>
        tidytable::select(
          candidate_structure_inchikey_connectivity_layer,
          candidate_organism_name
        ) |>
        tidytable::distinct()
    )

  df2 <- df1 |>
    tidytable::inner_join(
      y = annotation_table_taxed |>
        tidytable::select(
          sample_organism_name,
          sample_organism_01_domain,
          sample_organism_02_kingdom,
          sample_organism_03_phylum,
          sample_organism_04_class,
          sample_organism_05_order,
          # sample_organism_05_1_infraorder,
          sample_organism_06_family,
          # sample_organism_06_1_subfamily,
          sample_organism_07_tribe,
          # sample_organism_07_1_subtribe,
          sample_organism_08_genus,
          # sample_organism_08_1_subgenus,
          sample_organism_09_species,
          # sample_organism_09_1_subspecies,
          sample_organism_10_varietas
        ) |>
        tidytable::distinct()
    ) |>
    tidytable::inner_join(y = df0) |>
    tidytable::distinct(
      sample_organism_name,
      sample_organism_01_domain,
      sample_organism_02_kingdom,
      sample_organism_03_phylum,
      sample_organism_04_class,
      sample_organism_05_order,
      # sample_organism_05_1_infraorder,
      sample_organism_06_family,
      # sample_organism_06_1_subfamily,
      sample_organism_07_tribe,
      # sample_organism_07_1_subtribe,
      sample_organism_08_genus,
      # sample_organism_08_1_subgenus,
      sample_organism_09_species,
      # sample_organism_09_1_subspecies,
      sample_organism_10_varietas,
      candidate_organism_name,
      candidate_organism_01_domain,
      candidate_organism_02_kingdom,
      candidate_organism_03_phylum,
      candidate_organism_04_class,
      candidate_organism_05_order,
      # candidate_organism_05_1_infraorder,
      candidate_organism_06_family,
      # candidate_organism_06_1_subfamily,
      candidate_organism_07_tribe,
      # candidate_organism_07_1_subtribe,
      candidate_organism_08_genus,
      # candidate_organism_08_1_subgenus,
      candidate_organism_09_species,
      # candidate_organism_09_1_subspecies,
      candidate_organism_10_varietas
    )
  rm(df0)

  # Helper Function for Taxonomic Level Scoring ----

  score_per_level_bio <- function(df, candidates, samples, score, score_name) {
    df |>
      tidytable::distinct(!!as.name(candidates), !!as.name(samples)) |>
      tidytable::filter(!is.na(!!as.name(samples))) |>
      tidytable::filter(!!as.name(samples) != "ND") |>
      tidytable::filter(!is.na(!!as.name(candidates))) |>
      tidytable::filter(!!as.name(candidates) != "notClassified") |>
      tidytable::filter(
        stringi::stri_detect_regex(
          pattern = !!as.name(candidates),
          str = !!as.name(samples)
        )
      ) |>
      tidytable::mutate(
        !!as.name(score_name) := tidytable::if_else(
          condition = !!as.name(samples) != "notClassified",
          true = score * 1,
          false = 0
        )
      )
  }

  # Calculate Biological Scores at All Taxonomic Levels ----

  # log_trace("Calculating biological score at all levels ...")

  # Define all taxonomic levels and their scores (DRY principle)
  taxonomic_levels <- list(
    list(
      level = "domain",
      num = "01",
      candidate = "candidate_organism_01_domain",
      sample = "sample_organism_01_domain",
      score = score_biological_domain
    ),
    list(
      level = "kingdom",
      num = "02",
      candidate = "candidate_organism_02_kingdom",
      sample = "sample_organism_02_kingdom",
      score = score_biological_kingdom
    ),
    list(
      level = "phylum",
      num = "03",
      candidate = "candidate_organism_03_phylum",
      sample = "sample_organism_03_phylum",
      score = score_biological_phylum
    ),
    list(
      level = "class",
      num = "04",
      candidate = "candidate_organism_04_class",
      sample = "sample_organism_04_class",
      score = score_biological_class
    ),
    list(
      level = "order",
      num = "05",
      candidate = "candidate_organism_05_order",
      sample = "sample_organism_05_order",
      score = score_biological_order
    ),
    list(
      level = "family",
      num = "06",
      candidate = "candidate_organism_06_family",
      sample = "sample_organism_06_family",
      score = score_biological_family
    ),
    list(
      level = "tribe",
      num = "07",
      candidate = "candidate_organism_07_tribe",
      sample = "sample_organism_07_tribe",
      score = score_biological_tribe
    ),
    list(
      level = "genus",
      num = "08",
      candidate = "candidate_organism_08_genus",
      sample = "sample_organism_08_genus",
      score = score_biological_genus
    ),
    list(
      level = "species",
      num = "09",
      candidate = "candidate_organism_09_species",
      sample = "sample_organism_09_species",
      score = score_biological_species
    ),
    list(
      level = "varietas",
      num = "10",
      candidate = "candidate_organism_10_varietas",
      sample = "sample_organism_10_varietas",
      score = score_biological_variety
    )
  )

  # Calculate scores for all levels
  supp_tables <- lapply(taxonomic_levels, function(tax_level) {
    # log_trace("... %s", tax_level$level)

    df2 |>
      score_per_level_bio(
        candidates = tax_level$candidate,
        samples = tax_level$sample,
        score = tax_level$score,
        score_name = paste0("score_biological_", tax_level$num)
      )
  })

  # log_trace("Keeping best biological score")

  annot_table_wei_bio_init <- purrr::reduce(
    .x = supp_tables,
    .init = df2,
    .f = tidytable::left_join
  ) |>
    tidytable::select(
      sample_organism_name,
      -tidyselect::contains(match = "sample_organism_0"),
      tidyselect::contains(match = "candidate_organism"),
      tidyselect::contains(match = "score")
    )
  rm(supp_tables, df2)

  annot_table_wei_bio_init <- annot_table_wei_bio_init |>
    tidytable::mutate(
      score_biological = pmax(
        score_biological_01,
        score_biological_02,
        score_biological_03,
        score_biological_04,
        score_biological_05,
        # score_biological_05_1,
        score_biological_06,
        # score_biological_06_1,
        score_biological_07,
        # score_biological_07_1,
        score_biological_08,
        # score_biological_08_1,
        score_biological_09,
        # score_biological_09_1,
        score_biological_10,
        0,
        na.rm = TRUE
      )
    ) |>
    tidytable::select(-tidyselect::contains(match = "score_biological_")) |>
    # Ensure all organism columns are character type (fixes NA type mismatch)
    tidytable::mutate(
      tidytable::across(
        .cols = tidyselect::starts_with(match = "candidate_organism_"),
        .fns = as.character
      )
    ) |>
    tidytable::mutate(
      candidate_structure_organism_occurrence_closest = tidytable::case_when(
        score_biological == score_biological_domain ~
          candidate_organism_01_domain,
        score_biological == score_biological_kingdom ~
          candidate_organism_02_kingdom,
        score_biological == score_biological_phylum ~
          candidate_organism_03_phylum,
        score_biological == score_biological_class ~
          candidate_organism_04_class,
        score_biological == score_biological_order ~
          candidate_organism_05_order,
        # score_biological == score_biological_05_1 ~
        # candidate_organism_05_1_infraorder,
        score_biological == score_biological_family ~
          candidate_organism_06_family,
        # score_biological == score_biological_06_1 ~
        # candidate_organism_06_1_subfamily,
        score_biological == score_biological_tribe ~
          candidate_organism_07_tribe,
        # score_biological == score_biological_07_1 ~
        # candidate_organism_07_1_subtribe,
        score_biological == score_biological_genus ~
          candidate_organism_08_genus,
        # score_biological == score_biological_08_1 ~
        # candidate_organism_08_1_subgenus,
        score_biological == score_biological_species ~
          candidate_organism_09_species,
        # score_biological == score_biological_09_1 ~
        # candidate_organism_09_1_subspecies,
        score_biological == score_biological_variety ~
          candidate_organism_10_varietas,
        .default = NA_character_
      )
    ) |>
    tidytable::filter(
      !is.na(candidate_structure_organism_occurrence_closest)
    ) |>
    tidytable::select(
      sample_organism_name,
      candidate_organism_name,
      score_biological,
      candidate_structure_organism_occurrence_closest
    ) |>
    tidytable::distinct()
  annot_table_wei_bio_init <- annot_table_wei_bio_init |>
    tidytable::right_join(y = df1) |>
    tidytable::mutate(
      score_biological = tidytable::if_else(
        condition = is.na(score_biological),
        true = 0,
        false = score_biological
      )
    )
  rm(df1)

  annot_table_wei_bio_init <- annot_table_wei_bio_init |>
    tidytable::arrange(
      score_biological |>
        tidytable::desc()
    ) |>
    tidytable::distinct(
      candidate_structure_inchikey_connectivity_layer,
      sample_organism_name,
      .keep_all = TRUE
    )

  annot_table_wei_bio_big <- annot_table_wei_bio_init |>
    tidytable::inner_join(y = annotation_table_taxed) |>
    tidytable::select(
      -tidyselect::contains(match = "candidate_organism"),
      -tidyselect::contains(match = "sample_organism")
    )
  rm(
    annot_table_wei_bio_init,
    annotation_table_taxed
  )

  annot_table_wei_bio <- annot_table_wei_bio_big |>
    tidytable::mutate(tidytable::across(
      .cols = tidyselect::matches(match = "candidate_structure_tax"),
      .fns = ~ tidytable::replace_na(
        .x = .x,
        replace = "notClassified"
      )
    )) |>
    tidytable::mutate(
      candidate_score_sirius_csi_tmp = transform_score_sirius_csi(
        candidate_score_sirius_csi |>
          as.numeric()
      )
    ) |>
    tidytable::mutate(
      candidate_score_pseudo_initial = tidytable::case_when(
        !is.na(candidate_score_similarity) &
          !is.na(candidate_score_sirius_csi) ~
          (as.numeric(candidate_score_similarity) +
            candidate_score_sirius_csi_tmp) /
            2,
        !is.na(candidate_score_similarity) ~
          as.numeric(candidate_score_similarity),
        !is.na(candidate_score_sirius_csi) ~ candidate_score_sirius_csi_tmp,
        TRUE ~ 0
      )
    ) |>
    tidytable::select(-candidate_score_sirius_csi_tmp) |>
    tidytable::mutate(
      score_weighted_bio = compute_weighted_sum(
        score_biological,
        candidate_score_pseudo_initial,
        weights = c(weight_biological, weight_spectral)
      )
    )

  rm(annot_table_wei_bio_big)

  return(annot_table_wei_bio)
}
