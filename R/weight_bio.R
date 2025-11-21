#' @title Weight bio
#'
#' @description This function weights MS annotations according to their biological
#'     source by comparing the taxonomic hierarchy of candidate structures'
#'     reported organisms with the sample's organism taxonomy. Higher taxonomic
#'     similarity results in higher biological scores.
#'
#' @include transform_score_sirius_csi.R
#'
#' @param annotation_table_taxed Data frame containing initial annotations with
#'     sample taxonomy information
#' @param structure_organism_pairs_table Data frame containing structure-organism
#'     pairs with complete taxonomic hierarchies
#' @param weight_spectral Numeric weight for spectral similarity score (0-1)
#' @param weight_biological Numeric weight for biological source score (0-1)
#' @param score_biological_domain Numeric score for domain-level taxonomic match
#' @param score_biological_kingdom Numeric score for kingdom-level match
#' @param score_biological_phylum Numeric score for phylum-level match
#' @param score_biological_class Numeric score for class-level match
#' @param score_biological_order Numeric score for order-level match
#' @param score_biological_family Numeric score for family-level match
#' @param score_biological_tribe Numeric score for tribe-level match
#' @param score_biological_genus Numeric score for genus-level match
#' @param score_biological_species Numeric score for species-level match
#' @param score_biological_variety Numeric score for variety-level match (highest)
#'
#' @return Data frame containing biologically weighted annotations with
#'     biological scores and combined weighted scores
#'
#' @examples NULL
weight_bio <- function(
  annotation_table_taxed = get(
    "annotation_table_taxed",
    envir = parent.frame()
  ),
  structure_organism_pairs_table = get(
    "structure_organism_pairs_table",
    envir = parent.frame()
  ),
  weight_spectral = get("weight_spectral", envir = parent.frame()),
  weight_biological = get("weight_biological", envir = parent.frame()),
  score_biological_domain = get(
    "score_biological_domain",
    envir = parent.frame()
  ),
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

  # Validate data frames
  if (!is.data.frame(annotation_table_taxed)) {
    stop("annotation_table_taxed must be a data frame")
  }

  if (!is.data.frame(structure_organism_pairs_table)) {
    stop("structure_organism_pairs_table must be a data frame")
  }

  # Early exit for empty input
  n_annotations <- nrow(annotation_table_taxed)
  if (n_annotations == 0L) {
    logger::log_warn(
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

  logger::log_info("Weighting {n_annotations} annotations by biological source")
  logger::log_debug(
    "Weights - Spectral: {weight_spectral}, Biological: {weight_biological}"
  )

  # Filter Structure-Organism Pairs ----

  # logger::log_trace("Filtering structure-organism pairs")
  df0 <- structure_organism_pairs_table |>
    tidytable::filter(!is.na(structure_inchikey_connectivity_layer)) |>
    tidytable::filter(!is.na(organism_taxonomy_ottid)) |>
    tidytable::select(
      candidate_structure_inchikey_connectivity_layer = structure_inchikey_connectivity_layer,
      candidate_organism_name = organism_name,
      candidate_organism_01_domain = organism_taxonomy_01domain,
      candidate_organism_02_kingdom = organism_taxonomy_02kingdom,
      candidate_organism_03_phylum = organism_taxonomy_03phylum,
      candidate_organism_04_class = organism_taxonomy_04class,
      candidate_organism_05_order = organism_taxonomy_05order,
      # candidate_organism_05_1_infraorder =
      # organism_taxonomy_05_1infraorder,
      candidate_organism_06_family = organism_taxonomy_06family,
      # candidate_organism_06_1_subfamily =
      # organism_taxonomy_06_1subfamily,
      candidate_organism_07_tribe = organism_taxonomy_07tribe,
      # candidate_organism_07_1_subtribe = organism_taxonomy_07_1subtribe,
      candidate_organism_08_genus = organism_taxonomy_08genus,
      # candidate_organism_08_1_subgenus = organism_taxonomy_08_1subgenus,
      candidate_organism_09_species = organism_taxonomy_09species,
      # candidate_organism_09_1_subspecies =
      # organism_taxonomy_09_1subspecies,
      candidate_organism_10_varietas = organism_taxonomy_10varietas
    ) |>
    tidytable::distinct() |>
    tidytable::mutate(tidytable::across(
      .cols = tidyselect::where(fn = is.character),
      .fns = ~ tidytable::na_if(x = .x, y = "")
    ))

  logger::log_debug("Filtered to {nrow(df0)} structure-organism pairs")

  # logger::log_trace("Preparing annotation table")
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

  # logger::log_trace("Calculating biological score at all levels ...")

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
    # logger::log_trace("... {tax_level$level}")

    df2 |>
      score_per_level_bio(
        candidates = tax_level$candidate,
        samples = tax_level$sample,
        score = tax_level$score,
        score_name = paste0("score_biological_", tax_level$num)
      )
  })

  # logger::log_trace("Keeping best biological score")

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
      .fns = ~ tidytable::replace_na(.x = .x, replace = "notClassified")
    )) |>
    tidytable::mutate(
      candidate_score_sirius_csi_tmp = transform_score_sirius_csi(
        candidate_score_sirius_csi |> as.numeric()
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
      score_weighted_bio = (1 / (weight_biological + weight_spectral)) *
        weight_biological *
        score_biological +
        (1 / (weight_biological + weight_spectral)) *
          weight_spectral *
          candidate_score_pseudo_initial
    )

  rm(annot_table_wei_bio_big)

  return(annot_table_wei_bio)
}
