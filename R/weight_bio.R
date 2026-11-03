#' @title Weight annotations by biological source
#'
#' @description Weights MS annotations according to their biological source by
#'     comparing the taxonomic hierarchy of candidate structures' reported
#'     organisms with the sample's organism taxonomy. Higher taxonomic
#'     similarity results in higher biological scores.
#'
#'     **Special case: Biota domain** - Candidates from the "Biota" domain
#'     (organism_taxonomy_01domain == "Biota") represent shared core metabolism
#'     found across all life forms. These candidates ALWAYS receive the maximum
#'     biological score, ensuring they are considered in all annotations
#' regardless of sample taxonomy. This recognizes universal metabolic pathways
#' like glycolysis, citric acid cycle, and other fundamental biochemical
#'     processes.
#'
#'     Internal helper for weight_annotations().
#'
#' @details The weights are automatically normalized by dividing by their sum,
#'     so they do NOT need to sum to 1. For example, weights of (1, 1) produce
#'     the same result as (0.5, 0.5).
#'
#' @include transform_score_sirius_csi.R
#' @include validations_utils.R
#' @include weights_utils.R
#' @include logs_utils.R
#' @include columns_utils.R
#' @include predicates_utils.R
#' @include validations_params.R
#'
#' @param annotation_table_taxed [data.frame] Data frame with initial
#'     annotations and sample taxonomy
#' @param structure_organism_pairs_table [data.frame] Data frame with
#'     structure-organism pairs and taxonomies
#' @param weight_spectral [numeric] Weight for spectral similarity score (any
#'     positive number)
#' @param weight_biological [numeric] Weight for biological source score (any
#'     positive number)
#' @param score_biological_domain [numeric] Score for domain-level taxonomic
#'     match (0-1)
#' @param score_biological_kingdom [numeric] Score for kingdom-level match (0-1)
#' @param score_biological_phylum [numeric] Score for phylum-level match (0-1)
#' @param score_biological_class [numeric] Score for class-level match (0-1)
#' @param score_biological_order [numeric] Score for order-level match (0-1)
#' @param score_biological_family [numeric] Score for family-level match (0-1)
#' @param score_biological_tribe [numeric] Score for tribe-level match (0-1)
#' @param score_biological_genus [numeric] Score for genus-level match (0-1)
#' @param score_biological_species [numeric] Score for species-level match (0-1)
#' @param score_biological_variety [numeric] Score for variety-level match (0-1,
#'     highest)
#' @param score_biological_biota [numeric] Score for Biota-level match (0-1,
#'     special)
#'
#' @return Data frame with biologically weighted annotations including
#'     biological
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
#'   weight_spectral = 1, # Will be normalized to 0.5
#'   weight_biological = 1, # Will be normalized to 0.5
#'   score_biological_domain = 0.01,
#'   score_biological_kingdom = 0.02,
#'   score_biological_phylum = 0.05,
#'   score_biological_class = 0.1,
#'   score_biological_order = 0.2,
#'   score_biological_family = 0.3,
#'   score_biological_tribe = 0.5,
#'   score_biological_genus = 0.7,
#'   score_biological_species = 0.9,
#'   score_biological_variety = 1.0,
#'   score_biological_biota = 1.0
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
  score_biological_variety,
  score_biological_biota
) {
  ctx <- log_operation(
    "weight_bio",
    n_annotations = nrow(annotation_table_taxed),
    n_sop = nrow(structure_organism_pairs_table)
  )
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

  annotation_table_taxed <- select_weight_annotations_working_columns(
    annotation_table_taxed
  )
  structure_organism_pairs_table <- select_structure_organism_pair_columns(
    structure_organism_pairs_table
  )

  # Check Data Frames ----

  if (!is.data.frame(structure_organism_pairs_table)) {
    cli::cli_abort(
      "structure_organism_pairs_table must be a data frame",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
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

  is_valid_weight <- vapply(
    X = weights,
    FUN = function(w) is_numeric_in_range(w, 0, 1),
    logical(1)
  )

  if (!all(is_valid_weight)) {
    invalid_weights <- names(weights)[!is_valid_weight]
    cli::cli_abort(
      c(
        "weight(s) must be between 0 and 1",
        "x" = paste(invalid_weights, collapse = ", ")
      ),
      class = c("tima_validation_error", "tima_error"),
      call = NULL
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

  is_valid_score <- vapply(
    X = bio_scores,
    FUN = function(s) is_numeric_in_range(s, 0, 1),
    logical(1)
  )

  if (!all(is_valid_score)) {
    invalid_scores <- names(bio_scores)[!is_valid_score]
    cli::cli_abort(
      c(
        "biological score(s) must be between 0 and 1",
        "x" = paste(
          paste0("score_biological_", invalid_scores),
          collapse = ", "
        )
      ),
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  # Log Processing Information ----

  log_info("Weighting %d annotations by biological source", n_annotations)
  log_debug(
    "Weights - Spectral: %.2f, Biological: %.2f",
    weight_spectral,
    weight_biological
  )

  # Filter Structure-Organism Pairs ----

  # Predicate pushdown optimization: Filter structure-organism pairs to only
  # those present in the annotation table before joining. This can reduce the
  # right-side table size, significantly reducing memory for the subsequent
  # many-to-many joins.
  needed_structures <- annotation_table_taxed |>
    tidytable::distinct(candidate_structure_inchikey_connectivity_layer) |>
    tidytable::pull(candidate_structure_inchikey_connectivity_layer)
  # Remove NAs from needed_structures
  needed_structures <- needed_structures[!is.na(needed_structures)]

  df0 <- if (length(needed_structures) > 0L) {
    structure_organism_pairs_table |>
      tidytable::filter(
        structure_inchikey_connectivity_layer %in% needed_structures
      ) |>
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
            # "candidate_organism_05_1_infraorder" =
            # "organism_taxonomy_05_1infraorder",
            "candidate_organism_06_family" = "organism_taxonomy_06family",
            # "candidate_organism_06_1_subfamily" =
            # "organism_taxonomy_06_1subfamily",
            "candidate_organism_07_tribe" = "organism_taxonomy_07tribe",
            # "candidate_organism_07_1_subtribe" =
            # "organism_taxonomy_07_1subtribe",
            "candidate_organism_08_genus" = "organism_taxonomy_08genus",
            # "candidate_organism_08_1_subgenus" =
            # "organism_taxonomy_08_1subgenus",
            "candidate_organism_09_species" = "organism_taxonomy_09species",
            # "candidate_organism_09_1_subspecies" =
            # "organism_taxonomy_09_1subspecies",
            "candidate_organism_10_varietas" = "organism_taxonomy_10varietas"
          )
        )
      ) |>
      tidytable::mutate(tidytable::across(
        .cols = tidyselect::where(fn = is.character),
        .fns = ~ tidytable::na_if(x = .x, y = "")
      )) |>
      tidytable::distinct()
  } else {
    # Fallback if no structures found: use original behavior
    structure_organism_pairs_table |>
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
            # "candidate_organism_05_1_infraorder" =
            # "organism_taxonomy_05_1infraorder",
            "candidate_organism_06_family" = "organism_taxonomy_06family",
            # "candidate_organism_06_1_subfamily" =
            # "organism_taxonomy_06_1subfamily",
            "candidate_organism_07_tribe" = "organism_taxonomy_07tribe",
            # "candidate_organism_07_1_subtribe" =
            # "organism_taxonomy_07_1subtribe",
            "candidate_organism_08_genus" = "organism_taxonomy_08genus",
            # "candidate_organism_08_1_subgenus" =
            # "organism_taxonomy_08_1subgenus",
            "candidate_organism_09_species" = "organism_taxonomy_09species",
            # "candidate_organism_09_1_subspecies" =
            # "organism_taxonomy_09_1subspecies",
            "candidate_organism_10_varietas" = "organism_taxonomy_10varietas"
          )
        )
      ) |>
      tidytable::mutate(tidytable::across(
        .cols = tidyselect::where(fn = is.character),
        .fns = ~ tidytable::na_if(x = .x, y = "")
      )) |>
      tidytable::distinct()
  }

  log_debug("Filtered to %d structure-organism pairs", nrow(df0))

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
        tidytable::distinct(),
      by = "candidate_structure_inchikey_connectivity_layer"
    )

  # Collapse df1 (~1.5M structure-organism rows) to distinct
  # (sample_organism_name, candidate_organism_name) pairs (<=37k rows) BEFORE
  # joining wide 20-column taxonomy metadata.
  sample_tax_distinct <- annotation_table_taxed |>
    tidytable::select(
      sample_organism_name,
      sample_organism_01_domain,
      sample_organism_02_kingdom,
      sample_organism_03_phylum,
      sample_organism_04_class,
      sample_organism_05_order,
      sample_organism_06_family,
      sample_organism_07_tribe,
      sample_organism_08_genus,
      sample_organism_09_species,
      sample_organism_10_varietas
    ) |>
    tidytable::distinct()

  cand_tax_distinct <- df0 |>
    tidytable::select(-candidate_structure_inchikey_connectivity_layer) |>
    tidytable::distinct()
  rm(df0)

  df2 <- df1 |>
    tidytable::distinct(sample_organism_name, candidate_organism_name) |>
    tidytable::filter(
      !is.na(sample_organism_name) & !is.na(candidate_organism_name)
    ) |>
    tidytable::inner_join(
      y = sample_tax_distinct,
      by = "sample_organism_name"
    ) |>
    tidytable::inner_join(
      y = cand_tax_distinct,
      by = "candidate_organism_name"
    ) |>
    tidytable::distinct(
      sample_organism_name,
      sample_organism_01_domain,
      sample_organism_02_kingdom,
      sample_organism_03_phylum,
      sample_organism_04_class,
      sample_organism_05_order,
      sample_organism_06_family,
      sample_organism_07_tribe,
      sample_organism_08_genus,
      sample_organism_09_species,
      sample_organism_10_varietas,
      candidate_organism_name,
      candidate_organism_01_domain,
      candidate_organism_02_kingdom,
      candidate_organism_03_phylum,
      candidate_organism_04_class,
      candidate_organism_05_order,
      candidate_organism_06_family,
      candidate_organism_07_tribe,
      candidate_organism_08_genus,
      candidate_organism_09_species,
      candidate_organism_10_varietas
    )
  rm(sample_tax_distinct, cand_tax_distinct)

  # OPTIMIZATION: Pre-compute the distinct pairs once to avoid 10x distinct+filter+regex
  df_pairs <- df2 |>
    tidytable::distinct(
      sample_organism_name,
      candidate_organism_name,
      candidate_organism_01_domain,
      candidate_organism_02_kingdom,
      candidate_organism_03_phylum,
      candidate_organism_04_class,
      candidate_organism_05_order,
      candidate_organism_06_family,
      candidate_organism_07_tribe,
      candidate_organism_08_genus,
      candidate_organism_09_species,
      candidate_organism_10_varietas,
      sample_organism_01_domain,
      sample_organism_02_kingdom,
      sample_organism_03_phylum,
      sample_organism_04_class,
      sample_organism_05_order,
      sample_organism_06_family,
      sample_organism_07_tribe,
      sample_organism_08_genus,
      sample_organism_09_species,
      sample_organism_10_varietas
    ) |>
    tidytable::filter(!is.na(sample_organism_name)) |>
    tidytable::filter(sample_organism_name != "ND") |>
    tidytable::filter(!is.na(candidate_organism_name)) |>
    tidytable::filter(candidate_organism_name != "notClassified")

  # Single pass: compute all level scores at once using vectorized operations
  # This avoids 10 separate tidytable pipelines with regex matching
  df_pairs <- df_pairs |>
    tidytable::mutate(
      # Pre-compute valid flags for each level
      .valid_01 = !is.na(candidate_organism_01_domain) &
        candidate_organism_01_domain != "notClassified" &
        sample_organism_01_domain != "notClassified",
      .valid_02 = !is.na(candidate_organism_02_kingdom) &
        candidate_organism_02_kingdom != "notClassified" &
        sample_organism_02_kingdom != "notClassified",
      .valid_03 = !is.na(candidate_organism_03_phylum) &
        candidate_organism_03_phylum != "notClassified" &
        sample_organism_03_phylum != "notClassified",
      .valid_04 = !is.na(candidate_organism_04_class) &
        candidate_organism_04_class != "notClassified" &
        sample_organism_04_class != "notClassified",
      .valid_05 = !is.na(candidate_organism_05_order) &
        candidate_organism_05_order != "notClassified" &
        sample_organism_05_order != "notClassified",
      .valid_06 = !is.na(candidate_organism_06_family) &
        candidate_organism_06_family != "notClassified" &
        sample_organism_06_family != "notClassified",
      .valid_07 = !is.na(candidate_organism_07_tribe) &
        candidate_organism_07_tribe != "notClassified" &
        sample_organism_07_tribe != "notClassified",
      .valid_08 = !is.na(candidate_organism_08_genus) &
        candidate_organism_08_genus != "notClassified" &
        sample_organism_08_genus != "notClassified",
      .valid_09 = !is.na(candidate_organism_09_species) &
        candidate_organism_09_species != "notClassified" &
        sample_organism_09_species != "notClassified",
      .valid_10 = !is.na(candidate_organism_10_varietas) &
        candidate_organism_10_varietas != "notClassified" &
        sample_organism_10_varietas != "notClassified"
    )

  # Compute exact token matches for all valid pairs at once (much faster than 10 separate passes)
  # Only match where both candidate and sample are valid
  if (nrow(df_pairs) > 0L) {
    # Domain level
    mask <- df_pairs$.valid_01
    if (any(mask, na.rm = TRUE)) {
      df_pairs$score_biological_01 <- 0
      df_pairs$score_biological_01[mask] <- as.integer(
        stri_detect_token(
          pattern = df_pairs$candidate_organism_01_domain[mask],
          str = df_pairs$sample_organism_01_domain[mask]
        )
      ) *
        score_biological_domain
    } else {
      df_pairs$score_biological_01 <- 0
    }

    # Kingdom level
    mask <- df_pairs$.valid_02
    if (any(mask, na.rm = TRUE)) {
      df_pairs$score_biological_02 <- 0
      df_pairs$score_biological_02[mask] <- as.integer(
        stri_detect_token(
          pattern = df_pairs$candidate_organism_02_kingdom[mask],
          str = df_pairs$sample_organism_02_kingdom[mask]
        )
      ) *
        score_biological_kingdom
    } else {
      df_pairs$score_biological_02 <- 0
    }

    # Phylum level
    mask <- df_pairs$.valid_03
    if (any(mask, na.rm = TRUE)) {
      df_pairs$score_biological_03 <- 0
      df_pairs$score_biological_03[mask] <- as.integer(
        stri_detect_token(
          pattern = df_pairs$candidate_organism_03_phylum[mask],
          str = df_pairs$sample_organism_03_phylum[mask]
        )
      ) *
        score_biological_phylum
    } else {
      df_pairs$score_biological_03 <- 0
    }

    # Class level
    mask <- df_pairs$.valid_04
    if (any(mask, na.rm = TRUE)) {
      df_pairs$score_biological_04 <- 0
      df_pairs$score_biological_04[mask] <- as.integer(
        stri_detect_token(
          pattern = df_pairs$candidate_organism_04_class[mask],
          str = df_pairs$sample_organism_04_class[mask]
        )
      ) *
        score_biological_class
    } else {
      df_pairs$score_biological_04 <- 0
    }

    # Order level
    mask <- df_pairs$.valid_05
    if (any(mask, na.rm = TRUE)) {
      df_pairs$score_biological_05 <- 0
      df_pairs$score_biological_05[mask] <- as.integer(
        stri_detect_token(
          pattern = df_pairs$candidate_organism_05_order[mask],
          str = df_pairs$sample_organism_05_order[mask]
        )
      ) *
        score_biological_order
    } else {
      df_pairs$score_biological_05 <- 0
    }

    # Family level
    mask <- df_pairs$.valid_06
    if (any(mask, na.rm = TRUE)) {
      df_pairs$score_biological_06 <- 0
      df_pairs$score_biological_06[mask] <- as.integer(
        stri_detect_token(
          pattern = df_pairs$candidate_organism_06_family[mask],
          str = df_pairs$sample_organism_06_family[mask]
        )
      ) *
        score_biological_family
    } else {
      df_pairs$score_biological_06 <- 0
    }

    # Tribe level
    mask <- df_pairs$.valid_07
    if (any(mask, na.rm = TRUE)) {
      df_pairs$score_biological_07 <- 0
      df_pairs$score_biological_07[mask] <- as.integer(
        stri_detect_token(
          pattern = df_pairs$candidate_organism_07_tribe[mask],
          str = df_pairs$sample_organism_07_tribe[mask]
        )
      ) *
        score_biological_tribe
    } else {
      df_pairs$score_biological_07 <- 0
    }

    # Genus level
    mask <- df_pairs$.valid_08
    if (any(mask, na.rm = TRUE)) {
      df_pairs$score_biological_08 <- 0
      df_pairs$score_biological_08[mask] <- as.integer(
        stri_detect_token(
          pattern = df_pairs$candidate_organism_08_genus[mask],
          str = df_pairs$sample_organism_08_genus[mask]
        )
      ) *
        score_biological_genus
    } else {
      df_pairs$score_biological_08 <- 0
    }

    # Species level
    mask <- df_pairs$.valid_09
    if (any(mask, na.rm = TRUE)) {
      df_pairs$score_biological_09 <- 0
      df_pairs$score_biological_09[mask] <- as.integer(
        stri_detect_token(
          pattern = df_pairs$candidate_organism_09_species[mask],
          str = df_pairs$sample_organism_09_species[mask]
        )
      ) *
        score_biological_species
    } else {
      df_pairs$score_biological_09 <- 0
    }

    # Varietas level
    mask <- df_pairs$.valid_10
    if (any(mask, na.rm = TRUE)) {
      df_pairs$score_biological_10 <- 0
      df_pairs$score_biological_10[mask] <- as.integer(
        stri_detect_token(
          pattern = df_pairs$candidate_organism_10_varietas[mask],
          str = df_pairs$sample_organism_10_varietas[mask]
        )
      ) *
        score_biological_variety
    } else {
      df_pairs$score_biological_10 <- 0
    }
  } else {
    # Empty case - initialize all score columns to 0
    for (i in 1:10) {
      df_pairs[[paste0("score_biological_", sprintf("%02d", i))]] <- numeric(0)
    }
  }

  # Clean up temporary columns
  df_pairs <- df_pairs |>
    tidytable::select(
      -tidyselect::starts_with(".valid_")
    )

  # Join back to df2 using the pair keys
  df2 <- df2 |>
    tidytable::left_join(
      y = df_pairs |>
        tidytable::select(
          sample_organism_name,
          candidate_organism_name,
          tidyselect::starts_with("score_biological_")
        ),
      by = c("sample_organism_name", "candidate_organism_name")
    )

  rm(df_pairs)

  # df2 already has all score_biological_* columns from the df_pairs join above,
  # so the old score_per_level_bio / taxonomic_levels / purrr::reduce code was
  # redundant dead code that re-computed the same columns — removed.

  annot_table_wei_bio_init <- df2 |>
    tidytable::select(
      sample_organism_name,
      -tidyselect::contains(match = "sample_organism_0"),
      -tidyselect::contains(match = "sample_organism_1"),
      tidyselect::contains(match = "candidate_organism"),
      tidyselect::contains(match = "score")
    )
  rm(df2)

  annot_table_wei_bio_init <- annot_table_wei_bio_init |>
    tidytable::mutate(
      score_biological = pmax(
        pmax(
          score_biological_01,
          score_biological_02,
          score_biological_03,
          score_biological_04,
          na.rm = TRUE
        ),
        pmax(
          score_biological_05,
          score_biological_06,
          score_biological_07,
          score_biological_08,
          na.rm = TRUE
        ),
        # pmax(score_biological_05_1, score_biological_06_1, score_biological_07_1, score_biological_08_1, score_biological_09_1, na.rm = TRUE),
        pmax(score_biological_09, score_biological_10, na.rm = TRUE),
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
    # Special handling for "Biota" domain (shared core metabolism)
    # Organisms from Biota always get maximum biological score (1.0)
    # These represent universal metabolites found across all life
    tidytable::mutate(
      score_biological = tidytable::if_else(
        condition = !is.na(candidate_organism_01_domain) &
          candidate_organism_01_domain == "Biota",
        true = score_biological_biota,
        false = score_biological
      )
    ) |>
    tidytable::mutate(
      candidate_structure_organism_occurrence_closest = tidytable::case_when(
        # Biota gets special treatment - always considered closest match
        !is.na(candidate_organism_01_domain) &
          candidate_organism_01_domain == "Biota" ~
          "Biota",
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
    tidytable::right_join(
      y = df1,
      by = c(
        "sample_organism_name",
        "candidate_organism_name"
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

  # Attach biological scores onto annotation_table_taxed in-place via fmatch
  # (avoids 7 full-table copies across up to 14.5M rows).
  annot_table_wei_bio <- tidytable::as_tidytable(annotation_table_taxed)
  rm(annotation_table_taxed)

  n_ann <- nrow(annot_table_wei_bio)
  if (nrow(annot_table_wei_bio_init) > 0L && n_ann > 0L) {
    u_so <- unique(c(
      annot_table_wei_bio$sample_organism_name,
      annot_table_wei_bio_init$sample_organism_name
    ))
    if (length(u_so) == 1L && !is.na(u_so[[1L]])) {
      m_bio <- fastmatch::fmatch(
        annot_table_wei_bio$candidate_structure_inchikey_connectivity_layer,
        annot_table_wei_bio_init$candidate_structure_inchikey_connectivity_layer
      )
    } else {
      lhs_key <- paste(
        annot_table_wei_bio$candidate_structure_inchikey_connectivity_layer,
        annot_table_wei_bio$sample_organism_name,
        sep = "\r"
      )
      rhs_key <- paste(
        annot_table_wei_bio_init$candidate_structure_inchikey_connectivity_layer,
        annot_table_wei_bio_init$sample_organism_name,
        sep = "\r"
      )
      m_bio <- fastmatch::fmatch(lhs_key, rhs_key)
    }
    score_bio_v <- annot_table_wei_bio_init$score_biological[m_bio]
    occ_closest_v <- annot_table_wei_bio_init$candidate_structure_organism_occurrence_closest[
      m_bio
    ]
  } else {
    score_bio_v <- rep(NA_real_, n_ann)
    occ_closest_v <- rep(NA_character_, n_ann)
  }
  rm(annot_table_wei_bio_init)

  drop_org_cols <- grep(
    "^(candidate_organism|sample_organism)",
    names(annot_table_wei_bio),
    value = TRUE
  )
  if (length(drop_org_cols) > 0L) {
    data.table::set(annot_table_wei_bio, j = drop_org_cols, value = NULL)
  }
  data.table::set(
    annot_table_wei_bio,
    j = "score_biological",
    value = score_bio_v
  )
  data.table::set(
    annot_table_wei_bio,
    j = "candidate_structure_organism_occurrence_closest",
    value = occ_closest_v
  )
  rm(score_bio_v, occ_closest_v)

  if (!"candidate_score_sirius_confidence" %in% names(annot_table_wei_bio)) {
    data.table::set(
      annot_table_wei_bio,
      j = "candidate_score_sirius_confidence",
      value = rep(NA_real_, n_ann)
    )
  }

  csi_tmp <- transform_score_sirius_csi(
    as.numeric(annot_table_wei_bio$candidate_score_sirius_csi)
  )
  conf_num <- as.numeric(annot_table_wei_bio$candidate_score_sirius_confidence)
  sirius_tmp <- ifelse(!is.na(conf_num), conf_num, csi_tmp)
  sim_num <- as.numeric(annot_table_wei_bio$candidate_score_similarity)

  has_sim <- !is.na(sim_num)
  has_sir <- !is.na(sirius_tmp)
  pseudo_init <- rep(NA_real_, n_ann)
  both_idx <- which(has_sim & has_sir)
  if (length(both_idx) > 0L) {
    pseudo_init[both_idx] <- (sim_num[both_idx] + sirius_tmp[both_idx]) / 2
  }
  sim_only <- which(has_sim & !has_sir)
  if (length(sim_only) > 0L) {
    pseudo_init[sim_only] <- sim_num[sim_only]
  }
  sir_only <- which(!has_sim & has_sir)
  if (length(sir_only) > 0L) {
    pseudo_init[sir_only] <- sirius_tmp[sir_only]
  }
  rm(csi_tmp, conf_num, sirius_tmp, sim_num, has_sim, has_sir)

  data.table::set(
    annot_table_wei_bio,
    j = "candidate_score_pseudo_initial",
    value = pseudo_init
  )
  data.table::set(
    annot_table_wei_bio,
    j = "score_weighted_bio",
    value = compute_weighted_sum(
      annot_table_wei_bio$score_biological,
      pseudo_init,
      weights = c(weight_biological, weight_spectral)
    )
  )
  rm(pseudo_init)

  log_complete(ctx, n_weighted = nrow(annot_table_wei_bio))

  annot_table_wei_bio
}
