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
  needed_structures <- unique(
    annotation_table_taxed$candidate_structure_inchikey_connectivity_layer
  )
  needed_structures <- needed_structures[!is.na(needed_structures)]

  sop_ik <- structure_organism_pairs_table$structure_inchikey_connectivity_layer
  sop_ott <- structure_organism_pairs_table$organism_taxonomy_ottid
  keep_sop <- !is.na(sop_ik) & !is.na(sop_ott)
  if (length(needed_structures) > 0L) {
    keep_sop <- keep_sop & !is.na(fastmatch::fmatch(sop_ik, needed_structures))
  }
  keep_sop_idx <- which(keep_sop)
  rm(keep_sop, needed_structures)

  na_if_empty_chr <- function(x) {
    x_chr <- if (is.character(x)) x else as.character(x)
    if (length(x_chr) == 0L) {
      return(character(0L))
    }
    u <- unique(x_chr)
    if (!any(u == "", na.rm = TRUE)) {
      return(x_chr)
    }
    m <- if (anyNA(u)) match(x_chr, u) else fastmatch::fmatch(x_chr, u)
    u[u == ""] <- NA_character_
    u[m]
  }

  sop_ik_sub <- na_if_empty_chr(sop_ik[keep_sop_idx])
  sop_org_sub <- na_if_empty_chr(
    structure_organism_pairs_table$organism_name[keep_sop_idx]
  )
  u_sop_ik <- unique(sop_ik_sub)
  u_sop_org <- unique(sop_org_sub)
  ik_id_s <- if (anyNA(u_sop_ik)) {
    match(sop_ik_sub, u_sop_ik)
  } else {
    fastmatch::fmatch(sop_ik_sub, u_sop_ik)
  }
  org_id_s <- if (anyNA(u_sop_org)) {
    match(sop_org_sub, u_sop_org)
  } else {
    fastmatch::fmatch(sop_org_sub, u_sop_org)
  }
  n_u_org <- max(1L, length(u_sop_org))
  use_int_io <- as.double(length(u_sop_ik)) * n_u_org < .Machine$integer.max
  io_key <- if (use_int_io) {
    (ik_id_s - 1L) * n_u_org + org_id_s
  } else {
    as.double(ik_id_s - 1L) * n_u_org + org_id_s
  }
  keep_io <- if (use_int_io) {
    !duplicated(io_key)
  } else {
    fastmatch::fmatch(io_key, io_key) == seq_along(io_key)
  }

  sop_ik_org <- tidytable::tidytable(
    candidate_structure_inchikey_connectivity_layer = sop_ik_sub[keep_io],
    candidate_organism_name = sop_org_sub[keep_io]
  )
  log_debug("Filtered to %d structure-organism pairs", nrow(sop_ik_org))
  rm(sop_ik_sub, ik_id_s, io_key, keep_io)

  sop_ott_sub <- sop_ott[keep_sop_idx]
  u_ott <- unique(sop_ott_sub)
  ott_id_s <- if (anyNA(u_ott)) {
    match(sop_ott_sub, u_ott)
  } else {
    fastmatch::fmatch(sop_ott_sub, u_ott)
  }
  n_u_ott <- max(1L, length(u_ott))
  use_int_oo <- as.double(n_u_org) * n_u_ott < .Machine$integer.max
  oo_key <- if (use_int_oo) {
    (org_id_s - 1L) * n_u_ott + ott_id_s
  } else {
    as.double(org_id_s - 1L) * n_u_ott + ott_id_s
  }
  keep_oo <- if (use_int_oo) {
    which(!duplicated(oo_key))
  } else {
    which(fastmatch::fmatch(oo_key, oo_key) == seq_along(oo_key))
  }
  first_org_idx <- keep_sop_idx[keep_oo]

  cand_tax_distinct <- tidytable::tidytable(
    candidate_organism_name = sop_org_sub[keep_oo],
    candidate_organism_01_domain = na_if_empty_chr(
      structure_organism_pairs_table$organism_taxonomy_01domain[first_org_idx]
    ),
    candidate_organism_02_kingdom = na_if_empty_chr(
      structure_organism_pairs_table$organism_taxonomy_02kingdom[first_org_idx]
    ),
    candidate_organism_03_phylum = na_if_empty_chr(
      structure_organism_pairs_table$organism_taxonomy_03phylum[first_org_idx]
    ),
    candidate_organism_04_class = na_if_empty_chr(
      structure_organism_pairs_table$organism_taxonomy_04class[first_org_idx]
    ),
    candidate_organism_05_order = na_if_empty_chr(
      structure_organism_pairs_table$organism_taxonomy_05order[first_org_idx]
    ),
    candidate_organism_06_family = na_if_empty_chr(
      structure_organism_pairs_table$organism_taxonomy_06family[first_org_idx]
    ),
    candidate_organism_07_tribe = na_if_empty_chr(
      structure_organism_pairs_table$organism_taxonomy_07tribe[first_org_idx]
    ),
    candidate_organism_08_genus = na_if_empty_chr(
      structure_organism_pairs_table$organism_taxonomy_08genus[first_org_idx]
    ),
    candidate_organism_09_species = na_if_empty_chr(
      structure_organism_pairs_table$organism_taxonomy_09species[first_org_idx]
    ),
    candidate_organism_10_varietas = na_if_empty_chr(
      structure_organism_pairs_table$organism_taxonomy_10varietas[first_org_idx]
    )
  ) |>
    tidytable::distinct()
  rm(
    sop_org_sub,
    org_id_s,
    sop_ott_sub,
    ott_id_s,
    oo_key,
    keep_oo,
    first_org_idx
  )

  ann_ik <- annotation_table_taxed$candidate_structure_inchikey_connectivity_layer
  ann_so <- annotation_table_taxed$sample_organism_name
  u_ann_ik <- unique(ann_ik)
  u_ann_so <- unique(ann_so)
  aik_id <- if (anyNA(u_ann_ik)) {
    match(ann_ik, u_ann_ik)
  } else {
    fastmatch::fmatch(ann_ik, u_ann_ik)
  }
  aso_id <- if (anyNA(u_ann_so)) {
    match(ann_so, u_ann_so)
  } else {
    fastmatch::fmatch(ann_so, u_ann_so)
  }
  n_aso <- max(1L, length(u_ann_so))
  use_int_as <- as.double(length(u_ann_ik)) * n_aso < .Machine$integer.max
  as_key <- if (use_int_as) {
    (aik_id - 1L) * n_aso + aso_id
  } else {
    as.double(aik_id - 1L) * n_aso + aso_id
  }
  keep_as <- if (use_int_as) {
    !duplicated(as_key)
  } else {
    fastmatch::fmatch(as_key, as_key) == seq_along(as_key)
  }

  df1 <- tidytable::tidytable(
    candidate_structure_inchikey_connectivity_layer = ann_ik[keep_as],
    sample_organism_name = ann_so[keep_as]
  ) |>
    tidytable::left_join(
      y = sop_ik_org,
      by = "candidate_structure_inchikey_connectivity_layer"
    )
  rm(sop_ik_org, aik_id, aso_id, as_key, keep_as)

  samp_cols <- c(
    "sample_organism_name",
    "sample_organism_01_domain",
    "sample_organism_02_kingdom",
    "sample_organism_03_phylum",
    "sample_organism_04_class",
    "sample_organism_05_order",
    "sample_organism_06_family",
    "sample_organism_07_tribe",
    "sample_organism_08_genus",
    "sample_organism_09_species",
    "sample_organism_10_varietas"
  )
  sample_tax_distinct <- if ("feature_id" %in% names(annotation_table_taxed)) {
    f_col <- annotation_table_taxed$feature_id
    u_f <- unique(f_col)
    f_id <- if (anyNA(u_f)) match(f_col, u_f) else fastmatch::fmatch(f_col, u_f)
    first_f <- which(!duplicated(f_id))
    annotation_table_taxed[first_f, ..samp_cols] |>
      tidytable::distinct()
  } else {
    annotation_table_taxed[, ..samp_cols] |>
      tidytable::distinct()
  }

  d1_so <- df1$sample_organism_name
  d1_co <- df1$candidate_organism_name
  valid_d1 <- which(!is.na(d1_so) & !is.na(d1_co))
  if (length(valid_d1) > 0L) {
    so_v <- d1_so[valid_d1]
    co_v <- d1_co[valid_d1]
    u_so_v <- unique(so_v)
    u_co_v <- unique(co_v)
    so_id <- fastmatch::fmatch(so_v, u_so_v)
    co_id <- fastmatch::fmatch(co_v, u_co_v)
    n_co <- max(1L, length(u_co_v))
    use_int_sc <- as.double(length(u_so_v)) * n_co < .Machine$integer.max
    sc_key <- if (use_int_sc) {
      (so_id - 1L) * n_co + co_id
    } else {
      as.double(so_id - 1L) * n_co + co_id
    }
    first_sc <- if (use_int_sc) {
      !duplicated(sc_key)
    } else {
      fastmatch::fmatch(sc_key, sc_key) == seq_along(sc_key)
    }
    keep_sc <- valid_d1[first_sc]
    df2_keys <- tidytable::tidytable(
      sample_organism_name = df1$sample_organism_name[keep_sc],
      candidate_organism_name = df1$candidate_organism_name[keep_sc]
    )
  } else {
    df2_keys <- tidytable::tidytable(
      sample_organism_name = character(0L),
      candidate_organism_name = character(0L)
    )
  }

  df2 <- df2_keys |>
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
  rm(sample_tax_distinct, cand_tax_distinct, df2_keys)

  n_df2 <- nrow(df2)
  if (n_df2 > 0L) {
    valid_pair <- !is.na(df2$sample_organism_name) &
      df2$sample_organism_name != "ND" &
      !is.na(df2$candidate_organism_name) &
      df2$candidate_organism_name != "notClassified"

    score_level <- function(cand_col, samp_col, w_val) {
      res <- rep(NA_real_, n_df2)
      res[valid_pair] <- 0
      mask <- which(
        valid_pair &
          !is.na(cand_col) &
          cand_col != "notClassified" &
          !is.na(samp_col) &
          samp_col != "notClassified"
      )
      if (length(mask) > 0L) {
        res[mask] <- as.integer(stri_detect_token(
          str = samp_col[mask],
          pattern = cand_col[mask]
        )) *
          w_val
      }
      res
    }

    s01 <- score_level(
      df2$candidate_organism_01_domain,
      df2$sample_organism_01_domain,
      score_biological_domain
    )
    s02 <- score_level(
      df2$candidate_organism_02_kingdom,
      df2$sample_organism_02_kingdom,
      score_biological_kingdom
    )
    s03 <- score_level(
      df2$candidate_organism_03_phylum,
      df2$sample_organism_03_phylum,
      score_biological_phylum
    )
    s04 <- score_level(
      df2$candidate_organism_04_class,
      df2$sample_organism_04_class,
      score_biological_class
    )
    s05 <- score_level(
      df2$candidate_organism_05_order,
      df2$sample_organism_05_order,
      score_biological_order
    )
    s06 <- score_level(
      df2$candidate_organism_06_family,
      df2$sample_organism_06_family,
      score_biological_family
    )
    s07 <- score_level(
      df2$candidate_organism_07_tribe,
      df2$sample_organism_07_tribe,
      score_biological_tribe
    )
    s08 <- score_level(
      df2$candidate_organism_08_genus,
      df2$sample_organism_08_genus,
      score_biological_genus
    )
    s09 <- score_level(
      df2$candidate_organism_09_species,
      df2$sample_organism_09_species,
      score_biological_species
    )
    s10 <- score_level(
      df2$candidate_organism_10_varietas,
      df2$sample_organism_10_varietas,
      score_biological_variety
    )

    sc_bio <- pmax(
      pmax(s01, s02, s03, s04, na.rm = TRUE),
      pmax(s05, s06, s07, s08, na.rm = TRUE),
      pmax(s09, s10, na.rm = TRUE),
      na.rm = TRUE
    )
    is_biota <- !is.na(df2$candidate_organism_01_domain) &
      df2$candidate_organism_01_domain == "Biota"
    sc_bio[is_biota] <- score_biological_biota

    occ <- rep(NA_character_, n_df2)
    m10 <- !is.na(sc_bio) & sc_bio == score_biological_variety
    occ[m10] <- as.character(df2$candidate_organism_10_varietas[m10])
    m09 <- !is.na(sc_bio) & sc_bio == score_biological_species
    occ[m09] <- as.character(df2$candidate_organism_09_species[m09])
    m08 <- !is.na(sc_bio) & sc_bio == score_biological_genus
    occ[m08] <- as.character(df2$candidate_organism_08_genus[m08])
    m07 <- !is.na(sc_bio) & sc_bio == score_biological_tribe
    occ[m07] <- as.character(df2$candidate_organism_07_tribe[m07])
    m06 <- !is.na(sc_bio) & sc_bio == score_biological_family
    occ[m06] <- as.character(df2$candidate_organism_06_family[m06])
    m05 <- !is.na(sc_bio) & sc_bio == score_biological_order
    occ[m05] <- as.character(df2$candidate_organism_05_order[m05])
    m04 <- !is.na(sc_bio) & sc_bio == score_biological_class
    occ[m04] <- as.character(df2$candidate_organism_04_class[m04])
    m03 <- !is.na(sc_bio) & sc_bio == score_biological_phylum
    occ[m03] <- as.character(df2$candidate_organism_03_phylum[m03])
    m02 <- !is.na(sc_bio) & sc_bio == score_biological_kingdom
    occ[m02] <- as.character(df2$candidate_organism_02_kingdom[m02])
    m01 <- !is.na(sc_bio) & sc_bio == score_biological_domain
    occ[m01] <- as.character(df2$candidate_organism_01_domain[m01])
    occ[is_biota] <- "Biota"

    keep_occ <- which(!is.na(occ))
    annot_table_wei_bio_init <- tidytable::tidytable(
      sample_organism_name = df2$sample_organism_name[keep_occ],
      candidate_organism_name = df2$candidate_organism_name[keep_occ],
      score_biological = sc_bio[keep_occ],
      candidate_structure_organism_occurrence_closest = occ[keep_occ]
    ) |>
      tidytable::distinct()
  } else {
    annot_table_wei_bio_init <- tidytable::tidytable(
      sample_organism_name = character(0L),
      candidate_organism_name = character(0L),
      score_biological = numeric(0L),
      candidate_structure_organism_occurrence_closest = character(0L)
    )
  }
  rm(df2)

  # Map annot_table_wei_bio_init onto df1 via integer pair keys, then pick
  # highest score_biological per (inchikey_connectivity_layer, sample_organism_name)
  if (nrow(annot_table_wei_bio_init) > 0L && nrow(df1) > 0L) {
    ord_init <- order(
      -annot_table_wei_bio_init$score_biological,
      na.last = TRUE,
      method = "radix"
    )
    annot_table_wei_bio_init <- annot_table_wei_bio_init[ord_init, ]
    u_so_all <- unique(c(
      df1$sample_organism_name,
      annot_table_wei_bio_init$sample_organism_name
    ))
    u_co_all <- unique(c(
      df1$candidate_organism_name,
      annot_table_wei_bio_init$candidate_organism_name
    ))
    so_i <- if (anyNA(u_so_all)) {
      match(annot_table_wei_bio_init$sample_organism_name, u_so_all)
    } else {
      fastmatch::fmatch(annot_table_wei_bio_init$sample_organism_name, u_so_all)
    }
    co_i <- if (anyNA(u_co_all)) {
      match(annot_table_wei_bio_init$candidate_organism_name, u_co_all)
    } else {
      fastmatch::fmatch(
        annot_table_wei_bio_init$candidate_organism_name,
        u_co_all
      )
    }
    n_co_all <- max(1L, length(u_co_all))
    use_int_init <- as.double(length(u_so_all)) * n_co_all <
      .Machine$integer.max
    k_init <- if (use_int_init) {
      (so_i - 1L) * n_co_all + co_i
    } else {
      as.double(so_i - 1L) * n_co_all + co_i
    }
    keep_u_init <- if (use_int_init) {
      !duplicated(k_init)
    } else {
      fastmatch::fmatch(k_init, k_init) == seq_along(k_init)
    }
    k_init_u <- k_init[keep_u_init]
    sc_init_u <- annot_table_wei_bio_init$score_biological[keep_u_init]
    occ_init_u <- annot_table_wei_bio_init$candidate_structure_organism_occurrence_closest[
      keep_u_init
    ]

    so_d1 <- if (anyNA(u_so_all)) {
      match(df1$sample_organism_name, u_so_all)
    } else {
      fastmatch::fmatch(df1$sample_organism_name, u_so_all)
    }
    co_d1 <- if (anyNA(u_co_all)) {
      match(df1$candidate_organism_name, u_co_all)
    } else {
      fastmatch::fmatch(df1$candidate_organism_name, u_co_all)
    }
    k_d1 <- if (use_int_init) {
      (so_d1 - 1L) * n_co_all + co_d1
    } else {
      as.double(so_d1 - 1L) * n_co_all + co_d1
    }
    m_d1 <- fastmatch::fmatch(k_d1, k_init_u)
    d1_sc <- sc_init_u[m_d1]
    d1_occ <- occ_init_u[m_d1]

    ord_d1 <- order(-d1_sc, na.last = TRUE, method = "radix")
    ik_d1_ord <- df1$candidate_structure_inchikey_connectivity_layer[ord_d1]
    so_d1_ord <- so_d1[ord_d1]
    u_ik_d1 <- unique(ik_d1_ord)
    ik_d1_id <- if (anyNA(u_ik_d1)) {
      match(ik_d1_ord, u_ik_d1)
    } else {
      fastmatch::fmatch(ik_d1_ord, u_ik_d1)
    }
    n_so_all <- max(1L, length(u_so_all))
    use_int_ikso <- as.double(length(u_ik_d1)) * n_so_all < .Machine$integer.max
    k_ikso <- if (use_int_ikso) {
      (ik_d1_id - 1L) * n_so_all + so_d1_ord
    } else {
      as.double(ik_d1_id - 1L) * n_so_all + so_d1_ord
    }
    keep_ikso <- if (use_int_ikso) {
      !duplicated(k_ikso)
    } else {
      fastmatch::fmatch(k_ikso, k_ikso) == seq_along(k_ikso)
    }
    win_d1 <- ord_d1[keep_ikso]
    k_ikso_u <- k_ikso[keep_ikso]
    win_sc <- d1_sc[win_d1]
    win_occ <- d1_occ[win_d1]
  } else {
    win_sc <- numeric(0L)
  }
  rm(df1, annot_table_wei_bio_init)

  # Attach biological scores onto annotation_table_taxed in-place via fmatch
  annot_table_wei_bio <- tidytable::as_tidytable(annotation_table_taxed)
  rm(annotation_table_taxed)

  n_ann <- nrow(annot_table_wei_bio)
  if (length(win_sc) > 0L && n_ann > 0L) {
    m_ik_ann <- if (anyNA(u_ik_d1)) {
      match(
        annot_table_wei_bio$candidate_structure_inchikey_connectivity_layer,
        u_ik_d1
      )
    } else {
      fastmatch::fmatch(
        annot_table_wei_bio$candidate_structure_inchikey_connectivity_layer,
        u_ik_d1
      )
    }
    m_so_ann <- if (anyNA(u_so_all)) {
      match(annot_table_wei_bio$sample_organism_name, u_so_all)
    } else {
      fastmatch::fmatch(annot_table_wei_bio$sample_organism_name, u_so_all)
    }
    k_ann <- if (use_int_ikso) {
      (m_ik_ann - 1L) * n_so_all + m_so_ann
    } else {
      as.double(m_ik_ann - 1L) * n_so_all + m_so_ann
    }
    m_bio <- fastmatch::fmatch(k_ann, k_ikso_u)
    score_bio_v <- win_sc[m_bio]
    occ_closest_v <- win_occ[m_bio]
  } else {
    score_bio_v <- rep(NA_real_, n_ann)
    occ_closest_v <- rep(NA_character_, n_ann)
  }

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
    .as_numeric_dedup(annot_table_wei_bio$candidate_score_sirius_csi)
  )
  conf_num <- .as_numeric_dedup(
    annot_table_wei_bio$candidate_score_sirius_confidence
  )
  sirius_tmp <- ifelse(!is.na(conf_num), conf_num, csi_tmp)
  sim_num <- .as_numeric_dedup(annot_table_wei_bio$candidate_score_similarity)

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
