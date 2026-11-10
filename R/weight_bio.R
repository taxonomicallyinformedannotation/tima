#' Combine spectral library similarity and SIRIUS scores
#' @noRd
corroborate_spectral_scores <- function(
  s_sim,
  s_sir,
  method = c("mean", "corroborated", "noisy_or")
) {
  method <- match.arg(method)
  n <- max(length(s_sim), length(s_sir))
  if (n == 0L) {
    return(numeric(0L))
  }
  s1 <- rep_len(as.double(s_sim), n)
  s2 <- rep_len(as.double(s_sir), n)
  has1 <- !is.na(s1)
  has2 <- !is.na(s2)
  out <- rep(NA_real_, n)
  only1 <- which(has1 & !has2)
  if (length(only1) > 0L) {
    out[only1] <- s1[only1]
  }
  only2 <- which(!has1 & has2)
  if (length(only2) > 0L) {
    out[only2] <- s2[only2]
  }
  both <- which(has1 & has2)
  if (length(both) > 0L) {
    a <- s1[both]
    b <- s2[both]
    out[both] <- switch(
      method,
      mean = (a + b) / 2,
      corroborated = {
        hi <- pmax(a, b)
        lo <- pmin(a, b)
        hi + 0.5 * lo * pmax(0, 1 - hi)
      },
      noisy_or = {
        a_c <- pmin(pmax(a, 0), 1)
        b_c <- pmin(pmax(b, 0), 1)
        1 - (1 - a_c) * (1 - b_c)
      }
    )
  }
  out
}

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
#' @param score_biological_infraorder [numeric] Optional score for
#'     infraorder-level match (0-1)
#' @param score_biological_subfamily [numeric] Optional score for
#'     subfamily-level match (0-1)
#' @param score_biological_subtribe [numeric] Optional score for
#'     subtribe-level match (0-1)
#' @param score_biological_subgenus [numeric] Optional score for
#'     subgenus-level match (0-1)
#' @param score_biological_subspecies [numeric] Optional score for
#'     subspecies-level match (0-1)
#' @param spectral_fusion [character] Method to combine library MS2 similarity
#'     and SIRIUS scores when both are present: `"mean"` (default),
#'     `"corroborated"` (synergistic monotone fusion), or `"noisy_or"`.
#' @param use_zodiac [logical] If `TRUE` and `candidate_score_sirius_zodiac`
#'     is present, modulates transformed SIRIUS CSI scores by Zodiac confidence.
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
  score_biological_biota,
  score_biological_infraorder = NULL,
  score_biological_subfamily = NULL,
  score_biological_subtribe = NULL,
  score_biological_subgenus = NULL,
  score_biological_subspecies = NULL,
  spectral_fusion = c("mean", "corroborated", "noisy_or"),
  use_zodiac = FALSE
) {
  spectral_fusion <- match.arg(spectral_fusion)
  ctx <- log_operation(
    "weight_bio",
    n_annotations = nrow(annotation_table_taxed),
    n_sop = nrow(structure_organism_pairs_table)
  )
  validate_dataframe(
    annotation_table_taxed,
    param_name = "annotation_table_taxed"
  )
  validate_dataframe(
    structure_organism_pairs_table,
    param_name = "structure_organism_pairs_table"
  )

  validate_weights(
    c(
      weight_spectral,
      weight_biological
    )
  )

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

  # Early exit for empty input
  n_annotations <- nrow(annotation_table_taxed)
  if (n_annotations == 0L) {
    log_warn(
      "Empty annotation table provided, skipping biological weighting"
    )
    return(annotation_table_taxed)
  }

  subrank_scores <- list(
    infraorder = score_biological_infraorder,
    subfamily = score_biological_subfamily,
    subtribe = score_biological_subtribe,
    subgenus = score_biological_subgenus,
    subspecies = score_biological_subspecies
  )
  for (sr_nm in names(subrank_scores)) {
    sr_val <- subrank_scores[[sr_nm]]
    if (!is.null(sr_val)) {
      validate_numeric_range(
        sr_val,
        0,
        1,
        param_name = paste0("score_biological_", sr_nm)
      )
    }
  }

  log_info("Weighting %d annotations by biological source", n_annotations)
  log_debug(
    "Weights - Spectral: %.2f, Biological: %.2f",
    weight_spectral,
    weight_biological
  )

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
    x_chr <- as.character(x)
    if (length(x_chr) == 0L) {
      return(character(0L))
    }
    u <- unique(x_chr)
    if (!any(u == "", na.rm = TRUE)) {
      return(x_chr)
    }
    m <- match(x_chr, u)
    u[u == ""] <- NA_character_
    u[m]
  }

  sop_ik_sub <- na_if_empty_chr(sop_ik[keep_sop_idx])
  sop_org_sub <- na_if_empty_chr(
    structure_organism_pairs_table$organism_name[keep_sop_idx]
  )
  u_sop_ik <- unique(sop_ik_sub)
  u_sop_org <- unique(sop_org_sub)
  ik_id_s <- match(sop_ik_sub, u_sop_ik)
  org_id_s <- match(sop_org_sub, u_sop_org)
  n_u_org <- max(1L, length(u_sop_org))
  io_key <- as.double(ik_id_s - 1L) * n_u_org + org_id_s
  keep_io <- !duplicated(io_key)

  sop_ik_org <- tidytable::tidytable(
    candidate_structure_inchikey_connectivity_layer = sop_ik_sub[keep_io],
    candidate_organism_name = sop_org_sub[keep_io]
  )
  log_debug("Filtered to %d structure-organism pairs", nrow(sop_ik_org))
  rm(sop_ik_sub, ik_id_s, io_key, keep_io)

  sop_ott_sub <- sop_ott[keep_sop_idx]
  u_ott <- unique(sop_ott_sub)
  ott_id_s <- match(sop_ott_sub, u_ott)
  n_u_ott <- max(1L, length(u_ott))
  oo_key <- as.double(org_id_s - 1L) * n_u_ott + ott_id_s
  keep_oo <- which(!duplicated(oo_key))
  first_org_idx <- keep_sop_idx[keep_oo]

  subrank_specs <- list(
    list(
      score = score_biological_infraorder,
      sop_col = "organism_taxonomy_05_1infraorder",
      cand_col = "candidate_organism_05_1_infraorder",
      samp_col = "sample_organism_05_1_infraorder"
    ),
    list(
      score = score_biological_subfamily,
      sop_col = "organism_taxonomy_06_1subfamily",
      cand_col = "candidate_organism_06_1_subfamily",
      samp_col = "sample_organism_06_1_subfamily"
    ),
    list(
      score = score_biological_subtribe,
      sop_col = "organism_taxonomy_07_1subtribe",
      cand_col = "candidate_organism_07_1_subtribe",
      samp_col = "sample_organism_07_1_subtribe"
    ),
    list(
      score = score_biological_subgenus,
      sop_col = "organism_taxonomy_08_1subgenus",
      cand_col = "candidate_organism_08_1_subgenus",
      samp_col = "sample_organism_08_1_subgenus"
    ),
    list(
      score = score_biological_subspecies,
      sop_col = "organism_taxonomy_09_1subspecies",
      cand_col = "candidate_organism_09_1_subspecies",
      samp_col = "sample_organism_09_1_subspecies"
    )
  )
  active_subranks <- Filter(
    function(sp) {
      !is.null(sp$score) &&
        sp$sop_col %in% names(structure_organism_pairs_table) &&
        sp$samp_col %in% names(annotation_table_taxed)
    },
    subrank_specs
  )

  cand_tax_raw <- tidytable::tidytable(
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
  )
  for (sp in active_subranks) {
    data.table::set(
      cand_tax_raw,
      j = sp$cand_col,
      value = na_if_empty_chr(
        structure_organism_pairs_table[[sp$sop_col]][first_org_idx]
      )
    )
  }
  cand_tax_distinct <- .fast_distinct_with_key_guard(
    cand_tax_raw,
    "candidate_organism_name"
  )
  rm(
    cand_tax_raw,
    sop_org_sub,
    org_id_s,
    sop_ott_sub,
    ott_id_s,
    oo_key,
    keep_oo,
    first_org_idx
  )

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
    "sample_organism_10_varietas",
    vapply(active_subranks, `[[`, character(1L), "samp_col")
  )
  sample_tax_distinct <- if ("feature_id" %in% names(annotation_table_taxed)) {
    f_col <- annotation_table_taxed$feature_id
    u_f <- unique(f_col)
    f_id <- if (anyNA(u_f)) match(f_col, u_f) else fastmatch::fmatch(f_col, u_f)
    first_f <- which(!duplicated(f_id))
    .fast_distinct_with_key_guard(
      tidytable::as_tidytable(as.list(annotation_table_taxed)[samp_cols])[
        first_f,
      ],
      "sample_organism_name"
    )
  } else {
    .fast_distinct_with_key_guard(
      tidytable::as_tidytable(as.list(annotation_table_taxed)[samp_cols]),
      "sample_organism_name"
    )
  }

  ann_ik <- annotation_table_taxed$candidate_structure_inchikey_connectivity_layer
  ann_so <- annotation_table_taxed$sample_organism_name
  u_ann_so <- unique(ann_so)

  single_so_fast <- length(u_ann_so) == 1L &&
    !is.na(u_ann_so[1L]) &&
    nrow(sample_tax_distinct) == 1L

  if (single_so_fast) {
    df1 <- sop_ik_org
    rm(sop_ik_org)
    valid_cand <- which(!is.na(cand_tax_distinct$candidate_organism_name))
    n_vc <- length(valid_cand)
    df2_list <- vector("list", length(samp_cols) + length(cand_tax_distinct))
    names(df2_list) <- c(samp_cols, names(cand_tax_distinct))
    for (sc in samp_cols) {
      df2_list[[sc]] <- rep.int(sample_tax_distinct[[sc]][1L], n_vc)
    }
    for (cc in names(cand_tax_distinct)) {
      df2_list[[cc]] <- cand_tax_distinct[[cc]][valid_cand]
    }
    df2 <- tidytable::as_tidytable(df2_list)
    rm(sample_tax_distinct, cand_tax_distinct, df2_list)
  } else {
    u_ann_ik <- unique(ann_ik)
    aik_id <- match(ann_ik, u_ann_ik)
    aso_id <- match(ann_so, u_ann_so)
    n_aso <- max(1L, length(u_ann_so))
    as_key <- as.double(aik_id - 1L) * n_aso + aso_id
    keep_as <- !duplicated(as_key)

    df1 <- tidytable::tidytable(
      candidate_structure_inchikey_connectivity_layer = ann_ik[keep_as],
      sample_organism_name = ann_so[keep_as]
    ) |>
      tidytable::left_join(
        y = sop_ik_org,
        by = "candidate_structure_inchikey_connectivity_layer"
      )
    rm(sop_ik_org, aik_id, aso_id, as_key, keep_as)

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
      sc_key <- as.double(so_id - 1L) * n_co + co_id
      first_sc <- !duplicated(sc_key)
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

    m_s <- fastmatch::fmatch(
      df2_keys$sample_organism_name,
      sample_tax_distinct$sample_organism_name
    )
    m_c <- fastmatch::fmatch(
      df2_keys$candidate_organism_name,
      cand_tax_distinct$candidate_organism_name
    )
    keep_jc <- which(!is.na(m_s) & !is.na(m_c))
    ms_k <- m_s[keep_jc]
    mc_k <- m_c[keep_jc]
    df2_list <- c(
      lapply(as.list(sample_tax_distinct), `[`, ms_k),
      lapply(as.list(cand_tax_distinct), `[`, mc_k)
    )
    df2 <- tidytable::as_tidytable(df2_list)
    rm(sample_tax_distinct, cand_tax_distinct, df2_keys)
  }

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
    sr_scores_list <- vector("list", length(active_subranks))
    for (sr_i in seq_along(active_subranks)) {
      sp <- active_subranks[[sr_i]]
      s_sr <- score_level(
        df2[[sp$cand_col]],
        df2[[sp$samp_col]],
        sp$score
      )
      sr_scores_list[[sr_i]] <- s_sr
      sc_bio <- pmax(sc_bio, s_sr, na.rm = TRUE)
    }
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
    for (sr_i in seq_along(active_subranks)) {
      sp <- active_subranks[[sr_i]]
      s_sr <- sr_scores_list[[sr_i]]
      m_sr <- !is.na(sc_bio) &
        sc_bio == sp$score &
        !is.na(s_sr) &
        s_sr == sp$score
      occ[m_sr] <- as.character(df2[[sp$cand_col]][m_sr])
    }
    occ[is_biota] <- "Biota"

    keep_occ <- which(!is.na(occ))
    annot_table_wei_bio_init <- tidytable::tidytable(
      sample_organism_name = df2$sample_organism_name[keep_occ],
      candidate_organism_name = df2$candidate_organism_name[keep_occ],
      score_biological = sc_bio[keep_occ],
      candidate_structure_organism_occurrence_closest = occ[keep_occ]
    )
  } else {
    annot_table_wei_bio_init <- tidytable::tidytable(
      sample_organism_name = character(0L),
      candidate_organism_name = character(0L),
      score_biological = numeric(0L),
      candidate_structure_organism_occurrence_closest = character(0L)
    )
  }
  rm(df2)

  # Attach biological scores onto annotation_table_taxed in-place via fmatch
  annot_table_wei_bio <- tidytable::as_tidytable(as.list(
    annotation_table_taxed
  ))
  rm(annotation_table_taxed)
  n_ann <- nrow(annot_table_wei_bio)

  if (single_so_fast) {
    if (nrow(annot_table_wei_bio_init) > 0L && nrow(df1) > 0L && n_ann > 0L) {
      if (
        anyDuplicated(annot_table_wei_bio_init$candidate_organism_name) > 0L
      ) {
        ord_init <- order(
          -annot_table_wei_bio_init$score_biological,
          na.last = TRUE,
          method = "radix"
        )
        annot_table_wei_bio_init <- annot_table_wei_bio_init[ord_init, ]
      }
      m_d1 <- fastmatch::fmatch(
        df1$candidate_organism_name,
        annot_table_wei_bio_init$candidate_organism_name
      )
      d1_sc <- annot_table_wei_bio_init$score_biological[m_d1]
      d1_occ <- annot_table_wei_bio_init$candidate_structure_organism_occurrence_closest[
        m_d1
      ]
      ord_d1 <- order(-d1_sc, na.last = TRUE, method = "radix")
      ik_d1_ord <- df1$candidate_structure_inchikey_connectivity_layer[ord_d1]
      u_ik_d1 <- unique(ik_d1_ord)
      ik_d1_id <- match(ik_d1_ord, u_ik_d1)
      keep_ik <- !duplicated(ik_d1_id)
      win_d1 <- ord_d1[keep_ik]
      win_ik <- ik_d1_ord[keep_ik]
      win_sc <- d1_sc[win_d1]
      win_occ <- d1_occ[win_d1]
      m_bio <- match(
        annot_table_wei_bio$candidate_structure_inchikey_connectivity_layer,
        win_ik
      )
      score_bio_v <- win_sc[m_bio]
      occ_closest_v <- win_occ[m_bio]
    } else {
      score_bio_v <- rep.int(NA_real_, n_ann)
      occ_closest_v <- rep.int(NA_character_, n_ann)
    }
    rm(df1, annot_table_wei_bio_init)
  } else {
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
      so_i <- match(annot_table_wei_bio_init$sample_organism_name, u_so_all)
      co_i <- match(annot_table_wei_bio_init$candidate_organism_name, u_co_all)
      n_co_all <- max(1L, length(u_co_all))
      k_init <- as.double(so_i - 1L) * n_co_all + co_i
      keep_u_init <- !duplicated(k_init)
      k_init_u <- k_init[keep_u_init]
      sc_init_u <- annot_table_wei_bio_init$score_biological[keep_u_init]
      occ_init_u <- annot_table_wei_bio_init$candidate_structure_organism_occurrence_closest[
        keep_u_init
      ]

      so_d1 <- match(df1$sample_organism_name, u_so_all)
      co_d1 <- match(df1$candidate_organism_name, u_co_all)
      k_d1 <- as.double(so_d1 - 1L) * n_co_all + co_d1
      m_d1 <- fastmatch::fmatch(k_d1, k_init_u)
      d1_sc <- sc_init_u[m_d1]
      d1_occ <- occ_init_u[m_d1]

      ord_d1 <- order(-d1_sc, na.last = TRUE, method = "radix")
      ik_d1_ord <- df1$candidate_structure_inchikey_connectivity_layer[ord_d1]
      so_d1_ord <- so_d1[ord_d1]
      u_ik_d1 <- unique(ik_d1_ord)
      ik_d1_id <- match(ik_d1_ord, u_ik_d1)
      n_so_all <- max(1L, length(u_so_all))
      k_ikso <- as.double(ik_d1_id - 1L) * n_so_all + so_d1_ord
      keep_ikso <- !duplicated(k_ikso)
      win_d1 <- ord_d1[keep_ikso]
      k_ikso_u <- k_ikso[keep_ikso]
      win_sc <- d1_sc[win_d1]
      win_occ <- d1_occ[win_d1]
    } else {
      win_sc <- numeric(0L)
    }
    rm(df1, annot_table_wei_bio_init)

    if (length(win_sc) > 0L && n_ann > 0L) {
      m_ik_ann <- match(
        annot_table_wei_bio$candidate_structure_inchikey_connectivity_layer,
        u_ik_d1
      )
      m_so_ann <- match(annot_table_wei_bio$sample_organism_name, u_so_all)
      k_ann <- as.double(m_ik_ann - 1L) * n_so_all + m_so_ann
      m_bio <- fastmatch::fmatch(k_ann, k_ikso_u)
      score_bio_v <- win_sc[m_bio]
      occ_closest_v <- win_occ[m_bio]
    } else {
      score_bio_v <- rep(NA_real_, n_ann)
      occ_closest_v <- rep(NA_character_, n_ann)
    }
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

  csi_num <- .as_numeric_dedup(annot_table_wei_bio$candidate_score_sirius_csi)
  zodiac_num <- if (
    isTRUE(use_zodiac) &&
      "candidate_score_sirius_zodiac" %in% names(annot_table_wei_bio)
  ) {
    .as_numeric_dedup(annot_table_wei_bio$candidate_score_sirius_zodiac)
  } else {
    NULL
  }
  conf_num <- .as_numeric_dedup(
    annot_table_wei_bio$candidate_score_sirius_confidence
  )
  has_conf <- !is.na(conf_num)
  sirius_tmp <- if (any(has_conf)) {
    out_s <- transform_score_sirius_csi(csi_num, zodiac_score = zodiac_num)
    conf_idx <- which(has_conf)
    out_s[conf_idx] <- if (!is.null(zodiac_num)) {
      z_c <- zodiac_num[conf_idx]
      ok_zc <- !is.na(z_c) & is.finite(z_c)
      res_c <- conf_num[conf_idx]
      if (any(ok_zc)) {
        res_c[ok_zc] <- res_c[ok_zc] * pmin(pmax(z_c[ok_zc], 0), 1)
      }
      res_c
    } else {
      conf_num[conf_idx]
    }
    out_s
  } else {
    transform_score_sirius_csi(csi_num, zodiac_score = zodiac_num)
  }
  sim_num <- .as_numeric_dedup(annot_table_wei_bio$candidate_score_similarity)

  pseudo_init <- corroborate_spectral_scores(
    s_sim = sim_num,
    s_sir = sirius_tmp,
    method = spectral_fusion
  )
  rm(csi_num, zodiac_num, conf_num, has_conf, sirius_tmp, sim_num)

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
