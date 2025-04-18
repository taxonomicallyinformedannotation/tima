#' @title Weight bio
#'
#' @description This function weights the eventually MS1
#' complemented annotations according their biological source
#'
#' @include transform_score_sirius_csi.R
#'
#' @param annotation_table_taxed Table containing the initial annotation
#' eventually complemented by additional MS1 annotations
#' @param structure_organism_pairs_table Table containing the
#' structure - organism pairs
#' @param weight_spectral Weight for the spectral score
#' @param weight_biological Weight for the biological score
#' @param score_biological_domain Score for a `domain` match
#' (should be lower than `kingdom`)
#' @param score_biological_kingdom Score for a `kingdom` match
#'  (should be lower than `phylum`)
#' @param score_biological_phylum Score for a `phylum` match
#'  (should be lower than `class`)
#' @param score_biological_class Score for a `class` match
#' (should be lower than `order`)
#' @param score_biological_order Score for a `order` match
#' (should be lower than `family`)
#' @param score_biological_family Score for a `family` match
#' (should be lower than `tribe`)
#' @param score_biological_tribe Score for a `tribe` match
#' (should be lower than `genus`)
#' @param score_biological_genus Score for a `genus` match
#' (should be lower than `species`)
#' @param score_biological_species Score for a `species` match
#'  (should be lower than `variety`)
#' @param score_biological_variety Score for a `variety` match
#' (should be the highest)
#'
#' @return A table containing the biologically weighted annotation
#'
#' @examples NULL
weight_bio <-
  function(
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
        .cols = tidyselect::where(is.character),
        .fns = function(x) {
          tidytable::na_if(x, "")
        }
      ))
    df1 <- annotation_table_taxed |>
      tidytable::select(
        candidate_structure_inchikey_connectivity_layer,
        sample_organism_name
      ) |>
      tidytable::distinct() |>
      tidytable::left_join(
        df0 |>
          tidytable::select(
            candidate_structure_inchikey_connectivity_layer,
            candidate_organism_name
          ) |>
          tidytable::distinct()
      )

    df2 <- df1 |>
      tidytable::inner_join(
        annotation_table_taxed |>
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
      tidytable::inner_join(df0) |>
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

    logger::log_trace("Calculating biological score at all levels ...")
    score_per_level_bio <-
      function(df, candidates, samples, score, score_name) {
        score <- df |>
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
              true = !!as.name(score) * 1,
              false = 0
            )
          )
      }

    logger::log_trace("... domain")
    step_dom <- df2 |>
      score_per_level_bio(
        candidates = "candidate_organism_01_domain",
        samples = "sample_organism_01_domain",
        score = "score_biological_domain",
        score_name = "score_biological_01"
      )
    logger::log_trace("... kingdom")
    step_kin <- df2 |>
      score_per_level_bio(
        candidates = "candidate_organism_02_kingdom",
        samples = "sample_organism_02_kingdom",
        score = "score_biological_kingdom",
        score_name = "score_biological_02"
      )
    logger::log_trace("... phylum")
    step_phy <- df2 |>
      score_per_level_bio(
        candidates = "candidate_organism_03_phylum",
        samples = "sample_organism_03_phylum",
        score = "score_biological_phylum",
        score_name = "score_biological_03"
      )
    logger::log_trace("... class")
    step_cla <- df2 |>
      score_per_level_bio(
        candidates = "candidate_organism_04_class",
        samples = "sample_organism_04_class",
        score = "score_biological_class",
        score_name = "score_biological_04"
      )
    logger::log_trace("... order")
    step_ord <- df2 |>
      score_per_level_bio(
        candidates = "candidate_organism_05_order",
        samples = "sample_organism_05_order",
        score = "score_biological_order",
        score_name = "score_biological_05"
      )
    # logger::log_trace("... infraorder")
    # step_ord2 <- df2 |>
    #   score_per_level_bio(
    #     candidates = "candidate_organism_05_1_infraorder",
    #     samples = "sample_organism_05_1_infraorder",
    #     score = "score_biological_infraorder",
    #     score_name = "score_biological_05_1"
    #   )
    logger::log_trace("... family")
    step_fam <- df2 |>
      score_per_level_bio(
        candidates = "candidate_organism_06_family",
        samples = "sample_organism_06_family",
        score = "score_biological_family",
        score_name = "score_biological_06"
      )
    # logger::log_trace("... subfamily")
    # step_fam2 <- df2 |>
    #   score_per_level_bio(
    #     candidates = "candidate_organism_06_1_subfamily",
    #     samples = "sample_organism_06_1_subfamily",
    #     score = "score_biological_subfamily",
    #     score_name = "score_biological_06_1"
    #   )
    logger::log_trace("... tribe")
    step_tri <- df2 |>
      score_per_level_bio(
        candidates = "candidate_organism_07_tribe",
        samples = "sample_organism_07_tribe",
        score = "score_biological_tribe",
        score_name = "score_biological_07"
      )
    # logger::log_trace("... subtribe")
    # step_tri2 <- df2 |>
    #   score_per_level_bio(
    #     candidates = "candidate_organism_07_1_subtribe",
    #     samples = "sample_organism_07_1_subtribe",
    #     score = "score_biological_subtribe",
    #     score_name = "score_biological_07_1"
    #   )
    logger::log_trace("... genus")
    step_gen <- df2 |>
      score_per_level_bio(
        candidates = "candidate_organism_08_genus",
        samples = "sample_organism_08_genus",
        score = "score_biological_genus",
        score_name = "score_biological_08"
      )
    # logger::log_trace("... subgenus")
    # step_gen2 <- df2 |>
    #   score_per_level_bio(
    #     candidates = "candidate_organism_08_1_subgenus",
    #     samples = "sample_organism_08_1_subgenus",
    #     score = "score_biological_subgenus",
    #     score_name = "score_biological_08_1"
    #   )
    logger::log_trace("... species")
    step_spe <- df2 |>
      score_per_level_bio(
        candidates = "candidate_organism_09_species",
        samples = "sample_organism_09_species",
        score = "score_biological_species",
        score_name = "score_biological_09"
      )
    # logger::log_trace("... subspecies")
    # step_spe2 <- df2 |>
    #   score_per_level_bio(
    #     candidates = "candidate_organism_09_1_subspecies",
    #     samples = "sample_organism_09_1_subspecies",
    #     score = "score_biological_subspecies",
    #     score_name = "score_biological_09_1"
    #   )
    logger::log_trace("... varietas")
    step_var <- df2 |>
      score_per_level_bio(
        candidates = "candidate_organism_10_varietas",
        samples = "sample_organism_10_varietas",
        score = "score_biological_variety",
        score_name = "score_biological_10"
      )

    logger::log_trace("Keeping best biological score")
    supp_tables <- list(
      step_dom,
      step_kin,
      step_phy,
      step_cla,
      step_ord,
      # step_ord2,
      step_fam,
      # step_fam2,
      step_tri,
      # step_tri2,
      step_gen,
      # step_gen2,
      step_spe,
      # step_spe2,
      step_var
    )
    rm(
      step_dom,
      step_kin,
      step_phy,
      step_cla,
      step_ord,
      ## step_ord2,
      step_fam,
      ## step_fam2,
      step_tri,
      ## step_tri2,
      step_gen,
      ## step_gen2,
      step_spe,
      ## step_spe2,
      step_var
    )

    annot_table_wei_bio_init <- purrr::reduce(
      .x = supp_tables,
      .init = df2,
      .f = function(x, y) {
        tidytable::left_join(x, y)
      }
    ) |>
      tidytable::select(
        sample_organism_name,
        -tidyselect::contains("sample_organism_0"),
        tidyselect::contains("candidate_organism"),
        tidyselect::contains("score")
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
      tidytable::select(-tidyselect::contains("score_biological_")) |>
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
      tidytable::right_join(df1) |>
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
      tidytable::inner_join(annotation_table_taxed) |>
      tidytable::select(
        -tidyselect::contains("candidate_organism"),
        -tidyselect::contains("sample_organism")
      )
    rm(
      annot_table_wei_bio_init,
      annotation_table_taxed
    )

    annot_table_wei_bio <- annot_table_wei_bio_big |>
      tidytable::mutate(tidytable::across(
        .cols = tidyselect::matches("candidate_structure_tax"),
        .fns = function(x) {
          tidytable::replace_na(x, "notClassified")
        }
      )) |>
      tidytable::mutate(
        candidate_score_sirius_csi_tmp = transform_score_sirius_csi(
          candidate_score_sirius_csi
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
