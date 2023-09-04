#' @title Weight bio
#'
#' @description This function weights the eventually MS1
#' complemented annotations according their biological source
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
#' @export
#'
#' @examples NULL
weight_bio <-
  function(annotation_table_taxed = get("annotation_table_taxed",
             envir = parent.frame()
           ),
           structure_organism_pairs_table = get("structure_organism_pairs_table",
             envir = parent.frame()
           ),
           weight_spectral = get("weight_spectral",
             envir = parent.frame()
           ),
           weight_biological = get("weight_biological",
             envir = parent.frame()
           ),
           score_biological_domain = get("score_biological_domain",
             envir = parent.frame()
           ),
           score_biological_kingdom = get("score_biological_kingdom",
             envir = parent.frame()
           ),
           score_biological_phylum = get("score_biological_phylum",
             envir = parent.frame()
           ),
           score_biological_class = get("score_biological_class",
             envir = parent.frame()
           ),
           score_biological_order = get("score_biological_order",
             envir = parent.frame()
           ),
           score_biological_family = get("score_biological_family",
             envir = parent.frame()
           ),
           score_biological_tribe = get("score_biological_tribe",
             envir = parent.frame()
           ),
           score_biological_genus = get("score_biological_genus",
             envir = parent.frame()
           ),
           score_biological_species = get("score_biological_species",
             envir = parent.frame()
           ),
           score_biological_variety = get("score_biological_variety",
             envir = parent.frame()
           )) {
    df0 <- annotation_table_taxed |>
      tidytable::distinct(
        candidate_structure_tax_cla_01kin,
        candidate_structure_tax_npc_01pat,
        candidate_structure_tax_cla_02sup,
        candidate_structure_tax_npc_02sup,
        candidate_structure_tax_cla_03cla,
        candidate_structure_tax_npc_03cla,
        candidate_structure_tax_cla_04dirpar
      ) |>
      log_pipe("adding \"notClassified\" \n") |>
      tidytable::mutate(
        tidytable::across(
          .cols = tidytable::matches("candidate_structure_"),
          .fns = function(x) {
            tidytable::replace_na(x, "notClassified")
          }
        )
      )

    df1 <- annotation_table_taxed |>
      tidytable::select(
        candidate_structure_inchikey_no_stereo,
        candidate_score_similarity,
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
      tidytable::distinct() |>
      tidytable::left_join(
        structure_organism_pairs_table |>
          tidytable::filter(!is.na(structure_inchikey_no_stereo)) |>
          tidytable::select(
            candidate_structure_inchikey_no_stereo = structure_inchikey_no_stereo,
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
            .cols = tidytable::where(is.character),
            .fns = function(x) {
              tidytable::na_if(x, "")
            }
          ))
      )

    df2 <- df1 |>
      tidytable::distinct(
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

    log_debug("calculating biological score at all levels ... \n")
    log_debug("... domain \n")
    step_dom <- df2 |>
      tidytable::distinct(
        candidate_organism_01_domain,
        sample_organism_01_domain
      ) |>
      tidytable::filter(!is.na(sample_organism_01_domain)) |>
      tidytable::filter(sample_organism_01_domain != "ND") |>
      tidytable::filter(!is.na(candidate_organism_01_domain)) |>
      tidytable::filter(candidate_organism_01_domain != "notClassified") |>
      tidytable::filter(
        stringi::stri_detect_regex(
          pattern = candidate_organism_01_domain,
          str = sample_organism_01_domain
        ) |
          sample_organism_01_domain == "notClassified"
      ) |>
      tidytable::mutate(
        score_biological_01 = ifelse(
          test = sample_organism_01_domain != "notClassified",
          yes = score_biological_domain * 1,
          no = 0
        )
      )

    log_debug("... kingdom \n")
    step_kin <- df2 |>
      tidytable::distinct(
        candidate_organism_02_kingdom,
        sample_organism_02_kingdom
      ) |>
      tidytable::filter(!is.na(sample_organism_02_kingdom)) |>
      tidytable::filter(sample_organism_02_kingdom != "ND") |>
      tidytable::filter(!is.na(candidate_organism_02_kingdom)) |>
      tidytable::filter(candidate_organism_02_kingdom != "notClassified") |>
      tidytable::filter(
        stringi::stri_detect_regex(
          pattern = candidate_organism_02_kingdom,
          str = sample_organism_02_kingdom
        ) |
          sample_organism_02_kingdom == "notClassified"
      ) |>
      tidytable::mutate(
        score_biological_02 = ifelse(
          test = sample_organism_02_kingdom != "notClassified",
          yes = score_biological_kingdom * 1,
          no = 0
        )
      )

    log_debug("... phylum \n")
    step_phy <- df2 |>
      tidytable::distinct(
        candidate_organism_03_phylum,
        sample_organism_03_phylum
      ) |>
      tidytable::filter(!is.na(sample_organism_03_phylum)) |>
      tidytable::filter(sample_organism_03_phylum != "ND") |>
      tidytable::filter(!is.na(candidate_organism_03_phylum)) |>
      tidytable::filter(candidate_organism_03_phylum != "notClassified") |>
      tidytable::filter(
        stringi::stri_detect_regex(
          pattern = candidate_organism_03_phylum,
          str = sample_organism_03_phylum
        ) |
          sample_organism_03_phylum == "notClassified"
      ) |>
      tidytable::mutate(
        score_biological_03 = ifelse(
          test = sample_organism_03_phylum != "notClassified",
          yes = score_biological_phylum * 1,
          no = 0
        )
      )

    log_debug("... class \n")
    step_cla <- df2 |>
      tidytable::distinct(
        candidate_organism_04_class,
        sample_organism_04_class
      ) |>
      tidytable::filter(!is.na(sample_organism_04_class)) |>
      tidytable::filter(sample_organism_04_class != "ND") |>
      tidytable::filter(!is.na(candidate_organism_04_class)) |>
      tidytable::filter(candidate_organism_04_class != "notClassified") |>
      tidytable::filter(
        stringi::stri_detect_regex(
          pattern = candidate_organism_04_class,
          str = sample_organism_04_class
        ) |
          sample_organism_04_class == "notClassified"
      ) |>
      tidytable::mutate(
        score_biological_04 = ifelse(
          test = sample_organism_04_class != "notClassified",
          yes = score_biological_class * 1,
          no = 0
        )
      )

    log_debug("... order \n")
    step_ord <- df2 |>
      tidytable::distinct(
        candidate_organism_05_order,
        sample_organism_05_order
      ) |>
      tidytable::filter(!is.na(sample_organism_05_order)) |>
      tidytable::filter(sample_organism_05_order != "ND") |>
      tidytable::filter(!is.na(candidate_organism_05_order)) |>
      tidytable::filter(candidate_organism_05_order != "notClassified") |>
      tidytable::filter(
        stringi::stri_detect_regex(
          pattern = candidate_organism_05_order,
          str = sample_organism_05_order
        ) |
          sample_organism_05_order == "notClassified"
      ) |>
      tidytable::mutate(
        score_biological_05 = ifelse(
          test = sample_organism_05_order != "notClassified",
          yes = score_biological_order * 1,
          no = 0
        )
      )

    ## log_debug("... infraorder \n")
    # step_ord2 <- df2 |>
    #   tidytable::distinct(candidate_infraorder,
    #                       sample_organism_05_1_infraorder) |>
    #   tidytable::filter(!is.na(sample_organism_05_order)) |>
    #   tidytable::filter(sample_organism_05_1_infraorder != "ND") |>
    #   tidytable::filter(!is.na(candidate_infraorder)) |>
    #   tidytable::filter(candidate_infraorder != "notClassified") |>
    #   tidytable::filter(
    #     stringi::stri_detect_regex(
    #       pattern = candidate_infraorder,
    #       str = sample_organism_05_1_infraorder
    #     ) |
    #       sample_organism_05_1_infraorder == "notClassified"
    #   ) |>
    #   tidytable::mutate(
    #     score_biological_05_1 = ifelse(
    #       test = sample_organism_05_1_infraorder != "notClassified",
    #       yes = score_biological_infraorder * 1,
    #       no = 0
    #     )
    #   )

    log_debug("... family \n")
    step_fam <- df2 |>
      tidytable::distinct(
        candidate_organism_06_family,
        sample_organism_06_family
      ) |>
      tidytable::filter(!is.na(sample_organism_06_family)) |>
      tidytable::filter(sample_organism_06_family != "ND") |>
      tidytable::filter(!is.na(candidate_organism_06_family)) |>
      tidytable::filter(candidate_organism_06_family != "notClassified") |>
      tidytable::filter(
        stringi::stri_detect_regex(
          pattern = candidate_organism_06_family,
          str = sample_organism_06_family
        ) |
          sample_organism_06_family == "notClassified"
      ) |>
      tidytable::mutate(
        score_biological_06 = ifelse(
          test = sample_organism_06_family != "notClassified",
          yes = score_biological_family * 1,
          no = 0
        )
      )

    # log_debug("... subfamily \n")
    # step_fam2 <- df2 |>
    #   tidytable::distinct(candidate_organism_06_1_subfamily,
    #                       sample_organism_06_1_subfamily) |>
    #   tidytable::filter(!is.na(sample_organism_06_1_subfamily)) |>
    #   tidytable::filter(sample_organism_06_1_subfamily != "ND") |>
    #   tidytable::filter(!is.na(candidate_organism_06_1_subfamily)) |>
    #   tidytable::filter(candidate_organism_06_1_subfamily !=
    # "notClassified") |>
    #   tidytable::filter(
    #     stringi::stri_detect_regex(
    #       pattern = candidate_organism_06_1_subfamily,
    #       str = sample_organism_06_1_subfamily
    #     ) |
    #       sample_organism_06_1_subfamily == "notClassified"
    #   ) |>
    #   tidytable::mutate(
    #     score_biological_06_1 = ifelse(
    #       test = sample_organism_06_1_subfamily != "notClassified",
    #       yes = score_biological_subfamily * 1,
    #       no = 0
    #     )
    #   )

    log_debug("... tribe \n")
    step_tri <- df2 |>
      tidytable::distinct(
        candidate_organism_07_tribe,
        sample_organism_07_tribe
      ) |>
      tidytable::filter(!is.na(sample_organism_07_tribe)) |>
      tidytable::filter(sample_organism_07_tribe != "ND") |>
      tidytable::filter(!is.na(candidate_organism_07_tribe)) |>
      tidytable::filter(candidate_organism_07_tribe != "notClassified") |>
      tidytable::filter(
        stringi::stri_detect_regex(
          pattern = candidate_organism_07_tribe,
          str = sample_organism_07_tribe
        ) |
          sample_organism_07_tribe == "notClassified"
      ) |>
      tidytable::mutate(
        score_biological_07 = ifelse(
          test = sample_organism_07_tribe != "notClassified",
          yes = score_biological_tribe * 1,
          no = 0
        )
      )

    # log_debug("... subtribe \n")
    # step_tri2 <- df2 |>
    #   tidytable::distinct(candidate_organism_07_1_subtribe,
    #                       sample_organism_07_1_subtribe) |>
    #   tidytable::filter(!is.na(sample_organism_07_1_subtribe)) |>
    #   tidytable::filter(sample_organism_07_1_subtribe != "ND") |>
    #   tidytable::filter(!is.na(candidate_organism_07_1_subtribe)) |>
    #   tidytable::filter(candidate_organism_07_1_subtribe !=
    # "notClassified") |>
    #   tidytable::filter(
    #     stringi::stri_detect_regex(
    #       pattern = candidate_organism_07_1_subtribe,
    #       str = sample_organism_07_1_subtribe
    #     ) |
    #       sample_organism_07_1_subtribe == "notClassified"
    #   ) |>
    #   tidytable::mutate(
    #     score_biological_07_1 = ifelse(
    #       test = sample_organism_07_1_subtribe != "notClassified",
    #       yes = score_biological_subtribe * 1,
    #       no = 0
    #     )
    #   )

    log_debug("... genus \n")
    step_gen <- df2 |>
      tidytable::distinct(
        candidate_organism_08_genus,
        sample_organism_08_genus
      ) |>
      tidytable::filter(!is.na(sample_organism_08_genus)) |>
      tidytable::filter(sample_organism_08_genus != "ND") |>
      tidytable::filter(!is.na(candidate_organism_08_genus)) |>
      tidytable::filter(candidate_organism_08_genus != "notClassified") |>
      tidytable::filter(
        stringi::stri_detect_regex(
          pattern = candidate_organism_08_genus,
          str = sample_organism_08_genus
        ) |
          sample_organism_08_genus == "notClassified"
      ) |>
      tidytable::mutate(
        score_biological_08 = ifelse(
          test = sample_organism_08_genus != "notClassified",
          yes = score_biological_genus * 1,
          no = 0
        )
      )

    # log_debug("... subgenus \n")
    # step_gen2 <- df2 |>
    #   tidytable::distinct(candidate_organism_08_1_subgenus,
    #                       sample_organism_08_1_subgenus) |>
    #   tidytable::filter(!is.na(sample_organism_08_1_subgenus)) |>
    #   tidytable::filter(sample_organism_08_1_subgenus != "ND") |>
    #   tidytable::filter(!is.na(candidate_organism_08_1_subgenus)) |>
    #   tidytable::filter(candidate_organism_08_1_subgenus !=
    # "notClassified") |>
    #   tidytable::filter(
    #     stringi::stri_detect_regex(
    #       pattern = candidate_organism_08_1_subgenus,
    #       str = sample_organism_08_1_subgenus
    #     ) |
    #       sample_organism_08_1_subgenus == "notClassified"
    #   ) |>
    #   tidytable::mutate(
    #     score_biological_08_1 = ifelse(
    #       test = sample_organism_08_1_subgenus != "notClassified",
    #       yes = score_biological_subgenus * 1,
    #       no = 0
    #     )
    #   )

    log_debug("... species \n")
    step_spe <- df2 |>
      tidytable::distinct(
        candidate_organism_09_species,
        sample_organism_09_species
      ) |>
      tidytable::filter(!is.na(sample_organism_09_species)) |>
      tidytable::filter(sample_organism_09_species != "ND") |>
      tidytable::filter(!is.na(candidate_organism_09_species)) |>
      tidytable::filter(candidate_organism_09_species != "notClassified") |>
      tidytable::filter(
        stringi::stri_detect_regex(
          pattern = candidate_organism_09_species,
          str = sample_organism_09_species
        ) |
          sample_organism_09_species == "notClassified"
      ) |>
      tidytable::mutate(
        score_biological_09 = ifelse(
          test = sample_organism_09_species != "notClassified",
          yes = score_biological_species * 1,
          no = 0
        )
      )

    # log_debug("... subspecies \n")
    # step_spe2 <- df2 |>
    #   tidytable::distinct(candidate_organism_09_1_subspecies,
    #                       sample_organism_09_1_subspecies) |>
    #   tidytable::filter(!is.na(sample_organism_09_1_subspecies)) |>
    #   tidytable::filter(sample_organism_09_1_subspecies != "ND") |>
    #   tidytable::filter(!is.na(candidate_organism_09_1_subspecies)) |>
    #   tidytable::filter(candidate_organism_09_1_subspecies !=
    # "notClassified") |>
    #   tidytable::filter(
    #     stringi::stri_detect_regex(
    #       pattern = candidate_organism_09_1_subspecies,
    #       str = sample_organism_09_1_subspecies
    #     ) |
    #       sample_organism_09_1_subspecies == "notClassified"
    #   ) |>
    #   tidytable::mutate(
    #     score_biological_09_1 = ifelse(
    #       test = sample_organism_09_1_subspecies != "notClassified",
    #       yes = score_biological_subspecies * 1,
    #       no = 0
    #     )
    #   )

    log_debug("... varietas \n")
    step_var <- df2 |>
      tidytable::distinct(
        candidate_organism_10_varietas,
        sample_organism_10_varietas
      ) |>
      tidytable::filter(!is.na(sample_organism_10_varietas)) |>
      tidytable::filter(sample_organism_10_varietas != "ND") |>
      tidytable::filter(!is.na(candidate_organism_10_varietas)) |>
      tidytable::filter(candidate_organism_10_varietas != "notClassified") |>
      tidytable::filter(
        stringi::stri_detect_regex(
          pattern = candidate_organism_10_varietas,
          str = sample_organism_10_varietas
        ) |
          sample_organism_10_varietas == "notClassified"
      ) |>
      tidytable::mutate(
        score_biological_10 = ifelse(
          test = sample_organism_10_varietas != "notClassified",
          yes = score_biological_variety * 1,
          no = 0
        )
      )

    log_debug("... keeping best biological score \n")
    annot_table_wei_bio <- df2 |>
      tidytable::left_join(step_dom) |>
      tidytable::left_join(step_kin) |>
      tidytable::left_join(step_phy) |>
      tidytable::left_join(step_cla) |>
      tidytable::left_join(step_ord) |>
      # tidytable::left_join(step_ord2) |>
      tidytable::left_join(step_fam) |>
      # tidytable::left_join(step_fam2) |>
      tidytable::left_join(step_tri) |>
      # tidytable::left_join(step_tri2) |>
      tidytable::left_join(step_gen) |>
      # tidytable::left_join(step_gen2) |>
      tidytable::left_join(step_spe) |>
      # tidytable::left_join(step_spe2) |>
      tidytable::left_join(step_var) |>
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
      tidytable::select(
        -tidytable::contains("score_biological_")
      ) |>
      tidytable::mutate(candidate_structure_organism_occurrence_closest = tidytable::case_when(
        score_biological == score_biological_domain
        ~ candidate_organism_01_domain,
        score_biological == score_biological_kingdom
        ~ candidate_organism_02_kingdom,
        score_biological == score_biological_phylum
        ~ candidate_organism_03_phylum,
        score_biological == score_biological_class
        ~ candidate_organism_04_class,
        score_biological == score_biological_order
        ~ candidate_organism_05_order,
        # score_biological == score_biological_05_1 ~
        # candidate_organism_05_1_infraorder,
        score_biological == score_biological_family
        ~ candidate_organism_06_family,
        # score_biological == score_biological_06_1 ~
        # candidate_organism_06_1_subfamily,
        score_biological == score_biological_tribe
        ~ candidate_organism_07_tribe,
        # score_biological == score_biological_07_1 ~
        # candidate_organism_07_1_subtribe,
        score_biological == score_biological_genus
        ~ candidate_organism_08_genus,
        # score_biological == score_biological_08_1 ~
        # candidate_organism_08_1_subgenus,
        score_biological == score_biological_species
        ~ candidate_organism_09_species,
        # score_biological == score_biological_09_1 ~
        # candidate_organism_09_1_subspecies,
        score_biological == score_biological_variety ~
          candidate_organism_10_varietas,
        .default = NA_character_
      )) |>
      tidytable::right_join(df1) |>
      tidytable::arrange(tidytable::desc(score_biological)) |>
      tidytable::distinct(
        candidate_structure_inchikey_no_stereo,
        candidate_score_similarity,
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
        .keep_all = TRUE
      ) |>
      tidytable::inner_join(annotation_table_taxed) |>
      tidytable::select(
        -tidytable::contains("candidate_organism"),
        -tidytable::contains("sample_organism")
      ) |>
      tidytable::left_join(df0) |>
      log_pipe("... calculating weighted biological score \n") |>
      ## TODO TEMP
      tidytable::mutate(
        candidate_score_similarity =
          ifelse(
            test = is.na(candidate_score_similarity),
            yes = as.character(0),
            no = candidate_score_similarity
          )
      ) |>
      tidytable::mutate(
        score_pondered_bio =
          (1 / (weight_biological + weight_spectral)) *
            weight_biological *
            score_biological +
            (1 / (weight_biological + weight_spectral)) *
              weight_spectral *
              as.numeric(candidate_score_similarity)
      )

    rm(
      annotation_table_taxed,
      df0,
      df1,
      df2,
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

    return(annot_table_wei_bio)
  }
