# require(...)

#' Title
#'
#' @noRd
#'
#' @param annotationTable TODO
#' @param structureOrganismPairsTable TODO
#'
#' @return TODO
#' @export
#'
#' @examples TODO
biological_weighting <-
  function(annotationTable = annotation_table_ms1_taxed,
           structureOrganismPairsTable = structure_organism_pairs_table) {
    cat("normalizing initial score \n")
    metadata <- annotationTable |>
      dplyr::select(
        feature_id,
        component_id,
        inchikey_2D,
        smiles_2D,
        score_input,
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
        structure_taxonomy_npclassifier_01pathway,
        structure_taxonomy_npclassifier_02superclass,
        structure_taxonomy_npclassifier_03class
      ) |>
      dplyr::mutate(dplyr::across(score_input, as.numeric)) |>
      dplyr::mutate(score_initialNormalized = score_input)
    ## better not
    # dplyr::mutate(score_initialNormalized = (score_input -
    #   min(score_input)) /
    #   (max(score_input) -
    #     min(score_input)))

    sample_domain <- annotationTable |>
      dplyr::distinct(
        inchikey_2D,
        sample_organism_01_domain
      )

    sample_kingdom <- annotationTable |>
      dplyr::distinct(
        inchikey_2D,
        sample_organism_02_kingdom
      )

    sample_phylum <- annotationTable |>
      dplyr::distinct(
        inchikey_2D,
        sample_organism_03_phylum
      )

    sample_class <- annotationTable |>
      dplyr::distinct(
        inchikey_2D,
        sample_organism_04_class
      )

    sample_order <- annotationTable |>
      dplyr::distinct(
        feature_id,
        inchikey_2D,
        sample_organism_05_order
      )

    # sample_infraorder <- annotationTable |>
    #   dplyr::distinct(
    #     inchikey_2D,
    #     sample_organism_05_1_infraorder
    #   )

    sample_family <- annotationTable |>
      dplyr::distinct(
        inchikey_2D,
        sample_organism_06_family
      )

    # sample_subfamily <- annotationTable |>
    #   dplyr::distinct(
    #     inchikey_2D,
    #     sample_organism_06_1_subfamily
    #   )

    sample_tribe <- annotationTable |>
      dplyr::distinct(
        inchikey_2D,
        sample_organism_07_tribe
      )

    # sample_subtribe <- annotationTable |>
    #   dplyr::distinct(
    #     inchikey_2D,
    #     sample_organism_07_1_subtribe
    #   )

    sample_genus <- annotationTable |>
      dplyr::distinct(
        inchikey_2D,
        sample_organism_08_genus
      )

    # sample_subgenus <- annotationTable |>
    #   dplyr::distinct(
    #     inchikey_2D,
    #     sample_organism_08_1_subgenus
    #   )

    sample_species <- annotationTable |>
      dplyr::distinct(
        inchikey_2D,
        sample_organism_09_species
      )

    # sample_subspecies <- annotationTable |>
    #   dplyr::distinct(
    #     inchikey_2D,
    #     sample_organism_09_1_subspecies
    #   )

    sample_varietas <- annotationTable |>
      dplyr::distinct(
        inchikey_2D,
        sample_organism_10_varietas
      )

    rm(annotationTable)

    cat("selecting DB columns \n")
    candidates <- structureOrganismPairsTable |>
      dplyr::filter(!is.na(structure_inchikey_2D)) |>
      dplyr::select(
        inchikey_2D = structure_inchikey_2D,
        candidate_organism_01_domain = organism_taxonomy_01domain,
        candidate_organism_02_kingdom = organism_taxonomy_02kingdom,
        candidate_organism_03_phylum = organism_taxonomy_03phylum,
        candidate_organism_04_class = organism_taxonomy_04class,
        candidate_organism_05_order = organism_taxonomy_05order,
        # candidate_organism_05_1_infraorder = organism_taxonomy_05_1infraorder,
        candidate_organism_06_family = organism_taxonomy_06family,
        # candidate_organism_06_1_subfamily = organism_taxonomy_06_1subfamily,
        candidate_organism_07_tribe = organism_taxonomy_07tribe,
        # candidate_organism_07_1_subtribe = organism_taxonomy_07_1subtribe,
        candidate_organism_08_genus = organism_taxonomy_08genus,
        # candidate_organism_08_1_subgenus = organism_taxonomy_08_1subgenus,
        candidate_organism_09_species = organism_taxonomy_09species,
        # candidate_organism_09_1_subspecies = organism_taxonomy_09_1subspecies,
        candidate_organism_10_varietas = organism_taxonomy_10varietas
      )

    cat("keeping distinct candidates per taxonomical rank \n")
    candidate_domain <- candidates |>
      dplyr::distinct(
        inchikey_2D,
        candidate_organism_01_domain
      )

    candidate_kingdom <- candidates |>
      dplyr::distinct(
        inchikey_2D,
        candidate_organism_02_kingdom
      )

    candidate_phylum <- candidates |>
      dplyr::distinct(
        inchikey_2D,
        candidate_organism_03_phylum
      )

    candidate_class <- candidates |>
      dplyr::distinct(
        inchikey_2D,
        candidate_organism_04_class
      )

    candidate_order <- candidates |>
      dplyr::distinct(
        inchikey_2D,
        candidate_organism_05_order
      )

    # candidate_infraorder <- candidates |>
    #   distinct(
    #     inchikey_2D,
    #     candidate_organism_05_1_infraorder
    #   )

    candidate_family <- candidates |>
      dplyr::distinct(
        inchikey_2D,
        candidate_organism_06_family
      )

    # candidate_subfamily <- candidates |>
    #   distinct(
    #     inchikey_2D,
    #     candidate_organism_06_1_subfamily
    #   )

    candidate_tribe <- candidates |>
      dplyr::distinct(
        inchikey_2D,
        candidate_organism_07_tribe
      )

    # candidate_subtribe <- candidates |>
    #   distinct(
    #     inchikey_2D,
    #     candidate_organism_07_1_subtribe
    #   )

    candidate_genus <- candidates |>
      dplyr::distinct(
        inchikey_2D,
        candidate_organism_08_genus
      )

    # candidate_subgenus <- candidates |>
    #   distinct(
    #     inchikey_2D,
    #     candidate_organism_08_1_subgenus
    #   )

    candidate_species <- candidates |>
      dplyr::distinct(
        inchikey_2D,
        candidate_organism_09_species
      )

    # candidate_subspecies <- candidates |>
    #   distinct(
    #     inchikey_2D,
    #     candidate_organism_09_1_subspecies
    #   )

    candidate_varietas <- candidates |>
      dplyr::distinct(
        inchikey_2D,
        candidate_organism_10_varietas
      )

    cat("calculating biological scores ... \n")

    cat("... domain \n")
    step_dom <- dplyr::left_join(sample_domain, candidate_domain) |>
      dplyr::filter(candidate_organism_01_domain != "notClassified") |>
      dplyr::filter(
        stringr::str_detect(pattern = candidate_organism_01_domain, string = sample_organism_01_domain)
      ) |>
      dplyr::mutate(score_biological = params$score$biological$domain) |>
      dplyr::left_join(metadata |> dplyr::distinct(
        feature_id,
        inchikey_2D,
        sample_organism_01_domain
      )) |>
      dplyr::distinct(feature_id,
        inchikey_2D,
        best_candidate = candidate_organism_01_domain,
        score_biological
      )

    cat("... kingdom \n")
    step_kin <- dplyr::full_join(step_dom, sample_kingdom) |>
      dplyr::distinct(
        inchikey_2D,
        sample_organism_02_kingdom
      )
    step_kin <- dplyr::left_join(step_kin, candidate_kingdom) |>
      dplyr::filter(candidate_organism_02_kingdom != "notClassified") |>
      dplyr::filter(
        stringr::str_detect(pattern = candidate_organism_02_kingdom, string = sample_organism_02_kingdom)
      ) |>
      dplyr::mutate(score_biological = params$score$biological$kingdom) |>
      dplyr::left_join(metadata |> dplyr::distinct(
        feature_id,
        inchikey_2D,
        sample_organism_02_kingdom
      )) |>
      dplyr::distinct(feature_id,
        inchikey_2D,
        best_candidate = candidate_organism_02_kingdom,
        score_biological
      )

    cat("... phylum \n")
    step_phy <- dplyr::full_join(step_kin, sample_phylum) |>
      dplyr::distinct(
        inchikey_2D,
        sample_organism_03_phylum
      )
    step_phy <- dplyr::left_join(step_phy, candidate_phylum) |>
      dplyr::filter(candidate_organism_03_phylum != "notClassified") |>
      dplyr::filter(
        stringr::str_detect(pattern = candidate_organism_03_phylum, string = sample_organism_03_phylum)
      ) |>
      dplyr::mutate(score_biological = params$score$biological$phylum) |>
      dplyr::left_join(metadata |> dplyr::distinct(
        feature_id,
        inchikey_2D,
        sample_organism_03_phylum
      )) |>
      dplyr::distinct(feature_id,
        inchikey_2D,
        best_candidate = candidate_organism_03_phylum,
        score_biological
      )

    cat("... class \n")
    step_cla <- dplyr::full_join(step_phy, sample_class) |>
      dplyr::distinct(
        inchikey_2D,
        sample_organism_04_class
      )
    step_cla <- dplyr::left_join(step_cla, candidate_class) |>
      dplyr::filter(candidate_organism_04_class != "notClassified") |>
      dplyr::filter(
        stringr::str_detect(pattern = candidate_organism_04_class, string = sample_organism_04_class)
      ) |>
      dplyr::mutate(score_biological = params$score$biological$class) |>
      dplyr::left_join(metadata |> dplyr::distinct(
        feature_id,
        inchikey_2D,
        sample_organism_04_class
      )) |>
      dplyr::distinct(feature_id,
        inchikey_2D,
        best_candidate = candidate_organism_04_class,
        score_biological
      )

    cat("... order \n")
    step_ord <- dplyr::full_join(step_cla, sample_order) |>
      dplyr::distinct(
        inchikey_2D,
        sample_organism_05_order
      )
    step_ord <- dplyr::left_join(step_ord, candidate_order) |>
      dplyr::filter(candidate_organism_05_order != "notClassified") |>
      dplyr::filter(
        stringr::str_detect(pattern = candidate_organism_05_order, string = sample_organism_05_order)
      ) |>
      dplyr::mutate(score_biological = params$score$biological$order) |>
      dplyr::left_join(metadata |> dplyr::distinct(
        feature_id,
        inchikey_2D,
        sample_organism_05_order
      )) |>
      dplyr::distinct(feature_id,
        inchikey_2D,
        best_candidate = candidate_organism_05_order,
        score_biological
      )

    # cat("... infraorder \n")
    # step_ord2 <- full_join(step_ord, sample_infraorder) |>
    #   distinct(
    #     inchikey_2D,
    #     sample_organism_05_1_infraorder
    #   )
    # step_ord2 <- left_join(step_ord2, candidate_infraorder) |>
    #   filter(candidate_organism_05_1_infraorder != "notClassified") |>
    #   filter(
    #     str_detect(pattern = candidate_organism_05_1_infraorder, string = sample_organism_05_1_infraorder)
    #   ) |>
    #   dplyr::mutate(score_biological = params$score$biological$infraorder) |>
    #   left_join(
    #     metadata |> distinct(
    #       feature_id,
    #       inchikey_2D,
    #       sample_organism_05_1_infraorder
    #     )
    #   ) |>
    #   distinct(
    #     feature_id,
    #     inchikey_2D,
    #     best_candidate = candidate_organism_05_1_infraorder,
    #     score_biological
    #   )

    cat("... family \n")
    step_fam <- dplyr::full_join(step_ord, sample_family) |>
      dplyr::distinct(
        inchikey_2D,
        sample_organism_06_family
      )
    step_fam <- dplyr::left_join(step_fam, candidate_family) |>
      dplyr::filter(candidate_organism_06_family != "notClassified") |>
      dplyr::filter(
        stringr::str_detect(pattern = candidate_organism_06_family, string = sample_organism_06_family)
      ) |>
      dplyr::mutate(score_biological = params$score$biological$family) |>
      dplyr::left_join(metadata |> dplyr::distinct(
        feature_id,
        inchikey_2D,
        sample_organism_06_family
      )) |>
      dplyr::distinct(feature_id,
        inchikey_2D,
        best_candidate = candidate_organism_06_family,
        score_biological
      )

    # cat("... subfamily \n")
    # step_fam2 <- full_join(step_fam, sample_subfamily) |>
    #   distinct(
    #     inchikey_2D,
    #     sample_organism_06_1_subfamily
    #   )
    # step_fam2 <- left_join(step_fam2, candidate_subfamily) |>
    #   filter(candidate_organism_06_1_subfamily != "notClassified") |>
    #   filter(
    #     str_detect(pattern = candidate_organism_06_1_subfamily, string = sample_organism_06_1_subfamily)
    #   ) |>
    #   dplyr::mutate(score_biological = params$score$biological$subfamily) |>
    #   left_join(
    #     metadata |> distinct(
    #       feature_id,
    #       inchikey_2D,
    #       sample_organism_06_1_subfamily
    #     )
    #   ) |>
    #   distinct(
    #     feature_id,
    #     inchikey_2D,
    #     best_candidate = candidate_organism_06_1_subfamily,
    #     score_biological
    #   )

    cat("... tribe \n")
    step_tri <- dplyr::full_join(step_fam, sample_tribe) |>
      dplyr::distinct(
        inchikey_2D,
        sample_organism_07_tribe
      )
    step_tri <- dplyr::left_join(step_tri, candidate_tribe) |>
      dplyr::filter(candidate_organism_07_tribe != "notClassified") |>
      dplyr::filter(
        stringr::str_detect(pattern = candidate_organism_07_tribe, string = sample_organism_07_tribe)
      ) |>
      dplyr::mutate(score_biological = params$score$biological$tribe) |>
      dplyr::left_join(metadata |> dplyr::distinct(
        feature_id,
        inchikey_2D,
        sample_organism_07_tribe
      )) |>
      dplyr::distinct(feature_id,
        inchikey_2D,
        best_candidate = candidate_organism_07_tribe,
        score_biological
      )

    # cat("... subtribe \n")
    # step_tri2 <- full_join(step_tri, sample_subtribe) |>
    #   distinct(
    #     inchikey_2D,
    #     sample_organism_07_1_subtribe
    #   )
    # step_tri2 <- left_join(step_tri2, candidate_subtribe) |>
    #   filter(candidate_organism_07_1_subtribe != "notClassified") |>
    #   filter(
    #     str_detect(pattern = candidate_organism_07_1_subtribe, string = sample_organism_07_1_subtribe)
    #   ) |>
    #   dplyr::mutate(score_biological = params$score$biological$subtribe) |>
    #   left_join(
    #     metadata |> distinct(
    #       feature_id,
    #       inchikey_2D,
    #       sample_organism_07_1_subtribe
    #     )
    #   ) |>
    #   distinct(
    #     feature_id,
    #     inchikey_2D,
    #     best_candidate = candidate_organism_07_1_subtribe,
    #     score_biological
    #   )

    cat("... genus \n")
    step_gen <- dplyr::full_join(step_tri, sample_genus) |>
      dplyr::distinct(
        inchikey_2D,
        sample_organism_08_genus
      )
    step_gen <- dplyr::left_join(step_gen, candidate_genus) |>
      dplyr::filter(candidate_organism_08_genus != "notClassified") |>
      dplyr::filter(
        stringr::str_detect(pattern = candidate_organism_08_genus, string = sample_organism_08_genus)
      ) |>
      dplyr::mutate(score_biological = params$score$biological$genus) |>
      dplyr::left_join(metadata |> dplyr::distinct(
        feature_id,
        inchikey_2D,
        sample_organism_08_genus
      )) |>
      dplyr::distinct(feature_id,
        inchikey_2D,
        best_candidate = candidate_organism_08_genus,
        score_biological
      )

    # cat("... subgenus \n")
    # step_gen2 <- full_join(step_gen, sample_subgenus) |>
    #   distinct(
    #     inchikey_2D,
    #     sample_organism_08_1_subgenus
    #   )
    # step_gen2 <-
    #   left_join(step_gen2, candidate_subgenus) |>
    #   filter(candidate_organism_08_1_subgenus != "notClassified") |>
    #   filter(
    #     str_detect(pattern = candidate_organism_08_1_subgenus, string = sample_organism_08_1_subgenus)
    #   ) |>
    #   dplyr::mutate(score_biological = params$score$biological$subgenus) |>
    #   left_join(
    #     metadata |> distinct(
    #       feature_id,
    #       inchikey_2D,
    #       sample_organism_08_1_subgenus
    #     )
    #   ) |>
    #   distinct(
    #     feature_id,
    #     inchikey_2D,
    #     best_candidate = candidate_organism_08_1_subgenus,
    #     score_biological
    #   )

    cat("... species \n")
    step_spe <- dplyr::full_join(step_gen, sample_species) |>
      dplyr::distinct(
        inchikey_2D,
        sample_organism_09_species
      )
    step_spe <- dplyr::left_join(step_spe, candidate_species) |>
      dplyr::filter(candidate_organism_09_species != "notClassified") |>
      dplyr::filter(
        stringr::str_detect(pattern = candidate_organism_09_species, string = sample_organism_09_species)
      ) |>
      dplyr::mutate(score_biological = params$score$biological$species) |>
      dplyr::left_join(metadata |> dplyr::distinct(
        feature_id,
        inchikey_2D,
        sample_organism_09_species
      )) |>
      dplyr::distinct(feature_id,
        inchikey_2D,
        best_candidate = candidate_organism_09_species,
        score_biological
      )

    # cat("... subspecies \n")
    # step_spe2 <- full_join(step_spe, sample_subspecies) |>
    #   distinct(
    #     inchikey_2D,
    #     sample_organism_09_1_subspecies
    #   )
    # step_spe2 <-
    #   left_join(step_spe2, candidate_subspecies) |>
    #   filter(candidate_organism_09_1_subspecies != "notClassified") |>
    #   filter(
    #     str_detect(pattern = candidate_organism_09_1_subspecies, string = sample_organism_09_1_subspecies)
    #   ) |>
    #   mutate(score_biological = params$score$biological$subspecies) |>
    #   left_join(
    #     metadata |> distinct(
    #       feature_id,
    #       inchikey_2D,
    #       sample_organism_09_1_subspecies
    #     )
    #   ) |>
    #   distinct(
    #     feature_id,
    #     inchikey_2D,
    #     best_candidate = candidate_organism_09_1_subspecies,
    #     score_biological
    #   )

    cat("... varietas \n")
    step_var <- dplyr::full_join(step_spe, sample_varietas) |>
      dplyr::distinct(
        inchikey_2D,
        sample_organism_10_varietas
      )
    step_var <- dplyr::left_join(step_var, candidate_varietas) |>
      dplyr::filter(candidate_organism_10_varietas != "notClassified") |>
      dplyr::filter(
        stringr::str_detect(pattern = candidate_organism_10_varietas, string = sample_organism_10_varietas)
      ) |>
      dplyr::mutate(score_biological = params$score$biological$variety) |>
      dplyr::left_join(
        metadata |> dplyr::distinct(
          feature_id,
          inchikey_2D,
          sample_organism_10_varietas
        )
      ) |>
      dplyr::distinct(feature_id,
        inchikey_2D,
        best_candidate = candidate_organism_10_varietas,
        score_biological
      )

    cat("keeping best biological score only \n")
    biologically_weighted <- dplyr::bind_rows(
      step_dom,
      step_kin,
      step_phy,
      step_cla,
      step_ord,
      # step_ord2,
      step_fam,
      # step_fam2,
      step_gen,
      # step_gen2,
      step_spe,
      # step_spe2,
      step_var
    ) |>
      dplyr::group_by(feature_id) |>
      dplyr::arrange(dplyr::desc(score_biological)) |>
      dplyr::distinct(feature_id,
        inchikey_2D,
        .keep_all = TRUE
      ) |>
      dplyr::ungroup()

    cat("joining with initial results \n")
    biologically_weighted_full <-
      dplyr::left_join(metadata, biologically_weighted)

    biologically_weighted_full$score_biological[is.na(biologically_weighted_full$score_biological)] <-
      0

    biologically_weighted_full <- biologically_weighted_full |>
      dplyr::mutate(
        score_pondered_bio = (
          (1 / (
            params$weight$biological + params$weight$spectral
          )) *
            params$weight$biological *
            score_biological +
            (1 / (
              params$weight$biological + params$weight$spectral
            )) *
              params$weight$spectral *
              score_initialNormalized
        )
      )

    biologically_weighted_full$score_pondered_bio[is.na(biologically_weighted_full$score_pondered_bio)] <-
      0

    biologically_weighted_full <- biologically_weighted_full |>
      dplyr::group_by(feature_id) |>
      dplyr::arrange(dplyr::desc(score_pondered_bio)) |>
      dplyr::distinct(feature_id,
        inchikey_2D,
        .keep_all = TRUE
      ) |>
      dplyr::mutate(
        rank_initial = (dplyr::dense_rank(-score_initialNormalized)),
        rank_final = (dplyr::dense_rank(-score_pondered_bio))
      ) |>
      dplyr::arrange(
        rank_final,
        dplyr::desc(-score_pondered_bio)
      ) |>
      dplyr::arrange(as.numeric(feature_id)) |>
      dplyr::ungroup() |>
      dplyr::tibble()

    cat("adding \"notClassified\" \n")
    biologically_weighted_full[, sapply(biologically_weighted_full, class) == "character"][is.na(biologically_weighted_full[, sapply(biologically_weighted_full, class) == "character"])] <-
      "notClassified"

    return(biologically_weighted_full)
  }
