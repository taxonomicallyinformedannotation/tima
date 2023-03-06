#' @title Weight bio
#'
#' @description This function weights the eventually MS1 complemented annotations according their biological source
#'
#' @param annotationTable Table containing the initial annotation eventually complemented by additional MS1 annotations
#' @param structureOrganismPairsTable Table containing the structure - organism pairs
#' @param weightSpectral Weight for the spectral score
#' @param weightBiological Weight for the biological score
#' @param scoreBiologicalDomain Score for a `domain` match (should be lower than `kingdom`)
#' @param scoreBiologicalKingdom Score for a `kingdom` match (should be lower than `phylum`)
#' @param scoreBiologicalPhylum Score for a `phylum` match (should be lower than `class`)
#' @param scoreBiologicalClass Score for a `class` match (should be lower than `order`)
#' @param scoreBiologicalOrder Score for a `order` match (should be lower than `family`)
#' @param scoreBiologicalFamily Score for a `family` match (should be lower than `tribe`)
#' @param scoreBiologicalTribe Score for a `tribe` match (should be lower than `genus`)
#' @param scoreBiologicalGenus Score for a `genus` match (should be lower than `species`)
#' @param scoreBiologicalSpecies Score for a `species` match (should be lower than `variety`)
#' @param scoreBiologicalVariety Score for a `variety` match (should be the highest)
#'
#' @return A table containing the biologically weighted annotation
#'
#' @export
#'
#' @examples NULL
weight_bio <-
  function(annotationTable = annotation_table_taxed,
           structureOrganismPairsTable = structure_organism_pairs_table,
           weightSpectral = weight_spectral,
           weightBiological = weight_biological,
           scoreBiologicalDomain = score_biological_domain,
           scoreBiologicalKingdom = score_biological_kingdom,
           scoreBiologicalPhylum = score_biological_phylum,
           scoreBiologicalClass = score_biological_class,
           scoreBiologicalOrder = score_biological_order,
           scoreBiologicalFamily = score_biological_family,
           scoreBiologicalTribe = score_biological_tribe,
           scoreBiologicalGenus = score_biological_genus,
           scoreBiologicalSpecies = score_biological_species,
           scoreBiologicalVariety = score_biological_variety) {
    metadata <- annotationTable |>
      dplyr::select(
        feature_id,
        structure_molecular_formula,
        structure_inchikey_2D,
        structure_smiles_2D,
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
        sample_organism_10_varietas
      )

    sample_domain <- annotationTable |>
      dplyr::distinct(
        structure_inchikey_2D,
        sample_organism_01_domain
      )

    sample_kingdom <- annotationTable |>
      dplyr::distinct(
        structure_inchikey_2D,
        sample_organism_02_kingdom
      )

    sample_phylum <- annotationTable |>
      dplyr::distinct(
        structure_inchikey_2D,
        sample_organism_03_phylum
      )

    sample_class <- annotationTable |>
      dplyr::distinct(
        structure_inchikey_2D,
        sample_organism_04_class
      )

    sample_order <- annotationTable |>
      dplyr::distinct(
        structure_inchikey_2D,
        sample_organism_05_order
      )

    # sample_infraorder <- annotationTable |>
    #   dplyr::distinct(
    #     structure_inchikey_2D,
    #     sample_organism_05_1_infraorder
    #   )

    sample_family <- annotationTable |>
      dplyr::distinct(
        structure_inchikey_2D,
        sample_organism_06_family
      )

    # sample_subfamily <- annotationTable |>
    #   dplyr::distinct(
    #     structure_inchikey_2D,
    #     sample_organism_06_1_subfamily
    #   )

    sample_tribe <- annotationTable |>
      dplyr::distinct(
        structure_inchikey_2D,
        sample_organism_07_tribe
      )

    # sample_subtribe <- annotationTable |>
    #   dplyr::distinct(
    #     structure_inchikey_2D,
    #     sample_organism_07_1_subtribe
    #   )

    sample_genus <- annotationTable |>
      dplyr::distinct(
        structure_inchikey_2D,
        sample_organism_08_genus
      )

    # sample_subgenus <- annotationTable |>
    #   dplyr::distinct(
    #     structure_inchikey_2D,
    #     sample_organism_08_1_subgenus
    #   )

    sample_species <- annotationTable |>
      dplyr::distinct(
        structure_inchikey_2D,
        sample_organism_09_species
      )

    # sample_subspecies <- annotationTable |>
    #   dplyr::distinct(
    #     structure_inchikey_2D,
    #     sample_organism_09_1_subspecies
    #   )

    sample_varietas <- annotationTable |>
      dplyr::distinct(
        structure_inchikey_2D,
        sample_organism_10_varietas
      )

    log_debug("selecting DB columns \n")
    candidates <- structureOrganismPairsTable |>
      dplyr::filter(!is.na(structure_inchikey_2D)) |>
      dplyr::select(
        structure_inchikey_2D,
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
      ) |>
      dplyr::distinct()

    log_debug("keeping distinct candidates per taxonomical rank \n")

    candidate_domain <- candidates |>
      dplyr::filter(!is.na(candidate_organism_01_domain)) |>
      dplyr::distinct(
        structure_inchikey_2D,
        candidate_organism_01_domain
      )

    candidate_kingdom <- candidates |>
      dplyr::filter(!is.na(candidate_organism_02_kingdom)) |>
      dplyr::distinct(
        structure_inchikey_2D,
        candidate_organism_02_kingdom
      )

    candidate_phylum <- candidates |>
      dplyr::filter(!is.na(candidate_organism_03_phylum)) |>
      dplyr::distinct(
        structure_inchikey_2D,
        candidate_organism_03_phylum
      )

    candidate_class <- candidates |>
      dplyr::filter(!is.na(candidate_organism_04_class)) |>
      dplyr::distinct(
        structure_inchikey_2D,
        candidate_organism_04_class
      )

    candidate_order <- candidates |>
      dplyr::filter(!is.na(candidate_organism_05_order)) |>
      dplyr::distinct(
        structure_inchikey_2D,
        candidate_organism_05_order
      )

    # candidate_infraorder <- candidates |>
    #   dplyr::filter(!is.na(candidate_organism_05_1_infraorder)) |>
    #   dplyr::distinct(structure_inchikey_2D,
    #                   candidate_organism_05_1_infraorder)

    candidate_family <- candidates |>
      dplyr::filter(!is.na(candidate_organism_06_family)) |>
      dplyr::distinct(
        structure_inchikey_2D,
        candidate_organism_06_family
      )

    # candidate_subfamily <- candidates |>
    #   dplyr::filter(!is.na(candidate_organism_06_1_subfamily)) |>
    #   dplyr::distinct(structure_inchikey_2D,
    #                   candidate_organism_06_1_subfamily)

    candidate_tribe <- candidates |>
      dplyr::filter(!is.na(candidate_organism_07_tribe)) |>
      dplyr::distinct(
        structure_inchikey_2D,
        candidate_organism_07_tribe
      )

    # candidate_subtribe <- candidates |>
    #   dplyr::filter(!is.na(candidate_organism_07_1_subtribe)) |>
    #   dplyr::distinct(structure_inchikey_2D,
    #                   candidate_organism_07_1_subtribe)

    candidate_genus <- candidates |>
      dplyr::filter(!is.na(candidate_organism_08_genus)) |>
      dplyr::distinct(
        structure_inchikey_2D,
        candidate_organism_08_genus
      )

    # candidate_subgenus <- candidates |>
    #   dplyr::filter(!is.na(candidate_organism_08_1_subgenus)) |>
    #   dplyr::distinct(structure_inchikey_2D,
    #                   candidate_organism_08_1_subgenus)

    candidate_species <- candidates |>
      dplyr::filter(!is.na(candidate_organism_09_species)) |>
      dplyr::distinct(
        structure_inchikey_2D,
        candidate_organism_09_species
      )

    # candidate_subspecies <- candidates |>
    #   dplyr::filter(!is.na(candidate_organism_09_1_subspecies)) |>
    #   dplyr::distinct(structure_inchikey_2D,
    #                   candidate_organism_09_1_subspecies)

    candidate_varietas <- candidates |>
      dplyr::filter(!is.na(candidate_organism_10_varietas)) |>
      dplyr::distinct(
        structure_inchikey_2D,
        candidate_organism_10_varietas
      )

    log_debug("calculating biological scores ... \n")

    log_debug("... domain \n")
    step_dom <- dplyr::left_join(sample_domain, candidate_domain) |>
      dplyr::filter(candidate_organism_01_domain != "notClassified") |>
      dplyr::filter(
        stringr::str_detect(pattern = candidate_organism_01_domain, string = sample_organism_01_domain)
      ) |>
      dplyr::mutate(score_biological = scoreBiologicalDomain) |>
      dplyr::left_join(
        metadata |> dplyr::distinct(
          feature_id,
          structure_inchikey_2D,
          sample_organism_01_domain
        )
      ) |>
      dplyr::distinct(feature_id,
        structure_inchikey_2D,
        best_candidate = candidate_organism_01_domain,
        score_biological
      )

    log_debug("... kingdom \n")
    step_kin <- dplyr::full_join(step_dom, sample_kingdom) |>
      dplyr::distinct(
        structure_inchikey_2D,
        sample_organism_02_kingdom
      )
    step_kin <- dplyr::left_join(step_kin, candidate_kingdom) |>
      dplyr::filter(candidate_organism_02_kingdom != "notClassified") |>
      dplyr::filter(
        stringr::str_detect(pattern = candidate_organism_02_kingdom, string = sample_organism_02_kingdom)
      ) |>
      dplyr::mutate(score_biological = scoreBiologicalKingdom) |>
      dplyr::left_join(
        metadata |> dplyr::distinct(
          feature_id,
          structure_inchikey_2D,
          sample_organism_02_kingdom
        )
      ) |>
      dplyr::distinct(feature_id,
        structure_inchikey_2D,
        best_candidate = candidate_organism_02_kingdom,
        score_biological
      )

    log_debug("... phylum \n")
    step_phy <- dplyr::full_join(step_kin, sample_phylum) |>
      dplyr::distinct(
        structure_inchikey_2D,
        sample_organism_03_phylum
      )
    step_phy <- dplyr::left_join(step_phy, candidate_phylum) |>
      dplyr::filter(candidate_organism_03_phylum != "notClassified") |>
      dplyr::filter(
        stringr::str_detect(pattern = candidate_organism_03_phylum, string = sample_organism_03_phylum)
      ) |>
      dplyr::mutate(score_biological = scoreBiologicalPhylum) |>
      dplyr::left_join(
        metadata |> dplyr::distinct(
          feature_id,
          structure_inchikey_2D,
          sample_organism_03_phylum
        )
      ) |>
      dplyr::distinct(feature_id,
        structure_inchikey_2D,
        best_candidate = candidate_organism_03_phylum,
        score_biological
      )

    log_debug("... class \n")
    step_cla <- dplyr::full_join(step_phy, sample_class) |>
      dplyr::distinct(
        structure_inchikey_2D,
        sample_organism_04_class
      )
    step_cla <- dplyr::left_join(step_cla, candidate_class) |>
      dplyr::filter(candidate_organism_04_class != "notClassified") |>
      dplyr::filter(
        stringr::str_detect(pattern = candidate_organism_04_class, string = sample_organism_04_class)
      ) |>
      dplyr::mutate(score_biological = scoreBiologicalClass) |>
      dplyr::left_join(
        metadata |> dplyr::distinct(
          feature_id,
          structure_inchikey_2D,
          sample_organism_04_class
        )
      ) |>
      dplyr::distinct(feature_id,
        structure_inchikey_2D,
        best_candidate = candidate_organism_04_class,
        score_biological
      )

    log_debug("... order \n")
    step_ord <- dplyr::full_join(step_cla, sample_order) |>
      dplyr::distinct(
        structure_inchikey_2D,
        sample_organism_05_order
      )
    step_ord <- dplyr::left_join(step_ord, candidate_order) |>
      dplyr::filter(candidate_organism_05_order != "notClassified") |>
      dplyr::filter(
        stringr::str_detect(pattern = candidate_organism_05_order, string = sample_organism_05_order)
      ) |>
      dplyr::mutate(score_biological = scoreBiologicalOrder) |>
      dplyr::left_join(
        metadata |> dplyr::distinct(
          feature_id,
          structure_inchikey_2D,
          sample_organism_05_order
        )
      ) |>
      dplyr::distinct(feature_id,
        structure_inchikey_2D,
        best_candidate = candidate_organism_05_order,
        score_biological
      )

    # log_debug("... infraorder \n")
    # step_ord2 <- dplyr::full_join(step_ord, sample_infraorder) |>
    #   dplyr::distinct(structure_inchikey_2D,
    #                   sample_organism_05_1_infraorder)
    # step_ord2 <-
    #   dplyr::left_join(step_ord2, candidate_infraorder) |>
    #   dplyr::filter(candidate_organism_05_1_infraorder != "notClassified") |>
    #   dplyr::filter(
    #     stringr::str_detect(pattern = candidate_organism_05_1_infraorder, string = sample_organism_05_1_infraorder)
    #   ) |>
    #   dplyr::mutate(score_biological = scoreBiologicalInfraorder) |>
    #   dplyr::left_join(
    #     metadata |> dplyr::distinct(feature_id,
    #                                 structure_inchikey_2D,
    #                                 sample_organism_05_1_infraorder)
    #   ) |>
    #   dplyr::distinct(feature_id,
    #                   structure_inchikey_2D,
    #                   best_candidate = candidate_organism_05_1_infraorder,
    #                   score_biological)

    log_debug("... family \n")
    step_fam <- dplyr::full_join(step_ord, sample_family) |>
      dplyr::distinct(
        structure_inchikey_2D,
        sample_organism_06_family
      )
    step_fam <- dplyr::left_join(step_fam, candidate_family) |>
      dplyr::filter(candidate_organism_06_family != "notClassified") |>
      dplyr::filter(
        stringr::str_detect(pattern = candidate_organism_06_family, string = sample_organism_06_family)
      ) |>
      dplyr::mutate(score_biological = scoreBiologicalFamily) |>
      dplyr::left_join(
        metadata |> dplyr::distinct(
          feature_id,
          structure_inchikey_2D,
          sample_organism_06_family
        )
      ) |>
      dplyr::distinct(feature_id,
        structure_inchikey_2D,
        best_candidate = candidate_organism_06_family,
        score_biological
      )

    # log_debug("... subfamily \n")
    # step_fam2 <- dplyr::full_join(step_fam, sample_subfamily) |>
    #   dplyr::distinct(structure_inchikey_2D,
    #                   sample_organism_06_1_subfamily)
    # step_fam2 <- dplyr::left_join(step_fam2, candidate_subfamily) |>
    #   dplyr::filter(candidate_organism_06_1_subfamily != "notClassified") |>
    #   dplyr::filter(
    #     stringr::str_detect(pattern = candidate_organism_06_1_subfamily, string = sample_organism_06_1_subfamily)
    #   ) |>
    #   dplyr::mutate(score_biological = scoreBiologicalSubfamily) |>
    #   dplyr::left_join(
    #     metadata |> dplyr::distinct(feature_id,
    #                                 structure_inchikey_2D,
    #                                 sample_organism_06_1_subfamily)
    #   ) |>
    #   dplyr::distinct(feature_id,
    #                   structure_inchikey_2D,
    #                   best_candidate = candidate_organism_06_1_subfamily,
    #                   score_biological)

    log_debug("... tribe \n")
    step_tri <- dplyr::full_join(step_fam, sample_tribe) |>
      dplyr::distinct(
        structure_inchikey_2D,
        sample_organism_07_tribe
      )
    step_tri <- dplyr::left_join(step_tri, candidate_tribe) |>
      dplyr::filter(candidate_organism_07_tribe != "notClassified") |>
      dplyr::filter(
        stringr::str_detect(pattern = candidate_organism_07_tribe, string = sample_organism_07_tribe)
      ) |>
      dplyr::mutate(score_biological = scoreBiologicalTribe) |>
      dplyr::left_join(
        metadata |> dplyr::distinct(
          feature_id,
          structure_inchikey_2D,
          sample_organism_07_tribe
        )
      ) |>
      dplyr::distinct(feature_id,
        structure_inchikey_2D,
        best_candidate = candidate_organism_07_tribe,
        score_biological
      )

    # log_debug("... subtribe \n")
    # step_tri2 <- dplyr::full_join(step_tri, sample_subtribe) |>
    #   dplyr::distinct(
    #     structure_inchikey_2D,
    #     sample_organism_07_1_subtribe
    #   )
    # step_tri2 <- dplyr::left_join(step_tri2, candidate_subtribe) |>
    #   dplyr::filter(candidate_organism_07_1_subtribe != "notClassified") |>
    #   dplyr::filter(
    #     stringr::str_detect(pattern = candidate_organism_07_1_subtribe, string = sample_organism_07_1_subtribe)
    #   ) |>
    #   dplyr::mutate(score_biological = scoreBiologicalSubtribe) |>
    #   dplyr::left_join(
    #     metadata |> dplyr::distinct(feature_id,
    #                                 structure_inchikey_2D,
    #                                 sample_organism_07_1_subtribe)
    #   ) |>
    #   dplyr::distinct(feature_id,
    #                   structure_inchikey_2D,
    #                   best_candidate = candidate_organism_07_1_subtribe,
    #                   score_biological)

    log_debug("... genus \n")
    step_gen <- dplyr::full_join(step_tri, sample_genus) |>
      dplyr::distinct(
        structure_inchikey_2D,
        sample_organism_08_genus
      )
    step_gen <- dplyr::left_join(step_gen, candidate_genus) |>
      dplyr::filter(candidate_organism_08_genus != "notClassified") |>
      dplyr::filter(
        stringr::str_detect(pattern = candidate_organism_08_genus, string = sample_organism_08_genus)
      ) |>
      dplyr::mutate(score_biological = scoreBiologicalGenus) |>
      dplyr::left_join(
        metadata |> dplyr::distinct(
          feature_id,
          structure_inchikey_2D,
          sample_organism_08_genus
        )
      ) |>
      dplyr::distinct(feature_id,
        structure_inchikey_2D,
        best_candidate = candidate_organism_08_genus,
        score_biological
      )

    # log_debug("... subgenus \n")
    # step_gen2 <- dplyr::full_join(step_gen, sample_subgenus) |>
    #   dplyr::distinct(structure_inchikey_2D,
    #                   sample_organism_08_1_subgenus)
    # step_gen2 <-
    #   dplyr::left_join(step_gen2, candidate_subgenus) |>
    #   dplyr::filter(candidate_organism_08_1_subgenus != "notClassified") |>
    #   dplyr::filter(
    #     stringr::str_detect(pattern = candidate_organism_08_1_subgenus, string = sample_organism_08_1_subgenus)
    #   ) |>
    #   dplyr::mutate(score_biological = scoreBiologicalSubgenus) |>
    #   dplyr::left_join(
    #     metadata |> dplyr::distinct(feature_id,
    #                                 structure_inchikey_2D,
    #                                 sample_organism_08_1_subgenus)
    #   ) |>
    #   dplyr::distinct(feature_id,
    #                   structure_inchikey_2D,
    #                   best_candidate = candidate_organism_08_1_subgenus,
    #                   score_biological)

    log_debug("... species \n")
    step_spe <- dplyr::full_join(step_gen, sample_species) |>
      dplyr::distinct(
        structure_inchikey_2D,
        sample_organism_09_species
      )
    step_spe <- dplyr::left_join(step_spe, candidate_species) |>
      dplyr::filter(candidate_organism_09_species != "notClassified") |>
      dplyr::filter(
        stringr::str_detect(pattern = candidate_organism_09_species, string = sample_organism_09_species)
      ) |>
      dplyr::mutate(score_biological = scoreBiologicalSpecies) |>
      dplyr::left_join(
        metadata |> dplyr::distinct(
          feature_id,
          structure_inchikey_2D,
          sample_organism_09_species
        )
      ) |>
      dplyr::distinct(feature_id,
        structure_inchikey_2D,
        best_candidate = candidate_organism_09_species,
        score_biological
      )

    # log_debug("... subspecies \n")
    # step_spe2 <- dplyr::full_join(step_spe, sample_subspecies) |>
    #   dplyr::distinct(structure_inchikey_2D,
    #                   sample_organism_09_1_subspecies)
    # step_spe2 <-
    #   dplyr::left_join(step_spe2, candidate_subspecies) |>
    #   dplyr::filter(candidate_organism_09_1_subspecies != "notClassified") |>
    #   dplyr::filter(
    #     stringr::str_detect(pattern = candidate_organism_09_1_subspecies, string = sample_organism_09_1_subspecies)
    #   ) |>
    #   dplyr::mutate(score_biological = scoreBiologicalSubspecies) |>
    #   dplyr::left_join(
    #     metadata |> dplyr::distinct(feature_id,
    #                                 structure_inchikey_2D,
    #                                 sample_organism_09_1_subspecies)
    #   ) |>
    #   dplyr::distinct(feature_id,
    #                   structure_inchikey_2D,
    #                   best_candidate = candidate_organism_09_1_subspecies,
    #                   score_biological)

    log_debug("... varietas \n")
    step_var <- dplyr::full_join(step_spe, sample_varietas) |>
      dplyr::distinct(
        structure_inchikey_2D,
        sample_organism_10_varietas
      )
    step_var <- dplyr::left_join(step_var, candidate_varietas) |>
      dplyr::filter(candidate_organism_10_varietas != "notClassified") |>
      dplyr::filter(
        stringr::str_detect(pattern = candidate_organism_10_varietas, string = sample_organism_10_varietas)
      ) |>
      dplyr::mutate(score_biological = scoreBiologicalVariety) |>
      dplyr::left_join(
        metadata |> dplyr::distinct(
          feature_id,
          structure_inchikey_2D,
          sample_organism_10_varietas
        )
      ) |>
      dplyr::distinct(feature_id,
        structure_inchikey_2D,
        best_candidate = candidate_organism_10_varietas,
        score_biological
      )

    log_debug("keeping best biological score only \n")
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
        structure_inchikey_2D,
        .keep_all = TRUE
      ) |>
      dplyr::ungroup()

    log_debug("joining with initial results \n")
    biologically_weighted_full <-
      dplyr::left_join(
        annotationTable |>
          dplyr::select(-dplyr::contains("sample_")),
        biologically_weighted
      )

    biologically_weighted_full$score_biological[is.na(biologically_weighted_full$score_biological)] <-
      0

    biologically_weighted_full <- biologically_weighted_full |>
      dplyr::mutate(
        score_pondered_bio = (
          (1 / (weightBiological + weightSpectral)) *
            weightBiological *
            score_biological +
            (1 / (weightBiological + weightSpectral)) *
              weightSpectral *
              as.numeric(score_input)
        )
      )

    biologically_weighted_full$score_pondered_bio[is.na(biologically_weighted_full$score_pondered_bio)] <-
      0

    biologically_weighted_full <- biologically_weighted_full |>
      dplyr::group_by(feature_id) |>
      dplyr::arrange(dplyr::desc(score_pondered_bio)) |>
      dplyr::distinct(
        feature_id,
        structure_inchikey_2D,
        structure_smiles_2D,
        structure_taxonomy_npclassifier_01pathway,
        structure_taxonomy_npclassifier_02superclass,
        structure_taxonomy_npclassifier_03class,
        .keep_all = TRUE
      ) |>
      dplyr::mutate(
        rank_initial = (dplyr::dense_rank(-as.numeric(score_input))),
        rank_final = (dplyr::dense_rank(-score_pondered_bio))
      ) |>
      dplyr::arrange(
        rank_final,
        dplyr::desc(-score_pondered_bio)
      ) |>
      dplyr::arrange(as.numeric(feature_id)) |>
      dplyr::ungroup() |>
      dplyr::tibble()

    return(biologically_weighted_full)
  }
