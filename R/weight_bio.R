#' @title Weight bio
#'
#' @description TODO
#'
#' @param annotationTable TODO
#' @param structureOrganismPairsTable TODO
#' @param weightSpectral TODO
#' @param weightBiological TODO
#' @param scoreBiologicalDomain TODO
#' @param scoreBiologicalKingdom TODO
#' @param scoreBiologicalPhylum TODO
#' @param scoreBiologicalClass TODO
#' @param scoreBiologicalOrder TODO
#' @param scoreBiologicalFamily TODO
#' @param scoreBiologicalTribe TODO
#' @param scoreBiologicalGenus TODO
#' @param scoreBiologicalSpecies TODO
#' @param scoreBiologicalVariety TODO
#'
#' @return NULL
#'
#' @export
#'
#' @importFrom dplyr across arrange bind_rows dense_rank desc distinct filter
#' @importFrom dplyr full_join group_by left_join mutate select tibble ungroup
#' @importFrom stringr str_detect
#'
#' @examples NULL
weight_bio <-
  function(annotationTable = annotation_table_ms1_taxed,
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
    cat("normalizing initial score \n")
    metadata <- annotationTable |>
      dplyr::select(
        feature_id,
        component_id,
        molecular_formula,
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
    ## Better not
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
        organism_name,
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

    candidate_domain_full <- candidates |>
      dplyr::filter(!is.na(candidate_organism_01_domain)) |>
      dplyr::distinct(
        inchikey_2D,
        candidate_organism_01_domain,
        organism_name
      )

    candidate_domain <- candidate_domain_full |>
      dplyr::distinct(
        inchikey_2D,
        candidate_organism_01_domain
      )

    candidate_kingdom_full <- candidates |>
      dplyr::filter(!is.na(candidate_organism_02_kingdom)) |>
      dplyr::distinct(
        inchikey_2D,
        candidate_organism_02_kingdom,
        organism_name
      )

    candidate_kingdom <- candidate_kingdom_full |>
      dplyr::distinct(
        inchikey_2D,
        candidate_organism_02_kingdom
      )

    candidate_phylum_full <- candidates |>
      dplyr::filter(!is.na(candidate_organism_03_phylum)) |>
      dplyr::distinct(
        inchikey_2D,
        candidate_organism_03_phylum,
        organism_name
      )

    candidate_phylum <- candidate_phylum_full |>
      dplyr::distinct(
        inchikey_2D,
        candidate_organism_03_phylum
      )

    candidate_class_full <- candidates |>
      dplyr::filter(!is.na(candidate_organism_04_class)) |>
      dplyr::distinct(
        inchikey_2D,
        candidate_organism_04_class,
        organism_name
      )

    candidate_class <- candidate_class_full |>
      dplyr::distinct(
        inchikey_2D,
        candidate_organism_04_class
      )

    candidate_order_full <- candidates |>
      dplyr::filter(!is.na(candidate_organism_05_order)) |>
      dplyr::distinct(
        inchikey_2D,
        candidate_organism_05_order,
        organism_name
      )

    candidate_order <- candidate_order_full |>
      dplyr::distinct(
        inchikey_2D,
        candidate_organism_05_order
      )

    # candidate_infraorder_full <- candidates |>
    #   dplyr::filter(!is.na(candidate_organism_05_1_infraorder)) |>
    #   dplyr::distinct(inchikey_2D,
    #                   candidate_organism_05_1_infraorder,
    #                   organism_name)
    #
    # candidate_infraorder <- candidate_infraorder_full |>
    #   dplyr::distinct(inchikey_2D,
    #                   candidate_organism_05_1_infraorder)

    candidate_family_full <- candidates |>
      dplyr::filter(!is.na(candidate_organism_06_family)) |>
      dplyr::distinct(
        inchikey_2D,
        candidate_organism_06_family,
        organism_name
      )

    candidate_family <- candidate_family_full |>
      dplyr::distinct(
        inchikey_2D,
        candidate_organism_06_family
      )

    # candidate_subfamily_full <- candidates |>
    #   dplyr::filter(!is.na(candidate_organism_06_1_subfamily)) |>
    #   dplyr::distinct(inchikey_2D,
    #                   candidate_organism_06_1_subfamily,
    #                   organism_name)
    #
    # candidate_subfamily <- candidate_subfamily_full |>
    #   dplyr::distinct(inchikey_2D,
    #                   candidate_organism_06_1_subfamily)

    candidate_tribe_full <- candidates |>
      dplyr::filter(!is.na(candidate_organism_07_tribe)) |>
      dplyr::distinct(
        inchikey_2D,
        candidate_organism_07_tribe,
        organism_name
      )

    candidate_tribe <- candidate_tribe_full |>
      dplyr::distinct(
        inchikey_2D,
        candidate_organism_07_tribe
      )

    # candidate_subtribe_full <- candidates |>
    #   dplyr::filter(!is.na(candidate_organism_07_1_subtribe)) |>
    #   dplyr::distinct(inchikey_2D,
    #                   candidate_organism_07_1_subtribe,
    #                   organism_name)
    #
    # candidate_subtribe <- candidate_subtribe_full |>
    #   dplyr::distinct(inchikey_2D,
    #                   candidate_organism_07_1_subtribe)

    candidate_genus_full <- candidates |>
      dplyr::filter(!is.na(candidate_organism_08_genus)) |>
      dplyr::distinct(
        inchikey_2D,
        candidate_organism_08_genus,
        organism_name
      )

    candidate_genus <- candidate_genus_full |>
      dplyr::distinct(
        inchikey_2D,
        candidate_organism_08_genus
      )

    # candidate_subgenus_full <- candidates |>
    #   dplyr::filter(!is.na(candidate_organism_08_1_subgenus)) |>
    #   dplyr::distinct(inchikey_2D,
    #                   candidate_organism_08_1_subgenus,
    #                   organism_name)
    #
    # candidate_subgenus <- candidate_subgenus_full |>
    #   dplyr::distinct(inchikey_2D,
    #                   candidate_organism_08_1_subgenus)

    candidate_species_full <- candidates |>
      dplyr::filter(!is.na(candidate_organism_09_species)) |>
      dplyr::distinct(
        inchikey_2D,
        candidate_organism_09_species,
        organism_name
      )

    candidate_species <- candidate_species_full |>
      dplyr::distinct(
        inchikey_2D,
        candidate_organism_09_species
      )

    # candidate_subspecies_full <- candidates |>
    #   dplyr::filter(!is.na(candidate_organism_09_1_subspecies)) |>
    #   dplyr::distinct(inchikey_2D,
    #                   candidate_organism_09_1_subspecies,
    #                   organism_name)
    #
    # candidate_subspecies <- candidate_subspecies_full |>
    #   dplyr::distinct(inchikey_2D,
    #                   candidate_organism_09_1_subspecies)

    candidate_varietas_full <- candidates |>
      dplyr::filter(!is.na(candidate_organism_10_varietas)) |>
      dplyr::distinct(
        inchikey_2D,
        candidate_organism_10_varietas,
        organism_name
      )

    candidate_varietas <- candidate_varietas_full |>
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
      dplyr::mutate(score_biological = scoreBiologicalDomain) |>
      dplyr::left_join(metadata |> dplyr::distinct(
        feature_id,
        inchikey_2D,
        sample_organism_01_domain
      )) |>
      dplyr::left_join(candidate_domain_full) |>
      dplyr::distinct(feature_id,
        inchikey_2D,
        best_candidate = candidate_organism_01_domain,
        organism_name,
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
      dplyr::mutate(score_biological = scoreBiologicalKingdom) |>
      dplyr::left_join(metadata |> dplyr::distinct(
        feature_id,
        inchikey_2D,
        sample_organism_02_kingdom
      )) |>
      dplyr::left_join(candidate_kingdom_full) |>
      dplyr::distinct(feature_id,
        inchikey_2D,
        best_candidate = candidate_organism_02_kingdom,
        organism_name,
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
      dplyr::mutate(score_biological = scoreBiologicalPhylum) |>
      dplyr::left_join(metadata |> dplyr::distinct(
        feature_id,
        inchikey_2D,
        sample_organism_03_phylum
      )) |>
      dplyr::left_join(candidate_phylum_full) |>
      dplyr::distinct(feature_id,
        inchikey_2D,
        best_candidate = candidate_organism_03_phylum,
        organism_name,
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
      dplyr::mutate(score_biological = scoreBiologicalClass) |>
      dplyr::left_join(metadata |> dplyr::distinct(
        feature_id,
        inchikey_2D,
        sample_organism_04_class
      )) |>
      dplyr::left_join(candidate_class_full) |>
      dplyr::distinct(feature_id,
        inchikey_2D,
        best_candidate = candidate_organism_04_class,
        organism_name,
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
      dplyr::mutate(score_biological = scoreBiologicalOrder) |>
      dplyr::left_join(metadata |> dplyr::distinct(
        feature_id,
        inchikey_2D,
        sample_organism_05_order
      )) |>
      dplyr::left_join(candidate_order_full) |>
      dplyr::distinct(feature_id,
        inchikey_2D,
        best_candidate = candidate_organism_05_order,
        organism_name,
        score_biological
      )

    # cat("... infraorder \n")
    # step_ord2 <- dplyr::full_join(step_ord, sample_infraorder) |>
    #   dplyr::distinct(inchikey_2D,
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
    #                                 inchikey_2D,
    #                                 sample_organism_05_1_infraorder)
    #   ) |>
    #   dplyr::left_join(candidate_infraorder_full) |>
    #   dplyr::distinct(feature_id,
    #                   inchikey_2D,
    #                   best_candidate = candidate_organism_05_1_infraorder,
    #                   organism_name,
    #                   score_biological)

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
      dplyr::mutate(score_biological = scoreBiologicalFamily) |>
      dplyr::left_join(metadata |> dplyr::distinct(
        feature_id,
        inchikey_2D,
        sample_organism_06_family
      )) |>
      dplyr::left_join(candidate_family_full) |>
      dplyr::distinct(feature_id,
        inchikey_2D,
        best_candidate = candidate_organism_06_family,
        organism_name,
        score_biological
      )

    # cat("... subfamily \n")
    # step_fam2 <- dplyr::full_join(step_fam, sample_subfamily) |>
    #   dplyr::distinct(inchikey_2D,
    #                   sample_organism_06_1_subfamily)
    # step_fam2 <- dplyr::left_join(step_fam2, candidate_subfamily) |>
    #   dplyr::filter(candidate_organism_06_1_subfamily != "notClassified") |>
    #   dplyr::filter(
    #     sstringr::str_detect(pattern = candidate_organism_06_1_subfamily, string = sample_organism_06_1_subfamily)
    #   ) |>
    #   dplyr::mutate(score_biological = scoreBiologicalSubfamily) |>
    #   dplyr::left_join(
    #     metadata |> dplyr::distinct(feature_id,
    #                                 inchikey_2D,
    #                                 sample_organism_06_1_subfamily)
    #   ) |>
    #   dplyr::left_join(candidate_subfamily_full) |>
    #   dplyr::distinct(feature_id,
    #                   inchikey_2D,
    #                   best_candidate = candidate_organism_06_1_subfamily,
    #                   organism_name,
    #                   score_biological)

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
      dplyr::mutate(score_biological = scoreBiologicalTribe) |>
      dplyr::left_join(metadata |> dplyr::distinct(
        feature_id,
        inchikey_2D,
        sample_organism_07_tribe
      )) |>
      dplyr::left_join(candidate_tribe_full) |>
      dplyr::distinct(feature_id,
        inchikey_2D,
        best_candidate = candidate_organism_07_tribe,
        organism_name,
        score_biological
      )

    # cat("... subtribe \n")
    # step_tri2 <- dplyr::full_join(step_tri, sample_subtribe) |>
    #   dplyr::distinct(
    #     inchikey_2D,
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
    #                                 inchikey_2D,
    #                                 sample_organism_07_1_subtribe)
    #   ) |>
    #   dplyr::left_join(candidate_subtribe_full) |>
    #   dplyr::distinct(feature_id,
    #                   inchikey_2D,
    #                   best_candidate = candidate_organism_07_1_subtribe,
    #                   organism_name,
    #                   score_biological)

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
      dplyr::mutate(score_biological = scoreBiologicalGenus) |>
      dplyr::left_join(metadata |> dplyr::distinct(
        feature_id,
        inchikey_2D,
        sample_organism_08_genus
      )) |>
      dplyr::left_join(candidate_genus_full) |>
      dplyr::distinct(feature_id,
        inchikey_2D,
        best_candidate = candidate_organism_08_genus,
        organism_name,
        score_biological
      )

    # cat("... subgenus \n")
    # step_gen2 <- dplyr::full_join(step_gen, sample_subgenus) |>
    #   dplyr::distinct(inchikey_2D,
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
    #                                 inchikey_2D,
    #                                 sample_organism_08_1_subgenus)
    #   ) |>
    # dplyr::left_join(candidate_subgenus_full) |>
    #   dplyr::distinct(feature_id,
    #                   inchikey_2D,
    #                   best_candidate = candidate_organism_08_1_subgenus,
    #                   organism_name,
    #                   score_biological)

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
      dplyr::mutate(score_biological = scoreBiologicalSpecies) |>
      dplyr::left_join(metadata |> dplyr::distinct(
        feature_id,
        inchikey_2D,
        sample_organism_09_species
      )) |>
      dplyr::left_join(candidate_species_full) |>
      dplyr::distinct(feature_id,
        inchikey_2D,
        best_candidate = candidate_organism_09_species,
        organism_name,
        score_biological
      )

    # cat("... subspecies \n")
    # step_spe2 <- dplyr::full_join(step_spe, sample_subspecies) |>
    #   dplyr::distinct(inchikey_2D,
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
    #                                 inchikey_2D,
    #                                 sample_organism_09_1_subspecies)
    #   ) |>
    #   dplyr::left_join(candidate_subspecies_full) |>
    #   dplyr::distinct(feature_id,
    #                   inchikey_2D,
    #                   best_candidate = candidate_organism_09_1_subspecies,
    #                   organism_name,
    #                   score_biological)

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
      dplyr::mutate(score_biological = scoreBiologicalVariety) |>
      dplyr::left_join(
        metadata |> dplyr::distinct(
          feature_id,
          inchikey_2D,
          sample_organism_10_varietas
        )
      ) |>
      dplyr::left_join(candidate_varietas_full) |>
      dplyr::distinct(feature_id,
        inchikey_2D,
        best_candidate = candidate_organism_10_varietas,
        organism_name,
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
          (1 / (weightBiological + weightSpectral)) *
            weightBiological *
            score_biological +
            (1 / (weightBiological + weightSpectral)) *
              weightSpectral *
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
        smiles_2D,
        structure_taxonomy_npclassifier_01pathway,
        structure_taxonomy_npclassifier_02superclass,
        structure_taxonomy_npclassifier_03class,
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
