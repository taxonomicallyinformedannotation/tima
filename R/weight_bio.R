utils::globalVariables(
  c(
    "candidate_organism_01_domain",
    "candidate_organism_02_kingdom",
    "candidate_organism_03_phylum",
    "candidate_organism_04_class",
    "candidate_organism_05_order",
    "candidate_organism_06_family",
    "candidate_organism_07_tribe",
    "candidate_organism_08_genus",
    "candidate_organism_09_species",
    "candidate_organism_10_varietas",
    "feature_id",
    "organism_taxonomy_01domain",
    "organism_taxonomy_02kingdom",
    "organism_taxonomy_03phylum",
    "organism_taxonomy_04class",
    "organism_taxonomy_05order",
    "organism_taxonomy_06family",
    "organism_taxonomy_07tribe",
    "organism_taxonomy_08genus",
    "organism_taxonomy_09species",
    "organism_taxonomy_10varietas",
    "rank_final",
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
    "score_biological",
    "score_biological_class",
    "score_biological_domain",
    "score_biological_family",
    "score_biological_genus",
    "score_biological_kingdom",
    "score_biological_order",
    "score_biological_phylum",
    "score_biological_species",
    "score_biological_tribe",
    "score_biological_variety",
    "score_input",
    # "score_input_normalized",
    "score_pondered_bio",
    "structure_inchikey_2D",
    "structure_molecular_formula",
    "structure_organism_pairs_table",
    "structure_smiles_2D",
    "structure_taxonomy_npclassifier_01pathway",
    "structure_taxonomy_npclassifier_02superclass",
    "structure_taxonomy_npclassifier_03class",
    "weight_biological",
    "weight_spectral"
  )
)

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
      tidytable::select(
        feature_id,
        structure_molecular_formula,
        structure_inchikey_2D,
        structure_smiles_2D,
        score_input,
        # score_input_normalized,
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
      tidytable::filter(sample_organism_01_domain != "ND") |>
      tidytable::distinct(
        structure_inchikey_2D,
        sample_organism_01_domain
      )

    sample_kingdom <- annotationTable |>
      tidytable::filter(sample_organism_02_kingdom != "ND") |>
      tidytable::distinct(
        structure_inchikey_2D,
        sample_organism_02_kingdom
      )

    sample_phylum <- annotationTable |>
      tidytable::filter(sample_organism_03_phylum != "ND") |>
      tidytable::distinct(
        structure_inchikey_2D,
        sample_organism_03_phylum
      )

    sample_class <- annotationTable |>
      tidytable::filter(sample_organism_04_class != "ND") |>
      tidytable::distinct(
        structure_inchikey_2D,
        sample_organism_04_class
      )

    sample_order <- annotationTable |>
      tidytable::filter(sample_organism_05_order != "ND") |>
      tidytable::distinct(
        structure_inchikey_2D,
        sample_organism_05_order
      )

    # sample_infraorder <- annotationTable |>
    #   tidytable::filter(sample_organism_05_1_infraorder != "ND") |>
    #   tidytable::distinct(
    #     structure_inchikey_2D,
    #     sample_organism_05_1_infraorder
    #   )

    sample_family <- annotationTable |>
      tidytable::filter(sample_organism_06_family != "ND") |>
      tidytable::distinct(
        structure_inchikey_2D,
        sample_organism_06_family
      )

    # sample_subfamily <- annotationTable |>
    #   tidytable::filter(sample_organism_06_1_subfamily != "ND") |>
    #   tidytable::distinct(
    #     structure_inchikey_2D,
    #     sample_organism_06_1_subfamily
    #   )

    sample_tribe <- annotationTable |>
      tidytable::filter(sample_organism_07_tribe != "ND") |>
      tidytable::distinct(
        structure_inchikey_2D,
        sample_organism_07_tribe
      )

    # sample_subtribe <- annotationTable |>
    #   tidytable::filter(sample_organism_07_1_subtribe != "ND") |>
    #   tidytable::distinct(
    #     structure_inchikey_2D,
    #     sample_organism_07_1_subtribe
    #   )

    sample_genus <- annotationTable |>
      tidytable::filter(sample_organism_08_genus != "ND") |>
      tidytable::distinct(
        structure_inchikey_2D,
        sample_organism_08_genus
      )

    # sample_subgenus <- annotationTable |>
    #   tidytable::filter(sample_organism_08_1_subgenus != "ND") |>
    #   tidytable::distinct(
    #     structure_inchikey_2D,
    #     sample_organism_08_1_subgenus
    #   )

    sample_species <- annotationTable |>
      tidytable::filter(sample_organism_09_species != "ND") |>
      tidytable::distinct(
        structure_inchikey_2D,
        sample_organism_09_species
      )

    # sample_subspecies <- annotationTable |>
    #   tidytable::filter(sample_organism_09_1_subspecies != "ND") |>
    #   tidytable::distinct(
    #     structure_inchikey_2D,
    #     sample_organism_09_1_subspecies
    #   )

    sample_varietas <- annotationTable |>
      tidytable::filter(sample_organism_10_varietas != "ND") |>
      tidytable::distinct(
        structure_inchikey_2D,
        sample_organism_10_varietas
      )

    log_debug("selecting DB columns \n")
    candidates <- structureOrganismPairsTable |>
      tidytable::filter(!is.na(structure_inchikey_2D)) |>
      tidytable::select(
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
      tidytable::distinct() |>
      tidyft::mutate(tidytable::across(
        tidytable::everything(),
        .fns = function(x) {
          tidytable::na_if(x, "")
        }
      ))

    log_debug("keeping distinct candidates per taxonomical rank \n")

    candidate_domain <- candidates |>
      tidytable::filter(!is.na(candidate_organism_01_domain)) |>
      tidytable::distinct(
        structure_inchikey_2D,
        candidate_organism_01_domain
      )

    candidate_kingdom <- candidates |>
      tidytable::filter(!is.na(candidate_organism_02_kingdom)) |>
      tidytable::distinct(
        structure_inchikey_2D,
        candidate_organism_02_kingdom
      )

    candidate_phylum <- candidates |>
      tidytable::filter(!is.na(candidate_organism_03_phylum)) |>
      tidytable::distinct(
        structure_inchikey_2D,
        candidate_organism_03_phylum
      )

    candidate_class <- candidates |>
      tidytable::filter(!is.na(candidate_organism_04_class)) |>
      tidytable::distinct(
        structure_inchikey_2D,
        candidate_organism_04_class
      )

    candidate_order <- candidates |>
      tidytable::filter(!is.na(candidate_organism_05_order)) |>
      tidytable::distinct(
        structure_inchikey_2D,
        candidate_organism_05_order
      )

    # candidate_infraorder <- candidates |>
    #   tidytable::filter(!is.na(candidate_organism_05_1_infraorder)) |>
    #   tidytable::distinct(structure_inchikey_2D,
    #                   candidate_organism_05_1_infraorder)

    candidate_family <- candidates |>
      tidytable::filter(!is.na(candidate_organism_06_family)) |>
      tidytable::distinct(
        structure_inchikey_2D,
        candidate_organism_06_family
      )

    # candidate_subfamily <- candidates |>
    #   tidytable::filter(!is.na(candidate_organism_06_1_subfamily)) |>
    #   tidytable::distinct(structure_inchikey_2D,
    #                   candidate_organism_06_1_subfamily)

    candidate_tribe <- candidates |>
      tidytable::filter(!is.na(candidate_organism_07_tribe)) |>
      tidytable::distinct(
        structure_inchikey_2D,
        candidate_organism_07_tribe
      )

    # candidate_subtribe <- candidates |>
    #   tidytable::filter(!is.na(candidate_organism_07_1_subtribe)) |>
    #   tidytable::distinct(structure_inchikey_2D,
    #                   candidate_organism_07_1_subtribe)

    candidate_genus <- candidates |>
      tidytable::filter(!is.na(candidate_organism_08_genus)) |>
      tidytable::distinct(
        structure_inchikey_2D,
        candidate_organism_08_genus
      )

    # candidate_subgenus <- candidates |>
    #   tidytable::filter(!is.na(candidate_organism_08_1_subgenus)) |>
    #   tidytable::distinct(structure_inchikey_2D,
    #                   candidate_organism_08_1_subgenus)

    candidate_species <- candidates |>
      tidytable::filter(!is.na(candidate_organism_09_species)) |>
      tidytable::distinct(
        structure_inchikey_2D,
        candidate_organism_09_species
      )

    # candidate_subspecies <- candidates |>
    #   tidytable::filter(!is.na(candidate_organism_09_1_subspecies)) |>
    #   tidytable::distinct(structure_inchikey_2D,
    #                   candidate_organism_09_1_subspecies)

    candidate_varietas <- candidates |>
      tidytable::filter(!is.na(candidate_organism_10_varietas)) |>
      tidytable::distinct(
        structure_inchikey_2D,
        candidate_organism_10_varietas
      )

    log_debug("calculating biological scores ... \n")

    log_debug("... domain \n")
    step_dom <- tidytable::left_join(sample_domain, candidate_domain) |>
      tidytable::filter(candidate_organism_01_domain != "notClassified") |>
      tidytable::filter(
        stringi::stri_detect_regex(pattern = candidate_organism_01_domain, str = sample_organism_01_domain)
      ) |>
      tidyft::mutate(score_biological = scoreBiologicalDomain) |>
      tidytable::left_join(
        metadata |> tidytable::distinct(
          feature_id,
          structure_inchikey_2D,
          sample_organism_01_domain
        )
      ) |>
      tidytable::distinct(feature_id,
        structure_inchikey_2D,
        best_candidate = candidate_organism_01_domain,
        score_biological
      )

    log_debug("... kingdom \n")
    step_kin <- tidytable::full_join(step_dom, sample_kingdom) |>
      tidytable::distinct(
        structure_inchikey_2D,
        sample_organism_02_kingdom
      )
    step_kin <- tidytable::left_join(step_kin, candidate_kingdom) |>
      tidytable::filter(candidate_organism_02_kingdom != "notClassified") |>
      tidytable::filter(
        stringi::stri_detect_regex(pattern = candidate_organism_02_kingdom, str = sample_organism_02_kingdom)
      ) |>
      tidyft::mutate(score_biological = scoreBiologicalKingdom) |>
      tidytable::left_join(
        metadata |> tidytable::distinct(
          feature_id,
          structure_inchikey_2D,
          sample_organism_02_kingdom
        )
      ) |>
      tidytable::distinct(feature_id,
        structure_inchikey_2D,
        best_candidate = candidate_organism_02_kingdom,
        score_biological
      )

    log_debug("... phylum \n")
    step_phy <- tidytable::full_join(step_kin, sample_phylum) |>
      tidytable::distinct(
        structure_inchikey_2D,
        sample_organism_03_phylum
      )
    step_phy <- tidytable::left_join(step_phy, candidate_phylum) |>
      tidytable::filter(candidate_organism_03_phylum != "notClassified") |>
      tidytable::filter(
        stringi::stri_detect_regex(pattern = candidate_organism_03_phylum, str = sample_organism_03_phylum)
      ) |>
      tidyft::mutate(score_biological = scoreBiologicalPhylum) |>
      tidytable::left_join(
        metadata |> tidytable::distinct(
          feature_id,
          structure_inchikey_2D,
          sample_organism_03_phylum
        )
      ) |>
      tidytable::distinct(feature_id,
        structure_inchikey_2D,
        best_candidate = candidate_organism_03_phylum,
        score_biological
      )

    log_debug("... class \n")
    step_cla <- tidytable::full_join(step_phy, sample_class) |>
      tidytable::distinct(
        structure_inchikey_2D,
        sample_organism_04_class
      )
    step_cla <- tidytable::left_join(step_cla, candidate_class) |>
      tidytable::filter(candidate_organism_04_class != "notClassified") |>
      tidytable::filter(
        stringi::stri_detect_regex(pattern = candidate_organism_04_class, str = sample_organism_04_class)
      ) |>
      tidyft::mutate(score_biological = scoreBiologicalClass) |>
      tidytable::left_join(
        metadata |> tidytable::distinct(
          feature_id,
          structure_inchikey_2D,
          sample_organism_04_class
        )
      ) |>
      tidytable::distinct(feature_id,
        structure_inchikey_2D,
        best_candidate = candidate_organism_04_class,
        score_biological
      )

    log_debug("... order \n")
    step_ord <- tidytable::full_join(step_cla, sample_order) |>
      tidytable::distinct(
        structure_inchikey_2D,
        sample_organism_05_order
      )
    step_ord <- tidytable::left_join(step_ord, candidate_order) |>
      tidytable::filter(candidate_organism_05_order != "notClassified") |>
      tidytable::filter(
        stringi::stri_detect_regex(pattern = candidate_organism_05_order, str = sample_organism_05_order)
      ) |>
      tidyft::mutate(score_biological = scoreBiologicalOrder) |>
      tidytable::left_join(
        metadata |> tidytable::distinct(
          feature_id,
          structure_inchikey_2D,
          sample_organism_05_order
        )
      ) |>
      tidytable::distinct(feature_id,
        structure_inchikey_2D,
        best_candidate = candidate_organism_05_order,
        score_biological
      )

    # log_debug("... infraorder \n")
    # step_ord2 <- tidytable::full_join(step_ord, sample_infraorder) |>
    #   tidytable::distinct(structure_inchikey_2D,
    #                   sample_organism_05_1_infraorder)
    # step_ord2 <-
    #   tidytable::left_join(step_ord2, candidate_infraorder) |>
    #   tidytable::filter(candidate_organism_05_1_infraorder != "notClassified") |>
    #   tidytable::filter(
    #     stringi::stri_detect_regex(pattern = candidate_organism_05_1_infraorder, str = sample_organism_05_1_infraorder)
    #   ) |>
    #   tidyft::mutate(score_biological = scoreBiologicalInfraorder) |>
    #   tidytable::left_join(
    #     metadata |> tidytable::distinct(feature_id,
    #                                 structure_inchikey_2D,
    #                                 sample_organism_05_1_infraorder)
    #   ) |>
    #   tidytable::distinct(feature_id,
    #                   structure_inchikey_2D,
    #                   best_candidate = candidate_organism_05_1_infraorder,
    #                   score_biological)

    log_debug("... family \n")
    step_fam <- tidytable::full_join(step_ord, sample_family) |>
      tidytable::distinct(
        structure_inchikey_2D,
        sample_organism_06_family
      )
    step_fam <- tidytable::left_join(step_fam, candidate_family) |>
      tidytable::filter(candidate_organism_06_family != "notClassified") |>
      tidytable::filter(
        stringi::stri_detect_regex(pattern = candidate_organism_06_family, str = sample_organism_06_family)
      ) |>
      tidyft::mutate(score_biological = scoreBiologicalFamily) |>
      tidytable::left_join(
        metadata |> tidytable::distinct(
          feature_id,
          structure_inchikey_2D,
          sample_organism_06_family
        )
      ) |>
      tidytable::distinct(feature_id,
        structure_inchikey_2D,
        best_candidate = candidate_organism_06_family,
        score_biological
      )

    # log_debug("... subfamily \n")
    # step_fam2 <- tidytable::full_join(step_fam, sample_subfamily) |>
    #   tidytable::distinct(structure_inchikey_2D,
    #                   sample_organism_06_1_subfamily)
    # step_fam2 <- tidytable::left_join(step_fam2, candidate_subfamily) |>
    #   tidytable::filter(candidate_organism_06_1_subfamily != "notClassified") |>
    #   tidytable::filter(
    #     stringi::stri_detect_regex(pattern = candidate_organism_06_1_subfamily, str = sample_organism_06_1_subfamily)
    #   ) |>
    #   tidyft::mutate(score_biological = scoreBiologicalSubfamily) |>
    #   tidytable::left_join(
    #     metadata |> tidytable::distinct(feature_id,
    #                                 structure_inchikey_2D,
    #                                 sample_organism_06_1_subfamily)
    #   ) |>
    #   tidytable::distinct(feature_id,
    #                   structure_inchikey_2D,
    #                   best_candidate = candidate_organism_06_1_subfamily,
    #                   score_biological)

    log_debug("... tribe \n")
    step_tri <- tidytable::full_join(step_fam, sample_tribe) |>
      tidytable::distinct(
        structure_inchikey_2D,
        sample_organism_07_tribe
      )
    step_tri <- tidytable::left_join(step_tri, candidate_tribe) |>
      tidytable::filter(candidate_organism_07_tribe != "notClassified") |>
      tidytable::filter(
        stringi::stri_detect_regex(pattern = candidate_organism_07_tribe, str = sample_organism_07_tribe)
      ) |>
      tidyft::mutate(score_biological = scoreBiologicalTribe) |>
      tidytable::left_join(
        metadata |> tidytable::distinct(
          feature_id,
          structure_inchikey_2D,
          sample_organism_07_tribe
        )
      ) |>
      tidytable::distinct(feature_id,
        structure_inchikey_2D,
        best_candidate = candidate_organism_07_tribe,
        score_biological
      )

    # log_debug("... subtribe \n")
    # step_tri2 <- tidytable::full_join(step_tri, sample_subtribe) |>
    #   tidytable::distinct(
    #     structure_inchikey_2D,
    #     sample_organism_07_1_subtribe
    #   )
    # step_tri2 <- tidytable::left_join(step_tri2, candidate_subtribe) |>
    #   tidytable::filter(candidate_organism_07_1_subtribe != "notClassified") |>
    #   tidytable::filter(
    #     stringi::stri_detect_regex(pattern = candidate_organism_07_1_subtribe, str = sample_organism_07_1_subtribe)
    #   ) |>
    #   tidyft::mutate(score_biological = scoreBiologicalSubtribe) |>
    #   tidytable::left_join(
    #     metadata |> tidytable::distinct(feature_id,
    #                                 structure_inchikey_2D,
    #                                 sample_organism_07_1_subtribe)
    #   ) |>
    #   tidytable::distinct(feature_id,
    #                   structure_inchikey_2D,
    #                   best_candidate = candidate_organism_07_1_subtribe,
    #                   score_biological)

    log_debug("... genus \n")
    step_gen <- tidytable::full_join(step_tri, sample_genus) |>
      tidytable::distinct(
        structure_inchikey_2D,
        sample_organism_08_genus
      )
    step_gen <- tidytable::left_join(step_gen, candidate_genus) |>
      tidytable::filter(candidate_organism_08_genus != "notClassified") |>
      tidytable::filter(
        stringi::stri_detect_regex(pattern = candidate_organism_08_genus, str = sample_organism_08_genus)
      ) |>
      tidyft::mutate(score_biological = scoreBiologicalGenus) |>
      tidytable::left_join(
        metadata |> tidytable::distinct(
          feature_id,
          structure_inchikey_2D,
          sample_organism_08_genus
        )
      ) |>
      tidytable::distinct(feature_id,
        structure_inchikey_2D,
        best_candidate = candidate_organism_08_genus,
        score_biological
      )

    # log_debug("... subgenus \n")
    # step_gen2 <- tidytable::full_join(step_gen, sample_subgenus) |>
    #   tidytable::distinct(structure_inchikey_2D,
    #                   sample_organism_08_1_subgenus)
    # step_gen2 <-
    #   tidytable::left_join(step_gen2, candidate_subgenus) |>
    #   tidytable::filter(candidate_organism_08_1_subgenus != "notClassified") |>
    #   tidytable::filter(
    #     stringi::stri_detect_regex(pattern = candidate_organism_08_1_subgenus, str = sample_organism_08_1_subgenus)
    #   ) |>
    #   tidyft::mutate(score_biological = scoreBiologicalSubgenus) |>
    #   tidytable::left_join(
    #     metadata |> tidytable::distinct(feature_id,
    #                                 structure_inchikey_2D,
    #                                 sample_organism_08_1_subgenus)
    #   ) |>
    #   tidytable::distinct(feature_id,
    #                   structure_inchikey_2D,
    #                   best_candidate = candidate_organism_08_1_subgenus,
    #                   score_biological)

    log_debug("... species \n")
    step_spe <- tidytable::full_join(step_gen, sample_species) |>
      tidytable::distinct(
        structure_inchikey_2D,
        sample_organism_09_species
      )
    step_spe <- tidytable::left_join(step_spe, candidate_species) |>
      tidytable::filter(candidate_organism_09_species != "notClassified") |>
      tidytable::filter(
        stringi::stri_detect_regex(pattern = candidate_organism_09_species, str = sample_organism_09_species)
      ) |>
      tidyft::mutate(score_biological = scoreBiologicalSpecies) |>
      tidytable::left_join(
        metadata |> tidytable::distinct(
          feature_id,
          structure_inchikey_2D,
          sample_organism_09_species
        )
      ) |>
      tidytable::distinct(feature_id,
        structure_inchikey_2D,
        best_candidate = candidate_organism_09_species,
        score_biological
      )

    # log_debug("... subspecies \n")
    # step_spe2 <- tidytable::full_join(step_spe, sample_subspecies) |>
    #   tidytable::distinct(structure_inchikey_2D,
    #                   sample_organism_09_1_subspecies)
    # step_spe2 <-
    #   tidytable::left_join(step_spe2, candidate_subspecies) |>
    #   tidytable::filter(candidate_organism_09_1_subspecies != "notClassified") |>
    #   tidytable::filter(
    #     stringi::stri_detect_regex(pattern = candidate_organism_09_1_subspecies, str = sample_organism_09_1_subspecies)
    #   ) |>
    #   tidyft::mutate(score_biological = scoreBiologicalSubspecies) |>
    #   tidytable::left_join(
    #     metadata |> tidytable::distinct(feature_id,
    #                                 structure_inchikey_2D,
    #                                 sample_organism_09_1_subspecies)
    #   ) |>
    #   tidytable::distinct(feature_id,
    #                   structure_inchikey_2D,
    #                   best_candidate = candidate_organism_09_1_subspecies,
    #                   score_biological)

    log_debug("... varietas \n")
    step_var <- tidytable::full_join(step_spe, sample_varietas) |>
      tidytable::distinct(
        structure_inchikey_2D,
        sample_organism_10_varietas
      )
    step_var <- tidytable::left_join(step_var, candidate_varietas) |>
      tidytable::filter(candidate_organism_10_varietas != "notClassified") |>
      tidytable::filter(
        stringi::stri_detect_regex(pattern = candidate_organism_10_varietas, str = sample_organism_10_varietas)
      ) |>
      tidyft::mutate(score_biological = scoreBiologicalVariety) |>
      tidytable::left_join(
        metadata |> tidytable::distinct(
          feature_id,
          structure_inchikey_2D,
          sample_organism_10_varietas
        )
      ) |>
      tidytable::distinct(feature_id,
        structure_inchikey_2D,
        best_candidate = candidate_organism_10_varietas,
        score_biological
      )

    log_debug("keeping best biological score only \n")
    biologically_weighted <- tidytable::bind_rows(
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
      tidytable::arrange(tidytable::desc(score_biological)) |>
      tidytable::distinct(feature_id,
        structure_inchikey_2D,
        .keep_all = TRUE
      )

    log_debug("joining with initial results \n")
    biologically_weighted_full <-
      tidytable::left_join(
        annotationTable |>
          tidytable::select(-tidytable::contains("sample_")),
        biologically_weighted
      )

    biologically_weighted_full$score_biological[is.na(biologically_weighted_full$score_biological)] <-
      0

    biologically_weighted_full <- biologically_weighted_full |>
      tidyft::mutate(
        score_pondered_bio = (
          (1 / (weightBiological + weightSpectral)) *
            weightBiological *
            score_biological +
            (1 / (weightBiological + weightSpectral)) *
              weightSpectral *
              as.numeric(score_input)
          # as.numeric(score_input_normalized)
        )
      )

    biologically_weighted_full$score_pondered_bio[is.na(biologically_weighted_full$score_pondered_bio)] <-
      0

    biologically_weighted_full <- biologically_weighted_full |>
      tidytable::arrange(tidytable::desc(score_pondered_bio)) |>
      tidytable::distinct(
        feature_id,
        structure_inchikey_2D,
        structure_smiles_2D,
        structure_taxonomy_npclassifier_01pathway,
        structure_taxonomy_npclassifier_02superclass,
        structure_taxonomy_npclassifier_03class,
        .keep_all = TRUE
      ) |>
      tidyft::mutate(
        rank_initial = tidytable::dense_rank(-as.numeric(score_input)),
        rank_final = tidytable::dense_rank(-score_pondered_bio),
        .by = c(feature_id)
      ) |>
      tidytable::arrange(
        rank_final,
        score_pondered_bio
      ) |>
      tidytable::arrange(as.numeric(feature_id)) |>
      data.frame() |>
      tidytable::tidytable()

    return(biologically_weighted_full)
  }
