# require(...)

#' Title
#'
#' @param annotationTable
#' @param structureOrganismPairsTable
#'
#' @return
#' @export
#'
#' @examples
biological_weighting <-
  function(annotationTable = annotation_table_ms1_taxed,
           structureOrganismPairsTable = structure_organism_pairs_table) {
    cat("normalizing initial score \n")
    metadata <- annotationTable %>%
      select(
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
      ) %>%
      mutate(across(score_input, as.numeric)) %>%
      mutate(score_initialNormalized = score_input)
    ## better not
    # mutate(score_initialNormalized = (score_input -
    #   min(score_input)) /
    #   (max(score_input) -
    #     min(score_input)))

    sample_domain <- annotationTable %>%
      distinct(
        inchikey_2D,
        sample_organism_01_domain
      )

    sample_kingdom <- annotationTable %>%
      distinct(
        inchikey_2D,
        sample_organism_02_kingdom
      )

    sample_phylum <- annotationTable %>%
      distinct(
        inchikey_2D,
        sample_organism_03_phylum
      )

    sample_class <- annotationTable %>%
      distinct(
        inchikey_2D,
        sample_organism_04_class
      )

    sample_order <- annotationTable %>%
      distinct(
        feature_id,
        inchikey_2D,
        sample_organism_05_order
      )

    # sample_infraorder <- annotationTable %>%
    #   distinct(
    #     inchikey_2D,
    #     sample_organism_05_1_infraorder
    #   )

    sample_family <- annotationTable %>%
      distinct(
        inchikey_2D,
        sample_organism_06_family
      )

    # sample_subfamily <- annotationTable %>%
    #   distinct(
    #     inchikey_2D,
    #     sample_organism_06_1_subfamily
    #   )

    sample_tribe <- annotationTable %>%
      distinct(
        inchikey_2D,
        sample_organism_07_tribe
      )

    # sample_subtribe <- annotationTable %>%
    #   distinct(
    #     inchikey_2D,
    #     sample_organism_07_1_subtribe
    #   )

    sample_genus <- annotationTable %>%
      distinct(
        inchikey_2D,
        sample_organism_08_genus
      )

    # sample_subgenus <- annotationTable %>%
    #   distinct(
    #     inchikey_2D,
    #     sample_organism_08_1_subgenus
    #   )

    sample_species <- annotationTable %>%
      distinct(
        inchikey_2D,
        sample_organism_09_species
      )

    # sample_subspecies <- annotationTable %>%
    #   distinct(
    #     inchikey_2D,
    #     sample_organism_09_1_subspecies
    #   )

    sample_varietas <- annotationTable %>%
      distinct(
        inchikey_2D,
        sample_organism_10_varietas
      )

    rm(annotationTable)

    cat("selecting DB columns \n")
    candidates <- structureOrganismPairsTable %>%
      filter(!is.na(structure_inchikey_2D)) %>%
      select(
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
    candidate_domain <- candidates %>%
      distinct(
        inchikey_2D,
        candidate_organism_01_domain
      )

    candidate_kingdom <- candidates %>%
      distinct(
        inchikey_2D,
        candidate_organism_02_kingdom
      )

    candidate_phylum <- candidates %>%
      distinct(
        inchikey_2D,
        candidate_organism_03_phylum
      )

    candidate_class <- candidates %>%
      distinct(
        inchikey_2D,
        candidate_organism_04_class
      )

    candidate_order <- candidates %>%
      distinct(
        inchikey_2D,
        candidate_organism_05_order
      )

    # candidate_infraorder <- candidates %>%
    #   distinct(
    #     inchikey_2D,
    #     candidate_organism_05_1_infraorder
    #   )

    candidate_family <- candidates %>%
      distinct(
        inchikey_2D,
        candidate_organism_06_family
      )

    # candidate_subfamily <- candidates %>%
    #   distinct(
    #     inchikey_2D,
    #     candidate_organism_06_1_subfamily
    #   )

    candidate_tribe <- candidates %>%
      distinct(
        inchikey_2D,
        candidate_organism_07_tribe
      )

    # candidate_subtribe <- candidates %>%
    #   distinct(
    #     inchikey_2D,
    #     candidate_organism_07_1_subtribe
    #   )

    candidate_genus <- candidates %>%
      distinct(
        inchikey_2D,
        candidate_organism_08_genus
      )

    # candidate_subgenus <- candidates %>%
    #   distinct(
    #     inchikey_2D,
    #     candidate_organism_08_1_subgenus
    #   )

    candidate_species <- candidates %>%
      distinct(
        inchikey_2D,
        candidate_organism_09_species
      )

    # candidate_subspecies <- candidates %>%
    #   distinct(
    #     inchikey_2D,
    #     candidate_organism_09_1_subspecies
    #   )

    candidate_varietas <- candidates %>%
      distinct(
        inchikey_2D,
        candidate_organism_10_varietas
      )

    cat("calculating biological scores ... \n")

    cat("... domain \n")
    step_dom <- left_join(sample_domain, candidate_domain) %>%
      filter(candidate_organism_01_domain != "notClassified") %>%
      filter(
        str_detect(pattern = candidate_organism_01_domain, string = sample_organism_01_domain)
      ) %>%
      mutate(score_biological = params$score$biological$domain) %>%
      left_join(
        .,
        metadata %>% distinct(
          feature_id,
          inchikey_2D,
          sample_organism_01_domain
        )
      ) %>%
      distinct(feature_id,
        inchikey_2D,
        best_candidate = candidate_organism_01_domain,
        score_biological
      )

    cat("... kingdom \n")
    step_kin <- full_join(step_dom, sample_kingdom) %>%
      distinct(
        inchikey_2D,
        sample_organism_02_kingdom
      )
    step_kin <- left_join(step_kin, candidate_kingdom) %>%
      filter(candidate_organism_02_kingdom != "notClassified") %>%
      filter(
        str_detect(pattern = candidate_organism_02_kingdom, string = sample_organism_02_kingdom)
      ) %>%
      mutate(score_biological = params$score$biological$kingdom) %>%
      left_join(
        .,
        metadata %>% distinct(
          feature_id,
          inchikey_2D,
          sample_organism_02_kingdom
        )
      ) %>%
      distinct(feature_id,
        inchikey_2D,
        best_candidate = candidate_organism_02_kingdom,
        score_biological
      )

    cat("... phylum \n")
    step_phy <- full_join(step_kin, sample_phylum) %>%
      distinct(
        inchikey_2D,
        sample_organism_03_phylum
      )
    step_phy <- left_join(step_phy, candidate_phylum) %>%
      filter(candidate_organism_03_phylum != "notClassified") %>%
      filter(
        str_detect(pattern = candidate_organism_03_phylum, string = sample_organism_03_phylum)
      ) %>%
      mutate(score_biological = params$score$biological$phylum) %>%
      left_join(
        .,
        metadata %>% distinct(
          feature_id,
          inchikey_2D,
          sample_organism_03_phylum
        )
      ) %>%
      distinct(feature_id,
        inchikey_2D,
        best_candidate = candidate_organism_03_phylum,
        score_biological
      )

    cat("... class \n")
    step_cla <- full_join(step_phy, sample_class) %>%
      distinct(
        inchikey_2D,
        sample_organism_04_class
      )
    step_cla <- left_join(step_cla, candidate_class) %>%
      filter(candidate_organism_04_class != "notClassified") %>%
      filter(
        str_detect(pattern = candidate_organism_04_class, string = sample_organism_04_class)
      ) %>%
      mutate(score_biological = params$score$biological$class) %>%
      left_join(
        .,
        metadata %>% distinct(
          feature_id,
          inchikey_2D,
          sample_organism_04_class
        )
      ) %>%
      distinct(feature_id,
        inchikey_2D,
        best_candidate = candidate_organism_04_class,
        score_biological
      )

    cat("... order \n")
    step_ord <- full_join(step_cla, sample_order) %>%
      distinct(
        inchikey_2D,
        sample_organism_05_order
      )
    step_ord <- left_join(step_ord, candidate_order) %>%
      filter(candidate_organism_05_order != "notClassified") %>%
      filter(
        str_detect(pattern = candidate_organism_05_order, string = sample_organism_05_order)
      ) %>%
      mutate(score_biological = params$score$biological$order) %>%
      left_join(
        .,
        metadata %>% distinct(
          feature_id,
          inchikey_2D,
          sample_organism_05_order
        )
      ) %>%
      distinct(feature_id,
        inchikey_2D,
        best_candidate = candidate_organism_05_order,
        score_biological
      )

    # cat("... infraorder \n")
    # step_ord2 <- full_join(step_ord, sample_infraorder) %>%
    #   distinct(
    #     inchikey_2D,
    #     sample_organism_05_1_infraorder
    #   )
    # step_ord2 <- left_join(step_ord2, candidate_infraorder) %>%
    #   filter(candidate_organism_05_1_infraorder != "notClassified") %>%
    #   filter(
    #     str_detect(pattern = candidate_organism_05_1_infraorder, string = sample_organism_05_1_infraorder)
    #   ) %>%
    #   mutate(score_biological = params$score$biological$infraorder) %>%
    #   left_join(
    #     .,
    #     metadata %>% distinct(
    #       feature_id,
    #       inchikey_2D,
    #       sample_organism_05_1_infraorder
    #     )
    #   ) %>%
    #   distinct(
    #     feature_id,
    #     inchikey_2D,
    #     best_candidate = candidate_organism_05_1_infraorder,
    #     score_biological
    #   )

    cat("... family \n")
    step_fam <- full_join(step_ord, sample_family) %>%
      distinct(
        inchikey_2D,
        sample_organism_06_family
      )
    step_fam <- left_join(step_fam, candidate_family) %>%
      filter(candidate_organism_06_family != "notClassified") %>%
      filter(
        str_detect(pattern = candidate_organism_06_family, string = sample_organism_06_family)
      ) %>%
      mutate(score_biological = params$score$biological$family) %>%
      left_join(
        .,
        metadata %>% distinct(
          feature_id,
          inchikey_2D,
          sample_organism_06_family
        )
      ) %>%
      distinct(feature_id,
        inchikey_2D,
        best_candidate = candidate_organism_06_family,
        score_biological
      )

    # cat("... subfamily \n")
    # step_fam2 <- full_join(step_fam, sample_subfamily) %>%
    #   distinct(
    #     inchikey_2D,
    #     sample_organism_06_1_subfamily
    #   )
    # step_fam2 <- left_join(step_fam2, candidate_subfamily) %>%
    #   filter(candidate_organism_06_1_subfamily != "notClassified") %>%
    #   filter(
    #     str_detect(pattern = candidate_organism_06_1_subfamily, string = sample_organism_06_1_subfamily)
    #   ) %>%
    #   mutate(score_biological = params$score$biological$subfamily) %>%
    #   left_join(
    #     .,
    #     metadata %>% distinct(
    #       feature_id,
    #       inchikey_2D,
    #       sample_organism_06_1_subfamily
    #     )
    #   ) %>%
    #   distinct(
    #     feature_id,
    #     inchikey_2D,
    #     best_candidate = candidate_organism_06_1_subfamily,
    #     score_biological
    #   )

    cat("... tribe \n")
    step_tri <- full_join(step_fam, sample_tribe) %>%
      distinct(
        inchikey_2D,
        sample_organism_07_tribe
      )
    step_tri <- left_join(step_tri, candidate_tribe) %>%
      filter(candidate_organism_07_tribe != "notClassified") %>%
      filter(
        str_detect(pattern = candidate_organism_07_tribe, string = sample_organism_07_tribe)
      ) %>%
      mutate(score_biological = params$score$biological$tribe) %>%
      left_join(
        .,
        metadata %>% distinct(
          feature_id,
          inchikey_2D,
          sample_organism_07_tribe
        )
      ) %>%
      distinct(feature_id,
        inchikey_2D,
        best_candidate = candidate_organism_07_tribe,
        score_biological
      )

    # cat("... subtribe \n")
    # step_tri2 <- full_join(step_tri, sample_subtribe) %>%
    #   distinct(
    #     inchikey_2D,
    #     sample_organism_07_1_subtribe
    #   )
    # step_tri2 <- left_join(step_tri2, candidate_subtribe) %>%
    #   filter(candidate_organism_07_1_subtribe != "notClassified") %>%
    #   filter(
    #     str_detect(pattern = candidate_organism_07_1_subtribe, string = sample_organism_07_1_subtribe)
    #   ) %>%
    #   mutate(score_biological = params$score$biological$subtribe) %>%
    #   left_join(
    #     .,
    #     metadata %>% distinct(
    #       feature_id,
    #       inchikey_2D,
    #       sample_organism_07_1_subtribe
    #     )
    #   ) %>%
    #   distinct(
    #     feature_id,
    #     inchikey_2D,
    #     best_candidate = candidate_organism_07_1_subtribe,
    #     score_biological
    #   )

    cat("... genus \n")
    step_gen <- full_join(step_tri, sample_genus) %>%
      distinct(
        inchikey_2D,
        sample_organism_08_genus
      )
    step_gen <- left_join(step_gen, candidate_genus) %>%
      filter(candidate_organism_08_genus != "notClassified") %>%
      filter(
        str_detect(pattern = candidate_organism_08_genus, string = sample_organism_08_genus)
      ) %>%
      mutate(score_biological = params$score$biological$genus) %>%
      left_join(
        .,
        metadata %>% distinct(
          feature_id,
          inchikey_2D,
          sample_organism_08_genus
        )
      ) %>%
      distinct(feature_id,
        inchikey_2D,
        best_candidate = candidate_organism_08_genus,
        score_biological
      )

    # cat("... subgenus \n")
    # step_gen2 <- full_join(step_gen, sample_subgenus) %>%
    #   distinct(
    #     inchikey_2D,
    #     sample_organism_08_1_subgenus
    #   )
    # step_gen2 <-
    #   left_join(step_gen2, candidate_subgenus) %>%
    #   filter(candidate_organism_08_1_subgenus != "notClassified") %>%
    #   filter(
    #     str_detect(pattern = candidate_organism_08_1_subgenus, string = sample_organism_08_1_subgenus)
    #   ) %>%
    #   mutate(score_biological = params$score$biological$subgenus) %>%
    #   left_join(
    #     .,
    #     metadata %>% distinct(
    #       feature_id,
    #       inchikey_2D,
    #       sample_organism_08_1_subgenus
    #     )
    #   ) %>%
    #   distinct(
    #     feature_id,
    #     inchikey_2D,
    #     best_candidate = candidate_organism_08_1_subgenus,
    #     score_biological
    #   )

    cat("... species \n")
    step_spe <- full_join(step_gen, sample_species) %>%
      distinct(
        inchikey_2D,
        sample_organism_09_species
      )
    step_spe <- left_join(step_spe, candidate_species) %>%
      filter(candidate_organism_09_species != "notClassified") %>%
      filter(
        str_detect(pattern = candidate_organism_09_species, string = sample_organism_09_species)
      ) %>%
      mutate(score_biological = params$score$biological$species) %>%
      left_join(
        .,
        metadata %>% distinct(
          feature_id,
          inchikey_2D,
          sample_organism_09_species
        )
      ) %>%
      distinct(feature_id,
        inchikey_2D,
        best_candidate = candidate_organism_09_species,
        score_biological
      )

    # cat("... subspecies \n")
    # step_spe2 <- full_join(step_spe, sample_subspecies) %>%
    #   distinct(
    #     inchikey_2D,
    #     sample_organism_09_1_subspecies
    #   )
    # step_spe2 <-
    #   left_join(step_spe2, candidate_subspecies) %>%
    #   filter(candidate_organism_09_1_subspecies != "notClassified") %>%
    #   filter(
    #     str_detect(pattern = candidate_organism_09_1_subspecies, string = sample_organism_09_1_subspecies)
    #   ) %>%
    #   mutate(score_biological = params$score$biological$subspecies) %>%
    #   left_join(
    #     .,
    #     metadata %>% distinct(
    #       feature_id,
    #       inchikey_2D,
    #       sample_organism_09_1_subspecies
    #     )
    #   ) %>%
    #   distinct(
    #     feature_id,
    #     inchikey_2D,
    #     best_candidate = candidate_organism_09_1_subspecies,
    #     score_biological
    #   )

    cat("... varietas \n")
    step_var <- full_join(step_spe, sample_varietas) %>%
      distinct(
        inchikey_2D,
        sample_organism_10_varietas
      )
    step_var <- left_join(step_var, candidate_varietas) %>%
      filter(candidate_organism_10_varietas != "notClassified") %>%
      filter(
        str_detect(pattern = candidate_organism_10_varietas, string = sample_organism_10_varietas)
      ) %>%
      mutate(score_biological = params$score$biological$variety) %>%
      left_join(
        .,
        metadata %>% distinct(
          feature_id,
          inchikey_2D,
          sample_organism_10_varietas
        )
      ) %>%
      distinct(feature_id,
        inchikey_2D,
        best_candidate = candidate_organism_10_varietas,
        score_biological
      )

    cat("keeping best biological score only \n")
    biologically_weighted <- bind_rows(
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
    ) %>%
      group_by(feature_id) %>%
      arrange(desc(score_biological)) %>%
      distinct(feature_id,
        inchikey_2D,
        .keep_all = TRUE
      ) %>%
      ungroup()

    cat("joining with initial results \n")
    biologically_weighted_full <-
      left_join(metadata, biologically_weighted)

    biologically_weighted_full$score_biological[is.na(biologically_weighted_full$score_biological)] <-
      0

    biologically_weighted_full <- biologically_weighted_full %>%
      mutate(
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

    biologically_weighted_full <- biologically_weighted_full %>%
      group_by(feature_id) %>%
      arrange(desc(score_pondered_bio)) %>%
      distinct(feature_id,
        inchikey_2D,
        .keep_all = TRUE
      ) %>%
      mutate(
        rank_initial = (dense_rank(-score_initialNormalized)),
        rank_final = (dense_rank(-score_pondered_bio))
      ) %>%
      arrange(
        rank_final,
        desc(-score_pondered_bio)
      ) %>%
      arrange(as.numeric(feature_id)) %>%
      ungroup() %>%
      tibble()

    cat("adding \"notClassified\" \n")
    biologically_weighted_full[, sapply(biologically_weighted_full, class) == "character"][is.na(biologically_weighted_full[, sapply(biologically_weighted_full, class) == "character"])] <-
      "notClassified"

    return(biologically_weighted_full)
  }
