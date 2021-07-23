#########################   Functions - biological   ##########################

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
        !!as.name(feature_id_colname),
        !!as.name(component_id_colname),
        !!as.name(short_inchikey_colname),
        !!as.name(score_input_colname),
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
      ) %>%
      mutate(across(all_of(score_input_colname), as.numeric)) %>%
      mutate(score_initialNormalized = (!!as.name(score_input_colname) - min(!!as.name(
        score_input_colname
      ))) / (max(!!as.name(
        score_input_colname
      )) - min(!!as.name(
        score_input_colname
      ))))

    sample_domain <- annotationTable %>%
      distinct(
        !!as.name(short_inchikey_colname),
        sample_organism_01_domain
      )

    sample_kingdom <- annotationTable %>%
      distinct(
        !!as.name(short_inchikey_colname),
        sample_organism_02_kingdom
      )

    sample_phylum <- annotationTable %>%
      distinct(
        !!as.name(short_inchikey_colname),
        sample_organism_03_phylum
      )

    sample_class <- annotationTable %>%
      distinct(
        !!as.name(short_inchikey_colname),
        sample_organism_04_class
      )

    sample_order <- annotationTable %>%
      distinct(
        !!as.name(feature_id_colname),
        !!as.name(short_inchikey_colname),
        sample_organism_05_order
      )

    # sample_infraorder <- annotationTable %>%
    #   distinct(
    #     !!as.name(short_inchikey_colname),
    #     sample_organism_05_1_infraorder
    #   )

    sample_family <- annotationTable %>%
      distinct(
        !!as.name(short_inchikey_colname),
        sample_organism_06_family
      )

    # sample_subfamily <- annotationTable %>%
    #   distinct(
    #     !!as.name(short_inchikey_colname),
    #     sample_organism_06_1_subfamily
    #   )

    sample_tribe <- annotationTable %>%
      distinct(
        !!as.name(short_inchikey_colname),
        sample_organism_07_tribe
      )

    # sample_subtribe <- annotationTable %>%
    #   distinct(
    #     !!as.name(short_inchikey_colname),
    #     sample_organism_07_1_subtribe
    #   )

    sample_genus <- annotationTable %>%
      distinct(
        !!as.name(short_inchikey_colname),
        sample_organism_08_genus
      )

    # sample_subgenus <- annotationTable %>%
    #   distinct(
    #     !!as.name(short_inchikey_colname),
    #     sample_organism_08_1_subgenus
    #   )

    sample_species <- annotationTable %>%
      distinct(
        !!as.name(short_inchikey_colname),
        sample_organism_09_species
      )

    # sample_subspecies <- annotationTable %>%
    #   distinct(
    #     !!as.name(short_inchikey_colname),
    #     sample_organism_09_1_subspecies
    #   )

    sample_varietas <- annotationTable %>%
      distinct(
        !!as.name(short_inchikey_colname),
        sample_organism_10_varietas
      )

    rm(annotationTable)

    cat("selecting DB columns \n")
    candidates <- structureOrganismPairsTable %>%
      filter(!is.na(!!as.name(short_inchikey_colname_db))) %>%
      select(
        !!as.name(short_inchikey_colname) := !!as.name(short_inchikey_colname_db),
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
        !!as.name(short_inchikey_colname),
        candidate_organism_01_domain
      )

    candidate_kingdom <- candidates %>%
      distinct(
        !!as.name(short_inchikey_colname),
        candidate_organism_02_kingdom
      )

    candidate_phylum <- candidates %>%
      distinct(
        !!as.name(short_inchikey_colname),
        candidate_organism_03_phylum
      )

    candidate_class <- candidates %>%
      distinct(
        !!as.name(short_inchikey_colname),
        candidate_organism_04_class
      )

    candidate_order <- candidates %>%
      distinct(
        !!as.name(short_inchikey_colname),
        candidate_organism_05_order
      )

    # candidate_infraorder <- candidates %>%
    #   distinct(
    #     !!as.name(short_inchikey_colname),
    #     candidate_organism_05_1_infraorder
    #   )

    candidate_family <- candidates %>%
      distinct(
        !!as.name(short_inchikey_colname),
        candidate_organism_06_family
      )

    # candidate_subfamily <- candidates %>%
    #   distinct(
    #     !!as.name(short_inchikey_colname),
    #     candidate_organism_06_1_subfamily
    #   )

    candidate_tribe <- candidates %>%
      distinct(
        !!as.name(short_inchikey_colname),
        candidate_organism_07_tribe
      )

    # candidate_subtribe <- candidates %>%
    #   distinct(
    #     !!as.name(short_inchikey_colname),
    #     candidate_organism_07_1_subtribe
    #   )

    candidate_genus <- candidates %>%
      distinct(
        !!as.name(short_inchikey_colname),
        candidate_organism_08_genus
      )

    # candidate_subgenus <- candidates %>%
    #   distinct(
    #     !!as.name(short_inchikey_colname),
    #     candidate_organism_08_1_subgenus
    #   )

    candidate_species <- candidates %>%
      distinct(
        !!as.name(short_inchikey_colname),
        candidate_organism_09_species
      )

    # candidate_subspecies <- candidates %>%
    #   distinct(
    #     !!as.name(short_inchikey_colname),
    #     candidate_organism_09_1_subspecies
    #   )

    candidate_varietas <- candidates %>%
      distinct(
        !!as.name(short_inchikey_colname),
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
          !!as.name(feature_id_colname),
          !!as.name(short_inchikey_colname),
          sample_organism_01_domain
        )
      ) %>%
      distinct(
        !!as.name(feature_id_colname),
        !!as.name(short_inchikey_colname),
        best_candidate = candidate_organism_01_domain,
        score_biological
      )

    cat("... kingdom \n")
    step_kin <- full_join(step_dom, sample_kingdom) %>%
      distinct(
        !!as.name(short_inchikey_colname),
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
          !!as.name(feature_id_colname),
          !!as.name(short_inchikey_colname),
          sample_organism_02_kingdom
        )
      ) %>%
      distinct(
        !!as.name(feature_id_colname),
        !!as.name(short_inchikey_colname),
        best_candidate = candidate_organism_02_kingdom,
        score_biological
      )

    cat("... phylum \n")
    step_phy <- full_join(step_kin, sample_phylum) %>%
      distinct(
        !!as.name(short_inchikey_colname),
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
          !!as.name(feature_id_colname),
          !!as.name(short_inchikey_colname),
          sample_organism_03_phylum
        )
      ) %>%
      distinct(
        !!as.name(feature_id_colname),
        !!as.name(short_inchikey_colname),
        best_candidate = candidate_organism_03_phylum,
        score_biological
      )

    cat("... class \n")
    step_cla <- full_join(step_phy, sample_class) %>%
      distinct(
        !!as.name(short_inchikey_colname),
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
          !!as.name(feature_id_colname),
          !!as.name(short_inchikey_colname),
          sample_organism_04_class
        )
      ) %>%
      distinct(
        !!as.name(feature_id_colname),
        !!as.name(short_inchikey_colname),
        best_candidate = candidate_organism_04_class,
        score_biological
      )

    cat("... order \n")
    step_ord <- full_join(step_cla, sample_order) %>%
      distinct(
        !!as.name(short_inchikey_colname),
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
          !!as.name(feature_id_colname),
          !!as.name(short_inchikey_colname),
          sample_organism_05_order
        )
      ) %>%
      distinct(
        !!as.name(feature_id_colname),
        !!as.name(short_inchikey_colname),
        best_candidate = candidate_organism_05_order,
        score_biological
      )

    # cat("... infraorder \n")
    # step_ord2 <- full_join(step_ord, sample_infraorder) %>%
    #   distinct(
    #     !!as.name(short_inchikey_colname),
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
    #       !!as.name(feature_id_colname),
    #       !!as.name(short_inchikey_colname),
    #       sample_organism_05_1_infraorder
    #     )
    #   ) %>%
    #   distinct(
    #     !!as.name(feature_id_colname),
    #     !!as.name(short_inchikey_colname),
    #     best_candidate = candidate_organism_05_1_infraorder,
    #     score_biological
    #   )

    cat("... family \n")
    step_fam <- full_join(step_ord, sample_family) %>%
      distinct(
        !!as.name(short_inchikey_colname),
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
          !!as.name(feature_id_colname),
          !!as.name(short_inchikey_colname),
          sample_organism_06_family
        )
      ) %>%
      distinct(
        !!as.name(feature_id_colname),
        !!as.name(short_inchikey_colname),
        best_candidate = candidate_organism_06_family,
        score_biological
      )

    # cat("... subfamily \n")
    # step_fam2 <- full_join(step_fam, sample_subfamily) %>%
    #   distinct(
    #     !!as.name(short_inchikey_colname),
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
    #       !!as.name(feature_id_colname),
    #       !!as.name(short_inchikey_colname),
    #       sample_organism_06_1_subfamily
    #     )
    #   ) %>%
    #   distinct(
    #     !!as.name(feature_id_colname),
    #     !!as.name(short_inchikey_colname),
    #     best_candidate = candidate_organism_06_1_subfamily,
    #     score_biological
    #   )

    cat("... tribe \n")
    step_tri <- full_join(step_fam, sample_tribe) %>%
      distinct(
        !!as.name(short_inchikey_colname),
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
          !!as.name(feature_id_colname),
          !!as.name(short_inchikey_colname),
          sample_organism_07_tribe
        )
      ) %>%
      distinct(
        !!as.name(feature_id_colname),
        !!as.name(short_inchikey_colname),
        best_candidate = candidate_organism_07_tribe,
        score_biological
      )

    # cat("... subtribe \n")
    # step_tri2 <- full_join(step_tri, sample_subtribe) %>%
    #   distinct(
    #     !!as.name(short_inchikey_colname),
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
    #       !!as.name(feature_id_colname),
    #       !!as.name(short_inchikey_colname),
    #       sample_organism_07_1_subtribe
    #     )
    #   ) %>%
    #   distinct(
    #     !!as.name(feature_id_colname),
    #     !!as.name(short_inchikey_colname),
    #     best_candidate = candidate_organism_07_1_subtribe,
    #     score_biological
    #   )

    cat("... genus \n")
    step_gen <- full_join(step_tri, sample_genus) %>%
      distinct(
        !!as.name(short_inchikey_colname),
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
          !!as.name(feature_id_colname),
          !!as.name(short_inchikey_colname),
          sample_organism_08_genus
        )
      ) %>%
      distinct(
        !!as.name(feature_id_colname),
        !!as.name(short_inchikey_colname),
        best_candidate = candidate_organism_08_genus,
        score_biological
      )

    # cat("... subgenus \n")
    # step_gen2 <- full_join(step_gen, sample_subgenus) %>%
    #   distinct(
    #     !!as.name(short_inchikey_colname),
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
    #       !!as.name(feature_id_colname),
    #       !!as.name(short_inchikey_colname),
    #       sample_organism_08_1_subgenus
    #     )
    #   ) %>%
    #   distinct(
    #     !!as.name(feature_id_colname),
    #     !!as.name(short_inchikey_colname),
    #     best_candidate = candidate_organism_08_1_subgenus,
    #     score_biological
    #   )

    cat("... species \n")
    step_spe <- full_join(step_gen, sample_species) %>%
      distinct(
        !!as.name(short_inchikey_colname),
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
          !!as.name(feature_id_colname),
          !!as.name(short_inchikey_colname),
          sample_organism_09_species
        )
      ) %>%
      distinct(
        !!as.name(feature_id_colname),
        !!as.name(short_inchikey_colname),
        best_candidate = candidate_organism_09_species,
        score_biological
      )

    # cat("... subspecies \n")
    # step_spe2 <- full_join(step_spe, sample_subspecies) %>%
    #   distinct(
    #     !!as.name(short_inchikey_colname),
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
    #       !!as.name(feature_id_colname),
    #       !!as.name(short_inchikey_colname),
    #       sample_organism_09_1_subspecies
    #     )
    #   ) %>%
    #   distinct(
    #     !!as.name(feature_id_colname),
    #     !!as.name(short_inchikey_colname),
    #     best_candidate = candidate_organism_09_1_subspecies,
    #     score_biological
    #   )

    cat("... varietas \n")
    step_var <- full_join(step_spe, sample_varietas) %>%
      distinct(
        !!as.name(short_inchikey_colname),
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
          !!as.name(feature_id_colname),
          !!as.name(short_inchikey_colname),
          sample_organism_10_varietas
        )
      ) %>%
      distinct(
        !!as.name(feature_id_colname),
        !!as.name(short_inchikey_colname),
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
      group_by(!!as.name(feature_id_colname)) %>%
      arrange(desc(score_biological)) %>%
      distinct(!!as.name(feature_id_colname),
        !!as.name(short_inchikey_colname),
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
      group_by(!!as.name(feature_id_colname)) %>%
      arrange(desc(score_pondered_bio)) %>%
      distinct(!!as.name(feature_id_colname),
        !!as.name(short_inchikey_colname),
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
      arrange(as.numeric(!!as.name(feature_id_colname))) %>%
      ungroup() %>%
      tibble()

    cat("adding \"notClassified\" \n")
    biologically_weighted_full[, sapply(biologically_weighted_full, class) == "character"][is.na(biologically_weighted_full[, sapply(biologically_weighted_full, class) == "character"])] <-
      "notClassified"

    return(biologically_weighted_full)
  }

###############################################################################

#' Title
#'
#' @param gnfound
#' @param names
#'
#' @return
#' @export
#'
#' @examples
biocleaning <- function(gnfound, names) {
  cat("extracting GNFinder preferred results as a list of dataframes \n")
  df2 <- gnfound$names.verification$preferredResults

  cat("outputting row numbers \n")
  rows <- df2 %>%
    data.table() %>%
    mutate(nrow = row_number()) %>%
    filter(. != "NULL") %>%
    select(nrow)

  cat("adding row numbers \n")
  df3 <- bind_rows(df2,
    .id = "id"
  )

  cat("harmonizing rank names between taxonomic DB \n")
  df4 <- df3 %>%
    rowwise() %>%
    mutate(
      kingdom = sum(as.numeric(
        stri_detect(
          str = classificationRank,
          fixed = c(
            "kingdom",
            "Kingdom",
            "regn."
          )
        )
      )),
      phylum = sum(as.numeric(
        stri_detect(
          str = classificationRank,
          fixed = c(
            "phylum",
            "Phylum",
            "phyl."
          )
        )
      )),
      class = sum(as.numeric(
        stri_detect(
          str = classificationRank,
          fixed = c(
            "class",
            "Class",
            "cl."
          )
        )
      )),
      order = sum(as.numeric(
        stri_detect(
          str = classificationRank,
          fixed = c(
            "order",
            "Order",
            "ord."
          )
        )
      )),
      family = sum(as.numeric(
        stri_detect(
          str = classificationRank,
          fixed = c(
            "family",
            "Family",
            "fam."
          )
        )
      )),
      genus = sum(as.numeric(
        stri_detect(
          str = classificationRank,
          fixed = c(
            "genus",
            "Genus"
          )
        )
      )),
      species = sum(as.numeric(
        stri_detect(
          str = classificationRank,
          fixed = c(
            "species",
            "Species",
            "spec.",
            "sp."
          )
        )
      )),
      variety = sum(as.numeric(
        stri_detect(
          str = classificationRank,
          fixed = c(
            "variety",
            "varietas",
            "var"
          )
        )
      ))
    ) %>%
    ungroup()

  cat("counting \n")
  df4$kingdom[df4$kingdom >= 1] <- 1
  df4$phylum[df4$phylum >= 1] <- 1
  df4$class[df4$class >= 1] <- 1
  df4$order[df4$order >= 1] <- 1
  df4$family[df4$family >= 1] <- 1
  df4$genus[df4$genus >= 1] <- 1
  df4$species[df4$species >= 1] <- 1
  df4$variety[df4$variety >= 1] <- 1

  df4[setdiff(
    x = "isSynonym",
    y = names(df4)
  )] <- NA

  # the synonym part is there to avoid the (actually)
  ## non-optimal output from Catalogue of Life in GNFinder
  ### (explained in https://github.com/gnames/gnfinder/issues/48)
  cat("sorting best filled taxonomies \n")
  df5a <- df4 %>%
    mutate(
      n = rowSums(.[c(
        "kingdom",
        "phylum",
        "class",
        "order",
        "family",
        "genus",
        "species",
        "variety"
      )]),
      id = as.integer(id)
    ) %>%
    group_by(id) %>%
    arrange(desc(n), !is.na(isSynonym)) %>%
    ungroup() %>%
    distinct(id,
      .keep_all = TRUE
    ) %>%
    arrange(id) %>%
    select(id)

  df5b <- df4 %>%
    mutate(
      n = rowSums(.[c(
        "kingdom",
        "phylum",
        "class",
        "order",
        "family",
        "genus",
        "species",
        "variety"
      )]),
      id = as.integer(id)
    ) %>%
    group_by(id) %>%
    arrange(desc(n), !is.na(isSynonym)) %>%
    ungroup() %>%
    arrange(id)

  df5b[setdiff(
    x = "classificationIds",
    y = names(df5b)
  )] <- NA

  cat("joining with initial dataframe \n")
  df6a <- bind_cols(df5a, rows)
  df6b <- left_join(df6a, df5b) %>%
    filter(!is.na(classificationIds))

  if (nrow(df6b) == 0) {
    df6b[1, colnames(df6b)] <- NA
  }

  # adding row number
  df7 <- gnfound$names.start %>%
    data.table() %>%
    mutate(nrow = row_number())

  colnames(df7)[1] <- "sum"

  # joining
  taxo <- right_join(df6b, df7)

  taxo[setdiff(
    x = c(
      "matchedCanonicalFull",
      "currentCanonicalFull",
      "taxonId",
      "dataSourceTitle",
      "classificationPath",
      "classificationRank",
      "classificationIds"
    ),
    y = names(taxo)
  )] <- NA

  taxo <- taxo %>%
    select(
      canonicalname = matchedCanonicalFull,
      canonicalnameCurrent = currentCanonicalFull,
      taxonId,
      dbTaxo = dataSourceTitle,
      taxonomy = classificationPath,
      rank = classificationRank,
      ids = classificationIds,
      sum
    )

  dbQuality <- gnfound$names.verification$dataSourceQuality
  dbTaxo <- gnfound$
    names.verification$
    bestResult$
    dataSourceTitle

  dfQuality <- data.frame(dbTaxo, dbQuality) %>%
    distinct(dbTaxo, .keep_all = TRUE)

  taxoEnhanced <- left_join(taxo, dfQuality)

  # computing sum of characters to match with GNFinder results
  names$nchar <- nchar(x = names$organism)

  names[1, "sum"] <- nchar(colnames(names)[1]) + 1
  for (i in 2:nrow(names)) {
    names[i, "sum"] <- names[i - 1, "nchar"] + 1 + names[i - 1, "sum"]
  }

  # adding min and max to merge
  taxoEnhanced <- taxoEnhanced %>%
    mutate(
      value_min = sum,
      value_max = sum
    ) %>%
    data.table()

  # filtering non-empty values
  y_2 <- names %>%
    mutate(value_min = sum)

  # filling sum values
  y_2$value_min <- as.numeric(y_2$value_min)
  y_2$value_max <- homemadeShift(y_2$sum, 1) - 1
  y_2[nrow(y_2), 5] <- y_2[nrow(y_2), 4] + 10000

  # transforming as data table (needed for next function)
  y_2 <- y_2 %>%
    data.table()

  # setting joining keys
  setkey(taxoEnhanced, value_min, value_max)
  setkey(y_2, value_min, value_max)
  # joining
  pre_final_db <- foverlaps(
    taxoEnhanced,
    na.omit(y_2)
  )

  # selecting
  final_db <- left_join(
    names,
    pre_final_db
  ) %>%
    select(
      -i.sum,
      -i.value_max,
      -i.value_min
    )

  return(final_db)
}

###############################################################################

#' Title
#'
#' @return
#' @export
#'
#' @examples
gnfinder_cleaning <- function() {
  gnfound <- data.frame(fromJSON(
    txt = data_interim_organism_cleaned,
    simplifyDataFrame = TRUE
  ))

  data_bio <- read_delim(
    file = data_interim_organism_original,
    delim = "\t",
    escape_double = FALSE,
    trim_ws = FALSE
  ) %>%
    mutate_all(as.character)

  data_bio <- data_bio[!is.na(data_bio[, "organism"]), ]

  data_bio_clean <- biocleaning(
    gnfound = gnfound,
    names = data_bio
  ) %>%
    select(
      -nchar,
      -sum,
      -value_min,
      -value_max
    )

  return(data_bio_clean)
}

###############################################################################

#' Title
#'
#' @param dfsel
#' @param dic
#'
#' @return
#' @export
#'
#' @examples
manipulating_taxo <- function(dfsel, dic) {
  cat("taxonomical manipulation step \n")
  cat("creating variable for dictionary replacement \n")
  a <- paste0("\\b", dic$taxaRank, "\\b")
  b <- dic$taxaRankStandard

  dfsel <- dfsel %>%
    select(
      organismOriginal = organism,
      organismCleaned = canonicalname,
      organismDbTaxo = dbTaxo,
      dbQuality,
      taxonomy,
      rank,
      ids
    )

  dfsel$rank <- gsub(
    pattern = "[.]",
    replacement = "",
    x = dfsel$rank
  )

  dfsel$rank <- stri_replace_all_regex(
    str = dfsel$rank,
    pattern = a,
    replacement = b,
    case_insensitive = FALSE,
    vectorize_all = FALSE
  )

  cat("removing empty cells \n")
  dfsel$rank <- y_as_na(
    x = dfsel$rank,
    y = ""
  )

  dfsel$taxonomy <- y_as_na(
    x = dfsel$taxonomy,
    y = ""
  )

  dfsel$rank <- y_as_na(
    x = dfsel$rank,
    y = ""
  )

  cat("splitting taxonomies and ranks \n")
  df1 <- dfsel %>%
    select(
      organismOriginal,
      organismCleaned,
      organismDbTaxo,
      dbQuality,
      taxonomy,
      rank,
      ids
    ) %>%
    distinct(organismCleaned,
      organismDbTaxo,
      .keep_all = TRUE
    ) %>%
    cSplit(
      splitCols = "taxonomy",
      sep = "|"
    ) %>%
    cSplit(
      splitCols = "rank",
      sep = "|"
    ) %>%
    mutate_all(as.character) %>%
    tibble()

  cat("manipulating taxa \n")
  df2 <- df1 %>%
    pivot_longer(
      cols = 6:ncol(.),
      names_to = c(".value", "level"),
      names_sep = "_",
      values_to = "taxonomy",
      values_drop_na = TRUE
    ) %>%
    distinct(organismOriginal,
      organismCleaned,
      organismDbTaxo,
      level,
      .keep_all = TRUE
    )

  df2$rank <- ifelse(test = is.na(df2$rank),
    yes = "NA",
    no = df2$rank
  )

  cat("filtering desired levels \n")
  df3 <- df2 %>%
    filter(
      rank == "kingdom" |
        rank == "phylum" |
        rank == "class" |
        rank == "order" |
        rank == "family" |
        rank == "genus" |
        rank == "species" |
        rank == "variety"
    ) %>%
    pivot_wider(
      names_from = rank,
      values_from = taxonomy
    ) %>%
    select_if(
      names(.) %in%
        c(
          "organismOriginal",
          "organismCleaned",
          "organismDbTaxo",
          "ids",
          "dbQuality",
          "kingdom",
          "phylum",
          "class",
          "order",
          "family",
          "genus",
          "species",
          "variety"
        )
    )

  cat("preparing for pivoting ... \n")
  colnames(df3)[6:ncol(df3)] <-
    paste0("bio_", colnames(df3)[6:ncol(df3)])

  cat("... pivoting (long) \n")
  if (nrow(df3) != 0) {
    df4 <- df3 %>%
      pivot_longer(
        cols = 6:ncol(.),
        names_to = c(".value", "level"),
        names_sep = "_",
        values_to = "taxonomy",
        values_drop_na = TRUE
      )
  }

  cat("... pivoting (wide) \n")
  if (nrow(df3) != 0) {
    df5 <- df4 %>%
      group_by(organismCleaned, organismDbTaxo) %>%
      distinct(ids,
        level,
        .keep_all = TRUE
      ) %>%
      pivot_wider(
        names_from = level,
        values_from = bio
      ) %>%
      select_if(
        names(.) %in%
          c(
            "organismCleaned",
            "organismDbTaxo",
            "ids",
            "dbQuality",
            "kingdom",
            "phylum",
            "class",
            "order",
            "family",
            "genus",
            "species",
            "variety"
          )
      )
  }

  if (nrow(df3) != 0) {
    df5[setdiff(
      x = c(
        "organismCleaned",
        "organismDbTaxo",
        "ids",
        "dbQuality",
        "kingdom",
        "phylum",
        "class",
        "order",
        "family",
        "genus",
        "species",
        "variety"
      ),
      y = names(df5)
    )] <- NA
  }

  if (nrow(df3) != 0) {
    df5 <- df5 %>%
      select(
        organismCleaned,
        organismDbTaxo,
        organismDbTaxoQuality = dbQuality,
        organismTaxonId = ids,
        organism_1_kingdom = kingdom,
        organism_2_phylum = phylum,
        organism_3_class = class,
        organism_4_order = order,
        organism_5_family = family,
        organism_6_genus = genus,
        organism_7_species = species,
        organism_8_variety = variety
      )
  }

  cat("adding taxa to initial dataframe \n")
  if (nrow(df3) != 0) {
    df6 <-
      left_join(dfsel, df5) %>%
      select(
        organismOriginal,
        organismCleaned,
        organismDbTaxo,
        organismDbTaxoQuality = dbQuality,
        organismTaxonIds = ids,
        organismTaxonRanks = rank,
        organismTaxonomy = taxonomy,
        organism_1_kingdom,
        organism_2_phylum,
        organism_3_class,
        organism_4_order,
        organism_5_family,
        organism_6_genus,
        organism_7_species,
        organism_8_variety
      )
  }

  if (nrow(df3) == 0) {
    df6 <- data.frame() %>%
      mutate(
        organismOriginal = NA,
        organismCleaned = NA,
        organismDbTaxo = NA,
        organismDbTaxoQuality = NA,
        organismTaxonIds = NA,
        organismTaxonRanks = NA,
        organismTaxonomy = NA,
        organism_1_kingdom = NA,
        organism_2_phylum = NA,
        organism_3_class = NA,
        organism_4_order = NA,
        organism_5_family = NA,
        organism_6_genus = NA,
        organism_7_species = NA,
        organism_8_variety = NA
      )
  }

  return(df6)
}

##############################################################################
