utils::globalVariables(
  c(
    "score_input"
  )
)

#' @title Weight annotations
#'
#' @description This function weights and eventually complements initial annotations.
#'
#' @param library Library containing the keys
#' @param org_tax_ott File containing organisms taxonomy (OTT)
#' @param str_2D_3D File containing 2D and 3D structures
#' @param annotations Prepared annotations file
#' @param components Prepared components file
#' @param edges Prepared edges file
#' @param features Prepared features file
#' @param taxa Prepared taxed features file
#' @param output Output file
#' @param candidates_initial Number of initial candidates to keep
#' @param candidates_final Number of final candidates to keep
#' @param weight_spectral Weight for the spectral score
#' @param weight_chemical Weight for the biological score
#' @param weight_biological Weight for the chemical consistency score
#' @param score_biological_domain Score for a `domain` match (should be lower than `kingdom`)
#' @param score_biological_kingdom Score for a `kingdom` match (should be lower than `phylum`)
#' @param score_biological_phylum Score for a `phylum` match (should be lower than `class`)
#' @param score_biological_class Score for a `class` match (should be lower than `order`)
#' @param score_biological_order Score for a `order` match (should be lower than `infraorder`)
#' @param score_biological_infraorder Score for a `infraorder` match (should be lower than `order`)
#' @param score_biological_family Score for a `family` match (should be lower than `subfamily`)
#' @param score_biological_subfamily Score for a `subfamily` match (should be lower than `family`)
#' @param score_biological_tribe Score for a `tribe` match (should be lower than `subtribe`)
#' @param score_biological_subtribe Score for a `subtribe` match (should be lower than `genus`)
#' @param score_biological_genus Score for a `genus` match (should be lower than `subgenus`)
#' @param score_biological_subgenus Score for a `subgenus` match (should be lower than `species`)
#' @param score_biological_species Score for a `species` match (should be lower than `subspecies`)
#' @param score_biological_subspecies Score for a `subspecies` match (should be lower than `variety`)
#' @param score_biological_variety Score for a `variety` match (should be the highest)
#' @param score_chemical_cla_kingdom Score for a `Classyfire kingdom` match (should be lower than ` Classyfire superclass`)
#' @param score_chemical_cla_superclass Score for a `Classyfire superclass` match (should be lower than `Classyfire class`)
#' @param score_chemical_cla_class Score for a `Classyfire class` match (should be lower than `Classyfire parent`)
#' @param score_chemical_cla_parent Score for a `Classyfire parent` match (should be the highest)
#' @param score_chemical_npc_pathway Score for a `NPC pathway` match (should be lower than ` NPC superclass`)
#' @param score_chemical_npc_superclass Score for a `NPC superclass` match (should be lower than `NPC class`)
#' @param score_chemical_npc_class Score for a `NPC class` match (should be the highest)
#' @param force Force parameters. Use it at your own risk
#' @param minimal_ms1_bio Minimal biological score to keep MS1 based annotation
#' @param minimal_ms1_chemo Minimal chemical score to keep MS1 based annotation
#' @param ms1_only Boolean. Keep only MS1 annotations
#' @param summarise Boolean. Summarize results (1 row per feature)
#' @param pattern Pattern to identify your job. STRING
#' @param parameters Params
#'
#' @return NULL
#'
#' @export
#'
#' @seealso annotate_masses weight_bio weight_chemo
#'
#' @examples NULL
weight_annotations <-
  function(library = paths$data$interim$libraries$sop$merged$keys,
           org_tax_ott = paths$data$interim$libraries$sop$merged$organisms$taxonomies$ott,
           str_2D_3D = paths$data$interim$libraries$sop$merged$structures$dd_ddd,
           annotations = params$files$annotations$prepared,
           components = params$files$networks$spectral$components$prepared,
           edges = params$files$networks$spectral$edges$prepared,
           features = params$files$features$prepared,
           taxa = params$files$taxa$prepared,
           output = params$files$annotations$processed,
           candidates_initial = params$annotations$candidates$initial,
           candidates_final = params$annotations$candidates$final,
           weight_spectral = params$weights$global$spectral,
           weight_chemical = params$weights$global$chemical,
           weight_biological = params$weights$global$biological,
           score_biological_domain = params$weights$biological$domain,
           score_biological_kingdom = params$weights$biological$kingdom,
           score_biological_phylum = params$weights$biological$phylum,
           score_biological_class = params$weights$biological$class,
           score_biological_order = params$weights$biological$order,
           score_biological_infraorder = params$weights$biological$infraorder,
           score_biological_family = params$weights$biological$family,
           score_biological_subfamily = params$weights$biological$subfamily,
           score_biological_tribe = params$weights$biological$tribe,
           score_biological_subtribe = params$weights$biological$subtribe,
           score_biological_genus = params$weights$biological$genus,
           score_biological_subgenus = params$weights$biological$subgenus,
           score_biological_species = params$weights$biological$species,
           score_biological_subspecies = params$weights$biological$subspecies,
           score_biological_variety = params$weights$biological$variety,
           score_chemical_cla_kingdom = params$weights$chemical$cla$kingdom,
           score_chemical_cla_superclass = params$weights$chemical$cla$superclass,
           score_chemical_cla_class = params$weights$chemical$cla$class,
           score_chemical_cla_parent = params$weights$chemical$cla$parent,
           score_chemical_npc_pathway = params$weights$chemical$npc$pathway,
           score_chemical_npc_superclass = params$weights$chemical$npc$superclass,
           score_chemical_npc_class = params$weights$chemical$npc$class,
           minimal_ms1_bio = params$annotations$ms1$thresholds$biological,
           minimal_ms1_chemo = params$annotations$ms1$thresholds$chemical,
           # TODO ADD CONDITION,
           ms1_only = params$annotations$ms1only,
           summarise = params$options$summarise,
           pattern = params$files$pattern,
           force = params$options$force,
           parameters = params) {
    stopifnot(
      "Annotations file(s) do(es) not exist" =
        rep(TRUE, length(annotations)) ==
          lapply(X = annotations, file.exists)
    )
    stopifnot("Your library file does not exist." = file.exists(library))
    stopifnot("Your components file does not exist." = file.exists(components))
    stopifnot("Your edges file does not exist." = file.exists(edges))
    stopifnot("Your features file does not exist." = file.exists(features))
    stopifnot("Your taxa file does not exist." = file.exists(taxa))

    paths <<- parse_yaml_paths()
    params <<- parameters

    log_debug(x = "... files ...")
    log_debug(x = "... features")
    features_table <- tidytable::fread(
      file = features
    )
    log_debug(x = "... components")
    components_table <- tidytable::fread(
      file = components
    )

    log_debug(x = "... annotations")
    annotation_table <- lapply(
      X = annotations,
      FUN = tidytable::fread
    ) |>
      dplyr::bind_rows()

    log_debug(x = "... metadata_table_biological_annotation")
    taxed_features_table <- tidytable::fread(
      file = taxa
    )

    log_debug(x = "... edges table")
    edges_table <- tidytable::fread(
      file = edges
    )

    log_debug(x = "... structure-organism pairs table")
    structure_organism_pairs_table <-
      tidytable::fread(
        file = library
      ) |>
      dplyr::left_join(tidytable::fread(
        file = str_2D_3D
      )) |>
      dplyr::left_join(tidytable::fread(
        file = org_tax_ott
      ))

    if (ms1_only == TRUE) {
      annotation_table <- annotation_table |>
        dplyr::filter(score_input == 0)
    }

    log_debug(x = "adding biological organism metadata")
    annotation_table_taxed <- annotation_table |>
      dplyr::left_join(taxed_features_table)

    log_debug(x = "performing taxonomically informed scoring")
    annotation_table_weighted_bio <-
      weight_bio(
        annotationTable = annotation_table_taxed,
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
        scoreBiologicalVariety = score_biological_variety
      )

    annotation_table_weighted_bio |>
      decorate_bio(
        sc_kin = score_biological_kingdom,
        sc_phy = score_biological_phylum,
        sc_cla = score_biological_class,
        sc_ord = score_biological_order,
        sc_fam = score_biological_family,
        sc_gen = score_biological_genus,
        sc_spe = score_biological_species,
        sc_var = score_biological_variety
      )

    log_debug(x = "cleaning taxonomically informed results and preparing for chemically informed scoring")
    annotation_table_weighted_bio_cleaned <- clean_bio(
      annotationTableWeightedBio = annotation_table_weighted_bio,
      edgesTable = edges_table,
      candidatesInitial = candidates_initial,
      minimalMs1Bio = minimal_ms1_bio
    )

    log_debug(x = "performing chemically informed scoring")
    annotation_table_weighted_chemo <-
      weight_chemo(
        annotationTableWeightedBioCleaned = annotation_table_weighted_bio_cleaned,
        weightSpectral = weight_spectral,
        weightBiological = weight_biological,
        weightChemical = weight_chemical,
        scoreChemicalClaKingdom = score_chemical_cla_kingdom,
        scoreChemicalClaSuperclass = score_chemical_cla_superclass,
        scoreChemicalClaClass = score_chemical_cla_class,
        scoreChemicalClaParent = score_chemical_cla_parent,
        scoreChemicalNpcPathway = score_chemical_npc_pathway,
        scoreChemicalNpcSuperclass = score_chemical_npc_superclass,
        scoreChemicalNpcClass = score_chemical_npc_class
      )

    annotation_table_weighted_chemo |>
      decorate_chemo(
        sc_cla_kin = score_chemical_cla_kingdom,
        sc_cla_sup = score_chemical_cla_superclass,
        sc_cla_cla = score_chemical_cla_class,
        sc_cla_par = score_chemical_cla_parent,
        sc_npc_pat = score_chemical_npc_pathway,
        sc_npc_sup = score_chemical_npc_superclass,
        sc_npc_cla = score_chemical_npc_class
      )

    log_debug(x = "cleaning for export")
    results <- clean_chemo(
      annotationTableWeightedChemo = annotation_table_weighted_chemo,
      componentsTable = components_table,
      featuresTable = features_table,
      structureOrganismPairsTable = structure_organism_pairs_table,
      candidatesFinal = candidates_final,
      minimalMs1Bio = minimal_ms1_bio,
      minimalMs1Chemo = minimal_ms1_chemo,
      summarize = summarise
    )

    log_debug(x = "Exporting ...")
    time <- format(Sys.time(), "%y%m%d_%H%M%OS")
    dir_time <- file.path(paths$data$processed$path, paste0(time, "_", pattern))
    final_output <- file.path(
      dir_time,
      output
    )
    export_params(
      directory = dir_time,
      step = "weight_annotations"
    )
    export_output(
      x = results,
      file = final_output
    )

    return(final_output)
  }
