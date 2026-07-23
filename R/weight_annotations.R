#' @title Weight annotations
#'
#' @description This function weights annotations.
#'
#' @include clean_bio.R
#' @include clean_chemo.R
#' @include columns_utils.R
#' @include decorate_bio.R
#' @include decorate_chemo.R
#' @include get_default_paths.R
#' @include get_params.R
#' @include weight_bio.R
#' @include weight_chemo.R
#'
#' @param library Library containing the keys
#' @param org_tax_ott File containing organisms taxonomy (OTT)
#' @param str_stereo File containing structures stereo
#' @param annotations Prepared annotations file
#' @param canopus Prepared canopus file
#' @param formula Prepared formula file
#' @param components Prepared components file
#' @param edges Prepared edges file
#' @param taxa Prepared taxed features file
#' @param output Output file
#' @param candidates_neighbors Number of neighbors candidates to keep
#' @param candidates_final Number of final candidates to keep
#' @param best_percentile Numeric percentile threshold (0-1) for selecting top
#'     candidates within each feature (default: 0.9). Used for consistent
#'     filtering between mini and filtered outputs.
#' @param weight_spectral Weight for the spectral score
#' @param weight_chemical Weight for the biological score
#' @param weight_biological Weight for the chemical consistency score
#' @param score_biological_domain Score for a `domain` match
#' (should be lower than `kingdom`)
#' @param score_biological_kingdom Score for a `kingdom` match
#' (should be lower than `phylum`)
#' @param score_biological_phylum Score for a `phylum` match
#' (should be lower than `class`)
#' @param score_biological_class Score for a `class` match
#' (should be lower than `order`)
#' @param score_biological_order Score for a `order` match
#' (should be lower than `infraorder`)
#' @param score_biological_infraorder Score for a `infraorder` match
#' (should be lower than `order`)
#' @param score_biological_family Score for a `family` match
#' (should be lower than `subfamily`)
#' @param score_biological_subfamily Score for a `subfamily` match
#' (should be lower than `family`)
#' @param score_biological_tribe Score for a `tribe` match
#' (should be lower than `subtribe`)
#' @param score_biological_subtribe Score for a `subtribe` match
#' (should be lower than `genus`)
#' @param score_biological_genus Score for a `genus` match
#' (should be lower than `subgenus`)
#' @param score_biological_subgenus Score for a `subgenus` match
#' (should be lower than `species`)
#' @param score_biological_species Score for a `species` match
#' (should be lower than `subspecies`)
#' @param score_biological_subspecies Score for a `subspecies` match
#' (should be lower than `variety`)
#' @param score_biological_variety Score for a `variety` match
#' (should be the highest)
#' @param score_biological_biota Score for a `Biota` match
#' (should be the highest, special)
#' @param score_chemical_cla_kingdom Score for a `Classyfire kingdom` match
#' (should be lower than ` Classyfire superclass`)
#' @param score_chemical_cla_superclass
#' Score for a `Classyfire superclass` match
#' (should be lower than `Classyfire class`)
#' @param score_chemical_cla_class Score for a `Classyfire class` match
#' (should be lower than `Classyfire parent`)
#' @param score_chemical_cla_parent Score for a `Classyfire parent` match
#' (should be the highest)
#' @param score_chemical_npc_pathway Score for a `NPC pathway` match
#' (should be lower than ` NPC superclass`)
#' @param score_chemical_npc_superclass Score for a `NPC superclass`
#' match (should be lower than `NPC class`)
#' @param score_chemical_npc_class Score for a `NPC class` match
#' (should be the highest)
#' @param force Force parameters. Use it at your own risk
#' @param minimal_consistency Minimal consistency score for a class. FLOAT
#' @param minimal_ms1_bio Minimal biological score to keep MS1 based annotation
#' @param minimal_ms1_chemo Minimal chemical score to keep MS1 based annotation
#' @param minimal_ms1_condition Condition to be used. Must be "OR" or "AND".
#' @param ms1_only Keep only MS1 annotations. BOOLEAN
#' @param compounds_names Report compounds names. Can be very large. BOOLEAN
#' @param high_evidence Report high evidence candidates only. BOOLEAN
#' @param remove_ties Remove ties. BOOLEAN
#' @param summarize Summarize results (1 row per feature). BOOLEAN
#' @param pattern Pattern to identify your job. STRING
#' @param xrefs_file Optional character path to xrefs file from
#'     [get_compounds_xrefs()].
#'     If provided, external database identifiers will be added to results.
#'
#' @return The path to the weighted annotations
#'
#' @family annotation
#'
#' @export
#'
#' @seealso annotate_masses weight_bio weight_chemo
#'
#' @examples
#' \dontrun{
#' copy_backbone()
#' go_to_cache()
#' github <- "https://raw.githubusercontent.com/"
#' repo <- "taxonomicallyinformedannotation/tima-example-files/main/"
#' dir <- paste0(github, repo)
#' library <- get_params(
#'   step =
#'     "weight_annotations"
#' )$files$libraries$sop$merged$keys |>
#'   gsub(
#'     pattern = ".gz",
#'     replacement = "",
#'     fixed = TRUE
#'   )
#' org_tax_ott <- paste0(
#'   "data/interim/libraries/",
#'   "sop/merged/organisms/taxonomies/ott.tsv"
#' )
#' str_stereo <- paste0(
#'   "data/interim/libraries/",
#'   "sop/merged/structures/stereo.tsv"
#' )
#' annotations <- paste0(
#'   "data/interim/annotations/",
#'   "example_annotationsFiltered.tsv"
#' )
#' canopus <- paste0(
#'   "data/interim/annotations/",
#'   "example_canopusPrepared.tsv"
#' )
#' formula <- paste0(
#'   "data/interim/annotations/",
#'   "example_formulaPrepared.tsv"
#' )
#' components <- paste0(
#'   "data/interim/features/",
#'   "example_componentsPrepared.tsv"
#' )
#' edges <- paste0(
#'   "data/interim/features/",
#'   "example_edges.tsv"
#' )
#' taxa <- paste0(
#'   "data/interim/taxa/",
#'   "example_taxed.tsv"
#' )
#' get_file(url = paste0(dir, library), export = library)
#' get_file(url = paste0(dir, org_tax_ott), export = org_tax_ott)
#' get_file(url = paste0(dir, str_stereo), export = str_stereo)
#' get_file(url = paste0(dir, annotations), export = annotations)
#' get_file(url = paste0(dir, canopus), export = canopus)
#' get_file(url = paste0(dir, formula), export = formula)
#' get_file(url = paste0(dir, components), export = components)
#' get_file(url = paste0(dir, edges), export = edges)
#' get_file(url = paste0(dir, taxa), export = taxa)
#' weight_annotations(
#'   library = library,
#'   org_tax_ott = org_tax_ott,
#'   str_stereo = str_stereo,
#'   annotations = annotations,
#'   canopus = canopus,
#'   formula = formula,
#'   components = components,
#'   edges = edges,
#'   taxa = taxa
#' )
#' unlink("data", recursive = TRUE)
#' }
weight_annotations <- function(
  library = get_params(
    step = "weight_annotations"
  )$files$libraries$sop$merged$keys,
  org_tax_ott = get_params(
    step = "weight_annotations"
  )$files$libraries$sop$merged$organisms$taxonomies$ott,
  str_stereo = get_params(
    step = "weight_annotations"
  )$files$libraries$sop$merged$structures$stereo,
  annotations = get_params(
    step = "weight_annotations"
  )$files$annotations$filtered,
  canopus = get_params(
    step = "weight_annotations"
  )$files$annotations$prepared$canopus,
  formula = get_params(
    step = "weight_annotations"
  )$files$annotations$prepared$formula,
  components = get_params(
    step = "weight_annotations"
  )$files$networks$spectral$components$prepared,
  edges = get_params(
    step = "weight_annotations"
  )$files$networks$spectral$edges$prepared,
  taxa = get_params(step = "weight_annotations")$files$metadata$prepared,
  output = get_params(step = "weight_annotations")$files$annotations$processed,
  candidates_neighbors = get_params(
    step = "weight_annotations"
  )$annotations$candidates$neighbors,
  candidates_final = get_params(
    step = "weight_annotations"
  )$annotations$candidates$final,
  best_percentile = get_params(
    step = "weight_annotations"
  )$annotations$candidates$best_percentile,
  weight_spectral = get_params(
    step = "weight_annotations"
  )$weights$global$spectral,
  weight_chemical = get_params(
    step = "weight_annotations"
  )$weights$global$chemical,
  weight_biological = get_params(
    step = "weight_annotations"
  )$weights$global$biological,
  score_biological_domain = get_params(
    step = "weight_annotations"
  )$weights$biological$domain,
  score_biological_kingdom = get_params(
    step = "weight_annotations"
  )$weights$biological$kingdom,
  score_biological_phylum = get_params(
    step = "weight_annotations"
  )$weights$biological$phylum,
  score_biological_class = get_params(
    step = "weight_annotations"
  )$weights$biological$class,
  score_biological_order = get_params(
    step = "weight_annotations"
  )$weights$biological$order,
  score_biological_infraorder = get_params(
    step = "weight_annotations"
  )$weights$biological$infraorder,
  score_biological_family = get_params(
    step = "weight_annotations"
  )$weights$biological$family,
  score_biological_subfamily = get_params(
    step = "weight_annotations"
  )$weights$biological$subfamily,
  score_biological_tribe = get_params(
    step = "weight_annotations"
  )$weights$biological$tribe,
  score_biological_subtribe = get_params(
    step = "weight_annotations"
  )$weights$biological$subtribe,
  score_biological_genus = get_params(
    step = "weight_annotations"
  )$weights$biological$genus,
  score_biological_subgenus = get_params(
    step = "weight_annotations"
  )$weights$biological$subgenus,
  score_biological_species = get_params(
    step = "weight_annotations"
  )$weights$biological$species,
  score_biological_subspecies = get_params(
    step = "weight_annotations"
  )$weights$biological$subspecies,
  score_biological_variety = get_params(
    step = "weight_annotations"
  )$weights$biological$variety,
  score_biological_biota = get_params(
    step = "weight_annotations"
  )$weights$biological$biota,
  score_chemical_cla_kingdom = get_params(
    step = "weight_annotations"
  )$weights$chemical$cla$kingdom,
  score_chemical_cla_superclass = get_params(
    step = "weight_annotations"
  )$weights$chemical$cla$superclass,
  score_chemical_cla_class = get_params(
    step = "weight_annotations"
  )$weights$chemical$cla$class,
  score_chemical_cla_parent = get_params(
    step = "weight_annotations"
  )$weights$chemical$cla$parent,
  score_chemical_npc_pathway = get_params(
    step = "weight_annotations"
  )$weights$chemical$npc$pathway,
  score_chemical_npc_superclass = get_params(
    step = "weight_annotations"
  )$weights$chemical$npc$superclass,
  score_chemical_npc_class = get_params(
    step = "weight_annotations"
  )$weights$chemical$npc$class,
  minimal_consistency = get_params(
    step = "weight_annotations"
  )$annotations$thresholds$consistency,
  minimal_ms1_bio = get_params(
    step = "weight_annotations"
  )$annotations$thresholds$ms1$biological,
  minimal_ms1_chemo = get_params(
    step = "weight_annotations"
  )$annotations$thresholds$ms1$chemical,
  minimal_ms1_condition = get_params(
    step = "weight_annotations"
  )$annotations$thresholds$ms1$condition,
  ms1_only = get_params(step = "weight_annotations")$annotations$ms1only,
  compounds_names = get_params(
    step = "weight_annotations"
  )$options$compounds_names,
  high_evidence = get_params(
    step = "weight_annotations"
  )$options$high_evidence,
  remove_ties = get_params(step = "weight_annotations")$options$remove_ties,
  summarize = get_params(step = "weight_annotations")$options$summarize,
  pattern = get_params(step = "weight_annotations")$files$pattern,
  force = get_params(step = "weight_annotations")$options$force,
  xrefs_file = NULL
) {
  # Input Validation ----

  # These sub-rank weights are accepted for YAML config completeness
  # but not yet wired into scoring.
  force(score_biological_infraorder)
  force(score_biological_subfamily)
  force(score_biological_subtribe)
  force(score_biological_subgenus)
  force(score_biological_subspecies)

  validate_weight_annotations_inputs(
    library = library,
    components = components,
    edges = edges,
    taxa = taxa,
    annotations = annotations,
    str_stereo = str_stereo,
    org_tax_ott = org_tax_ott,
    canopus = canopus,
    formula = formula,
    minimal_ms1_condition = minimal_ms1_condition,
    weight_spectral = weight_spectral,
    weight_chemical = weight_chemical,
    weight_biological = weight_biological,
    minimal_consistency = minimal_consistency,
    minimal_ms1_bio = minimal_ms1_bio,
    minimal_ms1_chemo = minimal_ms1_chemo,
    ms1_only = ms1_only,
    compounds_names = compounds_names,
    high_evidence = high_evidence,
    remove_ties = remove_ties,
    summarize = summarize,
    force = force,
    candidates_neighbors = candidates_neighbors,
    candidates_final = candidates_final
  )

  log_info("Starting annotation weighting and scoring")
  log_debug(
    "Weights - Spectral: %s, Chemical: %s, Biological: %s",
    weight_spectral,
    weight_chemical,
    weight_biological
  )
  log_debug(
    "Candidates - Neighbors: %d, Final: %d, Best percentile: %s",
    candidates_neighbors,
    candidates_final,
    best_percentile
  )
  log_debug(
    "MS1-only mode: %s, High evidence: %s",
    ms1_only,
    high_evidence
  )

  ctx <- log_operation(
    "weight_annotations",
    n_candidates_neighbors = candidates_neighbors,
    n_candidates_final = candidates_final
  )

  # Load Input Data ----
  log_debug("Loading input data tables")

  components_table <- safe_fread(
    file = components,
    file_type = "components table",
    na.strings = c("", "NA"),
    colClasses = "character"
  )

  edges_table <- load_edges_table(edges, candidates_neighbors)

  structure_organism_pairs_table <- load_structure_organism_pairs(
    library,
    str_stereo,
    org_tax_ott
  )
  log_debug(
    "Loaded %d structure-organism pairs",
    nrow(structure_organism_pairs_table)
  )

  canopus_table <- safe_fread(
    file = canopus,
    file_type = "CANOPUS classifications",
    na.strings = c("", "NA"),
    colClasses = "character"
  )

  formula_table <- safe_fread(
    file = formula,
    file_type = "molecular formulas",
    na.strings = c("", "NA"),
    colClasses = "character"
  )

  annotation_table <- load_annotation_tables(annotations, ms1_only)

  # Log statistics
  log_annotation_stats(annotation_table)

  features_table <- annotation_table |>
    tidytable::distinct(tidyselect::any_of(x = c("feature_id", "rt", "mz")))
  log_debug("Extracted %d unique features", nrow(features_table))

  # Rearrange and Merge Annotations ----
  log_debug(
    "Merging annotation sources (spectra, SIRIUS, CANOPUS, formula)"
  )

  annotation_table <- rearrange_annotations(
    annotation_table,
    formula_table,
    canopus_table,
    edges_table
  )
  log_debug("Merged annotation table: %d rows", nrow(annotation_table))

  # Clean up intermediate objects
  rm(formula_table, canopus_table)

  # Add Biological Metadata and Perform Scoring ----
  log_debug("Adding taxonomic metadata and computing biological scores")

  annotation_table_taxed <- annotation_table |>
    tidytable::left_join(
      y = safe_fread(
        file = taxa,
        file_type = "features taxed",
        na.strings = c("", "NA"),
        colClasses = "character"
      ),
      by = "feature_id"
    )

  rm(annotation_table)

  # Perform taxonomically informed scoring
  annot_table_wei_bio <- weight_bio(
    annotation_table_taxed = annotation_table_taxed,
    structure_organism_pairs_table = structure_organism_pairs_table,
    weight_spectral = weight_spectral,
    weight_biological = weight_biological,
    score_biological_domain = score_biological_domain,
    score_biological_kingdom = score_biological_kingdom,
    score_biological_phylum = score_biological_phylum,
    score_biological_class = score_biological_class,
    score_biological_order = score_biological_order,
    score_biological_family = score_biological_family,
    score_biological_tribe = score_biological_tribe,
    score_biological_genus = score_biological_genus,
    score_biological_species = score_biological_species,
    score_biological_variety = score_biological_variety,
    score_biological_biota = score_biological_biota
  )
  rm(annotation_table_taxed)
  log_debug(
    "Biological scoring complete: %d candidates",
    nrow(annot_table_wei_bio)
  )

  annot_table_wei_bio |>
    decorate_bio(
      score_biological_kingdom = score_biological_kingdom,
      score_biological_phylum = score_biological_phylum,
      score_biological_class = score_biological_class,
      score_biological_order = score_biological_order,
      score_biological_family = score_biological_family,
      score_biological_tribe = score_biological_tribe,
      score_biological_genus = score_biological_genus,
      score_biological_species = score_biological_species,
      score_biological_variety = score_biological_variety,
      score_biological_biota = score_biological_biota
    )

  annot_table_wei_bio_clean <- annot_table_wei_bio |>
    clean_bio(
      edges_table = edges_table,
      minimal_consistency = minimal_consistency
    )
  rm(annot_table_wei_bio, edges_table)
  log_debug(
    "Biological cleaning complete: %d candidates",
    nrow(annot_table_wei_bio_clean)
  )

  log_debug("Computing chemical taxonomy scores")
  annot_table_wei_chemo <- annot_table_wei_bio_clean |>
    weight_chemo(
      weight_spectral = weight_spectral,
      weight_biological = weight_biological,
      weight_chemical = weight_chemical,
      score_chemical_cla_kingdom = score_chemical_cla_kingdom,
      score_chemical_cla_superclass = score_chemical_cla_superclass,
      score_chemical_cla_class = score_chemical_cla_class,
      score_chemical_cla_parent = score_chemical_cla_parent,
      score_chemical_npc_pathway = score_chemical_npc_pathway,
      score_chemical_npc_superclass = score_chemical_npc_superclass,
      score_chemical_npc_class = score_chemical_npc_class
    )
  rm(annot_table_wei_bio_clean)
  log_debug(
    "Chemical scoring complete: %d candidates",
    nrow(annot_table_wei_chemo)
  )

  annot_table_wei_chemo |>
    decorate_chemo(
      score_chemical_cla_kingdom = score_chemical_cla_kingdom,
      score_chemical_cla_superclass = score_chemical_cla_superclass,
      score_chemical_cla_class = score_chemical_cla_class,
      score_chemical_cla_parent = score_chemical_cla_parent,
      score_chemical_npc_pathway = score_chemical_npc_pathway,
      score_chemical_npc_superclass = score_chemical_npc_superclass,
      score_chemical_npc_class = score_chemical_npc_class
    )

  log_debug("Finalizing results (filtering, ranking, deduplication)")

  xrefs_table <- NULL
  if (!is.null(xrefs_file) && file.exists(xrefs_file)) {
    xrefs_table <- safe_fread(
      file = xrefs_file,
      file_type = "compound xrefs",
      na.strings = c("", "NA"),
      colClasses = "character"
    )
  }

  results_list <- annot_table_wei_chemo |>
    clean_chemo(
      components_table = components_table,
      features_table = features_table,
      structure_organism_pairs_table = structure_organism_pairs_table,
      candidates_final = candidates_final,
      best_percentile = best_percentile,
      minimal_ms1_bio = minimal_ms1_bio,
      minimal_ms1_chemo = minimal_ms1_chemo,
      minimal_ms1_condition = minimal_ms1_condition,
      compounds_names = compounds_names,
      high_evidence = high_evidence,
      remove_ties = remove_ties,
      summarize = summarize,
      score_chemical_cla_kingdom = score_chemical_cla_kingdom,
      score_chemical_cla_superclass = score_chemical_cla_superclass,
      score_chemical_cla_class = score_chemical_cla_class,
      score_chemical_cla_parent = score_chemical_cla_parent,
      score_chemical_npc_pathway = score_chemical_npc_pathway,
      score_chemical_npc_superclass = score_chemical_npc_superclass,
      score_chemical_npc_class = score_chemical_npc_class,
      max_per_score = 7L,
      xrefs_table = xrefs_table
    )
  rm(
    annot_table_wei_chemo,
    structure_organism_pairs_table,
    xrefs_table,
    components_table,
    features_table
  )

  log_complete(ctx, n_annotations = nrow(results_list$taxa))

  # Export Results ----
  log_debug("Exporting results to disk")

  output_paths <- export_results(results_list, output, pattern)
  rm(results_list)

  log_info("Results exported: %s", basename(output_paths["full"]))
  output_paths
}
