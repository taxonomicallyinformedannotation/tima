#' Validate Inputs for weight_annotations
#'
#' @description Internal helper to validate all input parameters for weight_annotations.
#'     Checks file existence, data types, ranges, and logical consistency.
#'
#' @param library Character, path to library file
#' @param components Character, path to components file
#' @param edges Character, path to edges file
#' @param taxa Character, path to taxa file
#' @param annotations Character vector, paths to annotation files
#' @param str_stereo Character, path to stereo structures file (optional)
#' @param org_tax_ott Character, path to organism taxonomy file (optional)
#' @param canopus Character, path to canopus file (optional)
#' @param formula Character, path to formula file (optional)
#' @param minimal_ms1_condition Character, "OR" or "AND"
#' @param weight_spectral Numeric (0-1)
#' @param weight_chemical Numeric (0-1)
#' @param weight_biological Numeric (0-1)
#' @param minimal_consistency Numeric (0-1)
#' @param minimal_ms1_bio Numeric (0-1)
#' @param minimal_ms1_chemo Numeric (0-1)
#' @param ms1_only Logical
#' @param compounds_names Logical
#' @param high_confidence Logical
#' @param remove_ties Logical
#' @param summarize Logical
#' @param force Logical
#' @param candidates_neighbors Integer >= 1
#' @param candidates_final Integer >= 1
#'
#' @return NULL (stops execution if validation fails)
#' @keywords internal
validate_weight_annotations_inputs <- function(
  library,
  components,
  edges,
  taxa,
  annotations,
  str_stereo = NULL,
  org_tax_ott = NULL,
  canopus = NULL,
  formula = NULL,
  minimal_ms1_condition,
  weight_spectral,
  weight_chemical,
  weight_biological,
  minimal_consistency,
  minimal_ms1_bio,
  minimal_ms1_chemo,
  ms1_only,
  compounds_names,
  high_confidence,
  remove_ties,
  summarize,
  force,
  candidates_neighbors,
  candidates_final
) {
  # Validate required file paths
  required_files <- list(
    library = library,
    components = components,
    edges = edges,
    taxa = taxa
  )

  for (file_name in names(required_files)) {
    file_path <- required_files[[file_name]]
    if (!file.exists(file_path)) {
      stop(
        "Required file not found: ",
        file_name,
        " at ",
        file_path,
        call. = FALSE
      )
    }
  }

  # Validate annotation files
  missing_annotations <- annotations[!file.exists(annotations)]
  if (length(missing_annotations) > 0L) {
    stop(
      "Annotation file(s) not found: ",
      paste(missing_annotations, collapse = ", "),
      call. = FALSE
    )
  }

  # Validate optional files with warnings
  optional_files <- list(
    str_stereo = str_stereo,
    org_tax_ott = org_tax_ott,
    canopus = canopus,
    formula = formula
  )

  for (file_name in names(optional_files)) {
    file_path <- optional_files[[file_name]]
    if (!is.null(file_path) && !file.exists(file_path)) {
      logger::log_warn("Optional file not found: {file_name} at {file_path}")
    }
  }

  # Validate minimal_ms1_condition
  if (!minimal_ms1_condition %in% c("OR", "AND")) {
    stop(
      "minimal_ms1_condition must be 'OR' or 'AND', got: ",
      minimal_ms1_condition,
      call. = FALSE
    )
  }

  # ---- Weights ----
  weights <- c(
    spectral = weight_spectral,
    chemical = weight_chemical,
    biological = weight_biological
  )
  if (!all(is.numeric(weights)) || any(is.na(weights))) {
    stop("All weights must be numeric and non-NA", call. = FALSE)
  }
  if (any(weights < 0)) {
    stop("All weights must be non-negative", call. = FALSE)
  }
  weight_sum <- sum(weights)
  if (abs(weight_sum - 1) > WEIGHT_SUM_TOLERANCE) {
    stop(
      "Weights must sum to 1 (tolerance ",
      WEIGHT_SUM_TOLERANCE,
      "), got: ",
      signif(weight_sum, 6),
      " (",
      paste(names(weights), weights, sep = ":", collapse = ", "),
      ")",
      call. = FALSE
    )
  }

  # Validate score parameters
  score_params <- list(
    minimal_consistency = minimal_consistency,
    minimal_ms1_bio = minimal_ms1_bio,
    minimal_ms1_chemo = minimal_ms1_chemo
  )

  for (param_name in names(score_params)) {
    param_value <- score_params[[param_name]]
    if (!is.numeric(param_value) || param_value < 0 || param_value > 1) {
      stop(
        param_name,
        " must be between 0 and 1, got: ",
        param_value,
        call. = FALSE
      )
    }
  }

  # Validate logical parameters
  logical_params <- list(
    ms1_only = ms1_only,
    compounds_names = compounds_names,
    high_confidence = high_confidence,
    remove_ties = remove_ties,
    summarize = summarize,
    force = force
  )

  purrr::iwalk(logical_params, function(val, nm) assert_flag(val, nm))

  # Validate candidates parameters
  assert_positive_integer(candidates_neighbors, "candidates_neighbors")
  assert_positive_integer(candidates_final, "candidates_final")

  invisible(NULL)
}

#' Load and Prepare Annotation Tables
#'
#' @description Internal helper to load, combine, and filter annotation tables.
#'
#' @param annotations Character vector of file paths
#' @param ms1_only Logical, keep only MS1 annotations
#'
#' @return Data frame with combined annotations
#' @keywords internal
load_annotation_tables <- function(annotations, ms1_only) {
  annotation_table <- purrr::map(
    .x = annotations,
    .f = tidytable::fread,
    na.strings = c("", "NA"),
    colClasses = "character"
  ) |>
    tidytable::bind_rows()

  if (ms1_only) {
    annotation_table <- annotation_table |>
      tidytable::filter(
        is.na(candidate_score_similarity) &
          is.na(candidate_score_sirius_csi)
      )
  }

  annotation_table
}

#' Load Structure-Organism Pairs
#'
#' @description Internal helper to load and join library tables.
#'
#' @param library Character, path to library file
#' @param str_stereo Character, path to stereo file (optional)
#' @param org_tax_ott Character, path to taxonomy file (optional)
#'
#' @return Data frame with structure-organism pairs
#' @keywords internal
load_structure_organism_pairs <- function(library, str_stereo, org_tax_ott) {
  library_table <- tidytable::fread(
    file = library,
    na.strings = c("", "NA"),
    colClasses = "character"
  )

  supp_files <- list(str_stereo, org_tax_ott)
  supp_tables <- purrr::map(
    .x = supp_files,
    .f = function(file.path) {
      tidytable::fread(
        file = file.path,
        na.strings = c("", "NA"),
        colClasses = "character"
      )
    }
  )

  purrr::reduce(
    .x = supp_tables,
    .init = library_table,
    .f = tidytable::left_join
  )
}

#' Load Edges Table with Neighbor Filtering
#'
#' @description Internal helper to load edges and keep top neighbors.
#'
#' @param edges Character, path to edges file
#' @param candidates_neighbors Integer, number of neighbors to keep
#'
#' @return Data frame with filtered edges
#' @keywords internal
load_edges_table <- function(edges, candidates_neighbors) {
  tidytable::fread(
    file = edges,
    na.strings = c("", "NA"),
    colClasses = "character"
  ) |>
    tidytable::group_by(feature_source) |>
    tidytable::slice_max(
      order_by = candidate_score_similarity,
      n = candidates_neighbors,
      with_ties = FALSE
    )
}

#' Log Annotation Statistics
#'
#' @description Internal helper to log annotation counts by library.
#'
#' @param annotation_table Data frame with annotations
#'
#' @return NULL (logs statistics as side effect)
#' @keywords internal
log_annotation_stats <- function(annotation_table) {
  annotation_stats <- annotation_table |>
    tidytable::filter(
      !is.na(candidate_structure_inchikey_connectivity_layer)
    ) |>
    tidytable::distinct(
      feature_id,
      candidate_library,
      candidate_structure_inchikey_connectivity_layer
    ) |>
    tidytable::group_by(candidate_library) |>
    tidytable::count() |>
    tidytable::arrange(tidytable::desc(n))

  logger::log_info(
    "\n{paste(capture.output(print.data.frame(annotation_stats, row.names = FALSE)), collapse = '\n')}"
  )

  invisible(NULL)
}

#' Rearrange Annotation Tables
#'
#' @description Internal helper to reorganize and merge annotation tables.
#'
#' @param annotation_table Data frame with annotations
#' @param formula_table Data frame with formula data
#' @param canopus_table Data frame with canopus data
#' @param edges_table Data frame with edges
#'
#' @return Data frame with rearranged annotations
#' @keywords internal
rearrange_annotations <- function(
  annotation_table,
  formula_table,
  canopus_table,
  edges_table
) {
  model <- columns_model()

  annotation_table_1 <- annotation_table |>
    tidytable::select(tidyselect::any_of(
      c(
        model$features_columns,
        model$candidates_calculated_columns,
        model$candidates_spectra_columns,
        model$candidates_structures_columns
      )
    )) |>
    tidytable::mutate(
      candidate_score_similarity = as.numeric(candidate_score_similarity)
    ) |>
    tidytable::arrange(tidytable::desc(candidate_score_similarity)) |>
    tidytable::distinct(
      feature_id,
      candidate_structure_inchikey_connectivity_layer,
      candidate_structure_smiles_no_stereo,
      .keep_all = TRUE
    )

  annotation_table_2 <- annotation_table |>
    tidytable::select(
      tidyselect::any_of(
        c(
          model$features_columns,
          model$candidates_sirius_str_columns,
          model$candidates_structures_columns
        )
      ),
      -candidate_structure_error_mz,
      -candidate_structure_error_rt
    ) |>
    tidytable::filter(!is.na(candidate_score_sirius_csi)) |>
    tidytable::distinct()

  tables_full <- list(
    annotation_table_1,
    annotation_table_2,
    formula_table,
    canopus_table
  )

  annotation_table_merged <- purrr::reduce(
    .x = tables_full,
    .f = function(x, y) {
      tidytable::full_join(x, y)
    }
  )

  annotation_table_merged |>
    tidytable::left_join(
      edges_table |>
        tidytable::distinct(
          feature_id = feature_source,
          feature_spectrum_entropy,
          feature_spectrum_peaks
        )
    )
}

#' Export Results to Files
#'
#' @description Internal helper to export all result tiers and parameters.
#'
#' @param results_list List with full, filtered, mini data frames
#' @param output Character, base output filename
#' @param pattern Character, pattern for directory naming
#'
#' @return Named character vector with output file paths
#' @keywords internal
export_results <- function(results_list, output, pattern) {
  time <- format(Sys.time(), "%Y%m%d_%H%M%S")
  dir_time <- file.path(
    get_default_paths()$data$processed$path,
    paste0(time, "_", pattern)
  )

  final_output <- file.path(dir_time, output)
  final_output_filtered <- file.path(
    dir_time,
    gsub(output, pattern = ".tsv", replacement = "_filtered.tsv", fixed = TRUE)
  )
  final_output_mini <- file.path(
    dir_time,
    gsub(output, pattern = ".tsv", replacement = "_mini.tsv", fixed = TRUE)
  )

  # Export parameters
  export_params(
    parameters = get_params(step = "prepare_params"),
    directory = dir_time,
    step = "prepare_params"
  )
  export_params(
    parameters = get_params(step = "prepare_params_advanced"),
    directory = dir_time,
    step = "prepare_params_advanced"
  )

  # Export results
  export_output(x = results_list$mini, file = final_output_mini)
  export_output(x = results_list$filtered, file = final_output_filtered)
  export_output(x = results_list$full, file = final_output)

  c(
    "filtered" = final_output_filtered,
    "full" = final_output
  )
}

#' @title Weight annotations
#'
#' @description This function weights annotations.
#'
#' @include clean_bio.R
#' @include clean_chemo.R
#' @include columns_model.R
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
#' @param high_confidence Report high confidence candidates only. BOOLEAN
#' @param remove_ties Remove ties. BOOLEAN
#' @param summarize Summarize results (1 row per feature). BOOLEAN
#' @param pattern Pattern to identify your job. STRING
#'
#' @return The path to the weighted annotations
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
#' library <- get_params(step = "weight_annotations")$files$libraries$sop$merged$keys |>
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
  high_confidence = get_params(
    step = "weight_annotations"
  )$options$high_confidence,
  remove_ties = get_params(step = "weight_annotations")$options$remove_ties,
  summarize = get_params(step = "weight_annotations")$options$summarize,
  pattern = get_params(step = "weight_annotations")$files$pattern,
  force = get_params(step = "weight_annotations")$options$force
) {
  # ============================================================================
  # Input Validation
  # ============================================================================

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
    high_confidence = high_confidence,
    remove_ties = remove_ties,
    summarize = summarize,
    force = force,
    candidates_neighbors = candidates_neighbors,
    candidates_final = candidates_final
  )

  logger::log_info("Starting annotation weighting and scoring")
  logger::log_debug(
    "Weights - Spectral: {weight_spectral}, ",
    "Chemical: {weight_chemical}, Biological: {weight_biological}"
  )
  logger::log_debug(
    "Candidates - Neighbors: {candidates_neighbors}, Final: {candidates_final}"
  )

  # ============================================================================
  # Load Input Data
  # ============================================================================

  components_table <- tidytable::fread(
    file = components,
    na.strings = c("", "NA"),
    colClasses = "character"
  )

  edges_table <- load_edges_table(edges, candidates_neighbors)

  structure_organism_pairs_table <- load_structure_organism_pairs(
    library,
    str_stereo,
    org_tax_ott
  )

  canopus_table <- tidytable::fread(
    file = canopus,
    na.strings = c("", "NA"),
    colClasses = "character"
  )

  formula_table <- tidytable::fread(
    file = formula,
    na.strings = c("", "NA"),
    colClasses = "character"
  )

  annotation_table <- load_annotation_tables(annotations, ms1_only)

  # Log statistics
  log_annotation_stats(annotation_table)

  features_table <- annotation_table |>
    tidytable::distinct(feature_id, rt, mz)

  # ============================================================================
  # Rearrange and Merge Annotations
  # ============================================================================

  annotation_table <- rearrange_annotations(
    annotation_table,
    formula_table,
    canopus_table,
    edges_table
  )

  # Clean up intermediate objects
  rm(formula_table, canopus_table)

  # ============================================================================
  # Add Biological Metadata and Perform Scoring
  # ============================================================================

  annotation_table_taxed <- annotation_table |>
    tidytable::left_join(tidytable::fread(
      file = taxa,
      na.strings = c("", "NA"),
      colClasses = "character"
    ))

  rm(annotation_table)

  # Perform taxonomically informed scoring
  annot_table_wei_bio <- weight_bio()
  rm(annotation_table_taxed)

  annot_table_wei_bio |>
    decorate_bio()

  annot_table_wei_bio_clean <- annot_table_wei_bio |>
    clean_bio(
      edges_table = edges_table,
      minimal_consistency = minimal_consistency
    )
  rm(annot_table_wei_bio)

  annot_table_wei_chemo <- annot_table_wei_bio_clean |>
    weight_chemo()
  rm(annot_table_wei_bio_clean)

  annot_table_wei_chemo |>
    decorate_chemo()

  results_list <- annot_table_wei_chemo |>
    clean_chemo()
  rm(annot_table_wei_chemo)

  # ============================================================================
  # Export Results
  # ============================================================================

  output_paths <- export_results(results_list, output, pattern)
  rm(results_list)

  return(output_paths)
}
