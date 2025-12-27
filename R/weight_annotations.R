#' Validate Inputs for weight_annotations
#'
#' @description Internal helper to validate all input parameters for weight_annotations.
#'     Checks file existence, data types, ranges, and logical consistency of weights.
#'     Stops execution with informative messages if any condition is violated.
#'
#' @include constants.R
#' @include safe_fread.R
#' @include validations_utils.R
#'
#' @param library Character, path to library file (required).
#' @param components Character, path to components file (required).
#' @param edges Character, path to edges file (required).
#' @param taxa Character, path to taxa file (required).
#' @param annotations Character vector, paths to annotation files (required).
#' @param str_stereo Character, path to stereo structures file (optional).
#' @param org_tax_ott Character, path to organism taxonomy file (optional).
#' @param canopus Character, path to canopus file (optional).
#' @param formula Character, path to formula file (optional).
#' @param minimal_ms1_condition Character, "OR" or "AND".
#' @param weight_spectral Numeric (0-Inf), weight for spectral score.
#' @param weight_chemical Numeric (0-Inf), weight for chemical score.
#' @param weight_biological Numeric (0-Inf), weight for biological score.
#' @param minimal_consistency Numeric (0-1), min consistency threshold.
#' @param minimal_ms1_bio Numeric (0-1), min biological score for MS1.
#' @param minimal_ms1_chemo Numeric (0-1), min chemical score for MS1.
#' @param ms1_only Logical, filter for MS1-only annotations.
#' @param compounds_names Logical, include compound names.
#' @param high_confidence Logical, report high-confidence candidates only.
#' @param remove_ties Logical, remove tied candidates.
#' @param summarize Logical, summarize to one row per feature.
#' @param force Logical, force execution (override warnings).
#' @param candidates_neighbors Integer >= 1, neighbors to keep.
#' @param candidates_final Integer >= 1, final candidates to keep.
#'
#' @return NULL (invisible). Stops execution if validation fails.
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
  # Validate required file existence
  required_files <- list(
    library = library,
    components = components,
    edges = edges,
    taxa = taxa
  )
  missing_required <- purrr::keep(.x = required_files, .p = ~ !file.exists(.x))
  if (length(missing_required) > 0L) {
    stop(
      "Required file(s) not found: ",
      paste(
        names(missing_required),
        missing_required,
        sep = " at ",
        collapse = "; "
      ),
      call. = FALSE
    )
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

  # Validate optional files (log warnings only)
  optional_files <- list(
    str_stereo = str_stereo,
    org_tax_ott = org_tax_ott,
    canopus = canopus,
    formula = formula
  )
  purrr::iwalk(.x = optional_files, .f = function(path, name) {
    if (!is.null(path) && !file.exists(path)) {
      log_warn("Optional file '%s' not found at: %s", name, path)
    }
  })

  # Validate minimal_ms1_condition
  assert_choice(minimal_ms1_condition, c("OR", "AND"), "minimal_ms1_condition")

  # Validate weights: must be numeric, non-negative, sum to ~1
  weights <- c(
    spectral = weight_spectral,
    chemical = weight_chemical,
    biological = weight_biological
  )
  if (!all(is.numeric(weights)) || any(is.na(weights))) {
    stop(
      "All weights (spectral, chemical, biological) must be numeric and non-NA",
      call. = FALSE
    )
  }
  if (any(weights < 0)) {
    stop(
      "All weights must be non-negative, got: ",
      paste(names(weights), "=", weights, collapse = ", "),
      call. = FALSE
    )
  }
  weight_sum <- sum(weights)
  if (abs(weight_sum - 1) > WEIGHT_SUM_TOLERANCE) {
    stop(
      "Weights must sum to 1 (tolerance ",
      WEIGHT_SUM_TOLERANCE,
      "), got sum = ",
      signif(weight_sum, 6),
      " (",
      paste(names(weights), weights, sep = "=", collapse = ", "),
      ")",
      call. = FALSE
    )
  }

  # Validate score thresholds (0-1)
  assert_scalar_numeric(
    minimal_consistency,
    "minimal_consistency",
    min = 0,
    max = 1
  )
  assert_scalar_numeric(minimal_ms1_bio, "minimal_ms1_bio", min = 0, max = 1)
  assert_scalar_numeric(
    minimal_ms1_chemo,
    "minimal_ms1_chemo",
    min = 0,
    max = 1
  )

  # Validate logical flags
  assert_flag(ms1_only, "ms1_only")
  assert_flag(compounds_names, "compounds_names")
  assert_flag(high_confidence, "high_confidence")
  assert_flag(remove_ties, "remove_ties")
  assert_flag(summarize, "summarize")
  assert_flag(force, "force")

  # Validate candidate counts
  assert_positive_integer(candidates_neighbors, "candidates_neighbors")
  assert_positive_integer(candidates_final, "candidates_final")

  invisible(NULL)
}

#' Load and Prepare Annotation Tables
#'
#' @description Internal helper to load, combine, and filter annotation tables.
#'     Reads multiple annotation files, binds rows, and optionally filters to
#'     retain only MS1-based annotations (those without MS2 similarity or SIRIUS CSI scores).
#'
#' @param annotations Character vector of file paths to annotation files.
#' @param ms1_only Logical; if TRUE, keep only annotations where both
#'     `candidate_score_similarity` and `candidate_score_sirius_csi` are NA.
#'
#' @return Data frame with combined (and optionally filtered) annotations.
#' @keywords internal
load_annotation_tables <- function(annotations, ms1_only) {
  log_debug("Loading %d annotation file(s)", length(annotations))
  annotation_table <- tryCatch(
    purrr::map2(
      .x = annotations,
      .y = seq_along(annotations),
      .f = ~ safe_fread(
        file = .x,
        file_type = paste0("annotation file ", .y),
        na.strings = c("", "NA"),
        colClasses = "character"
      )
    ) |>
      tidytable::bind_rows(),
    error = function(e) {
      stop(
        "Failed to load annotation files: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )

  if (ms1_only) {
    n_before <- nrow(annotation_table)
    annotation_table <- annotation_table |>
      tidytable::filter(
        is.na(candidate_score_similarity) &
          is.na(candidate_score_sirius_csi)
      )
    log_debug(
      "MS1-only filter: %d -> %d rows",
      n_before,
      nrow(annotation_table)
    )
  }

  if (nrow(annotation_table) == 0L) {
    log_warn("No annotations remaining after loading/filtering")
  }

  annotation_table
}

#' Load Structure-Organism Pairs
#'
#' @description Internal helper to load the main library table and sequentially
#'     left-join optional supplemental tables (stereo structures, organism taxonomy).
#'     Returns a combined data frame suitable for biological scoring.
#'
#' @param library Character, path to main library file (required).
#' @param str_stereo Character, path to stereo structures file (optional; NULL allowed).
#' @param org_tax_ott Character, path to organism taxonomy file (optional; NULL allowed).
#'
#' @return Data frame with structure-organism pairs, including any joined metadata.
#' @keywords internal
load_structure_organism_pairs <- function(library, str_stereo, org_tax_ott) {
  log_debug("Loading library from: %s", library)
  library_table <- tryCatch(
    safe_fread(
      file = library,
      file_type = "structure library",
      na.strings = c("", "NA"),
      colClasses = "character"
    ),
    error = function(e) {
      stop("Failed to load library file: ", conditionMessage(e), call. = FALSE)
    }
  )

  supp_files <- list(str_stereo, org_tax_ott)
  supp_names <- c("stereochemistry", "organism taxonomy")
  supp_tables <- purrr::map2(
    .x = supp_files,
    .y = supp_names,
    .f = function(path, name) {
      if (is.null(path) || !file.exists(path)) {
        return(tidytable::tidytable())
      }
      safe_fread(
        file = path,
        file_type = name,
        na.strings = c("", "NA"),
        colClasses = "character"
      )
    }
  )

  purrr::reduce(
    .x = supp_tables,
    .init = library_table,
    .f = function(acc, tbl) {
      if (nrow(tbl) == 0L) {
        return(acc)
      }
      tidytable::left_join(x = acc, y = tbl)
    }
  )
}

#' Load Edges Table with Neighbor Filtering
#'
#' @description Internal helper to load the edges file and retain only the top N
#'     neighbors (by similarity score) for each feature. This reduces complexity
#'     for downstream annotation propagation and scoring.
#'
#' @param edges Character, path to edges file.
#' @param candidates_neighbors Integer, number of top neighbors to keep per feature.
#'
#' @return Data frame with filtered edges (top `candidates_neighbors` per `feature_source`).
#' @keywords internal
load_edges_table <- function(edges, candidates_neighbors) {
  log_debug("Loading edges from: %s", edges)
  edges_table <- tryCatch(
    safe_fread(
      file = edges,
      file_type = "spectral edges table",
      na.strings = c("", "NA"),
      colClasses = "character"
    ),
    error = function(e) {
      stop("Failed to load edges file: ", conditionMessage(e), call. = FALSE)
    }
  )

  n_before <- nrow(edges_table)
  edges_table <- edges_table |>
    tidytable::group_by(feature_source) |>
    tidytable::slice_max(
      order_by = candidate_score_similarity,
      n = candidates_neighbors,
      with_ties = FALSE
    ) |>
    tidytable::ungroup()

  log_debug(
    "Edges filtered to top %d neighbors/feature: %d -> %d rows",
    candidates_neighbors,
    n_before,
    nrow(edges_table)
  )
  edges_table
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
    tidytable::arrange(tidytable::desc(x = n))

  log_info(
    "\n%s",
    paste(
      utils::capture.output(print.data.frame(
        x = annotation_stats,
        row.names = FALSE
      )),
      collapse = "\n"
    )
  )

  invisible(NULL)
}

#' Rearrange Annotation Tables
#'
#' @description Internal helper to reorganize and merge annotation tables.
#'
#' @include columns_utils.R
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
      x = c(
        model$features_columns,
        model$candidates_calculated_columns,
        model$candidates_spectra_columns,
        model$candidates_structures_columns
      )
    )) |>
    tidytable::mutate(
      candidate_score_similarity = as.numeric(candidate_score_similarity)
    ) |>
    tidytable::arrange(tidytable::desc(x = candidate_score_similarity)) |>
    tidytable::distinct(
      feature_id,
      candidate_structure_inchikey_connectivity_layer,
      candidate_structure_smiles_no_stereo,
      .keep_all = TRUE
    )

  annotation_table_2 <- annotation_table |>
    tidytable::select(
      tidyselect::any_of(
        x = c(
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
    .f = tidytable::full_join
  )

  annotation_table_merged |>
    tidytable::left_join(
      y = edges_table |>
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
  high_confidence = get_params(
    step = "weight_annotations"
  )$options$high_confidence,
  remove_ties = get_params(step = "weight_annotations")$options$remove_ties,
  summarize = get_params(step = "weight_annotations")$options$summarize,
  pattern = get_params(step = "weight_annotations")$files$pattern,
  force = get_params(step = "weight_annotations")$options$force
) {
  # Input Validation ----

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
    "MS1-only mode: %s, High confidence: %s",
    ms1_only,
    high_confidence
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
    tidytable::distinct(tidytable::any_of(x = c("feature_id", "rt", "mz")))
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
      )
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
  rm(annot_table_wei_bio)
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
      high_confidence = high_confidence,
      remove_ties = remove_ties,
      summarize = summarize,
      score_chemical_cla_kingdom = score_chemical_cla_kingdom,
      score_chemical_cla_superclass = score_chemical_cla_superclass,
      score_chemical_cla_class = score_chemical_cla_class,
      score_chemical_cla_parent = score_chemical_cla_parent,
      score_chemical_npc_pathway = score_chemical_npc_pathway,
      score_chemical_npc_superclass = score_chemical_npc_superclass,
      score_chemical_npc_class = score_chemical_npc_class,
      max_per_score = 7L
    )
  rm(annot_table_wei_chemo)

  log_complete(ctx, n_annotations = nrow(results_list$taxa))

  # Export Results ----
  log_debug("Exporting results to disk")

  output_paths <- export_results(results_list, output, pattern)
  rm(results_list)

  log_info("Results exported: %s", basename(output_paths['full']))
  return(output_paths)
}
