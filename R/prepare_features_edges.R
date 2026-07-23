#' @title Prepare features edges
#'
#' @description This function prepares molecular network edges by combining
#'     MS1-based and spectral similarity edges, adding entropy information,
#'     and standardizing column names. Edges represent relationships between
#'     features in the molecular network.
#'
#' @include get_params.R
#'
#' @param input [list] Named list containing paths to edge files. Must have
#'     "ms1" and
#'     "spectral" elements pointing to respective edge files.
#' @param output [character] Character string path where prepared edges should
#'     be saved
#' @param name_source [character] Character string name of the source feature
#'     column in input files
#' @param name_target [character] Character string name of the target feature
#'     column in input files
#'
#' @return Character string path to the prepared edges file
#'
#' @family preparation
#'
#' @export
#'
#' @examples
#' \dontrun{
#' copy_backbone()
#' go_to_cache()
#' github <- "https://raw.githubusercontent.com/"
#' repo <- "taxonomicallyinformedannotation/tima-example-files/main/"
#' dir <- paste0(github, repo)
#' input_1 <- get_params(
#'   step =
#'     "prepare_features_edges"
#' )$files$networks$spectral$edges$raw$ms1
#' input_2 <- get_params(
#'   step =
#'     "prepare_features_edges"
#' )$files$networks$spectral$edges$raw$spectral
#' get_file(url = paste0(dir, input_1), export = input_1)
#' get_file(url = paste0(dir, input_2), export = input_2)
#' prepare_features_edges(
#'   input = list("ms1" = input_1, "spectral" = input_2)
#' )
#' unlink("data", recursive = TRUE)
#' }
prepare_features_edges <- function(
  input = get_params(
    step = "prepare_features_edges"
  )$files$networks$spectral$edges$raw,
  output = get_params(
    step = "prepare_features_edges"
  )$files$networks$spectral$edges$prepared,
  name_source = get_params(step = "prepare_features_edges")$names$source,
  name_target = get_params(step = "prepare_features_edges")$names$target
) {
  # Input Validation ----

  # Validate input structure (must have ms1 and spectral)
  input_names <- names(input)
  if (
    is.null(input_names) ||
      !"ms1" %in% input_names ||
      !"spectral" %in% input_names
  ) {
    cli::cli_abort(
      "input must contain 'ms1' and 'spectral' elements",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  # Validate output and column names
  if (!is.character(output) || length(output) != 1L) {
    cli::cli_abort(
      "output must be a single character string",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  ctx <- log_operation("prepare_features_edges", n_edge_types = length(input))

  validate_character(
    name_source,
    param_name = "name_source",
    allow_empty = FALSE
  )
  validate_character(
    name_target,
    param_name = "name_target",
    allow_empty = FALSE
  )

  # File existence check
  all_files <- unlist(input)
  missing_files <- all_files[!file.exists(all_files)]
  if (length(missing_files) > 0L) {
    msg <- format_error(
      problem = "Edge file(s) not found",
      expected = "All edge files to exist",
      received = paste0(length(missing_files), " missing file(s)"),
      fix = paste0(
        "Check these files:\n",
        paste0("  - ", missing_files, collapse = "\n")
      )
    )
    cli::cli_abort(
      msg,
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  # Load Edge Tables ----
  log_debug("MS1 edges: %s", input[["ms1"]])
  log_debug("Spectral edges: %s", input[["spectral"]])

  # Load edges tables
  edges_tables <- tryCatch(
    {
      purrr::imap(
        .x = input,
        .f = ~ safe_fread(
          file = .x,
          file_type = paste0(.y, " edges"),
          na.strings = c("", "NA"),
          colClasses = "character"
        )
      )
    },
    error = function(e) {
      cli::cli_abort(
        c(
          "failed to read edge files",
          "x" = conditionMessage(e)
        ),
        class = c("tima_runtime_error", "tima_error"),
        call = NULL
      )
    }
  )

  edges_ms1 <- edges_tables[["ms1"]]
  edges_ms2 <- edges_tables[["spectral"]]
  rm(edges_tables)

  log_debug("MS1 edges: %d rows", nrow(edges_ms1))
  log_debug("Spectral edges: %d rows", nrow(edges_ms2))

  # Extract Entropy Information ----

  drop_legacy_similarity_columns <- function(edge_table) {
    legacy_columns <- intersect(c("score", "matched_peaks"), names(edge_table))
    if (length(legacy_columns) > 0L) {
      edge_table <- edge_table |>
        tidytable::select(-tidyselect::any_of(legacy_columns))
    }
    edge_table
  }

  standardize_spectral_similarity_columns <- function(edge_table) {
    if (
      !"candidate_score_similarity" %in% names(edge_table) &&
        "score" %in% names(edge_table)
    ) {
      edge_table <- edge_table |>
        tidytable::mutate(candidate_score_similarity = as.numeric(score))
    }

    if (
      !"candidate_count_similarity_peaks_matched" %in% names(edge_table) &&
        "matched_peaks" %in% names(edge_table)
    ) {
      edge_table <- edge_table |>
        tidytable::mutate(
          candidate_count_similarity_peaks_matched = as.integer(matched_peaks)
        )
    }

    edge_table
  }

  edges_ms2 <- standardize_spectral_similarity_columns(edges_ms2)

  entropy_columns <- c(
    name_source,
    "feature_spectrum_entropy",
    "feature_spectrum_peaks"
  )
  entropy_columns <- entropy_columns[entropy_columns %in% names(edges_ms2)]

  # Extract entropy information from spectral edges
  features_entropy <- edges_ms2 |>
    tidytable::select(tidyselect::any_of(entropy_columns)) |>
    tidytable::distinct()

  # Combine and Format Edges ----

  edges_table_treated <- edges_ms1 |>
    tidytable::full_join(y = features_entropy) |>
    tidytable::full_join(y = edges_ms2) |>
    tidytable::rename(
      feature_source = !!as.name(name_source),
      feature_target = !!as.name(name_target)
    ) |>
    tidytable::mutate(
      feature_target = tidytable::coalesce(feature_target, feature_source)
    ) |>
    tidytable::distinct() |>
    drop_legacy_similarity_columns()

  # Retain p_value from MS1 intensity covariance edges (intensity covariance edges only)
  # p_value column is automatically preserved in full_join and kept through processing

  log_complete(ctx, n_edges = nrow(edges_table_treated))

  # Explicit memory cleanup
  rm(edges_ms1, edges_ms2, features_entropy)

  # Export Results ----

  # Export parameters and results
  export_params(
    parameters = get_params(step = "prepare_features_edges"),
    step = "prepare_features_edges"
  )
  export_output(x = edges_table_treated, file = output)

  rm(edges_table_treated)
  output
}
