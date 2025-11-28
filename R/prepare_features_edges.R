#' @title Prepare features edges
#'
#' @description This function prepares molecular network edges by combining
#'     MS1-based and spectral similarity edges, adding entropy information,
#'     and standardizing column names. Edges represent relationships between
#'     features in the molecular network.
#'
#' @include get_params.R
#'
#' @param input Named list containing paths to edge files. Must have "ms1" and
#'     "spectral" elements pointing to respective edge files.
#' @param output Character string path where prepared edges should be saved
#' @param name_source Character string name of the source feature column in input files
#' @param name_target Character string name of the target feature column in input files
#'
#' @return Character string path to the prepared edges file
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
#' input_1 <- get_params(step = "prepare_features_edges")$files$networks$spectral$edges$raw$ms1
#' input_2 <- get_params(step = "prepare_features_edges")$files$networks$spectral$edges$raw$spectral
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
    stop("input must contain 'ms1' and 'spectral' elements")
  }

  # Validate output and column names
  if (!is.character(output) || length(output) != 1L) {
    stop("output must be a single character string")
  }

  if (!is.character(name_source) || length(name_source) != 1L) {
    stop("name_source must be a single character string")
  }

  if (!is.character(name_target) || length(name_target) != 1L) {
    stop("name_target must be a single character string")
  }

  # File existence check
  all_files <- unlist(input)
  missing_files <- all_files[!file.exists(all_files)]
  if (length(missing_files) > 0L) {
    stop("Input file(s) not found: ", paste(missing_files, collapse = ", "))
  }

  # Load Edge Tables ----

  log_info("Preparing molecular network edges")
  log_debug("MS1 edges: %s", input[['ms1']])
  log_debug("Spectral edges: %s", input[['spectral']])

  # Load edges tables
  # log_trace("Loading edge tables")
  edges_tables <- tryCatch(
    {
      purrr::map(
        .x = input,
        .f = tidytable::fread,
        na.strings = c("", "NA"),
        colClasses = "character"
      )
    },
    error = function(e) {
      stop("Failed to read edge files: ", conditionMessage(e))
    }
  )

  edges_ms1 <- edges_tables[["ms1"]]
  edges_ms2 <- edges_tables[["spectral"]]
  rm(edges_tables)

  log_debug("MS1 edges: %d rows", nrow(edges_ms1))
  log_debug("Spectral edges: %d rows", nrow(edges_ms2))

  # Extract Entropy Information ----

  # Extract entropy information from spectral edges
  features_entropy <- edges_ms2 |>
    tidytable::select(
      tidyselect::all_of(x = c(name_source)),
      feature_spectrum_entropy,
      feature_spectrum_peaks
    ) |>
    tidytable::distinct()

  # log_trace(
  # "Extracted entropy for %d features", nrow(features_entropy)
  # )

  # Combine and Format Edges ----

  # Combine and format edges table
  # log_trace("Combining and formatting edge tables")
  edges_table_treated <- edges_ms1 |>
    tidytable::full_join(y = features_entropy) |>
    tidytable::full_join(y = edges_ms2) |>
    tidytable::rename(
      feature_source = !!as.name(name_source),
      feature_target = !!as.name(name_target)
    ) |>
    tidytable::mutate(
      feature_target := tidytable::coalesce(feature_target, feature_source)
    )

  log_info("Prepared %d total edges", nrow(edges_table_treated))

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
  return(output)
}
