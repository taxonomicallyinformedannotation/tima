#' @title Prepare features components
#'
#' @description This function prepares molecular network component (cluster)
#'     assignments by loading, standardizing, and formatting component IDs
#'     for each feature. Components represent groups of related features in
#'     the molecular network.
#'
#' @include get_params.R
#' @include logs_utils.R
#' @include safe_fread.R
#'
#' @param input Character vector of paths to input component files. Can be
#'     a single file or multiple files that will be combined.
#' @param output Character string path where prepared components should be saved
#'
#' @return Character string path to the prepared features' components file
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
#' input <- get_params(step = "prepare_features_components")$files$networks$spectral$components$raw
#' get_file(url = paste0(dir, input), export = input)
#' prepare_features_components(input = input)
#' unlink("data", recursive = TRUE)
#' }
prepare_features_components <- function(
  input = get_params(
    step = "prepare_features_components"
  )$files$networks$spectral$components$raw,
  output = get_params(
    step = "prepare_features_components"
  )$files$networks$spectral$components$prepared
) {
  # Input Validation ----

  # Validate inputs
  if (!is.character(input) || length(input) == 0L) {
    stop("input must be a non-empty character vector")
  }

  ctx <- log_operation("prepare_features_components", n_files = length(input))

  validate_character(output, param_name = "output", allow_empty = FALSE)

  # File existence check
  missing_files <- input[!file.exists(input)]
  if (length(missing_files) > 0L) {
    msg <- format_error(
      problem = "Component file(s) not found",
      expected = "All input files to exist",
      received = paste0(length(missing_files), " missing file(s)"),
      fix = paste0(
        "Check these files:\n",
        paste0("  - ", missing_files, collapse = "\n")
      )
    )
    stop(msg, call. = FALSE)
  }

  # Load Component Data ----
  log_debug(
    "Loading molecular network components from %d file(s)",
    length(input)
  )

  # Load and combine component tables
  # log_trace("Loading component tables")
  table <- tryCatch(
    {
      purrr::map2(
        .x = input,
        .y = seq_along(input),
        .f = ~ safe_fread(
          file = .x,
          file_type = paste0("component file ", .y),
          na.strings = c("", "NA"),
          colClasses = "character"
        )
      ) |>
        tidytable::bind_rows()
    },
    error = function(e) {
      stop("Failed to read component files: ", conditionMessage(e))
    }
  )

  # Process Component Assignments ----

  # Early exit for empty data
  if (nrow(table) == 0L) {
    log_warn("No component data found in input files")
    table <- tidytable::tidytable(
      feature_id = character(0),
      component_id = character(0)
    )
  } else {
    log_debug(
      "Loaded {format_count(nrow(table))} component assignments"
    )

    # Standardize column names and select relevant columns
    table <- table |>
      tidytable::select(
        feature_id = `cluster index`,
        component_id = componentindex
      ) |>
      tidytable::distinct()

    log_complete(ctx, n_assignments = nrow(table))
  }

  # Export Results ----

  export_params(
    parameters = get_params(step = "prepare_features_components"),
    step = "prepare_features_components"
  )
  export_output(x = table, file = output)

  rm(table)
  return(output)
}
