#' @title Prepare features edges
#'
#' @description This function prepares molecular network edges by combining
#'     MS1-based and spectral similarity edges, adding entropy information,
#'     and standardizing column names. Edges represent relationships between
#'     features in the molecular network.
#'
#' @include get_params.R
#' @include logs_utils.R
#' @include errors_utils.R
#' @include safe_fread.R
#' @include export_params.R
#' @include export_output.R
#' @include validations_params.R
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
      stats::setNames(
        Map(
          function(x, y) {
            safe_fread(
              file = x,
              file_type = paste0(y, " edges"),
              na.strings = c("", "NA"),
              colClasses = "character"
            )
          },
          input,
          names(input)
        ),
        names(input)
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
  if (length(entropy_columns) > 0L) {
    features_entropy <- .fast_distinct_with_key_guard(
      tidytable::as_tidytable(as.list(edges_ms2)[entropy_columns]),
      name_source
    )
  } else {
    features_entropy <- tidytable::tidytable()
  }

  # Combine and Format Edges ----
  extra_ent_cols <- setdiff(entropy_columns, name_source)
  if (
    name_source %in%
      names(edges_ms1) &&
      name_source %in% names(features_entropy) &&
      !any(extra_ent_cols %in% names(edges_ms1)) &&
      !anyDuplicated(features_entropy[[name_source]])
  ) {
    step2 <- data.table::copy(edges_ms1)
    src_ms1 <- step2[[name_source]]
    src_ent <- features_entropy[[name_source]]
    m_ent <- if (anyNA(src_ent) || anyNA(src_ms1)) {
      match(src_ms1, src_ent)
    } else {
      fastmatch::fmatch(src_ms1, src_ent)
    }
    for (col in extra_ent_cols) {
      data.table::set(step2, j = col, value = features_entropy[[col]][m_ent])
    }
    unmatched_ent <- if (anyNA(src_ent) || anyNA(src_ms1)) {
      is.na(match(src_ent, src_ms1))
    } else {
      fastmatch::fmatch(src_ent, src_ms1, nomatch = 0L) == 0L
    }
    if (any(unmatched_ent)) {
      step2 <- tidytable::bind_rows(
        step2,
        features_entropy[which(unmatched_ent), ]
      )
    }
  } else {
    step2 <- edges_ms1 |>
      tidytable::full_join(y = features_entropy)
  }

  common_cols <- intersect(names(step2), names(edges_ms2))
  if (
    nrow(step2) > 0L &&
      nrow(edges_ms2) > 0L &&
      length(common_cols) > 0L
  ) {
    m12 <- .fast_match_rows(step2, edges_ms2, common_cols)
    has_match <- !is.na(m12)
    if (!any(has_match)) {
      edges_table_treated <- tidytable::bind_rows(step2, edges_ms2)
    } else {
      m21 <- .fast_match_rows(edges_ms2, step2, common_cols)
      idx1_matched <- which(has_match)
      idx2_matched <- which(!is.na(m21))
      if (
        length(idx1_matched) == length(idx2_matched) &&
          !anyDuplicated(m12[idx1_matched]) &&
          !anyDuplicated(m21[idx2_matched])
      ) {
        ms2_extra_cols <- setdiff(names(edges_ms2), common_cols)
        y_matched_rows <- m12[idx1_matched]
        for (col in ms2_extra_cols) {
          col_val <- rep(NA_character_, nrow(step2))
          col_val[idx1_matched] <- as.character(edges_ms2[[col]][
            y_matched_rows
          ])
          data.table::set(step2, j = col, value = col_val)
        }
        unmatched_ms2 <- which(is.na(m21))
        if (length(unmatched_ms2) > 0L) {
          edges_table_treated <- tidytable::bind_rows(
            step2,
            edges_ms2[unmatched_ms2, ]
          )
        } else {
          edges_table_treated <- step2
        }
      } else {
        edges_table_treated <- step2 |>
          tidytable::full_join(y = edges_ms2)
      }
    }
  } else {
    edges_table_treated <- step2 |>
      tidytable::full_join(y = edges_ms2)
  }
  rm(step2)

  if (name_source != "feature_source" || name_target != "feature_target") {
    edges_table_treated <- edges_table_treated |>
      tidytable::rename(
        feature_source = !!as.name(name_source),
        feature_target = !!as.name(name_target)
      )
  }
  ft_vec <- edges_table_treated$feature_target
  na_ft <- is.na(ft_vec)
  if (any(na_ft)) {
    ft_vec[na_ft] <- edges_table_treated$feature_source[na_ft]
    data.table::set(edges_table_treated, j = "feature_target", value = ft_vec)
  }
  edges_table_treated <- edges_table_treated |>
    .fast_distinct_with_key_guard(c("feature_source", "feature_target")) |>
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
