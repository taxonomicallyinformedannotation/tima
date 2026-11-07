#' @title Prepare libraries of structure organism pairs CLOSED
#'
#' @description This function prepares closed (private/restricted) structure-
#'     organism pair libraries by formatting columns, rounding values, and
#'     standardizing structure. Falls back to an empty template if the closed
#'     resource is not accessible.
#'
#' @include columns_utils.R
#' @include get_params.R
#' @include round_reals.R
#' @include select_sop_columns.R
#' @include logs_utils.R
#' @include safe_fread.R
#' @include export_params.R
#' @include export_output.R
#'
#' @param input [character] Path to input closed library file
#' @param output [character] Path where prepared library should
#'     be saved
#'
#' @return Character string path to the prepared structure-organism pairs
#'     library
#'
#' @family preparation
#'
#' @export
#'
#' @examples
#' \dontrun{
#' copy_backbone()
#' go_to_cache()
#' prepare_libraries_sop_closed()
#' unlink("data", recursive = TRUE)
#' }
prepare_libraries_sop_closed <- function(
  input = get_params(
    step = "prepare_libraries_sop_closed"
  )$files$libraries$sop$raw$closed,
  output = get_params(
    step = "prepare_libraries_sop_closed"
  )$files$libraries$sop$prepared$closed
) {
  if (!is.character(input) || length(input) != 1L) {
    cli::cli_abort(
      "input must be a single character string",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  if (!is.character(output) || length(output) != 1L) {
    cli::cli_abort(
      "output must be a single character string",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  log_info("Preparing closed structure-organism pairs library")

  if (file.exists(input)) {
    log_debug("Loading closed resource from: %s", input)

    closed <- safe_fread(
      file = input,
      file_type = "closed library",
      na.strings = c("", "NA"),
      colClasses = "character"
    )

    if (nrow(closed) == 0L) {
      log_warn("Closed library file is empty")
      closed_prepared <- fake_sop_columns()
    } else {
      n_closed <- nrow(closed)
      data.table::set(
        closed,
        j = "structure_inchikey_2D",
        value = rep.int(NA_character_, n_closed)
      )
      if ("structure_nameTraditional" %in% names(closed)) {
        data.table::setnames(
          closed,
          old = "structure_nameTraditional",
          new = "structure_name"
        )
      }
      data.table::set(
        closed,
        j = "reference_doi",
        value = rep.int(NA, n_closed)
      )
      closed_prepared <- closed |>
        select_sop_columns() |>
        round_reals() |>
        tidytable::distinct()

      log_info(
        "Formatted %d unique structure-organism pairs",
        nrow(closed_prepared)
      )
      rm(closed)
    }
  } else {
    log_warn(
      "Closed resource not accessible at: %s. Returning empty template instead.",
      input
    )
    closed_prepared <- fake_sop_columns()
  }

  # Export parameters and results
  export_params(
    parameters = get_params(step = "prepare_libraries_sop_closed"),
    step = "prepare_libraries_sop_closed"
  )
  export_output(x = closed_prepared, file = output)

  rm(closed_prepared)
  output
}
