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
#'
#' @param input Character string path to input closed library file
#' @param output Character string path where prepared library should be saved
#'
#' @return Character string path to the prepared structure-organism pairs library
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
  # Validate inputs
  if (!is.character(input) || length(input) != 1L) {
    stop("input must be a single character string")
  }

  if (!is.character(output) || length(output) != 1L) {
    stop("output must be a single character string")
  }

  log_info("Preparing closed structure-organism pairs library")

  if (file.exists(input)) {
    log_debug("Loading closed resource from: %s", input)

    closed <- tryCatch(
      {
        tidytable::fread(
          input,
          na.strings = c("", "NA"),
          colClasses = "character"
        )
      },
      error = function(e) {
        stop("Failed to read closed library file: ", conditionMessage(e))
      }
    )

    if (nrow(closed) == 0L) {
      log_warn("Closed library file is empty")
      closed_prepared <- fake_sop_columns()
    } else {
      # log_trace("Formatting closed resource (", nrow(closed), " rows)")

      closed_prepared <- closed |>
        tidytable::mutate(
          structure_inchikey_2D = stringi::stri_sub(
            str = structure_inchikey,
            from = 1,
            to = 14
          )
        ) |>
        tidytable::rename(structure_name = structure_nameTraditional) |>
        tidytable::mutate(reference_doi = NA) |>
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
  return(output)
}
