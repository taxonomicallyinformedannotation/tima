#' @title Prepare libraries of structure organism pairs CLOSED
#'
#' @include fake_sop_columns.R
#' @include get_params.R
#' @include round_reals.R
#' @include select_sop_columns.R
#'
#' @param input Input file
#' @param output Output file
#'
#' @return The path to the prepared structure-organism pairs library CLOSED
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
prepare_libraries_sop_closed <-
  function(
    input = get_params(
      step = "prepare_libraries_sop_closed"
    )$files$libraries$sop$raw$closed,
    output = get_params(
      step = "prepare_libraries_sop_closed"
    )$files$libraries$sop$prepared$closed
  ) {
    if (file.exists(input)) {
      logger::log_trace("Loading closed resources")
      closed <- input |>
        tidytable::fread(na.strings = c("", "NA"), colClasses = "character")

      logger::log_trace("Formatting closed resource")
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
      rm(closed)
    } else {
      logger::log_warn(
        "Sorry, you do not have access to the closed resource,
                returning an empty file instead"
      )
      closed_prepared <- fake_sop_columns()
    }

    export_params(
      parameters = get_params(step = "prepare_libraries_sop_closed"),
      step = "prepare_libraries_sop_closed"
    )
    export_output(x = closed_prepared, file = output)
    rm(closed_prepared)
    return(output)
  }
