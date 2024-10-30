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
#' tima:::copy_backbone()
#' go_to_cache()
#' prepare_libraries_sop_closed()
#' unlink("data", recursive = TRUE)
#' }
prepare_libraries_sop_closed <-
  function(input = get_params(step = "prepare_libraries_sop_closed")$files$libraries$sop$raw$closed,
           output = get_params(step = "prepare_libraries_sop_closed")$files$libraries$sop$prepared$closed) {
    if (file.exists(input)) {
      log_debug(x = "Loading closed resources")
      closed <- input |>
        tidytable::fread(na.strings = c("", "NA"), colClasses = "character")

      log_debug(x = "Formatting closed resource")
      closed_prepared <- closed |>
        tidytable::mutate(structure_inchikey_2D = stringi::stri_sub(str = structure_inchikey, from = 1, to = 14)) |>
        tidytable::rename(structure_name = structure_nameTraditional) |>
        tidytable::mutate(reference_doi = NA) |>
        tima:::select_sop_columns() |>
        tima:::round_reals() |>
        tidytable::distinct()
      rm(closed)
    } else {
      log_debug("Sorry, you do not have access to the closed resource,
                returning an empty file instead")
      closed_prepared <- fake_sop_columns()
    }

    tima:::export_params(
      parameters = get_params(step = "prepare_libraries_sop_closed"),
      step = "prepare_libraries_sop_closed"
    )
    tima:::export_output(x = closed_prepared, file = output)
    rm(closed_prepared)
    return(output)
  }
