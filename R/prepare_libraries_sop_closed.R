#' @title Prepare libraries of structure organism pairs CLOSED
#'
#' @include round_reals.R
#'
#' @param input Input file
#' @param output Output file
#' @param parameters params
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
prepare_libraries_sop_closed <-
  function(input = params$files$libraries$sop$raw$closed,
           output = params$files$libraries$sop$prepared,
           parameters = params) {
    params <- parameters
    if (file.exists(input)) {
      log_debug(x = "Loading closed resources")
      closed <- input |>
        tidytable::fread(
          na.strings = c("", "NA"),
          colClasses = "character"
        )

      log_debug(x = "Formatting closed resource")
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
    } else {
      log_debug("Sorry, you do not have access to the closed resource,
                returning an empty file instead")
      closed_prepared <- fake_sop_columns()
    }

    log_debug(x = "Exporting ...")
    export_params(step = "prepare_libraries_sop_closed")
    export_output(x = closed_prepared, file = output)
    return(output)
  }
