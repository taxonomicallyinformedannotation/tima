#' @title Prepare libraries of structure organism pairs CLOSED
#'
#' @importFrom stringi stri_sub
#' @importFrom tidytable distinct fread mutate rename
#'
#' @include fake_sop_columns.R
#' @include round_reals.R
#' @include select_sop_columns.R
#'
#' @param input Input file
#' @param output Output file
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
prepare_libraries_sop_closed <-
  function(input = get_params(step = "prepare_libraries_sop_closed")$files$libraries$sop$raw$closed,
           output = get_params(step = "prepare_libraries_sop_closed")$files$libraries$sop$prepared$closed) {
    if (file.exists(input)) {
      log_debug(x = "Loading closed resources")
      closed <- input |>
        fread(
          na.strings = c("", "NA"),
          colClasses = "character"
        )

      log_debug(x = "Formatting closed resource")
      closed_prepared <- closed |>
        mutate(
          structure_inchikey_2D = stri_sub(
            str = structure_inchikey,
            from = 1,
            to = 14
          )
        ) |>
        rename(structure_name = structure_nameTraditional) |>
        mutate(reference_doi = NA) |>
        select_sop_columns() |>
        round_reals() |>
        distinct()
      rm(closed)
    } else {
      log_debug("Sorry, you do not have access to the closed resource,
                returning an empty file instead")
      closed_prepared <- fake_sop_columns()
    }

    log_debug(x = "Exporting ...")
    export_params(parameters = get_params(step = "prepare_libraries_sop_closed"), step = "prepare_libraries_sop_closed")
    export_output(x = closed_prepared, file = output)
    rm(closed_prepared)
    return(output)
  }
