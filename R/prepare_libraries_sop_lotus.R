#' @title Prepare libraries of structure organism pairs LOTUS
#'
#' @description This function prepares the LOTUS structure-organism pairs
#'
#' @include round_reals.R
#'
#' @param input Input file
#' @param output Output file
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
prepare_libraries_sop_lotus <-
  function(input = get_params(step = "prepare_libraries_sop_lotus")$files$libraries$sop$raw$lotus,
           output = get_params(step = "prepare_libraries_sop_lotus")$files$libraries$sop$prepared) {
    if (file.exists(input)) {
      log_debug(x = "Loading and preparing LOTUS")
      lotus_prepared <- input |>
        tidytable::fread(
          na.strings = c("", "NA"),
          colClasses = "character"
        ) |>
        tidytable::mutate(
          structure_inchikey_2D = stringi::stri_sub(
            str = structure_inchikey,
            from = 1,
            to = 14
          )
        ) |>
        tidytable::rename(structure_name = structure_nameTraditional) |>
        select_sop_columns() |>
        round_reals() |>
        tidytable::distinct()
    } else {
      log_debug("Sorry, LOTUS not found, returning an empty file instead")
      lotus_prepared <- fake_sop_columns()
    }

    log_debug(x = "Exporting ...")
    export_output(x = lotus_prepared, file = output)
    rm(lotus_prepared)
    return(output)
  }
