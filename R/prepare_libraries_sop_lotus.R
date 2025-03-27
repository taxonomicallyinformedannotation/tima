#' @title Prepare libraries of structure organism pairs LOTUS
#'
#' @description This function prepares the LOTUS structure-organism pairs
#'
#' @include fake_sop_columns.R
#' @include get_params.R
#' @include round_reals.R
#' @include select_sop_columns.R
#'
#' @param input Input file
#' @param output Output file
#'
#' @return The path to the prepared structure-organism pairs library LOTUS
#'
#' @export
#'
#' @examples
#' \dontrun{
#' copy_backbone()
#' go_to_cache()
#' prepare_libraries_sop_lotus()
#' unlink("data", recursive = TRUE)
#' }
prepare_libraries_sop_lotus <-
  function(
    input = get_params(
      step = "prepare_libraries_sop_lotus"
    )$files$libraries$sop$raw$lotus,
    output = get_params(
      step = "prepare_libraries_sop_lotus"
    )$files$libraries$sop$prepared$lotus
  ) {
    if (file.exists(input)) {
      log_debug(x = "Loading and preparing LOTUS")
      lotus_prepared <- input |>
        tidytable::fread(na.strings = c("", "NA"), colClasses = "character") |>
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
