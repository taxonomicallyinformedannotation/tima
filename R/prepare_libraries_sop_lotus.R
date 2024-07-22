import::from(stringi, stri_sub, .into = environment())
import::from(tidytable, distinct, .into = environment())
import::from(tidytable, fread, .into = environment())
import::from(tidytable, mutate, .into = environment())
import::from(tidytable, rename, .into = environment())

#' @title Prepare libraries of structure organism pairs LOTUS
#'
#' @description This function prepares the LOTUS structure-organism pairs
#'
#' @importFrom stringi stri_sub
#' @importFrom tidytable distinct
#' @importFrom tidytable fread
#' @importFrom tidytable mutate
#' @importFrom tidytable rename
#'
#' @include fake_sop_columns.R
#' @include get_params.R
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
prepare_libraries_sop_lotus <-
  function(input = get_params(step = "prepare_libraries_sop_lotus")$files$libraries$sop$raw$lotus,
           output = get_params(step = "prepare_libraries_sop_lotus")$files$libraries$sop$prepared$lotus) {
    if (file.exists(input)) {
      log_debug(x = "Loading and preparing LOTUS")
      lotus_prepared <- input |>
        fread(na.strings = c("", "NA"), colClasses = "character") |>
        mutate(structure_inchikey_2D = stri_sub(str = structure_inchikey, from = 1, to = 14)) |>
        rename(structure_name = structure_nameTraditional) |>
        select_sop_columns() |>
        round_reals() |>
        distinct()
    } else {
      log_debug("Sorry, LOTUS not found, returning an empty file instead")
      lotus_prepared <- fake_sop_columns()
    }

    log_debug(x = "Exporting ...")
    export_output(x = lotus_prepared, file = output)
    rm(lotus_prepared)
    return(output)
  }
