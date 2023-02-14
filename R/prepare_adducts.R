#' @title Prepare adducts
#'
#' @description This function prepares adducts for further use
#'
#' @param adducts_input List of adducts taken as input
#' @param adducts_table_input Table of adducts taken as input
#' @param config_output_path Path where the configuration will be saved
#' @param adducts_output_path Path where the adducts will be saved
#' @param output_name Name of the file where adducts will be saved
#' @param masses_pos_output_path Path where positive adducts masses will be saved
#' @param masses_neg_output_path Path where negative adducts masses will be saved
#' @param parameters Parameters
#'
#' @return NULL
#'
#' @export
#'
#' @importFrom dplyr distinct filter mutate_all select
#' @importFrom readr read_delim write_delim
#' @importFrom stringr str_remove
#'
#' @examples NULL
prepare_adducts <-
  function(adducts_input = params$files$libraries$sop$merged,
           adducts_table_input = paths$data$source$adducts,
           config_output_path = paths$data$interim$config$path,
           adducts_output_path = paths$data$interim$adducts$path,
           output_name = params$files$libraries$adducts$processed,
           masses_pos_output_path = paths$data$interim$adducts$pos,
           masses_neg_output_path = paths$data$interim$adducts$neg,
           parameters = params) {
    stopifnot("Your input file does not exist" = file.exists(adducts_input))
    params <<- parameters
    log_debug("Loading files ...")
    log_debug("... exact masses")
    masses <- readr::read_delim(
      file = adducts_input,
      col_select = "structure_exact_mass"
    ) |>
      dplyr::select(exact_mass = structure_exact_mass) |>
      dplyr::distinct()

    log_debug("... adducts")
    adducts_table <-
      readr::read_delim(file = adducts_table_input)

    log_debug("Treating adducts table")
    adducts_t <- t(adducts_table) |>
      data.frame() |>
      dplyr::mutate_all(.funs = trimws) |>
      dplyr::mutate_all(
        .funs = function(x) {
          stringr::str_remove(
            string = x,
            pattern = ".* \\("
          )
        }
      ) |>
      dplyr::mutate_all(
        .funs = function(x) {
          stringr::str_remove(
            string = x,
            pattern = "\\)"
          )
        }
      )

    colnames(adducts_t) <- adducts_t[1, ]

    adducts_t <- adducts_t[2, ] |>
      dplyr::mutate_all(as.numeric)

    masses_adducts <- cbind(masses, adducts_t, row.names = NULL)

    log_debug("Adding adducts to exact masses ...")
    log_debug("... positive")
    adducts_pos <-
      form_adducts_pos(massesTable = masses_adducts, adductsTable = adducts_t)

    log_debug("... negative")
    adducts_neg <-
      form_adducts_neg(massesTable = masses_adducts, adductsTable = adducts_t)

    log_debug("... pure adducts masses ...")
    mass_null <-
      cbind(data.frame(exact_mass = 0), adducts_t)

    log_debug("... positive")
    pure_pos <-
      form_adducts_pos(massesTable = mass_null, adductsTable = adducts_t) |>
      dplyr::filter(grepl(
        pattern = "pos_1",
        x = adduct,
        fixed = TRUE
      )) |>
      dplyr::select(-exact_mass)

    log_debug("... negative")
    pure_neg <-
      form_adducts_neg(massesTable = mass_null, adductsTable = adducts_t) |>
      dplyr::filter(grepl(
        pattern = "neg_1",
        x = adduct,
        fixed = TRUE
      )) |>
      dplyr::select(-exact_mass)

    log_debug("Exporting ...")
    export_params(step = "prepare_adducts")
    output_pos <- file.path(
      adducts_output_path,
      paste0(output_name, "_pos.tsv.gz")
    )
    output_neg <- file.path(
      adducts_output_path,
      paste0(output_name, "_neg.tsv.gz")
    )
    export_output(
      x = adducts_pos,
      file = output_pos
    )
    export_output(
      x = adducts_neg,
      file = output_neg
    )
    export_output(x = pure_pos, file = masses_pos_output_path)
    export_output(x = pure_neg, file = masses_neg_output_path)

    return(c("pos" = output_pos, "neg" = output_neg))
  }
