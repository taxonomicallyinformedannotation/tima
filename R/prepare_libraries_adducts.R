utils::globalVariables(c(
  "adduct",
  "exact_mass",
  "params",
  "paths",
  "structure_exact_mass"
))

#' @title Prepare libraries of adducts
#'
#' @description This function prepares adducts for further use
#'
#' @include create_adducts_neg.R
#' @include create_adducts_pos.R
#' @include export_output.R
#' @include export_params.R
#'
#' @param str_met File containing structures metadata
#' @param adducts_masses Table of adducts taken as input
#' @param adducts_output_path Path where the adducts will be saved
#' @param output_name Name of the file where adducts will be saved
#' @param masses_pos_output_path Path where pos adducts masses will be saved
#' @param masses_neg_output_path Path where neg adducts masses will be saved
#' @param parameters Parameters
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
prepare_libraries_adducts <-
  function(str_met = params$files$libraries$sop$merged$structures$metadata,
           adducts_masses = system.file("extdata",
             "adducts.tsv",
             package = "timaR"
           ),
           adducts_output_path = paths$data$interim$libraries$adducts$path,
           output_name = params$files$libraries$adducts$prepared,
           masses_pos_output_path = params$files$libraries$adducts$pos,
           masses_neg_output_path = params$files$libraries$adducts$neg,
           parameters = params) {
    stopifnot(
      "Your structure metadata file does not exist" =
        file.exists(str_met)
    )
    params <<- parameters
    log_debug("Loading files ...")
    log_debug("... exact masses")
    masses <- tidytable::fread(
      file = str_met,
      select = "structure_exact_mass",
      na.strings = c("", "NA"),
      colClasses = "numeric"
    ) |>
      tidytable::select(exact_mass = structure_exact_mass) |>
      tidytable::distinct()

    log_debug("... adducts")
    adducts_t <-
      tidytable::fread(
        file = adducts_masses,
        na.strings = c("", "NA"),
        colClasses = "character"
      ) |>
      tidytable::mutate(adduct = stringi::stri_replace_all_regex(
        str = adduct,
        pattern = ".* \\(",
        replacement = ""
      )) |>
      tidytable::mutate(adduct = stringi::stri_replace_all_regex(
        str = adduct,
        pattern = "\\)",
        replacement = ""
      ))

    log_debug("Treating adducts table")
    adducts_table <- t(adducts_t) |>
      data.frame() |>
      tidytable::as_tidytable()

    colnames(adducts_table) <- adducts_table[1, ] |> as.character()

    adducts_table <- adducts_table[2, ] |>
      tidytable::mutate(
        tidytable::across(
          .cols = tidytable::where(is.character),
          .fns = as.numeric
        )
      )

    masses_table <- cbind(masses, adducts_table, row.names = NULL)

    log_debug("Adding adducts to exact masses ...")
    log_debug("... positive")
    adducts_pos <-
      create_adducts_pos()

    log_debug("... negative")
    adducts_neg <-
      create_adducts_neg()

    log_debug("... pure adducts masses ...")
    mass_null <-
      cbind(data.frame(exact_mass = 0), adducts_table)

    log_debug("... positive")
    pure_pos <- mass_null |>
      create_adducts_pos() |>
      tidytable::filter(grepl(
        pattern = "]1+",
        x = adduct,
        fixed = TRUE
      )) |>
      tidytable::select(-exact_mass)

    log_debug("... negative")
    pure_neg <- mass_null |>
      create_adducts_neg() |>
      tidytable::filter(grepl(
        pattern = "]1-",
        x = adduct,
        fixed = TRUE
      )) |>
      tidytable::select(-exact_mass)

    log_debug("Exporting ...")
    export_params(step = "prepare_libraries_adducts")
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
