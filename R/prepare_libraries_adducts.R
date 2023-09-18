#' @title Prepare libraries of adducts
#'
#' @description This function prepares adducts for further use
#'
#' @include create_adducts.R
#'
#' @param str_met File containing structures metadata
#' @param adducts_masses_list Table of adducts taken as input
#' @param adducts_output_path Path where the adducts will be saved
#' @param clusters_list Table of clusters taken as input
#' @param output_name Name of the file where adducts will be saved
#' @param masses_pos_output_path Path where pos adducts masses will be saved
#' @param masses_neg_output_path Path where neg adducts masses will be saved
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
prepare_libraries_adducts <-
  function(str_met = get_params(step = "prepare_libraries_adducts")$files$libraries$sop$merged$structures$metadata,
           adducts_masses_list = system.file("extdata",
             "adducts.tsv",
             package = "timaR"
           ),
           adducts_output_path = parse_yaml_paths()$data$interim$libraries$adducts$path,
           clusters_list = system.file("extdata",
             "clusters.tsv",
             package = "timaR"
           ),
           output_name = get_params(step = "prepare_libraries_adducts")$files$libraries$adducts$prepared,
           masses_pos_output_path = get_params(step = "prepare_libraries_adducts")$files$libraries$adducts$pos,
           masses_neg_output_path = get_params(step = "prepare_libraries_adducts")$files$libraries$adducts$neg) {
    stopifnot(
      "Your structure metadata file does not exist" =
        file.exists(str_met)
    )
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
        file = adducts_masses_list,
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

    log_debug("... clusters")
    clusters <-
      tidytable::fread(
        file = clusters_list,
        na.strings = c("", "NA"),
        colClasses = "character"
      ) |>
      tidytable::mutate(cluster = stringi::stri_replace_all_regex(
        str = cluster,
        pattern = ".* \\(",
        replacement = ""
      )) |>
      tidytable::mutate(cluster = stringi::stri_replace_all_regex(
        str = cluster,
        pattern = "\\)",
        replacement = ""
      )) |>
      tidytable::mutate(
        tidytable::across(
          .cols = c("mass"),
          .fns = as.numeric
        )
      )

    add_clu_t <- adducts_t |>
      tidytable::bind_rows(clusters |> tidytable::rename(adduct = cluster))
    rm(adducts_t, clusters)

    log_debug("Treating adducts table")
    adducts_table <- t(add_clu_t) |>
      data.frame() |>
      tidytable::as_tidytable()
    rm(add_clu_t)

    colnames(adducts_table) <- adducts_table[1, ] |> as.character()

    adducts_table <- adducts_table[2, ] |>
      tidytable::mutate(
        tidytable::across(
          .cols = tidytable::where(is.character),
          .fns = as.numeric
        )
      )

    masses_table <- cbind(masses, adducts_table, row.names = NULL)
    rm(masses)

    log_debug("Adding adducts to exact masses ...")
    log_debug("... positive")
    adducts_pos <-
      create_adducts(ms_mode = "pos")

    log_debug("... negative")
    adducts_neg <-
      create_adducts(ms_mode = "neg")

    log_debug("... pure adducts masses ...")
    mass_null <-
      cbind(data.frame(exact_mass = 0), adducts_table)

    log_debug("... positive")
    pure_pos <- mass_null |>
      create_adducts(ms_mode = "pos") |>
      tidytable::filter(grepl(pattern = "[1M", x = adduct, fixed = TRUE)) |>
      tidytable::filter(grepl(
        pattern = "]1+",
        x = adduct,
        fixed = TRUE
      )) |>
      ## do not take clusters
      tidytable::filter(!grepl(
        pattern = " + ",
        x = adduct,
        fixed = TRUE
      )) |>
      tidytable::select(-exact_mass)

    log_debug("... negative")
    pure_neg <- mass_null |>
      create_adducts(ms_mode = "neg") |>
      tidytable::filter(grepl(pattern = "[1M", x = adduct, fixed = TRUE)) |>
      tidytable::filter(grepl(
        pattern = "]1-",
        x = adduct,
        fixed = TRUE
      )) |>
      ## do not take clusters
      tidytable::filter(!grepl(
        pattern = " + ",
        x = adduct,
        fixed = TRUE
      )) |>
      tidytable::select(-exact_mass)
    rm(adducts_table, masses_table)

    log_debug("Exporting ...")
    export_params(parameters = get_params(step = "prepare_libraries_adducts"), step = "prepare_libraries_adducts")
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
    rm(adducts_pos, adducts_neg, pure_pos, pure_neg)

    return(c("pos" = output_pos, "neg" = output_neg))
  }
