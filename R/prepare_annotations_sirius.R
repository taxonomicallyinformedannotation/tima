#' @title Prepare annotations SIRIUS
#'
#' @description This function prepares Sirius results to make them compatible
#'
#' @include export_output.R
#' @include export_params.R
#' @include harmonize_names_sirius.R
#' @include pre_harmonize_names_sirius.R
#' @include select_annotations_columns.R
#' @include select_sirius_columns.R
#'
#' @param input_directory Directory containing the Sirius results
#' @param output Output where to save prepared results
#' @param str_stereo File containing structures stereo
#' @param str_met File containing structures metadata
#' @param str_nam File containing structures names
#' @param str_tax_cla File containing Classyfire taxonomy
#' @param str_tax_npc File containing NPClassifier taxonomy
#' @param parameters Params
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
prepare_annotations_sirius <-
  function(input_directory = params$files$annotations$raw$sirius,
           output = params$files$annotations$prepared,
           str_stereo = params$files$libraries$sop$merged$structures$stereo,
           str_met = params$files$libraries$sop$merged$structures$metadata,
           str_nam = params$files$libraries$sop$merged$structures$names,
           str_tax_cla =
             params$files$libraries$sop$merged$structures$taxonomies$cla,
           str_tax_npc =
             params$files$libraries$sop$merged$structures$taxonomies$npc,
           parameters = params) {
    params <<- parameters
    if (file.exists(input_directory)) {
      stopifnot("Your npc summary file must be named
                'canopus_compound_summary.tsv" = file.exists(
        file.path(
          input_directory,
          "canopus_compound_summary.tsv"
        )
      ))
      stopifnot(
        "You must generate the summaries before submission. \n
              Please do
        `sirius -i $WORKSPACE write-summaries -o $WORKSPACE_SUMMARIES.zip
        -c --digits 3" = length(
          list.files(
            path = input_directory,
            pattern = "structure_candidates.tsv",
            recursive = TRUE
          )
        ) != 0
      )
      log_debug("Loading and formatting SIRIUS results")
      canopus <-
        tidytable::fread(
          file = file.path(
            input_directory,
            "canopus_compound_summary.tsv"
          ),
          na.strings = c("", "NA"),
          colClasses = "character"
        )

      formula <-
        tidytable::fread(
          file = file.path(
            input_directory,
            "formula_identifications.tsv"
          ),
          na.strings = c("", "NA"),
          colClasses = "character"
        )

      formula_adducts <-
        tidytable::fread(
          file = file.path(
            input_directory,
            "formula_identifications_adducts.tsv"
          ),
          na.strings = c("", "NA"),
          colClasses = "character"
        )

      # compound <-
      #   tidytable::fread(file = file.path(
      #     input_directory,
      #     "compound_identifications.tsv"
      #   ),
      #         na.strings = c("","NA")),
      #     colClasses = "character"

      compound_adducts <-
        tidytable::fread(
          file = file.path(
            input_directory,
            "compound_identifications_adducts.tsv"
          ),
          na.strings = c("", "NA"),
          colClasses = "character"
        )

      compound_summary <- lapply(
        X = list.files(
          path = input_directory,
          pattern = "structure_candidates.tsv",
          full.names = TRUE,
          recursive = TRUE
        ),
        FUN = tidytable::fread,
        na.strings = c("", "NA"),
        colClasses = "character"
      )

      names(compound_summary) <- list.files(
        path = input_directory,
        pattern = "structure_candidates.tsv",
        recursive = TRUE
      ) |>
        pre_harmonize_names_sirius() |>
        harmonize_names_sirius()

      compound_summary <-
        compound_summary[lapply(compound_summary, nrow) > 0]

      compound_summary_ready <- compound_summary |>
        tidytable::bind_rows(.id = "feature_id")

      canopus_npc_prepared <- canopus |>
        tidytable::mutate(feature_id = harmonize_names_sirius(id)) |>
        tidytable::select(tidytable::any_of(
          c(
            "feature_id",
            "structure_tax_npc_01pat" = "NPC#pathway",
            "NPC#pathway Probability",
            "structure_tax_npc_02sup" = "NPC#superclass",
            "NPC#superclass Probability",
            "structure_tax_npc_03cla" = "NPC#class",
            "NPC#class Probability",
            "structure_tax_cla_01kin" = "ClassyFire#TODO",
            "ClassyFire#TODO Probability",
            "structure_tax_cla_02sup" =
              "ClassyFire#superclass",
            "ClassyFire#superclass probability",
            "structure_tax_cla_03cla" = "ClassyFire#class",
            "ClassyFire#class Probability",
            "structure_tax_cla_04dirpar" =
              "ClassyFire#most specific class",
            "ClassyFire#most specific class Probability"
          )
        ))

      compound_prepared <- compound_summary_ready |>
        select_sirius_columns()

      compound_adducts_prepared <- compound_adducts |>
        tidytable::mutate(feature_id = harmonize_names_sirius(id)) |>
        select_sirius_columns()

      formula_prepared <- formula |>
        select_sirius_columns_2()

      formula_adducts_prepared <- formula_adducts |>
        select_sirius_columns_2()

      compounds_prepared <-
        tidytable::bind_rows(compound_prepared, compound_adducts_prepared) |>
        tidytable::distinct()

      formulas_prepared <-
        tidytable::bind_rows(formula_prepared, formula_adducts_prepared) |>
        tidytable::distinct()

      table <- compounds_prepared |>
        tidytable::left_join(formulas_prepared) |>
        tidytable::left_join(canopus_npc_prepared) |>
        tidytable::distinct() |>
        tidytable::mutate(
          structure_tax_cla_chemontid = NA,
          structure_tax_cla_01kin = NA,
          ## mirror spectral match
          count_peaks_matched = NA
        ) |>
        select_annotations_columns(
          str_stereo = str_stereo,
          str_met = str_met,
          str_nam = str_nam,
          str_tax_cla = str_tax_cla,
          str_tax_npc = str_tax_npc
        )
    } else {
      log_debug("Sorry, your input directory does not exist,
                returning an empty file instead")
      table <- fake_annotations_columns()
    }
    log_debug(x = "Exporting ...")
    export_params(step = "prepare_annotations_sirius")
    export_output(x = table, file = output[[1]])

    return(output[[1]])
  }
