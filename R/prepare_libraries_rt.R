utils::globalVariables(
  c(
    "inchikey.x",
    "inchikey.y",
    "type"
  )
)

#' @title Prepare libraries of retention times
#'
#' @description This function prepares retention times libraries
#'    to be used for later
#'
#' @param mgf_exp MGF containing experimental retention times
#' @param mgf_is MGF containing in silico predicted retention times
#' @param temp_exp File containing experimental retention times
#' @param temp_is File containing in silico predicted retention times
#' @param output Output file
#' @param library Library containing the keys
#' @param col_ik Name of the InChIKey in mgf
#' @param col_rt Name of the retention time in mgf
#' @param col_sm Name of the SMILES in mgf
#' @param name_inchikey Name of the InChIKey in file
#' @param name_rt Name of the retention time in file
#' @param name_smiles Name of the SMILES in file
#' @param unit_rt Unit of the retention time. Must be "seconds" or "minutes"
#' @param parameters Params
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
prepare_libraries_rt <-
  function(mgf_exp = params$files$libraries$spectral$exp,
           mgf_is = params$files$libraries$spectral$is,
           temp_exp = params$files$libraries$temporal$exp,
           temp_is = params$files$libraries$temporal$is,
           output = params$files$libraries$temporal$prepared,
           library = params$files$libraries$sop$merged$keys,
           col_ik = params$names$mgf$inchikey,
           col_rt = params$names$mgf$retention_time,
           col_sm = params$names$mgf$smiles,
           name_inchikey = params$names$inchikey,
           name_rt = params$names$rt,
           name_smiles = params$names$smiles,
           unit_rt = params$units$rt,
           parameters = params) {
    stopifnot("Your library file does not exist." = file.exists(library))

    params <<- parameters

    ## default transforms from `Spectra`
    if (col_rt == "RTINSECONDS") {
      col_rt <- "rtime"
    }
    if (col_sm == "SMILES") {
      col_sm <- "smiles"
    }

    ## TODO improve
    if (length(mgf_exp$neg) == 0) {
      mgf_exp$neg <- NULL
    }
    if (length(mgf_exp$pos) == 0) {
      mgf_exp$pos <- NULL
    }
    if (length(mgf_exp) == 0) {
      mgf_exp <- NULL
    }
    if (length(mgf_is$neg) == 0) {
      mgf_is$neg <- NULL
    }
    if (length(mgf_is$pos) == 0) {
      mgf_is$pos <- NULL
    }
    if (length(mgf_is) == 0) {
      mgf_is <- NULL
    }
    if (length(temp_exp) == 0) {
      temp_exp <- NULL
    }
    if (length(temp_is) == 0) {
      temp_is <- NULL
    }

    rts_from_mgf <-
      function(mgf) {
        log_debug("Importing spectra ...")
        spectra <- mgf |>
          lapply(FUN = import_spectra)
        log_debug("Extracting retention times...")
        ## TODO refactor to avoid pos neg
        rts <-
          tidytable::bind_rows(
            spectra$neg@backend@spectraData |>
              data.frame() |>
              tidytable::tidytable(),
            spectra$pos@backend@spectraData |>
              data.frame() |>
              tidytable::tidytable()
          ) |>
          tidytable::select(tidytable::any_of(c(
            rt = col_rt,
            inchikey = col_ik,
            smiles = col_sm
          )))
        return(rts)
      }
    rts_from_tab <-
      function(tab) {
        log_debug("Importing file ...")
        rts <- tab |>
          lapply(FUN = tidytable::fread) |>
          tidytable::bind_rows() |>
          tidytable::select(tidytable::any_of(
            c(
              rt = name_rt,
              inchikey = name_inchikey,
              smiles = name_smiles
            )
          ))
        return(rts)
      }
    polish_df <-
      function(df,
               type = "experimental",
               unit = unit_rt) {
        df_polished <- df |>
          data.frame() |>
          dplyr::mutate(type = type) |>
          dplyr::rowwise() |>
          dplyr::mutate(rt = ifelse(unit == "seconds",
            yes = rt / 60,
            no = rt
          )) |>
          dplyr::bind_rows(data.frame(
            inchikey = NA_character_,
            smiles = NA_character_
          )) |>
          dplyr::filter(!is.na(as.numeric(rt))) |>
          dplyr::distinct() |>
          tidytable::tidytable()
        return(df_polished)
      }
    complete_df <- function(df, library = keys) {
      log_debug(
        "There are",
        nrow(df |>
          dplyr::filter(is.na(inchikey))),
        "entries without InChIKey.",
        "We would recommend you adding them but will try completing."
      )
      log_debug("Completing with existing metadata...")
      df_completed_smiles <- df |>
        tidytable::inner_join(library, by = c("smiles" = "smiles"))
      df_empty_smiles <- df |>
        tidytable::anti_join(library, by = c("smiles" = "smiles"))
      log_debug(
        "There are still",
        nrow(df_empty_smiles),
        "entries without InChIKey.",
        "We will query them on the fly, this might take some time."
      )
      ## TODO change with a small dependency
      smiles <- unique(df_empty_smiles$smiles)
      get_inchikey <- function(smiles, toolkit = "rdkit") {
        url <- paste0(
          "https://api.naturalproducts.net/latest/convert/inchikey?smiles=",
          utils::URLencode(smiles),
          "&toolkit=",
          toolkit
        )
        tryCatch(
          expr = jsonlite::fromJSON(txt = url),
          error = function(e) {
            return(NA_character_)
          }
        )
      }
      inchikey <- pbapply::pblapply(
        X = smiles,
        FUN = get_inchikey
      ) |>
        as.character()
      df_empty_smiles <- df_empty_smiles |>
        tidytable::select(-inchikey) |>
        tidytable::left_join(tidytable::tidytable(
          smiles,
          inchikey
        ))

      df_completed <- df_completed_smiles |>
        tidytable::bind_rows(df_empty_smiles) |>
        tidyft::mutate_vars(
          is.character,
          .func = function(x) {
            tidytable::na_if(x, "NA")
          }
        )
      df_completed <- df_completed |>
        data.frame() |>
        dplyr::mutate(
          structure_smiles = smiles,
          structure_inchikey = tidytable::coalesce(
            inchikey,
            inchikey.x,
            inchikey.y
          ),
        ) |>
        tidytable::tidytable() |>
        tidytable::select(
          rt,
          structure_smiles,
          structure_inchikey,
          type
        ) |>
        tidytable::distinct()
      log_debug(
        "There were still",
        nrow(df_completed |>
          dplyr::filter(is.na(
            structure_inchikey
          ))),
        "entries for which no InChIKey could not be found in the end."
      )
      return(df_completed)
    }

    keys <- library |>
      tidytable::fread(select = c(
        "structure_inchikey",
        "structure_smiles"
      )) |>
      tidytable::distinct() |>
      tidytable::select(
        inchikey = structure_inchikey,
        smiles = structure_smiles
      )

    empty_df <- tidytable::tidytable(
      rt = NA_real_,
      structure_smiles = NA_character_,
      structure_inchikey = NA_character_,
      type = NA_character_
    )

    ## from mgf
    if (!is.null(mgf_exp)) {
      rts_exp_1 <- mgf_exp |>
        rts_from_mgf() |>
        polish_df() |>
        complete_df()
    } else {
      rts_exp_1 <- empty_df
    }
    if (!is.null(mgf_is)) {
      rts_is_1 <- mgf_is |>
        rts_from_mgf() |>
        polish_df(type = "predicted") |>
        complete_df()
    } else {
      rts_is_1 <- empty_df
    }

    ## from csv
    if (!is.null(temp_exp)) {
      rts_exp_2 <- temp_exp |>
        rts_from_tab() |>
        polish_df() |>
        complete_df()
    } else {
      rts_exp_2 <- empty_df
    }
    if (!is.null(temp_is)) {
      rts_is_2 <- temp_is |>
        rts_from_tab() |>
        polish_df(type = "predicted") |>
        complete_df()
    } else {
      rts_is_2 <- empty_df
    }

    rts <- dplyr::bind_rows(
      rts_exp_1,
      rts_exp_2,
      rts_is_1,
      rts_is_2
    ) |>
      dplyr::filter(!is.na(as.numeric(rt))) |>
      dplyr::filter(!is.na((structure_inchikey))) |>
      dplyr::select(-structure_smiles) |>
      dplyr::distinct() |>
      dplyr::mutate(structure_inchikey_2D = gsub(
        pattern = "-.*",
        replacement = "",
        x = structure_inchikey
      )) |>
      ## TODO REMINDER FOR NOW
      dplyr::select(-structure_inchikey)

    if (nrow(rts) == 0) {
      log_debug("No retention time library found, returning an empty table.")
      rts <- tidytable::tidytable(
        rt = NA_real_,
        structure_inchikey_2D = NA_character_,
        type = NA_character_
      )
    }
    export_params(step = "prepare_libraries_rt")
    export_output(x = rts, file = output)
    return(output)
  }
