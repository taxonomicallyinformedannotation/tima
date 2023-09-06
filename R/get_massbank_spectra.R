#' @title Get MassBank spectra
#'
#' @description This function gets MassBank spectra
#'
#' @param output_dir Output where to store the spectra
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
get_massbank_spectra <-
  function(output_dir = "data/source/libraries/spectra/exp/") {
    log_debug("Loading last AnnotationHub version ...")
    ah <- AnnotationHub::AnnotationHub()

    log_debug("Querying for most recent MassBank version available ...")
    ahmb <- AnnotationHub::query(ah, "MassBank")
    mb_last <- ahmb$ah_id |> tail(1)

    export <- file.path(output_dir, paste0(mb_last, ".mgf"))

    log_debug("Checking if a previous MassBank version already exists")
    if (!file.exists(export)) {
      log_debug("Downloading most recent MassBank version available ...")
      mb_sp <- AnnotationHub::AnnotationHub()[[mb_last]] |>
        Spectra::Spectra()
      log_debug("Removing faulty columns")
      mb_sp_2 <- mb_sp |>
        Spectra::selectSpectraVariables(
          Spectra::spectraVariables(mb_sp)[!grepl(
            pattern = "synonym",
            x = Spectra::spectraVariables(mb_sp)
          )]
        ) |>
        log_pipe("Exporting") |>
        Spectra::export(
          backend = MsBackendMgf::MsBackendMgf(),
          file = export
        )
    } else {
      log_debug(
        "It appears you already have",
        "the most recent MassBank version available!"
      )
    }
    return(export)
  }
