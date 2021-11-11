source(file = "R/log_debug.R")
source(file = "R/preclean_gnverifier.R")

#' Title
#'
#' @return
#' @export
#'
#' @examples
clean_gnverifier <- function() {
  dataOrganismVerified <<-
    preclean_gnverifier(file = paths$data$interim$taxa$verified)

  warning <- dataOrganismVerified |>
    dplyr::filter(!is.na(organism)) |>
    dplyr::mutate(
      organismCleaned = ifelse(
        test = organismDbTaxo == "Open Tree of Life",
        yes = organismCleaned,
        no = NA
      )
    ) |>
    dplyr::distinct(organism, organismCleaned) |>
    dplyr::group_by(organism) |>
    dplyr::add_count() |>
    dplyr::ungroup() |>
    dplyr::filter(n == 1) |>
    dplyr::select(-n) |>
    dplyr::filter(is.na(organismCleaned))

  if (nrow(warning) != 0) {
    log_debug(
      "Warning:",
      warning$organism,
      "had no translation,",
      "trying with more flexible parameters"
    )

    organism_table_2_interim <- dataOrganismVerified |>
      dplyr::distinct(organism, organismCleaned) |>
      filter(organism != organismCleaned) |>
      dplyr::distinct(organism, organismCleaned)

    organism_table_2 <- organism_table_2_interim |>
      dplyr::distinct(organismCleaned)

    if (nrow(organism_table_2) != 0) {
      log_debug("Exporting organisms for GNVerifier resubmission")
      readr::write_delim(
        x = organism_table_2,
        file = paths$data$interim$taxa$original_2,
        quote = "none",
        delim = "\t"
      )

      log_debug("submitting to GNVerifier with more flexible parameters")
      if (.Platform$OS.type == "unix") {
        system(command = paste("bash", paths$inst$scripts$gnverifier))
      } else {
        shell(paste("bash", paths$inst$scripts$gnverifier))
      }

      dataOrganismVerified_2 <<-
        preclean_gnverifier(file = paths$data$interim$taxa$verified_2) |>
        dplyr::select(-organism) |>
        dplyr::left_join(organism_table_2_interim)

      log_debug("Joining both results")
      dataOrganismVerified_3 <-
        rbind(dataOrganismVerified, dataOrganismVerified_2) |>
        dplyr::filter(organismDbTaxo == "Open Tree of Life") |>
        dplyr::distinct()

      warning_2 <-
        dplyr::left_join(organism_table, dataOrganismVerified_3) %>%
        dplyr::filter(!is.na(organism)) %>%
        dplyr::filter(is.na(organismDbTaxo))

      if (nrow(warning_2) != 0) {
        log_debug(
          "Warning:",
          warning_2$organism,
          "had no translation,check for names at",
          "https://tree.opentreeoflife.org/"
        )
      } else {
        log_debug("Good news, all your organisms were found!")
      }
    } else {
      dataOrganismVerified_3 <- dataOrganismVerified |>
        dplyr::filter(organismDbTaxo == "Open Tree of Life") |>
        dplyr::distinct()
      log_debug("We will not find more!")
    }
  } else {
    dataOrganismVerified_3 <- dataOrganismVerified |>
      dplyr::filter(organismDbTaxo == "Open Tree of Life") |>
      dplyr::distinct()
    log_debug("Good news, all your organisms were found!")
  }
  return(dataOrganismVerified_3)
}
