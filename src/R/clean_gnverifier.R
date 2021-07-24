source(file = "R/preclean_gnverifier.R")
source(file = "R/warning_gnverifier.R")

#' Title
#'
#' @return
#' @export
#'
#' @examples
clean_gnverifier <- function() {
  dataOrganismVerified <<-
    preclean_gnverifier(file = paths$data$interim$taxa$verified)
  
  warning <- warning_gnverifier(dataframe = dataOrganismVerified)
  if (nrow(warning) != 0) {
    log_debug(
      "Warning:",
      warning$organism,
      "had no translation,",
      "trying with a more flexible option"
    )
    
    organism_table_2 <- dataOrganismVerified |>
      dplyr::distinct(organism, organismCleaned)
    
    organism_table_3 <- organism_table_2 |>
      dplyr::distinct(organismCleaned)
    
    readr::write_delim(
      x = organism_table_3,
      file = paths$data$interim$taxa$original_2,
      quote = "none"
    )
    
    log_debug("submitting to GNVerifier")
    system(command = paste("bash", paths$src$gnverifier_2))
    
    log_debug("cleaning GNVerifier results")
    dataOrganismVerified_2 <-
      preclean_gnverifier(file = paths$data$interim$taxa$verified_2)
    
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
    } else{
      log_debug("Good news, all your organisms were found!")
    }
  } else {
    dataOrganismVerified_3 <- dataOrganismVerified
    log_debug("Good news, all your organisms were found!")
  }
  return(dataOrganismVerified_3)
}