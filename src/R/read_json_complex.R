require(jsonlite)
require(splitstackshape)
require(dplyr)

#' Title
#'
#' @param json
#'
#' @return
#' @export
#'
#' @examples
read_json_complex <- function(json) {
  cat("reading json file \n")
  taxedFeaturesTable <- jsonlite::stream_in(con = gzfile(json))

  cat("keeping unique kingdom/kingdoms per feature \n")
  kingdom <- taxedFeaturesTable %>%
    splitstackshape::cSplit("sample_organism_1kingdom", ", ", direction = "long") |>
    dplyr::group_by(!!as.name(feature_id_colname)) |>
    dplyr::summarise_at("sample_organism_1kingdom", function(x) {
      x <- list(paste(sort(unique(x))))
    })

  cat("keeping unique phylum/phyla per feature \n")
  phylum <- taxedFeaturesTable %>%
    splitstackshape::cSplit("sample_organism_2phylum", ", ", direction = "long") |>
    dplyr::group_by(!!as.name(feature_id_colname)) |>
    dplyr::summarise_at("sample_organism_2phylum", function(x) {
      x <- list(paste(sort(unique(x))))
    })

  cat("keeping unique class/classes per feature \n")
  class <- taxedFeaturesTable |>
    splitstackshape::cSplit("sample_organism_3class", ", ", direction = "long") |>
    dplyr::group_by(!!as.name(feature_id_colname)) |>
    dplyr::summarise_at("sample_organism_3class", function(x) {
      x <- list(paste(sort(unique(x))))
    })

  cat("keeping unique order/orders per feature \n")
  order <- taxedFeaturesTable |>
    splitstackshape::cSplit("sample_organism_4order", ", ", direction = "long") |>
    dplyr::group_by(!!as.name(feature_id_colname)) |>
    dplyr::summarise_at("sample_organism_4order", function(x) {
      x <- list(paste(sort(unique(x))))
    })

  cat("keeping unique family/families per feature \n")
  family <- taxedFeaturesTable |>
    splitstackshape::cSplit("sample_organism_5family", ", ", direction = "long") |>
    dplyr::group_by(!!as.name(feature_id_colname)) |>
    dplyr::summarise_at("sample_organism_5family", function(x) {
      x <- list(paste(sort(unique(x))))
    })

  cat("keeping unique genus/genera per feature \n")
  genus <- taxedFeaturesTable |>
    splitstackshape::cSplit("sample_organism_6genus", ", ", direction = "long") |>
    dplyr::group_by(!!as.name(feature_id_colname)) |>
    dplyr::summarise_at("sample_organism_6genus", function(x) {
      x <- list(paste(sort(unique(x))))
    })

  cat("keeping unique species per feature \n")
  species <- taxedFeaturesTable |>
    splitstackshape::cSplit("sample_organism_7species", ", ", direction = "long") |>
    dplyr::group_by(!!as.name(feature_id_colname)) |>
    dplyr::summarise_at("sample_organism_7species", function(x) {
      x <- list(paste(sort(unique(x))))
    })

  cat("keeping unique variety/varieties per feature \n")
  variety <- taxedFeaturesTable |>
    splitstackshape::cSplit("sample_organism_8variety", ", ", direction = "long") |>
    dplyr::group_by(!!as.name(feature_id_colname)) |>
    dplyr::summarise_at("sample_organism_8variety", function(x) {
      x <- list(paste(sort(unique(x))))
    })

  table <- taxedFeaturesTable %>%
    dplyr::select(dplyr::all_of(feature_id_colname))

  cat("joining all together \n")
  table <- dplyr::left_join(table, kingdom)
  table <- dplyr::left_join(table, phylum)
  table <- dplyr::left_join(table, class)
  table <- dplyr::left_join(table, order)
  table <- dplyr::left_join(table, family)
  table <- dplyr::left_join(table, genus)
  table <- dplyr::left_join(table, species)
  table <- dplyr::left_join(table, variety) |>
    dplyr::arrange(dplyr::across(dplyr::all_of((
      feature_id_colname
    ))))

  return(table)
}
