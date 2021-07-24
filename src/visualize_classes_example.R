start <- Sys.time()

source(file = "R/helpers.R")
source(file = "R/colors.R")
source(file = "R/prepare-hierarchy.R")
source(file = "R/prepare-hierarchy_2.R")
source(file = "R/plot_histograms.R")
source(file = "R/prepare_plot.R")

log_debug(x = "loading libraries")
library(crayon)
library(docopt)
library(dplyr)
library(plotly)
library(readr)
library(splitstackshape)

## new
library(forcats)
library(microshades)
library(ggplot2)
library(ggpubr)

## beautiful lib
## remotes::install_github("KarstensLab/microshades")
library(microshades)
## docopt to do

weighted_ms1_path <-
  "../data/processed/210724_164728/FinalResults.tsv.gz"

weighted_path <-
  "../data/processed/210724_164510/FinalResults.tsv.gz"

log_debug("This script performs",
          green("extract annotations visualization"))
log_debug("Authors: \n", green("AR"))
log_debug("Contributors: ...")

log_debug(x = "loading ...")
log_debug(x = "... parameters")
params <- list()
params$top_k$candidates$initial <- 1
params$job$gnps <- "b988e66d1542430cbc6e703be781e49c"
inchikey_colname <- "inchikey_2D"
score_input_colname <- "score_final"
clean_xanthones <- TRUE

log_debug(x = "... functions")
source(file = "R/get_gnps.R")

log_debug(x = "... files ...")
log_debug(x = "... weighted + ms1 ISDB")
ms1 <-
  readr::read_delim(file = weighted_ms1_path) |>
  dplyr::mutate(dplyr::across(feature_id, as.numeric))

no_ms1 <-
  readr::read_delim(file = weighted_path) |>
  dplyr::mutate(dplyr::across(feature_id, as.numeric))

log_debug(x = "... metadata_table_biological_annotation")
log_debug(x = "loading feature table")

feature_table <- read_featuretable(id = params$job$gnps)

ms1 <-
  right_join(ms1,
             feature_table |> distinct(`row ID`),
             by = c("feature_id" = "row ID"))

no_ms1 <-
  right_join(no_ms1,
             feature_table |> distinct(`row ID`),
             by = c("feature_id" = "row ID"))
log_debug(x = "loading metadata table")
metadata_table <- read_metadatatable(id = params$job$gnps)

log_debug(x = "removing \" Peak area\" from column names")
colnames(feature_table) <-
  gsub(
    pattern = " Peak area",
    replacement = "",
    x = colnames(feature_table)
  )

log_debug(x = "removing \"row m/z\" and from \"row retention time\" columns")
feature_table <- feature_table %>%
  dplyr::select(-"row m/z",
                -"row retention time") |>
  tibble::column_to_rownames(var = "row ID")

top_n <- feature_table |>
  tibble::rownames_to_column() |>
  tidyr::gather(column, value, -rowname) |>
  dplyr::mutate(column = gsub(
    pattern = "^X",
    replacement = "",
    x = column
  )) |>
  dplyr::arrange(rowname, desc(value))

top_m <- dplyr::left_join(
  top_n |>
    dplyr::mutate(column = gsub(
      pattern = " Peak area",
      replacement = "",
      x = column
    )),
  metadata_table |>
    mutate(
      filename = gsub(
        pattern = ".mzML",
        replacement = "",
        x = filename,
        fixed = TRUE
      )
    ),
  by = c("column" = "filename")
) |>
  dplyr::select(
    feature_id = rowname,
    sample = column,
    intensity = value,
    species = ATTRIBUTE_species
  )

final_table <- prepare_hierarchy(dataframe = ms1)

final_table_taxed <-
  dplyr::left_join(final_table,
                   metadata_table |> mutate(
                     filename = gsub(
                       pattern = ".mzML",
                       replacement = "",
                       x = filename,
                       fixed = TRUE
                     )
                   ),
                   by = c("sample" = "filename"))

final_table_no_ms1 <- prepare_hierarchy(dataframe = no_ms1)

final_table_taxed_no_ms1 <-
  dplyr::left_join(
    final_table_no_ms1,
    metadata_table |> mutate(
      filename = gsub(
        pattern = ".mzML",
        replacement = "",
        x = filename,
        fixed = TRUE
      )
    ),
    by = c("sample" = "filename")
  )

nice_colors <- rev(
  list(
    microshades_palette("micro_cvd_green", lightest = FALSE),
    microshades_palette("micro_cvd_orange", lightest = FALSE),
    microshades_palette("micro_cvd_blue", lightest = FALSE),
    microshades_palette("micro_cvd_turquoise", lightest = FALSE),
    microshades_palette("micro_cvd_purple", lightest = FALSE),
    microshades_palette("micro_cvd_gray", lightest = FALSE),
    microshades_palette("micro_brown", lightest = FALSE),
    microshades_palette("micro_purple", lightest = FALSE)
  )
)

sunburst_colors <- character()

for (i in seq_along(1:length(nice_colors))) {
  sunburst_colors[i] <- rev(nice_colors)[[i]][5]
}

samples <-
  prepare_plot(dataframe = final_table_taxed, organism = "ATTRIBUTE_species")

samples_no_ms1 <-
  prepare_plot(dataframe = final_table_taxed_no_ms1, organism = "ATTRIBUTE_species")

absolute <-
  plot_histograms(dataframe = samples, label = "weighting with MS1 completion")

absolute_no_ms1 <-
  plot_histograms(dataframe = samples_no_ms1, label = "weighting only")

ggpubr::ggarrange(absolute_no_ms1, absolute)

## specific sample exploration
sunbursts <- plotly::plot_ly() |>
  plotly::add_trace(
    data = final_table_taxed_no_ms1 |>
      dplyr::filter(ATTRIBUTE_species == "Salvia officinalis"),
    ids = ~ ids,
    name = "no_ms1",
    labels = ~ labels,
    parents = ~ parents,
    values = ~ values,
    maxdepth = 3,
    type = "sunburst",
    branchvalues = "total",
    domain = list(column = 0)
  ) |>
  plotly::add_trace(
    data = final_table_taxed |>
      dplyr::filter(ATTRIBUTE_species == "Salvia officinalis"),
    ids = ~ ids,
    name = "with_ms1",
    labels = ~ labels,
    parents = ~ parents,
    values = ~ values,
    maxdepth = 3,
    type = "sunburst",
    branchvalues = "total",
    domain = list(column = 1)
  ) |>
  plotly::layout(colorway = sunburst_colors,
                 grid = list(columns = 2, # 3
                             rows = 1))

sunbursts

final_table_terpenoids <-
  prepare_hierarchy_2(dataframe = ms1,
                      pathway = "Terpenoids")

final_table_terpenoids_taxed <-
  dplyr::left_join(
    final_table_terpenoids,
    metadata_table |> mutate(
      filename = gsub(
        pattern = ".mzML",
        replacement = "",
        x = filename,
        fixed = TRUE
      )
    ),
    by = c("sample" = "filename")
  )

samples_terpenoids <-
  prepare_plot(dataframe = final_table_terpenoids_taxed, organism = "ATTRIBUTE_species")

absolute_terpenoids <-
  plot_histograms(dataframe = samples_terpenoids, label = "Terpenoids only")

end <- Sys.time()

log_debug("Script finished in", green(format(end - start)))
