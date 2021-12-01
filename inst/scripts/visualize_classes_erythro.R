start <- Sys.time()

require(package = "timaR",
        quietly = TRUE)

log_debug("Loading packages")
if (!require(crayon)) {
  install.packages("crayon")
  require(
    package = "crayon",
    quietly = TRUE
  )
}
if (!require(docopt)) {
  install.packages("docopt")
  require(
    package = "docopt",
    quietly = TRUE
  )
}
if (!require(dplyr)) {
  install.packages("dplyr")
  require(
    package = "dplyr",
    quietly = TRUE,
    warn.conflicts = FALSE
  )
}
if (!require(forcats)) {
  install.packages("forcats")
  require(
    package = "forcats",
    quietly = TRUE
  )
}
if (!require(ggplot2)) {
  install.packages("ggplot2")
  require(
    package = "ggplot2",
    quietly = TRUE
  )
}
if (!require(ggpubr)) {
  install.packages("ggpubr")
  require(
    package = "ggpubr",
    quietly = TRUE
  )
}
if (!require(remotes)) {
  install.packages("remotes")
  require(
    package = "remotes",
    quietly = TRUE
  )
}
if (!require(microshades)) {
  remotes::install_github("KarstensLab/microshades")
  require(
    package = "microshades",
    quietly = TRUE
  )
}
if (!require(plotly)) {
  install.packages("plotly")
  require(
    package = "plotly",
    quietly = TRUE
  )
}
if (!require(readr)) {
  install.packages("readr")
  require(
    package = "readr",
    quietly = TRUE
  )
}
if (!require(splitstackshape)) {
  install.packages("splitstackshape")
  require(
    package = "splitstackshape",
    quietly = TRUE
  )
}

## TODO docopt

feature_table_path <-
  "data/interim/60b9945aa3fd4810b178e79870cca905_isdb.tsv.gz"

weighted_ms1_path <-
  "data/processed/210718_145306/yourFinalFile.tsv.gz"

log_debug(
  "This script performs",
  green("extract annotations visualization")
)
log_debug("Authors: \n", green("AR"))
log_debug("Contributors: ...")

log_debug(x = "loading ...")
log_debug(x = "... parameters")
params <- list()
params$top_k$candidates$initial <- 1
params$job$gnps <- "60b9945aa3fd4810b178e79870cca905"
inchikey_colname <- "inchikey_2D"
score_input_colname <- "score_final"
clean_xanthones <- TRUE

log_debug(x = "... files ...")
log_debug(x = "... weighted + ms1 ISDB")
ms1 <-
  data.table::fread(
    file = weighted_ms1_path,
    sep = "\t"
  ) |>
  dplyr::mutate(dplyr::across(feature_id, as.numeric))

log_debug(x = "... metadata_table_biological_annotation")
log_debug(x = "loading feature table")

feature_table <- read_features(id = params$job$gnps)

log_debug(x = "loading metadata table")
metadata_table <- read_metadata(id = params$job$gnps)

log_debug(x = "removing \" Peak area\" from column names")
colnames(feature_table) <-
  gsub(
    pattern = ".Peak.area",
    replacement = "",
    x = colnames(feature_table)
  )

log_debug(x = "removing \"row m/z\" and from \"row retention time\" columns")
feature_table <- feature_table %>%
  dplyr::select(
    -"row m/z",
    -"row retention time"
  ) |>
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

top_m <- dplyr::left_join(top_n |>
  dplyr::mutate(column = gsub(
    pattern = ".Peak.area",
    replacement = "",
    x = column
  )),
metadata_table,
by = c("column" = "filename")
) |>
  dplyr::select(
    feature_id = rowname,
    sample = column,
    intensity = value,
    species
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

for (i in seq_len(length(nice_colors))) {
  sunburst_colors[i] <- rev(nice_colors)[[i]][5]
}

samples <- prepare_plot(dataframe = final_table_taxed)

absolute <-
  plot_histograms(dataframe = samples, label = "absolute")

relative <-
  plot_histograms(
    dataframe = samples,
    y = "relative",
    label = "relative"
  )

ggpubr::ggarrange(absolute, relative)

## specific sample exploration
plotly::plot_ly(
  data = final_table_taxed |>
    dplyr::filter(species == "Erythroxylum coca") |>
    dplyr::filter(sample == "210218_PMA_SC_E09.mzXML"),
  ids = ~ids,
  labels = ~labels,
  parents = ~parents,
  values = ~values,
  maxdepth = 3,
  type = "sunburst",
  branchvalues = "total"
) |>
  plotly::layout(colorway = sunburst_colors)

final_table_alkaloids <-
  prepare_hierarchy_2(
    dataframe = ms1,
    pathway = "Alkaloids"
  )

samples_alkaloids <-
  prepare_plot(dataframe = final_table_alkaloids)

absolute_alkaloids <-
  plot_histograms(dataframe = samples_alkaloids, label = "Alkaloids only")

relative_alkaloids <-
  plot_histograms(
    dataframe = samples_alkaloids,
    y = "relative",
    label = "Alkaloids relative"
  )

end <- Sys.time()

log_debug("Script finished in", green(format(end - start)))
