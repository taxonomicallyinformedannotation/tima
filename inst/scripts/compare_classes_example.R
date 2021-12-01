start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

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

weighted_R_path <-
  "data/processed/210730_143021/FinalResults.tsv.gz"

weighted_mandelbrot_path <-
  "~/Downloads/QC_mix_spectral_match_results_repond_flat.tsv"

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
params$job$gnps <- "db1c51fa29a64892af520698a18783e4"
inchikey_colname <- "inchikey_2D"
score_input_colname <- "score_final"
clean_xanthones <- TRUE

log_debug(x = "... functions")

log_debug(x = "... files ...")
log_debug(x = "... weighted + ms1 ISDB")
tima <-
  readr::read_delim(file = weighted_R_path) |>
  dplyr::mutate(dplyr::across(feature_id, as.numeric))

mandle <-
  readr::read_delim(file = weighted_mandelbrot_path) |>
  dplyr::mutate(dplyr::across(feature_id, as.numeric))


## dirty TODO Mandle not same columns
structure_organism_pairs_table <-
  readr::read_delim(
    file = "data/interim/libraries/library.tsv.gz",
    col_types = "c"
  ) |>
  dplyr::filter(!is.na(structure_exact_mass)) |>
  dplyr::mutate(dplyr::across(structure_exact_mass, as.numeric)) |>
  dplyr::mutate_if(is.logical, as.character) |>
  dplyr::distinct(
    inchikey_2D = structure_inchikey_2D,
    best_candidate_1 = structure_taxonomy_npclassifier_01pathway,
    best_candidate_2 = structure_taxonomy_npclassifier_02superclass,
    best_candidate_3 = structure_taxonomy_npclassifier_03class
  ) |>
  dplyr::mutate(best_candidate = paste(best_candidate_1,
    best_candidate_2,
    best_candidate_3,
    sep = "§"
  )) |>
  dplyr::distinct(inchikey_2D, best_candidate)

structure_organism_pairs_table[is.na(structure_organism_pairs_table)] <-
  "notClassified"

mandle <-
  left_join(mandle,
    structure_organism_pairs_table,
    by = c("short_inchikey" = "inchikey_2D")
  ) |>
  dplyr::mutate(
    inchikey_2D = short_inchikey,
    smiles_2D = structure_smiles,
    score_biological = score_taxo,
    score_chemical = score_max_consistency,
    score_final = final_score
  )

log_debug(x = "... metadata_table_biological_annotation")
log_debug(x = "loading feature table")

feature_table <- read_features(id = params$job$gnps)

tima <-
  right_join(tima,
    feature_table |> distinct(`row ID`),
    by = c("feature_id" = "row ID")
  )

mandle <-
  right_join(mandle,
    feature_table |> distinct(`row ID`),
    by = c("feature_id" = "row ID")
  )

log_debug(x = "loading metadata table")
metadata_table <- read_metadata(id = params$job$gnps)

log_debug(x = "removing \" Peak area\" from column names")
colnames(feature_table) <-
  gsub(
    pattern = " Peak area",
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

top_m <- dplyr::left_join(
  top_n |>
    dplyr::mutate(column = gsub(
      pattern = " Peak area",
      replacement = "",
      x = column
    )),
  metadata_table |>
    dplyr::mutate(
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

final_table <- prepare_hierarchy(dataframe = tima)

final_table_taxed <-
  dplyr::left_join(final_table,
    metadata_table |>
      dplyr::mutate(
        filename = gsub(
          pattern = ".mzML",
          replacement = "",
          x = filename,
          fixed = TRUE
        )
      ),
    by = c("sample" = "filename")
  )

final_table_mandel <- prepare_hierarchy(dataframe = mandle)

final_table_mandel <-
  dplyr::left_join(
    final_table_mandel,
    metadata_table |>
      dplyr::mutate(
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

samples <-
  prepare_plot(dataframe = final_table_taxed, organism = "ATTRIBUTE_species")

samples_mandel <-
  prepare_plot(dataframe = final_table_mandel, organism = "ATTRIBUTE_species")

absolute <-
  plot_histograms(dataframe = samples, label = "tima version")

absolute_mandel <-
  plot_histograms(dataframe = samples_mandel, label = "mandel version")

ggpubr::ggarrange(absolute_mandel,
  absolute,
  legend = "right",
  common.legend = TRUE
)

## specific sample exploration
sunbursts <- plotly::plot_ly() |>
  plotly::add_trace(
    data = final_table_mandel |>
      dplyr::filter(ATTRIBUTE_species == "Salvia officinalis"),
    ids = ~ids,
    name = "no_ms1",
    labels = ~labels,
    parents = ~parents,
    values = ~values,
    maxdepth = 3,
    type = "sunburst",
    branchvalues = "total",
    domain = list(column = 0)
  ) |>
  plotly::add_trace(
    data = final_table_taxed |>
      dplyr::filter(ATTRIBUTE_species == "Salvia officinalis"),
    ids = ~ids,
    name = "with_ms1",
    labels = ~labels,
    parents = ~parents,
    values = ~values,
    maxdepth = 3,
    type = "sunburst",
    branchvalues = "total",
    domain = list(column = 1)
  ) |>
  plotly::layout(
    colorway = sunburst_colors,
    grid = list(
      columns = 2, # 3
      rows = 1
    )
  )

sunbursts

final_table_terpenoids <-
  prepare_hierarchy_2(
    dataframe = tima,
    pathway = "Terpenoids"
  )

final_table_terpenoids_mandle <-
  prepare_hierarchy_2(
    dataframe = mandle,
    pathway = "Terpenoids"
  )

final_table_terpenoids_taxed <-
  dplyr::left_join(
    final_table_terpenoids,
    metadata_table |>
      dplyr::mutate(
        filename = gsub(
          pattern = ".mzML",
          replacement = "",
          x = filename,
          fixed = TRUE
        )
      ),
    by = c("sample" = "filename")
  )

final_table_terpenoids_taxed_mandle <-
  dplyr::left_join(
    final_table_terpenoids_mandle,
    metadata_table |>
      dplyr::mutate(
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
  prepare_plot(
    dataframe = final_table_terpenoids_taxed,
    organism = "ATTRIBUTE_species"
  )

samples_terpenoids_mandle <-
  prepare_plot(
    dataframe = final_table_terpenoids_taxed_mandle,
    organism = "ATTRIBUTE_species"
  )

absolute_terpenoids <-
  plot_histograms(dataframe = samples_terpenoids, label = "Terpenoids only - TIMA")

absolute_terpenoids_mandle <-
  plot_histograms(dataframe = samples_terpenoids_mandle, label = "Terpenoids only - MANDLE")

ggpubr::ggarrange(absolute_terpenoids_mandle,
  absolute_terpenoids,
  legend = "right"
)

end <- Sys.time()

log_debug("Script finished in", green(format(end - start)))
