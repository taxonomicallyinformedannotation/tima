start <- Sys.time()

source(file = "src/R/helpers.R")

log_debug(x = "loading libraries")
library(crayon)
library(data.table)
library(dplyr)
library(plotly)
library(readr)
library(splitstackshape)
library(tidyr)

## docopt to do

## dirty paths here for now

raw_path <-
  "data/interim/annotations/e3fbd467e21b4197859111e8e6751918_isdb_result.tsv.gz"

raw_ms1_path <-
  "data/processed/210812_135814/FinalResults.tsv.gz"

weighted_path <-
  "data/processed/210812_141528/FinalResults.tsv.gz"

weigthed_chemo_path <-
  "data/processed/210812_140739/FinalResults.tsv.gz"

weighted_ms1_path <-
  "data/processed/210812_141314/FinalResults.tsv.gz"

weighted_ms1_chemo_path <-
  "data/processed/210812_140330/FinalResults.tsv.gz"

solutions_path <-
  "data/interim/benchmark_solutions.tsv.gz"

roc_path <- "../images/benchmark.svg"

log_debug("This script performs",
          crayon::green("result analysis"))
log_debug("Authors: \n", green("AR"))
log_debug("Contributors: ...")

log_debug(x = "loading ...")
log_debug(x = "... parameters")
params <- list()
params$top_k$initial <- 500
params$library <- "data/interim/libraries/library.tsv.gz"

log_debug(x = "... functions")
source(file = "src/R/ms1.R")
source(file = "src/R/biological_weighting.R")
source(file = "src/R/chemical_weighting.R")
source(file = "src/R/cleaning.R")

log_debug(x = "... files ...")

log_debug(x = "... LOTUS")
structure_organism_pairs_table <-
  readr::read_delim(file = params$library,
                    col_types = "c") |>
  dplyr::filter(!is.na(structure_exact_mass)) |>
  dplyr::mutate(dplyr::across(structure_exact_mass, as.numeric)) |>
  dplyr::mutate_if(is.logical, as.character)

log_debug(x = "... raw ISDB")
raw <- readr::read_delim(file = raw_path) |>
  distinct(feature_id,
           inchikey_2D = short_inchikey,
           score = msms_score)

log_debug(x = "... raw MS1")
raw_ms1 <- readr::read_delim(file = raw_ms1_path) |>
  distinct(feature_id,
           inchikey_2D,
           score = score_final)

log_debug(x = "... weighted ISDB")
wei <- readr::read_delim(file = weighted_path) |>
  distinct(feature_id,
           inchikey_2D,
           score = score_final)

log_debug(x = "... weighted + chemo new ISDB")
wei_che <- readr::read_delim(file = weigthed_chemo_path) |>
  distinct(feature_id,
           inchikey_2D,
           score = score_final)

log_debug(x = "... weighted + ms1 ISDB")
ms1 <- readr::read_delim(file = weighted_ms1_path) |>
  distinct(feature_id,
           inchikey_2D,
           score = score_final)

log_debug(x = "... weighted + ms1 + new chemo ISDB")
new <- readr::read_delim(file = weighted_ms1_chemo_path) |>
  distinct(feature_id,
           inchikey_2D,
           score = score_final)

log_debug(x = "... solutions")
solutions <-
  readr::read_delim(file = solutions_path)

raw_ranked <- raw |>
  splitstackshape::cSplit(c("inchikey_2D", "score"),
                          sep = "|",
                          direction = "long") |>
  dplyr::group_by(feature_id) |>
  dplyr::mutate(rank = dplyr::dense_rank(desc(score))) |>
  dplyr::distinct(feature_id,
                  inchikey_2D, ## problematic for MS1 with same score at rank 1?
                  rank,
                  .keep_all = TRUE)

raw_ms1_ranked <- raw_ms1 |>
  splitstackshape::cSplit(c("inchikey_2D", "score"),
                          sep = "|",
                          direction = "long") |>
  dplyr::group_by(feature_id) |>
  dplyr::mutate(rank = dplyr::dense_rank(desc(score))) |>
  dplyr::distinct(feature_id,
                  inchikey_2D, ## problematic for MS1 with same score at rank 1?
                  rank,
                  .keep_all = TRUE)

wei_ranked <- wei |>
  splitstackshape::cSplit(c("inchikey_2D", "score"),
                          sep = "|",
                          direction = "long") |>
  dplyr::group_by(feature_id) |>
  dplyr::mutate(rank = dplyr::dense_rank(desc(score))) |>
  dplyr::distinct(feature_id,
                  inchikey_2D, ## problematic for MS1 with same score at rank 1?
                  rank,
                  .keep_all = TRUE)

wei_che_ranked <- wei_che |>
  splitstackshape::cSplit(c("inchikey_2D", "score"),
                          sep = "|",
                          direction = "long") |>
  dplyr::group_by(feature_id) |>
  dplyr::mutate(rank = dplyr::dense_rank(desc(score))) |>
  dplyr::distinct(feature_id,
                  inchikey_2D, ## problematic for MS1 with same score at rank 1?
                  rank,
                  .keep_all = TRUE)

ms1_ranked <- ms1 |>
  splitstackshape::cSplit(c("inchikey_2D", "score"),
                          sep = "|",
                          direction = "long") |>
  dplyr::group_by(feature_id) |>
  dplyr::mutate(rank = dplyr::dense_rank(desc(score))) |>
  dplyr::distinct(feature_id,
                  inchikey_2D, ## problematic for MS1 with same score at rank 1?
                  rank,
                  .keep_all = TRUE)

new_ranked <- new |>
  splitstackshape::cSplit(c("inchikey_2D", "score"),
                          sep = "|",
                          direction = "long") |>
  dplyr::group_by(feature_id) |>
  dplyr::mutate(rank = dplyr::dense_rank(desc(score))) |>
  dplyr::distinct(feature_id,
                  inchikey_2D, ## problematic for MS1 with same score at rank 1?
                  rank,
                  .keep_all = TRUE)

raw_results <- left_join(solutions, raw_ranked) |>
  dplyr::filter(!is.na(rank)) |>
  dplyr::distinct()

raw_ms1_results <- left_join(solutions, raw_ms1_ranked) |>
  dplyr::filter(!is.na(rank)) |>
  dplyr::distinct()

wei_results <- left_join(solutions, wei_ranked) |>
  dplyr::filter(!is.na(rank)) |>
  dplyr::distinct()

wei_che_results <- left_join(solutions, wei_che_ranked) |>
  dplyr::filter(!is.na(rank)) |>
  dplyr::distinct()

ms1_results <- left_join(solutions, ms1_ranked) |>
  dplyr::filter(!is.na(rank)) |>
  dplyr::distinct()

new_results <- left_join(solutions, new_ranked) |>
  dplyr::filter(!is.na(rank)) |>
  dplyr::distinct()

top_k <- seq_len(params$top_k$initial)

raw <- top_k
for (i in seq_along(top_k)) {
  raw[[i]] <- nrow(subset(raw_results, rank <= i))
}

raw_ms1 <- top_k
for (i in seq_along(top_k)) {
  raw_ms1[[i]] <- nrow(subset(raw_ms1_results, rank <= i))
}

wei <- top_k
for (i in seq_along(top_k)) {
  wei[[i]] <- nrow(subset(wei_results, rank <= i))
}

wei_che <- top_k
for (i in seq_along(top_k)) {
  wei_che[[i]] <- nrow(subset(wei_che_results, rank <= i))
}

ms1 <- top_k
for (i in seq_along(top_k)) {
  ms1[[i]] <- nrow(subset(ms1_results, rank <= i))
}

new <- top_k
for (i in seq_along(top_k)) {
  new[[i]] <- nrow(subset(new_results, rank <= i))
}

ROC <- data.frame(raw, raw_ms1, wei, wei_che, ms1, new) |>
  dplyr::mutate(
    raw = raw / nrow(solutions) * 100,
    raw_ms1 = raw_ms1 / nrow(solutions) * 100,
    wei = wei / nrow(solutions) * 100,
    wei_che = wei_che / nrow(solutions) * 100,
    ms1 = ms1 / nrow(solutions) * 100,
    new = new / nrow(solutions) * 100
  )

ROC <- rbind(c(0, 0), ROC)
ROC$x <- 0:params$top_k$initial
ROC[1, 7] <- 1

roc <- plotly::plot_ly(ROC, x = ~ x) |>
  plotly::add_trace(
    y = ~ raw,
    name = "ms2",
    mode = "lines",
    line = list(shape = "hv",
                color = "#D71D62"),
    marker = list(size = 0,
                  opacity = 0)
  ) |>
  plotly::add_trace(
    y = ~ raw_ms1,
    name = "ms1",
    mode = "lines",
    line = list(shape = "hv",
                color = "#861450"),
    marker = list(size = 0,
                  opacity = 0)
  ) |>
  plotly::add_trace(
    y = ~ wei,
    name = "ms2 weighted",
    line = list(shape = "hv",
                color = "#2994D2"),
    marker = list(size = 0,
                  opacity = 0)
  ) |>
  plotly::add_trace(
    y = ~ wei,
    name = "ms2 weighted + chemo",
    line = list(shape = "hv",
                color = "#08589B"),
    marker = list(size = 0,
                  opacity = 0)
  ) |>
  plotly::add_trace(
    y = ~ ms1,
    name = "ms2 + ms1 weighted",
    line = list(shape = "hv",
                color = "#7CB13F"),
    marker = list(size = 0,
                  opacity = 0)
  ) |>
  plotly::add_trace(
    y = ~ new,
    name = "ms2 + ms1 weighted + chemo",
    line = list(shape = "hv",
                color = "#336A2D"),
    marker = list(size = 0,
                  opacity = 0)
  ) |>
  plotly::layout(
    title = "On 22,388 features (some of them with wrong masses)",
    colorway = "#C0C0C0",
    barmode = "stack",
    xaxis = list(
      showgrid = FALSE,
      zeroline = FALSE,
      title = "Rank",
      range = c(0, params$top_k$initial),
      automargin = TRUE
    ),
    yaxis = list(
      showgrid = FALSE,
      range = c(0, 100),
      zeroline = FALSE,
      title = "Correct short IK annotations [%]",
      automargin = TRUE
    )
  )

roc

plotly::orca(p = roc, file = roc_path)

end <- Sys.time()

log_debug("Script finished in", green(format(end - start)))
