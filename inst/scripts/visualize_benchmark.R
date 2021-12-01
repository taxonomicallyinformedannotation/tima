start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

## docopt to do

## dirty paths here for now

raw_path <-
  "data/interim/benchmark/benchmark_isdb_result.tsv.gz"

raw_ms1_path <-
  "data/processed/211102_154605/FinalResults.tsv"

weighted_ms1_chemo_path <-
  "data/processed/211102_155241/FinalResults.tsv"

weigthed_chemo_path <-
  "data/processed/211102_155807/FinalResults.tsv"

weighted_ms1_path <-
  "data/processed/211102_160624/FinalResults.tsv"

weighted_path <-
  "data/processed/211102_161255/FinalResults.tsv"

solutions_path <-
  "data/interim/benchmark_solutions.tsv.gz"

roc_path <- "man/figures/benchmark.svg"

log_debug(
  "This script performs",
  crayon::green("result analysis")
)
log_debug("Authors: \n", green("AR"))
log_debug("Contributors: ...")

log_debug(x = "loading ...")
log_debug(x = "... parameters")
params <- list()
params$top_k$initial <- 500
params$library <- "data/interim/libraries/library.tsv.gz"

log_debug(x = "... files ...")
log_debug(x = "... LOTUS")
structure_organism_pairs_table <-
  readr::read_delim(
    file = params$library,
    col_types = "c"
  ) |>
  dplyr::filter(!is.na(structure_exact_mass)) |>
  dplyr::mutate(dplyr::across(structure_exact_mass, as.numeric)) |>
  dplyr::mutate_if(is.logical, as.character)

log_debug(x = "... raw ISDB")
raw <- readr::read_delim(file = raw_path) |>
  distinct(feature_id,
    inchikey_2D = short_inchikey,
    score = msms_score,
    matched_peaks
  )

log_debug(x = "... raw MS1")
raw_ms1 <- readr::read_delim(file = raw_ms1_path) |>
  distinct(feature_id,
    inchikey_2D,
    score = score_final
  )

log_debug(x = "... weighted ISDB")
wei <- readr::read_delim(file = weighted_path) |>
  distinct(feature_id,
    inchikey_2D,
    score = score_final
  )

log_debug(x = "... weighted + chemo new ISDB")
wei_che <- readr::read_delim(file = weigthed_chemo_path) |>
  distinct(feature_id,
    inchikey_2D,
    score = score_final
  )

log_debug(x = "... weighted + ms1 ISDB")
ms1 <- readr::read_delim(file = weighted_ms1_path) |>
  distinct(feature_id,
    inchikey_2D,
    score = score_final
  )

log_debug(x = "... weighted + ms1 + new chemo ISDB")
new <- readr::read_delim(file = weighted_ms1_chemo_path) |>
  distinct(feature_id,
    inchikey_2D,
    score = score_final
  )

log_debug(x = "... solutions")
solutions <-
  readr::read_delim(file = solutions_path)

raw_ranked <- raw |>
  splitstackshape::cSplit(c("inchikey_2D", "score"),
    sep = "|",
    direction = "long"
  ) |>
  dplyr::group_by(feature_id) |>
  dplyr::mutate(rank = dplyr::dense_rank(desc(score))) |>
  dplyr::distinct(feature_id,
    inchikey_2D, ## problematic for MS1 with same score at rank 1?
    rank,
    .keep_all = TRUE
  )

raw_ms1_ranked <- raw_ms1 |>
  splitstackshape::cSplit(c("inchikey_2D", "score"),
    sep = "|",
    direction = "long"
  ) |>
  dplyr::group_by(feature_id) |>
  dplyr::mutate(rank = dplyr::dense_rank(desc(score))) |>
  dplyr::distinct(feature_id,
    inchikey_2D, ## problematic for MS1 with same score at rank 1?
    rank,
    .keep_all = TRUE
  )

wei_ranked <- wei |>
  splitstackshape::cSplit(c("inchikey_2D", "score"),
    sep = "|",
    direction = "long"
  ) |>
  dplyr::group_by(feature_id) |>
  dplyr::mutate(rank = dplyr::dense_rank(desc(score))) |>
  dplyr::distinct(feature_id,
    inchikey_2D, ## problematic for MS1 with same score at rank 1?
    rank,
    .keep_all = TRUE
  )

wei_che_ranked <- wei_che |>
  splitstackshape::cSplit(c("inchikey_2D", "score"),
    sep = "|",
    direction = "long"
  ) |>
  dplyr::group_by(feature_id) |>
  dplyr::mutate(rank = dplyr::dense_rank(desc(score))) |>
  dplyr::distinct(feature_id,
    inchikey_2D, ## problematic for MS1 with same score at rank 1?
    rank,
    .keep_all = TRUE
  )

ms1_ranked <- ms1 |>
  splitstackshape::cSplit(c("inchikey_2D", "score"),
    sep = "|",
    direction = "long"
  ) |>
  dplyr::group_by(feature_id) |>
  dplyr::mutate(rank = dplyr::dense_rank(desc(score))) |>
  dplyr::distinct(feature_id,
    inchikey_2D, ## problematic for MS1 with same score at rank 1?
    rank,
    .keep_all = TRUE
  )

new_ranked <- new |>
  splitstackshape::cSplit(c("inchikey_2D", "score"),
    sep = "|",
    direction = "long"
  ) |>
  dplyr::group_by(feature_id) |>
  dplyr::mutate(rank = dplyr::dense_rank(desc(score))) |>
  dplyr::distinct(feature_id,
    inchikey_2D, ## problematic for MS1 with same score at rank 1?
    rank,
    score,
    .keep_all = TRUE
  )

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

raw_k <- top_k
for (i in seq_along(top_k)) {
  raw_k[[i]] <- nrow(subset(raw_results, rank <= i))
}

raw_ms1_k <- top_k
for (i in seq_along(top_k)) {
  raw_ms1_k[[i]] <- nrow(subset(raw_ms1_results, rank <= i))
}

wei_k <- top_k
for (i in seq_along(top_k)) {
  wei_k[[i]] <- nrow(subset(wei_results, rank <= i))
}

wei_che_k <- top_k
for (i in seq_along(top_k)) {
  wei_che_k[[i]] <- nrow(subset(wei_che_results, rank <= i))
}

ms1_k <- top_k
for (i in seq_along(top_k)) {
  ms1_k[[i]] <- nrow(subset(ms1_results, rank <= i))
}

new_k <- top_k
for (i in seq_along(top_k)) {
  new_k[[i]] <- nrow(subset(new_results, rank <= i))
}

ROC <-
  data.frame(raw_k, raw_ms1_k, wei_k, wei_che_k, ms1_k, new_k) |>
  dplyr::mutate(
    raw_k = raw_k / nrow(solutions) * 100,
    raw_ms1_k = raw_ms1_k / nrow(solutions) * 100,
    wei_k = wei_k / nrow(solutions) * 100,
    wei_che_k = wei_che_k / nrow(solutions) * 100,
    ms1_k = ms1_k / nrow(solutions) * 100,
    new_k = new_k / nrow(solutions) * 100
  )

ROC <- rbind(c(0, 0), ROC)
ROC$x <- 0:params$top_k$initial
ROC[1, 7] <- 1

roc <- plotly::plot_ly(ROC, x = ~x) |>
  plotly::add_trace(
    y = ~raw_k,
    name = "ms2",
    mode = "lines",
    line = list(
      shape = "hv",
      color = "#D71D62"
    ),
    marker = list(
      size = 0,
      opacity = 0
    )
  ) |>
  plotly::add_trace(
    y = ~raw_ms1_k,
    name = "ms1",
    mode = "lines",
    line = list(
      shape = "hv",
      color = "#861450"
    ),
    marker = list(
      size = 0,
      opacity = 0
    )
  ) |>
  plotly::add_trace(
    y = ~wei_k,
    name = "ms2 weighted",
    line = list(
      shape = "hv",
      color = "#2994D2"
    ),
    marker = list(
      size = 0,
      opacity = 0
    )
  ) |>
  plotly::add_trace(
    y = ~wei_k,
    name = "ms2 weighted + chemo",
    line = list(
      shape = "hv",
      color = "#08589B"
    ),
    marker = list(
      size = 0,
      opacity = 0
    )
  ) |>
  plotly::add_trace(
    y = ~ms1_k,
    name = "ms2 + ms1 weighted",
    line = list(
      shape = "hv",
      color = "#7CB13F"
    ),
    marker = list(
      size = 0,
      opacity = 0
    )
  ) |>
  plotly::add_trace(
    y = ~new_k,
    name = "ms2 + ms1 weighted + chemo",
    line = list(
      shape = "hv",
      color = "#336A2D"
    ),
    marker = list(
      size = 0,
      opacity = 0
    )
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

plotly::save_image(p = roc, file = roc_path)

diff <- new_ranked |>
  left_join(new_results |> mutate(correct = "CORRECT")) |>
  left_join(raw |> distinct(feature_id, inchikey_2D, matched_peaks)) |>
  mutate(correct = if_else(
    condition = is.na(correct),
    true = "INCORRECT",
    false = correct
  ))

diff_wrong <- diff |>
  filter(correct == "INCORRECT")
diff_correct <- diff |>
  filter(correct == "CORRECT")

plotly::plot_ly() |>
  plotly::add_trace(
    data = diff_wrong,
    name = "incorrect hits",
    x = ~rank,
    y = ~score,
    marker = list(
      size = 5,
      color = "#bdbdbd",
      opacity = 0.2
    )
  ) |>
  plotly::add_trace(
    data = diff_correct,
    name = "correct hits",
    x = ~rank,
    y = ~score,
    color = ~matched_peaks,
    marker = list(
      size = 5,
      line = list(
        color = "red",
        opacity = 0.5,
        width = 0.2
      )
    )
  ) |>
  plotly::layout(
    title = "correct hits repartition (left:ms2 / right: ms2 + ms1 + taxo)",
    xaxis = list(range = c(
      0,
      params$top_k$initial
    ))
  ) |>
  plotly::toWebGL()

diff <- new_ranked |>
  left_join(new_results |> mutate(correct = "CORRECT")) |>
  left_join(raw |> distinct(feature_id, inchikey_2D, matched_peaks)) |>
  mutate(correct = if_else(
    condition = is.na(correct),
    true = "INCORRECT",
    false = correct
  ))

diff_wrong <- diff |>
  filter(correct == "INCORRECT")
diff_correct <- diff |>
  filter(correct == "CORRECT")

taxo_plot <- plotly::plot_ly() |>
  plotly::add_trace(
    data = diff_wrong,
    name = "incorrect hits",
    x = ~rank,
    y = ~score,
    marker = list(
      size = 5,
      color = "#bdbdbd",
      opacity = 0.2
    )
  ) |>
  plotly::add_trace(
    data = diff_correct,
    name = "correct hits",
    x = ~rank,
    y = ~score,
    color = ~matched_peaks,
    marker = list(
      size = 5,
      line = list(
        color = "red",
        opacity = 0.5,
        width = 0.2
      )
    )
  ) |>
  plotly::layout(
    title = "correct hits repartition (left: ms2 only / right: ms2 + ms1 + taxo)",
    xaxis = list(range = c(
      0,
      params$top_k$initial
    ))
  ) |>
  plotly::toWebGL()

diff_ms2 <- raw_ranked |>
  left_join(raw_results |> mutate(correct = "CORRECT")) |>
  mutate(correct = if_else(
    condition = is.na(correct),
    true = "INCORRECT",
    false = correct
  ))

diff_wrong_ms2 <- diff_ms2 |>
  filter(correct == "INCORRECT")
diff_correct_ms2 <- diff_ms2 |>
  filter(correct == "CORRECT")

ms2_plot <- plotly::plot_ly() |>
  plotly::add_trace(
    data = diff_wrong_ms2,
    name = "incorrect hits",
    x = ~rank,
    y = ~score,
    marker = list(
      size = 5,
      color = "#bdbdbd",
      opacity = 0.2
    ),
    showlegend = FALSE
  ) |>
  plotly::add_trace(
    data = diff_correct_ms2,
    name = "correct hits",
    x = ~rank,
    y = ~score,
    color = ~matched_peaks,
    marker = list(
      size = 5,
      line = list(
        color = "red",
        opacity = 0.5,
        width = 0.2
      )
    ),
    showlegend = FALSE
  ) |>
  plotly::hide_colorbar() |>
  plotly::layout(
    title = "correct hits repartition (ms2 only)",
    xaxis = list(range = c(
      0,
      params$top_k$initial
    ))
  ) |>
  plotly::toWebGL()

plotly::subplot(ms2_plot, taxo_plot, shareX = TRUE, shareY = TRUE)

end <- Sys.time()

log_debug("Script finished in", green(format(end - start)))
