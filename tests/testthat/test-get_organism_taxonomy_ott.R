# Test Suite: get_organism_taxonomy_ott ----

library(testthat)

test_that("get_organism_taxonomy_ott validates df input", {
  expect_error(
    get_organism_taxonomy_ott(df = 1),
    "must be a data frame",
    class = "tima_validation_error"
  )
})

test_that("get_organism_taxonomy_ott requires organism column", {
  expect_error(
    get_organism_taxonomy_ott(df = data.frame(x = "Homo sapiens")),
    "must contain an 'organism' column",
    class = "tima_validation_error"
  )
})

test_that("get_organism_taxonomy_ott works with valid organism", {
  skip_if_no_network("https://api.opentreeoflife.org")
  fake_taxon_df <- data.frame("organism" = "Gentiana lutea")

  result <- get_organism_taxonomy_ott(df = fake_taxon_df)
  expect_s3_class(result, "data.frame")
})

test_that("get_organism_taxonomy_ott handles invalid organism", {
  skip_if_no_network("https://api.opentreeoflife.org")
  wrong_taxon_df <- data.frame("organism" = "Gentiano luteo")

  expect_warning(
    expect_warning(
      result <- get_organism_taxonomy_ott(df = wrong_taxon_df),
      "gentiano are not matched",
      fixed = TRUE
    ),
    "Gentiano luteo are not matched",
    fixed = TRUE
  )
  expect_s3_class(result, "data.frame")
})

test_that("get_organism_taxonomy_ott handles API failures", {
  fake_taxon_df <- data.frame("organism" = "Gentiana lutea")

  expect_warning(
    result <- get_organism_taxonomy_ott(
      df = fake_taxon_df,
      url = "https://api.opentreeoflife.org/v3/taxonomy/fakeDown"
    ),
    class = "tima_ott_unavailable_warning"
  )
  # Same shape as a successful result, so callers can bind it
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0L)
  expect_true(all(
    c(
      "organism_name",
      "organism_taxonomy_ottid",
      "organism_taxonomy_09species"
    ) %in%
      names(result)
  ))
})

## Offline fixtures ----
# Minimal stand-ins for the Open Tree of Life API, shaped like what
# rotl::tnrs_match_names() and rotl::taxonomy_taxon_info() return.

.ott_taxon <- function(ott_id, rank, name, unique_name = name) {
  list(
    ott_id = ott_id,
    rank = rank,
    name = name,
    unique_name = unique_name,
    flags = list(),
    synonyms = list()
  )
}

# Lineage from the direct parent up to the root, as the API orders it
.ott_lineages <- list(
  gentiana = list(
    .ott_taxon(5262598L, "tribe", "Gentianeae"),
    .ott_taxon(922405L, "family", "Gentianaceae"),
    .ott_taxon(853764L, "order", "Gentianales"),
    .ott_taxon(5316182L, "no rank", "asterids"),
    .ott_taxon(99252L, "class", "Magnoliopsida"),
    .ott_taxon(361838L, "phylum", "Streptophyta"),
    .ott_taxon(5342313L, "kingdom", "Chloroplastida"),
    .ott_taxon(304358L, "domain", "Eukaryota"),
    .ott_taxon(805080L, "no rank", "life")
  ),
  homo = list(
    .ott_taxon(770309L, "genus", "Homo"),
    .ott_taxon(770311L, "family", "Hominidae"),
    .ott_taxon(913935L, "order", "Primates"),
    .ott_taxon(244265L, "class", "Mammalia"),
    .ott_taxon(125642L, "phylum", "Chordata"),
    .ott_taxon(691846L, "kingdom", "Metazoa"),
    .ott_taxon(304358L, "domain", "Eukaryota"),
    .ott_taxon(805080L, "no rank", "life")
  )
)

.ott_taxa <- list(
  "576855" = c(
    .ott_taxon(576855L, "species", "Gentiana lutea"),
    list(
      lineage = c(
        list(.ott_taxon(922413L, "genus", "Gentiana")),
        .ott_lineages$gentiana
      )
    )
  ),
  "922413" = c(
    .ott_taxon(922413L, "genus", "Gentiana", "Gentiana (genus in Eudicots)"),
    list(lineage = .ott_lineages$gentiana)
  ),
  "251935" = c(
    .ott_taxon(251935L, "species", "Mentha x piperita"),
    list(
      lineage = list(
        .ott_taxon(1008451L, "genus", "Mentha"),
        .ott_taxon(23736L, "family", "Lamiaceae"),
        .ott_taxon(304358L, "domain", "Eukaryota")
      )
    )
  ),
  "770315" = c(
    .ott_taxon(770315L, "species", "Homo sapiens"),
    list(lineage = .ott_lineages$homo)
  )
)

# search_string (lower case) -> matched taxon; anything else is unmatched
.ott_names <- list(
  "gentiana lutea" = list(ott_id = 576855L, name = "Gentiana lutea"),
  # A synonym resolved to the accepted species
  "gentiana major" = list(
    ott_id = 576855L,
    name = "Gentiana lutea",
    synonym = TRUE
  ),
  "gentiana" = list(ott_id = 922413L, name = "Gentiana"),
  "homo sapiens" = list(ott_id = 770315L, name = "Homo sapiens"),
  "mentha piperita" = list(ott_id = 251935L, name = "Mentha x piperita")
)

.fake_tnrs_match_names <- function(names, ...) {
  rows <- lapply(tolower(names), function(nm) {
    hit <- .ott_names[[nm]]
    data.frame(
      search_string = nm,
      unique_name = if (is.null(hit)) NA_character_ else hit$name,
      approximate_match = if (is.null(hit)) NA else FALSE,
      ott_id = if (is.null(hit)) NA_integer_ else hit$ott_id,
      is_synonym = if (is.null(hit)) NA else isTRUE(hit$synonym),
      flags = if (is.null(hit)) NA_character_ else "",
      number_matches = if (is.null(hit)) NA_integer_ else 1L,
      stringsAsFactors = FALSE
    )
  })
  res <- do.call(rbind, rows)
  class(res) <- c("match_names", "data.frame")
  res
}

.fake_taxonomy_taxon_info <- function(ott_ids, ...) {
  res <- lapply(as.character(ott_ids), function(id) .ott_taxa[[id]])
  names(res) <- ott_ids
  class(res) <- "taxon_info"
  res
}

# Mocks the API (availability check, TNRS, taxon info) and records the calls
local_fake_ott_api <- function(.env = parent.frame()) {
  calls <- new.env(parent = emptyenv())
  calls$tnrs <- list()
  calls$taxon_info <- list()
  local_mocked_bindings(
    req_perform = function(req, ...) structure(list(), class = "mock_resp"),
    resp_status_desc = function(resp) "OK",
    .package = "httr2",
    .env = .env
  )
  local_mocked_bindings(
    tnrs_match_names = function(names, ...) {
      calls$tnrs <- c(calls$tnrs, list(list(names = names, args = list(...))))
      .fake_tnrs_match_names(names)
    },
    taxonomy_taxon_info = function(ott_ids, ...) {
      calls$taxon_info <- c(calls$taxon_info, list(ott_ids))
      .fake_taxonomy_taxon_info(ott_ids)
    },
    .package = "rotl",
    .env = .env
  )
  calls
}

## Helpers ----

test_that(".create_batches splits into fixed-size batches and drops NAs", {
  items <- c(paste0("sp", 1:4), NA, "sp6", "sp7")
  batches <- .create_batches(items, batch_size = 3L)

  expect_equal(
    batches,
    list(c("sp1", "sp2", "sp3"), c("sp4", "sp6"), "sp7")
  )
  expect_equal(.create_batches(character(), 100L), list())
})

test_that(".extract_genus_names keeps only the first word of each name", {
  expect_equal(
    .extract_genus_names(
      c("gentiana lutea", "mentha spicata subsp. crispa", "gentiana")
    ),
    c("gentiana", "mentha", "gentiana")
  )
})

test_that(".build_taxonomy_lineage lists each taxon root-first under its id", {
  info <- .fake_taxonomy_taxon_info(c(770315L))
  lineage <- rotl::tax_lineage(info)

  otl <- .build_taxonomy_lineage(info, lineage, ott_ids = 770315L)

  expect_equal(unique(otl$id), 770315L)
  expect_type(otl$ott_id, "integer")
  # Root first, the queried taxon itself last
  expect_equal(otl$name[[1L]], "life")
  expect_equal(otl$rank[[nrow(otl)]], "species")
  expect_equal(otl$ott_id[[nrow(otl)]], 770315L)
  expect_equal(
    otl$name[otl$rank != "no rank"],
    c(
      "Eukaryota",
      "Metazoa",
      "Chordata",
      "Mammalia",
      "Primates",
      "Hominidae",
      "Homo",
      "Homo sapiens"
    )
  )
})

test_that(".create_empty_taxonomy_template returns single NA rows", {
  template <- .create_empty_taxonomy_template()

  expect_equal(nrow(template$matches), 1L)
  expect_true(all(is.na(template$matches)))
  expect_equal(nrow(template$taxonomy), 1L)
  expect_true(all(is.na(template$taxonomy)))
})

## Success path (mocked API) ----

test_that("get_organism_taxonomy_ott maps each OTT rank to its column", {
  calls <- local_fake_ott_api()

  result <- get_organism_taxonomy_ott(
    df = data.frame(organism = "Gentiana lutea")
  )

  expect_equal(nrow(result), 1L)
  expect_equal(result$organism_name, "Gentiana lutea")
  expect_equal(result$organism_taxonomy_ottid, 576855L)
  expect_equal(result$organism_taxonomy_01domain, "Eukaryota")
  expect_equal(result$organism_taxonomy_02kingdom, "Chloroplastida")
  expect_equal(result$organism_taxonomy_03phylum, "Streptophyta")
  expect_equal(result$organism_taxonomy_04class, "Magnoliopsida")
  expect_equal(result$organism_taxonomy_05order, "Gentianales")
  expect_equal(result$organism_taxonomy_06family, "Gentianaceae")
  expect_equal(result$organism_taxonomy_07tribe, "Gentianeae")
  expect_equal(result$organism_taxonomy_08genus, "Gentiana")
  expect_equal(result$organism_taxonomy_09species, "Gentiana lutea")
  # No varietas in the lineage, and "no rank" nodes are not columns
  expect_true(is.na(result$organism_taxonomy_10varietas))
  expect_false(any(grepl("no rank", names(result), fixed = TRUE)))

  # Exact matching only, then one detail query for the matched id
  expect_length(calls$tnrs, 1L)
  expect_false(calls$tnrs[[1L]]$args$do_approximate_matching)
  expect_equal(calls$taxon_info, list(576855L))
})

test_that("get_organism_taxonomy_ott fills absent ranks with NA per organism", {
  local_fake_ott_api()

  result <- get_organism_taxonomy_ott(
    df = data.frame(organism = c("Homo sapiens", "Gentiana lutea"))
  ) |>
    tidytable::arrange(organism_name)

  expect_equal(result$organism_name, c("Gentiana lutea", "Homo sapiens"))
  expect_equal(result$organism_taxonomy_ottid, c(576855L, 770315L))
  expect_equal(
    result$organism_taxonomy_02kingdom,
    c("Chloroplastida", "Metazoa")
  )
  expect_equal(
    result$organism_taxonomy_06family,
    c("Gentianaceae", "Hominidae")
  )
  # Homo has no tribe in its lineage
  expect_equal(result$organism_taxonomy_07tribe, c("Gentianeae", NA))
  expect_equal(result$organism_taxonomy_08genus, c("Gentiana", "Homo"))
})

test_that("get_organism_taxonomy_ott cleans, deduplicates and trims names", {
  calls <- local_fake_ott_api()

  result <- get_organism_taxonomy_ott(
    df = data.frame(
      organism = c(
        "  Homo sapiens ",
        "Homo sapiens",
        "Mentha x piperita",
        "× Mentha piperita",
        NA
      )
    )
  )

  # Each cleaned name is sent once, hybrid markers removed, NA dropped
  expect_equal(
    sort(calls$tnrs[[1L]]$names),
    c("Homo sapiens", "Mentha piperita")
  )
  expect_setequal(result$organism_name, c("Homo sapiens", "Mentha piperita"))
})

test_that("get_organism_taxonomy_ott keeps the input name of synonyms", {
  calls <- local_fake_ott_api()

  result <- get_organism_taxonomy_ott(
    df = data.frame(organism = c("Gentiana lutea", "Gentiana major")),
    retry = FALSE
  ) |>
    tidytable::arrange(organism_name)

  # Both names resolve to the accepted taxon, each keeps its own row
  expect_equal(result$organism_name, c("Gentiana lutea", "Gentiana major"))
  expect_equal(result$organism_taxonomy_ottid, c(576855L, 576855L))
  expect_equal(
    result$organism_taxonomy_09species,
    c("Gentiana lutea", "Gentiana lutea")
  )
  # The shared id is looked up only once
  expect_equal(calls$taxon_info, list(576855L))
})

test_that("get_organism_taxonomy_ott does not retry names that all matched", {
  calls <- local_fake_ott_api()

  result <- get_organism_taxonomy_ott(
    df = data.frame(organism = c("Gentiana lutea", "Gentiana major"))
  )

  expect_length(calls$tnrs, 1L)
  expect_setequal(result$organism_name, c("Gentiana lutea", "Gentiana major"))
})

## Partial matches ----

test_that("get_organism_taxonomy_ott retries only failed names by genus", {
  calls <- local_fake_ott_api()

  get_organism_taxonomy_ott(
    df = data.frame(organism = c("Homo sapiens", "Gentiana fakeepithet"))
  )

  expect_length(calls$tnrs, 2L)
  expect_equal(calls$tnrs[[2L]]$names, "gentiana")
  # The genus found by the retry is looked up along with the species
  expect_setequal(calls$taxon_info[[1L]], c(770315L, 922413L))
})

test_that("get_organism_taxonomy_ott does not retry when retry is FALSE", {
  calls <- local_fake_ott_api()

  result <- get_organism_taxonomy_ott(
    df = data.frame(organism = c("Homo sapiens", "Gentiana fakeepithet")),
    retry = FALSE
  )

  expect_length(calls$tnrs, 1L)
  expect_equal(calls$taxon_info, list(770315L))
  # Matched organisms are kept, unmatched ones dropped
  expect_equal(result$organism_name, "Homo sapiens")
  expect_equal(result$organism_taxonomy_09species, "Homo sapiens")
})

test_that("get_organism_taxonomy_ott drops names unmatched after retry", {
  calls <- local_fake_ott_api()

  result <- get_organism_taxonomy_ott(
    df = data.frame(organism = c("Homo sapiens", "Fakegenus fakeepithet"))
  )

  expect_equal(calls$tnrs[[2L]]$names, "fakegenus")
  expect_equal(result$organism_name, "Homo sapiens")
  expect_equal(result$organism_taxonomy_08genus, "Homo")
})

test_that("get_organism_taxonomy_ott assigns genus-level taxonomy on retry", {
  local_fake_ott_api()

  result <- get_organism_taxonomy_ott(
    df = data.frame(organism = c("Homo sapiens", "Gentiana fakeepithet"))
  ) |>
    tidytable::arrange(organism_name)

  expect_equal(result$organism_name, c("Gentiana fakeepithet", "Homo sapiens"))
  expect_equal(result$organism_taxonomy_ottid, c(922413L, 770315L))
  expect_equal(result$organism_taxonomy_08genus, c("Gentiana", "Homo"))
  expect_equal(result$organism_taxonomy_09species, c(NA, "Homo sapiens"))
})

test_that("get_organism_taxonomy_ott returns no rows when nothing matches", {
  calls <- local_fake_ott_api()

  result <- get_organism_taxonomy_ott(
    df = data.frame(organism = "Fakegenus fakeepithet")
  )

  expect_equal(nrow(result), 0L)
  # No detail query without any OTT id
  expect_length(calls$taxon_info, 0L)
})

test_that("get_organism_taxonomy_ott keeps its columns when nothing matches", {
  local_fake_ott_api()

  result <- get_organism_taxonomy_ott(
    df = data.frame(organism = "Fakegenus fakeepithet")
  )

  expect_equal(names(result), names(.empty_organism_taxonomy_table()))
})
