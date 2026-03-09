#' Retrieve external database identifiers for compounds by InChIKey via Wikidata
#'
#' Fetches mappings from the Bioregistry for a set of Wikidata property IDs,
#' queries QLever for compound identifiers, and returns a tidy long data.frame
#' with one row per InChIKey × database combination, including Wikidata QIDs.
#' Results are cached to disk; the query is only re-run when the cached file
#' is older than \code{max_age_hours} (default 24 h) or does not exist.
#'
#' @include create_dir.R
#' @include export_output.R
#' @include logs_utils.R
#'
#' @param props Character vector of Wikidata property IDs (without \code{wdt:}
#'   prefix), e.g. \code{c("P683", "P592")}.
#' @param bioregistry_url URL to the bulk bioregistry JSON. Defaults to the
#'   canonical GitHub raw URL.
#' @param qlever_url QLever SPARQL endpoint URL.
#' @param max_age_hours Numeric maximum age (in hours) of the cached file
#'   before it is refreshed. Default \code{24}.
#' @param output Character file path for the cached result. When used inside
#'   a \pkg{targets} pipeline with \code{format = "file"}, this path is
#'   tracked automatically.
#'
#' @return Character path to the exported file (invisibly), for
#'   \pkg{targets} \code{format = "file"} compatibility.
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#' props <- c("P231", "P592", "P683", "P715")
#' result_path <- get_compounds_xrefs(props)
#' head(tidytable::fread(result_path))
#' }
get_compounds_xrefs <- function(
  props = c(
    "P231",
    "P592",
    "P661",
    "P662",
    "P665",
    "P683",
    "P715",
    "P2057",
    "P2063",
    # P2084,
    "P2877",
    # P3890,
    "P8691"
  ),
  bioregistry_url = "https://raw.githubusercontent.com/biopragmatics/bioregistry/refs/heads/main/src/bioregistry/data/bioregistry.json",
  qlever_url = "https://qlever.cs.uni-freiburg.de/api/wikidata",
  max_age_hours = 24,
  output = get_default_paths()$data$interim$xrefs$compounds
) {
  # ── 0. Check cache ─────────────────────────────────────────────────────────
  if (file.exists(output)) {
    age_hours <- as.numeric(
      difftime(Sys.time(), file.mtime(output), units = "hours")
    )
    if (age_hours < max_age_hours) {
      log_debug(
        "Cached cross-references are %.1f h old (< %d h), skipping refresh: %s",
        age_hours,
        max_age_hours,
        basename(output)
      )
      return(invisible(output))
    }
    log_debug(
      "Cached cross-references are %.1f h old (>= %d h), refreshing",
      age_hours,
      max_age_hours
    )
  }

  log_info("Fetching compound cross-references from Wikidata / QLever")
  ctx <- log_operation("get_compounds_xrefs")

  # ── 1. Load bioregistry & match props to entry keys ────────────────────────
  bioregistry_json <- jsonlite::fromJSON(
    bioregistry_url,
    simplifyVector = FALSE
  )

  matched <- Filter(
    Negate(is.null),
    lapply(
      X = bioregistry_json,
      FUN = function(entry) {
        wd <- entry[["wikidata"]]
        if (is.list(wd)) {
          p <- wd[["prefix"]]
          if (!is.null(p) && p %in% props) {
            return(list(property = p))
          }
        }
        NULL
      }
    )
  )

  if (length(matched) == 0L) {
    stop(
      "None of the supplied props were found in the Bioregistry.",
      call. = FALSE
    )
  }

  # ── 2. Fetch preferred prefix ("banana") per entry from Bioregistry API ────
  .fetch_banana <- function(entry_key) {
    resp <- httr2::request(paste0(
      "https://bioregistry.io/api/registry/",
      entry_key
    )) |>
      httr2::req_headers(Accept = "application/json") |>
      httr2::req_error(is_error = \(r) FALSE) |> # handle errors manually
      httr2::req_perform()

    if (httr2::resp_status(resp) != 200L) {
      return(entry_key)
    }

    data <- httr2::resp_body_json(resp)
    preferred <- data[["banana"]]
    if (!is.null(preferred)) {
      preferred
    } else {
      entry_key
    }
  }

  preferred_prefixes <- vapply(
    X = names(matched),
    FUN = .fetch_banana,
    FUN.VALUE = character(1L)
  )

  # Build lookup: P683 → "ChEBI", P592 → "ChEMBL", …
  prop_to_prefix <- setNames(
    preferred_prefixes,
    vapply(matched, `[[`, character(1L), "property")
  )

  # ── 3. Build SPARQL query ───────────────────────────────────────────────────
  wdt_values <- paste(
    vapply(
      matched,
      function(x) {
        paste0("    wdt:", x$property)
      },
      character(1L)
    ),
    collapse = "\n"
  )

  sparql <- paste0(
    'PREFIX wdt: <http://www.wikidata.org/prop/direct/>
SELECT ?item ?inchikey ?db ?id WHERE {
  VALUES ?db {
',
    wdt_values,
    '
  }
  ?item wdt:P235 ?inchikey .
  ?item ?db ?id .
}'
  )

  # ── 4. Query QLever ─────────────────────────────────────────────────────────
  resp_qlever <- httr2::request(qlever_url) |>
    httr2::req_method("POST") |>
    httr2::req_body_form(query = sparql) |>
    httr2::req_headers(Accept = "text/csv") |>
    httr2::req_error(is_error = \(r) httr2::resp_status(r) >= 400L) |>
    httr2::req_perform()

  raw <- tidytable::fread(httr2::resp_body_string(resp_qlever))

  # ── 5. External DB rows ─────────────────────────────────────────────────────
  results_external <- raw |>
    tidytable::distinct(inchikey, db, id) |>
    tidytable::mutate(
      inchikey = substr(inchikey, 1L, 14L),
      property = sub(".*/", "", db),
      prefix = prop_to_prefix[property]
    ) |>
    tidytable::select(inchikey, prefix, id)

  # ── 6. Wikidata QID rows ────────────────────────────────────────────────────
  results_qid <- raw |>
    tidytable::distinct(inchikey, item) |>
    tidytable::mutate(
      inchikey = substr(inchikey, 1L, 14L),
      prefix = "wikidata",
      id = sub(".*/", "", item)
    ) |>
    tidytable::select(inchikey, prefix, id)

  # ── 7. Combine, save & return ───────────────────────────────────────────────
  result <- tidytable::bind_rows(results_external, results_qid) |>
    tidytable::arrange(inchikey, prefix)

  export_output(x = result, file = output)
  log_complete(ctx, n_rows = nrow(result))

  invisible(output)
}
