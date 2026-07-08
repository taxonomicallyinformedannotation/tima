library(testthat)


test_that("extract schema required handles null and missing required fields", {
  expect_identical(
    .extract_schema_required(NULL, "SmallMoleculeSummary"),
    character(0)
  )

  schema_no_required <- list(
    components = list(
      schemas = list(
        SmallMoleculeSummary = list(required = NULL)
      )
    )
  )
  expect_identical(
    .extract_schema_required(schema_no_required, "SmallMoleculeSummary"),
    character(0)
  )
})


test_that("extract schema required converts dashed keys to underscore", {
  schema <- list(
    components = list(
      schemas = list(
        SmallMoleculeSummary = list(
          required = c("database-identifier", "SML_ID")
        )
      )
    )
  )

  required <- .extract_schema_required(schema, "SmallMoleculeSummary")
  expect_identical(required, c("database_identifier", "SML_ID"))
})


test_that("load mzTab schema returns NULL when schema file is absent", {
  schema <- with_mocked_bindings(
    .get_mztab_schema_path = function() "",
    log_warn = function(...) invisible(NULL),
    .load_mztab_schema()
  )

  expect_null(schema)
})


test_that("load mzTab schema returns NULL on parse failure", {
  schema <- with_mocked_bindings(
    .get_mztab_schema_path = function() "dummy.yml",
    .mztab_path_exists = function(path) TRUE,
    .mztab_read_yaml = function(path) stop("bad yaml"),
    log_warn = function(...) invisible(NULL),
    .load_mztab_schema()
  )

  expect_null(schema)
})


test_that("get mzTab required columns falls back when schema is unavailable", {
  req <- with_mocked_bindings(
    .load_mztab_schema = function() NULL,
    get_mztab_required_columns()
  )

  expect_identical(req$MTD, MZTAB_REQUIRED_FALLBACK$MTD)
  expect_identical(req$SML, MZTAB_REQUIRED_FALLBACK$SML)
  expect_identical(req$SMF, MZTAB_REQUIRED_FALLBACK$SMF)
  expect_identical(req$SME, MZTAB_REQUIRED_FALLBACK$SME)
})


test_that("get mzTab required columns uses schema values and deduplicates", {
  schema <- list(
    components = list(
      schemas = list(
        SmallMoleculeSummary = list(
          required = c("SML-ID", "SML-ID", "database-identifier")
        ),
        SmallMoleculeFeature = list(
          required = c("SMF-ID", "exp-mass-to-charge")
        ),
        SmallMoleculeEvidence = list(required = c("SME-ID", "rank", "rank"))
      )
    )
  )

  req <- with_mocked_bindings(
    .load_mztab_schema = function() schema,
    get_mztab_required_columns()
  )

  expect_identical(req$MTD, MZTAB_REQUIRED_FALLBACK$MTD)
  expect_identical(req$SML, c("SML_ID", "database_identifier"))
  expect_identical(req$SMF, c("SMF_ID", "exp_mass_to_charge"))
  expect_identical(req$SME, c("SME_ID", "rank"))
})

test_that("mzTab schema catalog exposes shared defaults and ontology terms", {
  catalog <- .mztab_schema_catalog()

  expect_true("required_columns" %in% names(catalog))
  expect_identical(catalog$metadata$required, MZTAB_METADATA_REQUIRED_FALLBACK)
  expect_identical(
    catalog$terms$metadata_defaults[["mzTab-version"]],
    MZTAB_TERM_FALLBACKS$metadata$mzTab_version
  )
  expect_identical(
    catalog$terms$params$polarity$positive,
    MZTAB_TERM_FALLBACKS$params$polarity$positive
  )
  expect_true(any(vapply(
    catalog$terms$cv_registry,
    function(entry) {
      identical(entry$label, "TIMA")
    },
    logical(1)
  )))
})


test_that("mzTab template rendering replaces catalog placeholders", {
  rendered <- .mztab_render_templates(
    list(
      software = "[, , TIMA, {{software_version}}]",
      nested = list(description = "Built with {{software_version}}")
    ),
    list(software_version = "1.2.3")
  )

  expect_identical(rendered$software, "[, , TIMA, 1.2.3]")
  expect_identical(rendered$nested$description, "Built with 1.2.3")
})
