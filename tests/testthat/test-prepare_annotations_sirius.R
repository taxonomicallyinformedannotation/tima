# Test Suite: prepare_annotations_sirius ----

library(testthat)

## Internal Utility Helpers ----

.make_structs <- function(tmp) {
  dir.create(
    file.path(tmp, "structures", "taxonomies"),
    recursive = TRUE,
    showWarnings = FALSE
  )
  stereo <- file.path(tmp, "structures", "stereo.tsv")
  met <- file.path(tmp, "structures", "metadata.tsv")
  nam <- file.path(tmp, "structures", "names.tsv")
  cla <- file.path(tmp, "structures", "taxonomies", "classyfire.tsv")
  npc <- file.path(tmp, "structures", "taxonomies", "npc.tsv")
  writeLines("", stereo)
  writeLines("", met)
  writeLines("", nam)
  writeLines("", cla)
  writeLines("", npc)
  list(stereo = stereo, met = met, nam = nam, cla = cla, npc = npc)
}

## Validation ----

test_that("test-prepare_annotations_sirius validates sirius_version", {
  tmp <- withr::local_tempdir(.local_envir = parent.frame())
  s <- .make_structs(tmp)
  out_ann <- file.path(tmp, "ann.tsv")
  out_can <- file.path(tmp, "can.tsv")
  out_for <- file.path(tmp, "for.tsv")
  expect_error(
    prepare_annotations_sirius(
      input_directory = file.path(tmp, "missing.zip"),
      output_ann = out_ann,
      output_can = out_can,
      output_for = out_for,
      sirius_version = "7",
      str_stereo = s$stereo,
      str_met = s$met,
      str_nam = s$nam,
      str_tax_cla = s$cla,
      str_tax_npc = s$npc
    ),
    "sirius_version must be '5' or '6'"
  )
})

test_that("test-prepare_annotations_sirius validates output parameters and structure files", {
  tmp <- withr::local_tempdir(.local_envir = parent.frame())
  s <- .make_structs(tmp)
  expect_error(
    prepare_annotations_sirius(
      input_directory = file.path(tmp, "missing.zip"),
      output_ann = c("a", "b"),
      output_can = "can.tsv",
      output_for = "for.tsv",
      sirius_version = "5",
      str_stereo = s$stereo,
      str_met = s$met,
      str_nam = s$nam,
      str_tax_cla = s$cla,
      str_tax_npc = s$npc
    ),
    "output_ann must be a single character string",
    fixed = TRUE
  )
  expect_error(
    prepare_annotations_sirius(
      input_directory = file.path(tmp, "missing.zip"),
      output_ann = "ann.tsv",
      output_can = "can.tsv",
      output_for = "for.tsv",
      sirius_version = "5",
      str_stereo = file.path(tmp, "missing.tsv"),
      str_met = s$met,
      str_nam = s$nam,
      str_tax_cla = s$cla,
      str_tax_npc = s$npc
    ),
    "file not found",
    fixed = FALSE
  )
})

## Behavior ----

test_that("test-prepare_annotations_sirius handles missing input by producing empty outputs", {
  tmp <- withr::local_tempdir(.local_envir = parent.frame())
  s <- .make_structs(tmp)
  out_can <- file.path(tmp, "can.tsv")
  out_for <- file.path(tmp, "for.tsv")
  out_ann <- file.path(tmp, "ann.tsv")
  res <- prepare_annotations_sirius(
    input_directory = file.path(tmp, "not_there.zip"),
    output_ann = out_ann,
    output_can = out_can,
    output_for = out_for,
    sirius_version = "5",
    str_stereo = s$stereo,
    str_met = s$met,
    str_nam = s$nam,
    str_tax_cla = s$cla,
    str_tax_npc = s$npc
  )
  expect_true(file.exists(out_can))
  expect_true(file.exists(out_for))
  expect_true(file.exists(out_ann))
  expect_true(is.character(res[1]))
})
