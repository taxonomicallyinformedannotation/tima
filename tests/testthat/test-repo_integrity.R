# Test Suite: repo_integrity ----

library(testthat)

.repo_root <- function() {
  normalizePath(testthat::test_path("..", ".."), mustWork = TRUE)
}

test_that("human surfaces contain no agent-file references", {
  root <- .repo_root()
  skip_if_not(
    file.exists(file.path(root, "DESCRIPTION")) &&
      file.exists(file.path(root, "README.qmd")) &&
      dir.exists(file.path(root, ".github"))
  )

  candidates <- c(
    "README.qmd",
    "README.md",
    "TESTING.md",
    "cran-comments.md",
    list.files(
      file.path(root, "vignettes"),
      pattern = "\\.qmd$",
      recursive = TRUE,
      full.names = FALSE
    ) |>
      (\(x) file.path("vignettes", x))(),
    list.files(
      file.path(root, ".github"),
      pattern = "\\.(md|ya?ml)$",
      recursive = TRUE,
      full.names = FALSE
    ) |>
      (\(x) file.path(".github", x))(),
    list.files(
      file.path(root, "R"),
      pattern = "\\.[rR]$",
      recursive = TRUE,
      full.names = FALSE
    ) |>
      (\(x) file.path("R", x))(),
    list.files(
      file.path(root, "inst"),
      pattern = "\\.(R|r|txt|ya?ml|py|qmd|md)$",
      recursive = TRUE,
      full.names = FALSE
    ) |>
      (\(x) file.path("inst", x))()
  )

  agent_pat <- paste0(
    "(AGENTS\\.md|CLAUDE\\.md|GEMINI\\.md|\\.agents\\b|",
    "\\.agent-work|\\.claudeignore|\\.agentignore|\\.mcp\\.json)"
  )
  hits <- character(0)
  for (rel in candidates) {
    fp <- file.path(root, rel)
    if (!file.exists(fp)) {
      next
    }
    lines <- readLines(fp, warn = FALSE, encoding = "UTF-8")
    matched <- grep(agent_pat, lines, perl = TRUE)
    if (length(matched) > 0L) {
      hits <- c(hits, sprintf("%s:%d", rel, matched))
    }
  }

  expect_identical(hits, character(0))
})

test_that("human documentation contains no decorative emojis", {
  root <- .repo_root()
  skip_if_not(
    file.exists(file.path(root, "README.qmd")) &&
      dir.exists(file.path(root, ".github"))
  )

  candidates <- c(
    "README.qmd",
    "README.md",
    "TESTING.md",
    file.path(
      "vignettes",
      list.files(
        file.path(root, "vignettes"),
        pattern = "\\.qmd$",
        recursive = TRUE
      )
    ),
    file.path(
      ".github",
      list.files(
        file.path(root, ".github"),
        recursive = TRUE,
        pattern = "\\.(md|ya?ml)$"
      )
    )
  )

  emoji_pat <- "[\U0001F300-\U0001FAFF\u26A0\u2620\u2705\u274C\u2728]"
  hits <- character(0)
  for (rel in candidates) {
    fp <- file.path(root, rel)
    if (!file.exists(fp)) {
      next
    }
    lines <- readLines(fp, warn = FALSE, encoding = "UTF-8")
    matched <- grep(emoji_pat, lines, perl = TRUE)
    if (length(matched) > 0L) {
      hits <- c(hits, sprintf("%s:%d", rel, matched))
    }
  }

  expect_identical(hits, character(0))
})

test_that("human documentation and R comments contain no AI filler words", {
  root <- .repo_root()
  skip_if_not(
    file.exists(file.path(root, "README.qmd")) &&
      dir.exists(file.path(root, ".github"))
  )

  filler_pat <- "\\b(robust\\w*|seamless\\w*|comprehensive\\w*|powerful\\w*)\\b"
  doc_files <- c(
    "README.qmd",
    "README.md",
    "TESTING.md",
    "cran-comments.md",
    file.path(
      "vignettes",
      list.files(
        file.path(root, "vignettes"),
        pattern = "\\.qmd$",
        recursive = TRUE
      )
    ),
    file.path(
      ".github",
      list.files(
        file.path(root, ".github"),
        pattern = "\\.md$",
        recursive = TRUE
      )
    )
  )

  hits <- character(0)
  for (rel in doc_files) {
    fp <- file.path(root, rel)
    if (!file.exists(fp)) {
      next
    }
    lines <- readLines(fp, warn = FALSE, encoding = "UTF-8")
    if (identical(rel, "README.md")) {
      refs_idx <- grep("^<div id=\"refs\"", lines)
      if (length(refs_idx) > 0L) {
        lines <- lines[seq_len(refs_idx[[1L]] - 1L)]
      }
    }
    matched <- grep(filler_pat, lines, ignore.case = TRUE, perl = TRUE)
    if (length(matched) > 0L) {
      hits <- c(hits, sprintf("%s:%d", rel, matched))
    }
  }

  r_files <- list.files(
    file.path(root, "R"),
    pattern = "\\.[rR]$",
    full.names = FALSE
  )
  for (rf in r_files) {
    lines <- readLines(
      file.path(root, "R", rf),
      warn = FALSE,
      encoding = "UTF-8"
    )
    comment_idx <- grep("^\\s*#", lines)
    if (length(comment_idx) > 0L) {
      bad <- comment_idx[grepl(
        filler_pat,
        lines[comment_idx],
        ignore.case = TRUE,
        perl = TRUE
      )]
      if (length(bad) > 0L) {
        hits <- c(hits, sprintf("R/%s:%d", rf, bad))
      }
    }
  }

  expect_identical(hits, character(0))
})

test_that(".Rbuildignore is sorted, deduplicated, and ignores agent artifacts", {
  root <- .repo_root()
  rbi <- file.path(root, ".Rbuildignore")
  skip_if_not(file.exists(rbi))

  lines <- readLines(rbi, warn = FALSE, encoding = "UTF-8")
  expect_true(all(nzchar(lines)))
  expect_false(anyDuplicated(lines) > 0L)
  expect_identical(lines, withr::with_collate("C", sort(lines)))
  expect_true("^AGENTS[.]md$" %in% lines)
  expect_true("^\\.agents$" %in% lines)
  expect_true("^[.]agent-work$" %in% lines)
})

test_that("README.md is at least as recent as README.qmd", {
  root <- .repo_root()
  qmd <- file.path(root, "README.qmd")
  md <- file.path(root, "README.md")
  skip_if_not(file.exists(qmd) && file.exists(md))

  expect_true(as.numeric(file.mtime(md)) + 1 >= as.numeric(file.mtime(qmd)))
})

test_that("every docopt spec maps to an executable script and parses empty args", {
  root <- .repo_root()
  doc_dir <- file.path(root, "inst", "scripts", "docopt")
  scripts_dir <- file.path(root, "inst", "scripts")
  skip_if_not(dir.exists(doc_dir) && dir.exists(scripts_dir))

  txts <- sort(list.files(doc_dir, pattern = "\\.txt$", full.names = TRUE))
  expect_true(length(txts) > 0L)

  for (tf in txts) {
    stem <- sub("\\.txt$", "", basename(tf))
    expect_true(
      file.exists(file.path(scripts_dir, paste0(stem, ".R"))),
      info = stem
    )
    doc <- paste(
      readLines(tf, warn = FALSE, encoding = "UTF-8"),
      collapse = "\n"
    )
    expect_type(docopt::docopt(doc, args = character(0)), "list")
  }
})

test_that("codetools::checkUsagePackage reports no issues", {
  root <- .repo_root()
  skip_if_not(
    file.exists(file.path(root, "DESCRIPTION")) &&
      dir.exists(file.path(root, "R")) &&
      isNamespaceLoaded("tima")
  )

  rec <- new.env(parent = emptyenv())
  rec$issues <- character(0)
  codetools::checkUsagePackage(
    "tima",
    report = function(s) {
      rec$issues <- c(rec$issues, s)
    },
    skipWith = TRUE,
    suppressLocal = FALSE,
    suppressUndefined = utils::globalVariables(package = "tima")
  )
  expect_identical(rec$issues, character(0))
})
