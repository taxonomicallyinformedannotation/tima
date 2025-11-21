# Test Teardown - Verify No Pollution of tests/testthat

# Check that tests didn't create unwanted files/dirs in tests/testthat
testthat_root <- getwd()

# List of allowed items in tests/testthat (excluding temp R session files)
allowed_patterns <- c(
  "^fixtures$", # Fixture directory
  "^helper-.*\\.R$", # Helper files
  "^setup\\.R$", # Setup file
  "^setup-shinytest2.R$", # Setup shinytest2 file
  "^teardown\\.R$", # This file
  "^test-.*\\.R$", # Test files
  "^\\.Rdata$", # R session data (sometimes created)
  "^\\.Rhistory$", # R history (sometimes created)
  "^_snaps$", # specific
  "^tima.log$" # specific
)

# Get all items in current directory
all_items <- list.files(testthat_root, all.files = FALSE, no.. = TRUE)

# Filter to unexpected items
unexpected <- all_items[
  !sapply(all_items, function(item) {
    any(sapply(allowed_patterns, function(pat) grepl(pat, item)))
  })
]

# Warn about unexpected items (but don't fail - just inform)
if (length(unexpected) > 0) {
  message("\n")
  message("========================================")
  message("WARNING: Unexpected items in tests/testthat:")
  message("========================================")
  for (item in unexpected) {
    item_path <- file.path(testthat_root, item)
    if (dir.exists(item_path)) {
      n_files <- length(list.files(item_path, recursive = TRUE))
      message(sprintf("  [DIR]  %s (%d files)", item, n_files))
    } else {
      message(sprintf("  [FILE] %s", item))
    }
  }
  message("\nThese items should be cleaned up.")
  message("Tests should only write to tempdir(), not tests/testthat/")
  message("========================================")
  message("\n")
}

# Clean up session-level test temp directory if it exists
if (exists(".test_root", envir = .GlobalEnv)) {
  test_root <- get(".test_root", envir = .GlobalEnv)
  if (dir.exists(test_root)) {
    # This is in tempdir(), safe to remove
    unlink(test_root, recursive = TRUE, force = TRUE)
  }
}
