# TIMA Architecture Notes

## Execution Entry Points

- Package workflow: `R/run_tima.R` runs `execute_targets_pipeline()` via
  `targets::tar_make()` against `_targets.R` in the active workspace root.
- Shiny interface: `R/run_app.R` validates `host`, `port` (`1-65535`),
  `browser`, and `reinstall`, detects Docker environments, and launches
  `inst/app.R` alongside `inst/app_ui.R` and `inst/app_server.R`. Note there is
  no `inst/app/` subdirectory.
- Targets graph: `inst/_targets.R` resolves its sibling `targets/` directory in
  three stages (active `source()` call frames, installed `tima` package path, or
  `inst/targets/` under repository root), then sources `00_targets_setup.R`
  through `30_targets_benchmark.R`.
- CLI steps: `inst/scripts/` holds 36 step scripts paired with docopt specs in
  `inst/scripts/docopt/`. `get_params()` (`R/get_params.R`) and
  `parse_cli_params()` (`R/parse_cli_params.R`) merge YAML defaults from
  `inst/params/` and `inst/paths.yaml` with parsed CLI arguments.

## C Extension and Spectral Preconditions

- Registration: `src/init.c` exposes four `.Call()` symbols (`C_join_gnps`,
  `C_gnps`, `C_gnps_chain_dp`, `C_gnps_chain_dp_batch`) declared in `src/tima.h`
  with `R_NO_REMAP`, `R_useDynamicSymbols(dll, FALSE)`, and
  `R_forceSymbols(dll, TRUE)`.
- R boundary: all `.Call()` wrappers belong in `R/c_wrappers.R`.
- Chain-DP precondition (`src/gnps.c`): matching assumes each spectrum has at
  most one direct and one shifted match per peak, so inputs must have finite
  ascending `m/z`, finite positive intensities, and intra-spectrum peak spacing
  wider than matching tolerance. Violations silently mis-score rather than
  raising errors. Use `is_spectrum_sanitized()` and `sanitize_spectrum_matrix()`
  (`R/sanitize_spectrum_matrix.R`) when spectra bypass upstream
  `sanitize_spectra()` / `import_spectra()`.
- Portable C demo: `src/gnps_portable.h` allows standalone build and
  verification with `R CMD SHLIB src/gnps.c src/init.c` and
  `python3 inst/python/gnps_portable_demo.py`.

## Taxonomic Schema Across 15 Ranks

- `R/columns_utils.R` defines `.OTT_RANK_COL_MAP`, `.OTT_SUBRANK_COLS`, and
  `.SAMPLE_FROM_OTT_COLS` across all 15 taxonomic levels (`domain`, `kingdom`,
  `phylum`, `class`, `order`, `infraorder`, `family`, `subfamily`, `tribe`,
  `subtribe`, `genus`, `subgenus`, `species`, `subspecies`, `varietas`).
- Adding or renaming any rank requires synchronized updates across
  `R/columns_utils.R`, `R/prepare_params.R`, `R/parse_cli_params.R`,
  `R/decorate_bio.R`, `R/weight_bio.R`, `R/weight_annotations.R`, and
  `R/globals.R`.

## Packaging and Surface Separation

- `R/pkg_system_file.R` wraps `base::system.file()` as `pkg_system_file()` so
  tests can mock package resource lookups cleanly without patching base
  bindings.
- `.Rbuildignore` must stay sorted under `LC_ALL=C` with no blank or duplicate
  lines and must exclude `.lintr`, `.agent-work`, `AGENTS.md`, `.agents`,
  `.claudeignore`, and `.agentignore` from built tarballs.
- `tests/testthat/test-repo_integrity.R` enforces surface separation, absence of
  emojis and filler wording in human-facing files, `README.md` timestamp parity
  with `README.qmd`, `.Rbuildignore` sorting, docopt mapping alignment, and
  `codetools::checkUsagePackage("tima")` cleanliness.
