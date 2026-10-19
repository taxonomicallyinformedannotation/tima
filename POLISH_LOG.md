# POLISH_LOG — full-codebase polish pass

Branch: `polish/full-pass` (from `main` @ 7550d4e0)
Started: 2026-10-19

## Phase 0: Reconnaissance

### What this repo is
`tima` — **T**axonomically **I**nformed **M**etabolite **A**nnotation. An R package
(Bioconductor-adjacent) with:
- `R/` — 144 source files, ~54.5k lines (Collate order fixed in DESCRIPTION)
- `src/` — small C layer (`gnps.c`, `init.c`, headers; compiled `tima.so`)
- `tests/testthat/` — 155 test files + setup/helper
- `man/` — 505 roxygen-generated `.Rd` files (roxygen2 8.1.1, markdown ON)
- `inst/` — Shiny app, params/paths YAML, targets pipeline, Python helpers, Dockerfile, WORDLIST
- `vignettes/`, `docs/`, `_quarto/`, `pkgdown/`, `altdoc/` — docs site
- CI: `.github/workflows/` — R-CMD-check (5 OS/R matrix), format-check, test-coverage,
  mega-linter, altdoc, docker, external-use, etc.

### Build / lint / test commands (discovered)
| Purpose | Command | Notes |
|---|---|---|
| Format check | `air format . --check` | CI: format-check.yaml; air installed @ ~/.local/bin |
| Build | `R CMD build . --no-build-vignettes` | works, exit 0 |
| Tests | `Rscript -e 'devtools::test()'` | must load pkg; slow (network-y tests) |
| R CMD check | `rcmdcheck::rcmdcheck()` / CI | not run locally yet (slow) |
| Markdown fmt | `panache format .` (pre-commit) | panache installed |
| TOML fmt | `tombi format` / `tombi lint` | installed |
| Pre-commit | `prek` (hooks incl. air-format, parsable-R, deps-in-desc…) | installed |
| jarl (R lint) | `jarl check .` | **PANICS** (`lengths.rs:77` unwrap on None) — jarl bug, not our repo |

### Baseline (recorded before any change)
- `air format . --check`: **PASS** (exit 0)
- `R CMD build`: **PASS**
- `jarl check .`: **CRASH** (tool bug) — unavailable
- `devtools::test()`: **5497 assertions pass, 8 skips, 0 failures, 0 warnings, 0 errors**.
- `R CMD check` (as-cran, no-manual): **0 errors | 1 warning | 3 notes**
  - WARNING compilation flags `-mcpu=native` `-Wno-c23-extensions`: from user `~/.R/Makevars`, not repo.
  - NOTE top-level `POLISH_LOG.md`; hidden dirs `.claudeignore`/`.ruff_cache` -> now excluded via .Rbuildignore.
  - NOTE CRAN feasibility: stale/redirected URLs in NEWS.md/README.md (informational).

### Review queue (by priority)
1. Core logic: `annotate_masses*.R`, `annotate_spectra.R`, `clean_chemo*.R`,
   `weight_*.R`, `mass_evidence*.R`, `adduct_universe*.R`, `parse_adduct.R`
2. Public surface: exports in NAMESPACE, `install.R`, `run_tima.R`, `run_app.R`,
   `prepare_params.R`, `parse_cli_params.R`, `validate_inputs.R`
3. Utilities: `*_utils.R`, `errors_utils.R`, `assert_utils.R`, `logs_utils.R`,
   `safe_fread.R`, `retry_utils.R`, `go_to_cache.R`, `string_utils.R`
4. mztab cluster: `read_mztab.R`, `write_mztab*.R`, `mztab_*.R`
5. C code: `src/*.c`, `src/*.h`
6. Tests: `tests/testthat/*`
7. Docs/config: README.qmd/README.md, DESCRIPTION, NEWS.md, CI workflows, prek.toml

## Progress

(files marked ✅ reviewed & fixed; ⏭️ reviewed, no change; ⚠️ flagged, not fixed)

### Phase 1 — file-by-file
(in progress)

#### `src/gnps.c` (C core)
- ✅ Doc: fixed stale `gnps_fatal()` references (xmalloc/xcalloc now accurately
  describe error()/abort semantics); fixed doc claiming "never returns NULL".

#### `R/safe_fread.R`
- ✅ Documented previously-undocumented `.reader` argument (roxygen + Rd).

#### `.gitignore` / `.Rbuildignore` hygiene
- ✅ Added `.DS_Store`, `.Rhistory`, `.RData` to `.gitignore`.
- ✅ Added `.claudeignore`, `.ruff_cache`, `.gemini`, `.agents`, `.kb`,
  `POLISH_LOG.md` to `.Rbuildignore` (removes 2 R-CMD-check hidden/top-level
  NOTEs).

#### `R/string_utils.R` (shims for stringi)
- ⏭️ Reviewed `stri_*` shims — correct, well-tested. No change.

#### mzTab cluster (`write_mztab*`)
- ✅ **#2 (high):** Fixed data-loss bug in `.mztab_merge_sml_rows`: merging
  multiple features that map to the SAME compound dropped all but the last
  feature's references (R duplicated-index-assignment pitfall). Now folds
  references sequentially. Added regression test
  (`test-write_mztab.R`: "write_mztab merge unions all refs…").
- ✅ **#4:** Fixed pipe-detection regex `\\|` → `|` in
  `.mztab_build_ambiguity_comments` (multivalue detection was dead code).
- ✅ **#5:** `.mztab_library_to_identification_method`: `null`/empty/"" now map
  to `[, , unknown identification method, ]` instead of `[, , null, ]`. Added test.
- ✅ **#6:** COM provenance (`export`/`opt columns`/`edges`) comments no longer
  accumulate on repeated merges (deduped against passthrough). Added logic.
- ✅ **#8:** `.mztab_escape` now collapses `\t`, `\r`, `\n` (was tab-only); now
  applied to metadata values and to every section row value. Added test.
- ✅ **#11 (partial):** Additional xref `database[n]` blocks now continue
  numbering after any `database[n]` already present in the base file, instead
  of unconditionally restarting at `database[2]`.
- ✅ **#14:** Removed redundant `length(passthrough_lines) > 0L &&` re-check in
  write_mztab.
- ⚠️ #3 `#11 dup keys`, #7 orphaned roxygen, #1 dead-code, #15 duplication,
  #16 perf `.mztab_schema_catalog`/`.extract_embedded_mgf`/`_split_param_fields`
  — logged under Open questions / Proposals (need careful refactors/tests).

## Issues found but not fixed

## Open questions
- **#12 design (logged):** `prepare_annotations_mztab` maps overall confidence
  (`score_final`/`best_id_evidence_value`/`reliability`) into
  `candidate_score_similarity`, which downstream weighting treats as spectral
  similarity. Non-spectral evidence thus masquerades as spectral match.
  Recommendation (needs SME input): only populate `candidate_score_similarity`
  when the source library is spectral; otherwise map into a generic score
  column. Behavior change → proposal only.
- README historical NEWS.md URLs (tidyverse.org `www.` → redirect, yihui redirect)
  left as-is (changelog history); README.md/.qmd corrected to canonical targets.
- `-mcpu=native` / `-Wno-c23-extensions` WARNING originates from the maintainer
  `~/.R/Makevars`, not the package; CRAN/CI builds clean.

## Proposals requiring human decision
1. **Finding #15 remaining duplication** (`write_mztab_tables.R`): extract
   `.mztab_best_exact_mass()` across the 3 triplicated "pick best-scoring
   candidate exact mass" blocks (~682/766/866); hoist the 3× `priority_prefixes`
   vector in `write_mztab_comments.R` into one constant (order currently
   differs between copies); consolidate `.mztab_pluck_na`/`.mztab_norm_scalar`/
   `.mztab_safe_scalar` null-normalization semantics. (Tests pass either way;
   medium refactor risk.)
2. **Finding #16 remaining perf**: `.mztab_split_param_fields()` accumulates
   chars via `c()` per element (O(n²) per CV term). Rewrite with `strsplit` +
   unescape pass. (Niche parser; low impact.)
3. **Finding #7/#14 dead-code cleanup**: unused `best_local <- 1L` /
   `order_idx <- 1L` in the single-row SME branch; dead `if (length(order_idx)==0)`
   guard; dead `.mztab_pluck`/`.mztab_pluck_na` (tested but unused in production)
   + Rd — consider removal. (Low value, medium test-churn risk.)
4. **Finding #1 (invalidated):** merge was thought to wipe base SMF refs, but
   `feature_id`-less base rows still matched via the merge key correctly; no
   change needed. Verified by round-trip repro.
