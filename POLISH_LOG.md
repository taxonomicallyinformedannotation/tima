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
  numbering after the highest `database[n]` already present in the base file,
  instead of unconditionally restarting at `database[2]`.
- ✅ **#14:** Removed redundant `length(passthrough_lines) > 0L &&` re-check in
  write_mztab.
- ✅ **#15 (duplication):** Hoisted the 3× `priority_prefixes` literal in
  `write_mztab_comments.R` into one `.MZTAB_XREF_PRIORITY_PREFIXES` constant
  (Wikidata-first for URI/xref-block ordering; Wikidata excluded-then-last-resort
  for `database_identifier`, preserving original ordering); extracted
  `.mztab_best_exact_mass()` and reused it across all 3 candidate-mass blocks
  (group_sizes==1, single-row SME, multi-row SME).
- ✅ **#16 (perf):** `.mztab_split_param_fields()` rewritten with a `strsplit` +
  `gsub` unescape pass (no per-character `c()` growth).
- ✅ **#7/#14 (dead code):** Removed unused `.mztab_pluck`/`.mztab_pluck_na`
  (+ their tests); removed the two unreachable `if (length(order_idx)==0L)`
  guards in the multi-row SME branch and the dead `best_local<-1L`/`order_idx<-1L`
  in the single-row SME branch.
- ✅ **#12 (score split):** `prepare_annotations_mztab` no longer maps overall
  confidence into `candidate_score_similarity`. Spectral similarity now comes
  only from `id_confidence_measure[4]` (TIMA spectral) / `best_id_confidence_value`
  (standard mzTab); MS1 evidence is forced to NA (fixes the downstream
  `ms1_only` filter and the conflation of combined score with spectral
  similarity). Removed dead `.mztab_reliability_to_score` + `score_primary`;
  added `score_split.mztab` fixture + regression test.
- ✅ `R/mztab_parser.R`: `log_warn` on JSON/text parse failures instead of
  silent NULL return.
- ✅ `R/read_mztab.R`: `.mztab_metadata_table()` normalized to always return the
  `filename`/`ATTRIBUTE_species` schema on every branch (was 3 inconsistent
  schemas on fallback); `.extract_embedded_mgf` rewritten from O(n²) `c()`
  accumulation to O(n) list accumulation.
- ✅ `src/gnps.c`: doc accuracy fixes for `xmalloc`/`xcalloc`/`gnps_chain_dp_core_api`.
- ✅ `R/safe_fread.R`, `R/get_file.R`: documented `.reader` param; restored
  `timeout` option via `on.exit` (no global side-effect).
- ✅ `R/write_mztab_merge.R`: fixed data-loss bug in `.mztab_merge_sml_rows`
  (referencing refs overwritten by last-keeper instead of folded sequentially);
  regression test added.
- ✅ `R/write_mztab_comments.R`: pipe-detection regex `\\|`|`→`|`;
  `identification_method` null → `[, , unknown identification method, ]`;
  `.mztab_escape` collapses `\t\r\n` and is applied to all metadata + row values.
- ✅ `R/write_mztab_tables.R`: MS1 classification via `grepl("tima ms1",…)`
  (not loose `MS1|ms1`); extracted `.mztab_empty_sme()`.
- ✅ `R/mztab_schema_utils.R`: memoized `.load_mztab_schema` (by path) and
  `.mztab_schema_catalog` (by version) — no re-parse per call.
- ✅ README.md/README.qmd: docs URL `articles/`→`vignettes/articles/`;
  R-Markdown-Cookbook link → canonical `pkg.yihui.org`.
- ✅ Build hygiene: `.Rbuildignore` excludes `.claudeignore`, `.ruff_cache`,
  `.gemini`, `.agents`, `.kb`, `POLISH_LOG.md`; `.gitignore` excludes
  `.DS_Store`/`.Rhistory`/`.RData`.

### Verification
- `air format . --check`: **PASS**
- `devtools::test()`: **5842 pass, 12 skip, 0 fail, 0 err** (latest, after all edits).
- `R CMD check`: 0 errors / 1 warning (`-mcpu=native`, environmental, from `~/.R/Makevars`) / 1 note.

## Open questions / blocked
- NEWS.md historical URLs (`www.tidyverse.org` redirect, yihui redirects) left
  untouched (changelog history); README canonicalized.
- `-mcpu=native` / `-Wno-c23-extensions` WARNING is environmental, not repo.

## Resolved proposals (all approved & applied)
- #12 score split ✅ — applied (see above).
- #15 consolidation ✅ — applied.
- #16 perf ✅ — applied.
- #7/#14 dead-code removal ✅ — applied.

## Issues found but not fixed
(invites future passes; all current proposals resolved)

