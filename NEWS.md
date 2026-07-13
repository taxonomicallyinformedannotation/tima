# tima 2.13.0.9000 (unreleased)

## New features

- Expanded MS1 loss defaults for better fluorinated chemistry coverage:
  - Added PFAS-relevant neutral losses (`HF`, `CF2`, `CF2O`, `CF3`, `CHF2`,
    `C2F2`, `C2F4`, `C3F6`, `CF2O`, `C2F3O`)
  - Added common high-value losses (`SO2`, `SO3`)
- Added PubChem Lite as a new exposomics library, prepared through the same
  HMDB-like workflow as other SOP sources and flagged as `xenobiotic`
  (`Q409205`)
- Flattened `annotate_masses` modifier defaults so `clusters` and `solvents` are
  now single lists (no mode split), and threaded the flat schema through
  `prepare_params()`, the Shiny app, CLI parsing, and the annotation targets
- Refined spectral network construction in `create_edges_spectra()` to build
  community-aware edge graphs from the full spectral similarity network using
  weighted Louvain/Leiden-style clustering, retain isolated features, and avoid
  the old threshold-based edge filtering path.
- Exposed `build_components_from_edges()` as a public helper for reusable
  community detection and removed stale edge-threshold settings from the
  advanced parameter configuration.

## New libraries

- Added Enveda180 spectral library
- Added MultiMS2 spectral library

## Internal / performance

- **Candidate ranking now uses evidence coverage directly** in `clean_chemo()`
  and related ranking steps. The effective ranking score is computed as a
  conservative shrinkage of the raw weighted score by coverage, so a candidate
  with a slightly lower raw weighted score but substantially better coverage can
  outrank a candidate with a higher raw score but weak coverage.

- **MS1 adduct annotation improvements** in `annotate_masses()`:
  - Enforced tier-aware minimum support thresholds in evidence discovery: exotic
    adducts (tier 3+) now require stronger peer evidence (≥2 independent
    supporting neighbors) while common adducts (tier ≤1) are always inferred
  - Added hybrid evidence recovery fallback: when library exact-mass anchoring
    is restrictive, supplemental evidence pass recovers supported adduct
    hypotheses for previously unanchored features, improving coverage without
    sacrificing specificity
  - Added adduct-delta match quality metrics (`delta_error_da`,
    `delta_error_ppm`, `edge_match_score`) in pairwise matching to weight edges
    by mass accuracy
  - Replaced single-chain evidence-edge linking with sparse k-nearest m/z
    linking per cluster, improving triangulation signal while preserving O(n\*k)
    scalability
  - Re-enabled structural plausibility filter: loss-based annotations are now
    demoted to unmatched when the formula cannot satisfy required atoms (e.g.,
    `[M-H2O]` on a formula lacking oxygen)
  - Upgraded graph-level consistency enforcement from single-pass greedy to
    multi-start neighbor-aware optimization: assigns adduct states to maximize
    coverage-aware objectives (edge satisfaction, neighbor agreement, prior
    support) and favors states consistent across multiple neighbors over
    isolated high-scoring alternatives
  - Added bidirectional cluster and neutral-loss propagation to mirror cluster
    propagation
  - Reworked cluster-vs-loss ambiguity resolution to use hypothesis-level
    scoring (with neighboring-edge fallback tie-breaks) and added compact audit
    logging of resolved vs unresolved ambiguous modifier pairs
  - Added assignment-aware modifier-edge pruning so outdated cluster/loss links
    are removed when they contradict assigned adduct states, while retaining
    explicitly tagged contaminant edges

- **Adduct canonicalization alignment**:
  - Unified canonical adduct string ordering across structured universe
    generation (`adduct_to_string()`) and text harmonization
    (`canonicalize_adduct_notation()`)
  - Canonical order is now chemically consistent and deterministic: negative
    terms first (losses / deprotonations), then neutral cluster additions, then
    positive charge carriers
  - Updated regression tests to use canonical forms (e.g., `[M-H2O+H]+`,
    `[M+H2O+H]+`, `[M+NaCl+K]+`)
  - Improved ammonium/loss semantic reduction in harmonization (e.g.,
    `[M-H5NO+H4N]+ -> [M-H2O+H]+`, `[M-H3N+C2H7N+H4N]+ -> [M+C2H7N+H]+`) to
    reduce duplicated/over-complex carrier notation

- **Electron-mass correctness in legacy neutral-mass math**:
  - `calculate_mass_of_m()`, `calculate_mz_from_mass()`, and
    `calculate_mass_of_m_batch()` now apply signed electron-mass correction
    (`z * m_e`) consistently
  - Legacy inversion/forward formulas are now physically aligned with
    typed-universe mass arithmetic
  - Consolidated electron-mass precision to a single CODATA 2018 value
    (`ELECTRON_MASS_DALTONS = 0.000548579909065`) and reused it as
    `ELECTRON_MASS_DA`

- **Metadata enrichment cache robustness**:
  - `complement_metadata_structures()` reference cache now keys on both file
    path and file modification time, preventing stale cache reuse when
    temporary/updated reference files are rewritten during the same R session
  - Stereo required-column validation now occurs at reference-load time before
    cache insertion

- **Cross-stage adduct coupling (MS1 ↔ MS2 ↔ reranking)**:
  - Reinforced the bridge from MS1 adduct attribution to spectral annotation
    matching and final candidate reranking
  - Final percentile filtering now preserves rank-1/consensus-promoted entities,
    so cluster-consensus adduct attribution decisions are not lost when raw
    chemical scores are lower
  - This keeps adduct-consistent entities available for downstream spectral
    agreement and final ranking outputs

- **Weighted ranking semantics**:
  - Separated raw weighted evidence from coverage in the annotation scoring
    helpers, so `score_weighted_*` remains the primary ranking signal
  - Coverage is now emitted separately and used as a secondary confidence/tie-
    break signal, which prevents sparse candidates from outranking better-
    supported candidates solely because missing dimensions were discounted into
    the score

- **High-evidence filtering**:
  - `filter_high_evidence_only()` now requires sufficient coverage for rows that
    pass via the weighted final score, so sparse candidates do not qualify as
    high evidence on score alone

# tima 2.13.0

## Breaking changes

- `sanitize_spectra()`: `cutoff` parameter now defaults to `NULL` (dynamic)
  instead of `0`
- `annotate_spectra()` / `create_edges_spectra()`: the misspelled `qutoff`
  parameter is deprecated; use `cutoff` instead
- `install()` is deprecated in favor of `install_tima()`
- `validate_install_inputs()` no longer accepts a `test` parameter
- `show_system_messages()` no longer accepts a `test` parameter
- `R.utils` moved from Imports to Suggests

## New features

- Added full mzTab-M support across import, preparation, and export:
  - `read_mztab()` imports mzTab-M into TIMA feature/spectra/metadata files
  - `prepare_annotations_mztab()` maps mzTab structural annotations to TIMA
    schema
  - `write_mztab()` exports TIMA weighted annotations to mzTab-M (including
    merge mode)

- Added mzTab-M validation hardening with schema-backed required columns and
  strict SME checks

- `annotate_spectra()` now reports forward and reverse dot product scores
  (`candidate_score_similarity_forward`, `candidate_score_similarity_reverse`)
  alongside the main similarity score. Forward considers only query peaks
  (normalized by all query and matched library intensities); reverse considers
  only library peaks (normalized by matched query and all library intensities).
  Both are computed in C for consistency with the GNPS scoring engine.

- Added support for additional NPClassifier and ClassyFire taxonomy caches in
  `prepare_libraries_sop_merged()`, allowing structures missing taxonomy in the
  merged libraries to be enriched from external cache files. Caches grow over
  time as library entries are written back.

- Added a new validation helper, see
  <https://taxonomicallyinformedannotation.github.io/tima/vignettes/articles/0-validating.html>

- Added [BiGG](http://bigg.ucsd.edu/) as SOP library

- Added basic isotopologues handling

- Added external identifiers to the final results (#142)

- Added NORMAN SUSPECT LIST DATA in silico spectral libraries

- Added multiple new sub-libraries and a new `tag` column

- Added mzmine annotation support

- Added Sirius spectral results support

- Added special "Biota" superdomain handling for shared core metabolism

- `annotate_masses()` now enforces graph-level adduct consistency across
  connected edge hypotheses (removing globally impossible combinations while
  preserving consistent exotic states) and canonicalizes adduct states by
  parsed/net summed modifications so reordered loss/cluster text forms are
  treated as equivalent

- Enhanced `sanitize_spectra()` with adaptive noise filtering:
  - Dynamic intensity thresholds (MAD-based, now default when `cutoff = NULL`)
  - Low noise removal targets repetitive instrumental artifacts

## Internal / performance

- Improved log and error messages
- More in-depth molecular sanitization
- Standardized error handling to use `cli::cli_abort()` in exported functions
- Replaced nested `ifelse()` in `format_bytes()` with `findInterval()`
- Fixed `1:n` patterns to use `seq_len()` for safety
- Updated the C implementation of the spectral similarity

## Bug fixes

- Fixed adduct annotation bug and harmonization
- Fixed recognition of uncommon negative charges
- Fixed `split_tables_sop()` collapsing numerically identical exact masses into
  `"mass1 $ mass2"` strings due to `clean_collapse()` treating them as different
  character values; now uses `resolve_numeric_or_na()` with floating-point
  tolerance
- Removed xlogp from the structure metadata (`str_met`) table: xlogp is
  stereo-sensitive (Crippen atom-typing can differ between stereoisomers), so it
  is no longer collapsed by `inchikey_no_stereo`; the per-SMILES value computed
  by `process_smiles()` in the annotation pipeline is used instead

## Documentation

- Startup message now uses `cli::cli_inform()`

## Data updates

- Updated to Massbank version `2025.10`
- Updated to MSnLib `v7`

# tima 2.12.0

## Breaking changes

- `.RDS` spectra are now stored more efficiently. To avoid errors, delete any
  `.RDS` files created before version `2.12.0`
- `tima_full()` has been deprecated in favor of `run_tima()`
- Updated minimal R version to `4.4.0` (and related Bioconductor dependencies)

## New features

- Added automatic retention time conversion (in minutes)
- Added a minimal output
- Added a parameter to limit the numbers of neighbors used for chemical
  consistency calculation (#193)
- Added MERLIN spectral libraries (#190)
- Added optional compound name from RT libraries
- Added `RDKit`-based structures processing through `reticulate` (#19)
- Introduced similarity method argument (entropy and GNPS for now)

## Bug fixes

- Fixed memory crashes in case of large number of ties, limiting to 7 with note
  (#216)

## Internal / performance

- Externalized spectral libraries preparation to
  [SpectRalLibRaRies](https://github.com/Adafede/SpectRalLibRaRies)
- Implemented GNPS similarity method in C
- Improved high confidence filtering
- Improved logs using `logger` (#189)
- Keep (only) the best molecular formula and canopus annotations from SIRIUS
- Reduced dependencies and moved some to `Suggests`
- Refactored adducts parsing to read adducts like `[M+H]+/[M]+`
- Refactored MS1 annotation step to work per sample (#194)
- Refactored tests
- Renamed some functions/utils for consistency
- Replaced `logger` with `lgr` for `covr` compatibility

## Documentation

- Switched documentation from `pkgdown` to `altdoc`

## Data updates

- New ISDB version with 1 million compounds (see
  <https://doi.org/10.5281/zenodo.14887271>)
- Updated to Massbank version `2025.05.1`

# tima 2.11.1 (unreleased)

- Added `SIRIUS` feature tables support (#185)
- Added `.rar` compression support for `SIRIUS` workspaces (#186)

# tima 2.11.0

- Added convenience function to change small parameters (#177)
- Added demo files download to the app
- Better packaging
- Improved documentation
- Fixed all CRAN warnings
- Fixed some edge cases in spectra import
- Reduced dependencies
- Reduced exports
- Removed `CompoundDb` dependency as it was causing too many issues
- Removed `pak` install and switched to `r-universe`
- Replaced internal functions by `Spectra` equivalents (#166)
- Shinylive version available at
  <https://taxonomicallyinformedannotation.github.io/tima-shinylive>
- Simplified install and vignettes
- Switched from `base::lapply` to `purrr::map`

# tima 2.10.0

- Added alt text to vignettes
- Added the possibility to add internal libraries through the GUI (#159)
- Added the possibility to filter confident annotations only (#140)
- Added number of peaks in spectrum
- Brought back some older dependencies to be compatible with `oldrel`
- Changed package name, `usethis` update
- Clearer handling of SIRIUS scores (#146, #147)
- Exposed more parameters to the GUI (#159)
- Facilitated install, no need to clone the directory anymore
- Finally made it to the [r-universe](https://r-universe.dev/builds/)
- Fixed adducts and removed nitrogen rule
- Fixed number of matched peaks
- Improved imports
- Reduced warnings
- Updated benchmarking steps

# tima 2.9.6

- Added light-switch thanks to `pkgdown 2.1.0`.
- Attempt to simplify installation
- Fixed library/adducts confusion (#123)
- Fixed some incorrect adduct differences annotations
- Refactored adducts / neutral losses / dimers annotation to allow for more
  flexibility (#141, #144)

# tima 2.9.5

- Do not re-package if already the latest version
- SIRIUS 6 default and compatible (keeping SIRIUS 5 backward compatibility)
- Updated to Massbank version `2024.06`

# tima 2.9.4

- Automated update
- Added an option to remove ties (#134)
- Added some details for SIRIUS, added manual workspace addition (#132)
- Additional preprocessing (reduction) of noisy spectra
- Dependencies update
- Docker updates (#131)
- Handle cases when same (feature_id, mslevel) pairs are present within an MGF
  (#133)
- Improved documentation
- New working directory at `$HOME/.tima`
- Updated R and Bioconductor versions

# tima 2.9.3

- Allowed for SIRIUS jobs containing only summaries
- Allowed for underscores in job pattern
- Changed some default values (less stringent)
- Dependencies update
- Migrated app testing to `shinytest2`
- Removed further some inconsistent MS1 annotations
- Removed tests dependencies by default

# tima 2.9.2

- Added Nitrogen rule to filter out some annotations
- Better handling of partial downloads (#118)
- Dependencies update (mainly `targets 1.5.1`, will invalidate previous targets)
- Fixed some port issues in Shiny (#122)
- Removed completely empty columns from final output to avoid confusion (#120)

# tima 2.9.1

- Added [Waystation](https://caltechlibrary.github.io/waystation/) action
- Added structures from spectral libraries to SOP library (#113)
- Exposed all parameters (#107, #108)
- Fixed for Zenodo API
- HMDB structures support
- Optimized grep/gsub by adding `perl=TRUE` or `fixed=TRUE`
- Updated to Massbank version `2023.11`
- Updated SIRIUS preparation (#74, #115)

# tima 2.9.0

- Added compounds names as parameter
- Added MassBank spectral library (#77)
- Allowed files outside `data/source` (#89)
- Added RT library as annotation library (#86)
- Better handling of download errors
- Fixed Docker mount path
- Improved naming (#91)
- Internal variables refactoring
- Multiple Shiny fixes and tests addition (#60)
- Multiple fixes (#71, #81, #82)
- New adducts (#79, #80)
- Refactored adducts, clusters and neutral losses
- Refactored biological and chemical score
- Refactored RT matching (#76)
- Refactored Sirius scores (#92)
- Removed GNPS dependency by default

# tima 2.8.2

- Added spectral entropy
- Added MS1 only possibility
- Added Fluorine adduct
- Changed from pbmclapply to pblapply
- Documentation improvement
- Fixed empty chemical classes
- Fixed not classified taxa
- GitHub Actions improvement
- [renv](https://rstudio.github.io/renv/index.html) removal
- Performance improvement by replacing the
  [tidyverse](https://www.tidyverse.org) by the
  [fastverse](https://fastverse.github.io/fastverse) (in progress)
- Reduced warnings (CRAN and jscpd)

# tima 2.8.1

- Adapted tests
- Added `retry` parameter to `get_organism_taxonomy_ott`
- Dependencies update
- Minor fixes
- Moved `/params` and `paths.yaml` to `/inst` as more standard. (see
  <https://r-pkgs.org/misc.html#other-directories>)
- Performance improvement by replacing the
  [tidyverse](https://www.tidyverse.org) by the
  [fastverse](https://fastverse.github.io/fastverse) (in progress)
- Replaced `extdata` loading

# tima 2.8.0

- Added GUI prototype
- Started using [renv](https://rstudio.github.io/renv/index.html)

# tima 2.7.4

- Clearer vocabulary
- ECMDB support
- Edges (mass and spectra-based) and components are generated if not present.
- Fixed case when no GNPS job ID
- Further [Targets](https://books.ropensci.org/targets/) improvements
- Lot of fixes
- Parameters refactoring
- Re-introduced Classyfire support.
- Retention time matching additionally to MS2 if RT present in library
- Steps refactoring

# tima 2.7.3

- Improved calculations over redundant formulas
- Minor fixes
- Parameters refactoring
- Spectral matching update (see
  <https://github.com/rformassspectrometry/MetaboAnnotation/issues/93>)
- [Targets](https://books.ropensci.org/targets/) implementation

# tima 2.7.2

- Benchmark update (including negative mode)
- Improved parameters documentation
- Minor fixes
- Spectral comparison + intensity filtering update
- Switched r-base Docker image to bioconductor with ARM support

# tima 2.7.1

- Added MONA helpers
- Added parallelization on process_spectra
- Added sqlite storing for spectra
- Improved code documentation
- Improved testing time
- Minor fixes

# tima 2.7.0

- Added HMDB helpers for both taxo and ISDB
- Added MS2 annotation capability (kudos @jorainer for the awesome *Spectra*
  suite)
- Minor fixes

# tima 2.6.0

- Added Docker container
- Changed data architecture
- Minor fixes

# tima 2.5.6

- Dependencies removal (e.g. metabo-store)
- Minor fixes
- Partial functions cleanup

# tima 2.5.5

- Automation and parameters improvement
- Minor fixes

# tima 2.5.4

- Minor fixes
- Metadata completion improvement
- Molecular formula and adducts formalism improvement

# tima 2.5.3

- Imports improvements
- LOTUS update

# tima 2.5.2

- Packaging improvements

# tima 2.5.1

- Improved support for SIRIUS (with new summaries)

# tima 2.5.0

- LOTUS update
- Minor fixes

# tima 2.4.0

- Added chemical names and xlogp to output (#33)
- Added support for case when no consensus is found (#30)
- Improved output (#34)
- Minor fixes

# tima 2.3.0

- Added support for annotation without MN (#28)
- Added support for multi tool annotations (#27)
- Added support for classical MN GNPS jobs (#25)
- Added support for new version of LOTUS
- General improvements for manual inputs
- Improved tests code coverage
- Minor fixes
- Updated adducts

# tima 2.2.2

- Additional benchmark figure ([Candidates
  distribution](https://taxonomicallyinformedannotation.github.io/tima/articles/IV-benchmarking.html#candidates-distribution))
- Minor fixes

# tima 2.2.1

- Minor version name fixes

# tima 2.2.0

- Added benchmark
  ([here](https://taxonomicallyinformedannotation.github.io/tima/articles/IV-benchmarking.html))
- Various fixes

# tima 2.1.0

- Fixes, deletion of binary dependencies.

# tima 2.0.0

- Initial version.
