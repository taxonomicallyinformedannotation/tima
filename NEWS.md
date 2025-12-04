# tima

# tima 2.12.0

* Added automatic retention time conversion (in minutes)
* Added a minimal output
* Added a parameter to limit the numbers of neighbors used for chemical consistency calculation (#193)
* Added MERLIN spectral libraries (#190)
* Added optional compound name from RT libraries
* Added `RDKit`-based structures processing through `reticulate` (#19)
* Breaking Change: `.RDS` spectra are now stored more efficiently. To avoid errors, delete any `.RDS` files created before version `2.12.0`
* Externalized spectral libraries preparation to [SpectRalLibRaRies](https://github.com/Adafede/SpectRalLibRaRies)
* Fix memory crashes in case of large number of ties, limiting to 7 with note (#216)
* Introduced similarity method argument (entropy and GNPS for now)
* Implemented GNPS similarity method in C
* Improved high confidence filtering
* Improved logs using `logger` (#189)
* Keep (only) the best molecular formula and canopus annotations from SIRIUS
* New ISDB version with 1 million compounds (see <https://doi.org/10.5281/zenodo.14887271>)
* Reduced dependencies and moved some to `Suggests`
* Refactored adducts parsing to read adducts like `[M+H]+/[M]+`
* Refactored MS1 annotation step to work per sample (#194)
* Refactored tests
* Renamed some functions/utils for consistency
* Replaced `logger` with `lgr` for `covr` compatibility
* Switched documentation from `pkgdown` to `altdoc`
* `tima_full()` has been deprecated in favor of `run_tima()`
* Updated to Massbank version `2025.05.1`
* Updated minimal R version to `4.4.0` (and related Bioconductor dependencies)

# tima 2.11.1 (unreleased)

* Added `SIRIUS` feature tables support (#185)
* Added `.rar` compression support for `SIRIUS` workspaces (#186)

# tima 2.11.0

* Added convenience function to change small parameters (#177)
* Added demo files download to the app
* Better packaging
* Improved documentation
* Fixed all CRAN warnings
* Fixed some edge cases in spectra import
* Reduced dependencies
* Reduced exports
* Removed `CompoundDb` dependency as it was causing too many issues
* Removed `pak` install and switched to `r-universe`
* Replaced internal functions by `Spectra` equivalents (#166)
* Shinylive version available at <https://taxonomicallyinformedannotation.github.io/tima-shinylive>
* Simplified install and vignettes
* Switched from `base::lapply` to `purrr::map`

# tima 2.10.0

* Added alt text to vignettes
* Added the possibility to add internal libraries through the GUI (#159)
* Added the possibility to filter confident annotations only (#140)
* Added number of peaks in spectrum
* Brought back some older dependencies to be compatible with `oldrel`
* Changed package name, `usethis` update
* Clearer handling of SIRIUS scores (#146, #147)
* Exposed more parameters to the GUI (#159)
* Facilitated install, no need to clone the directory anymore
* Finally made it to the [r-universe](https://r-universe.dev/builds/)
* Fixed adducts and removed nitrogen rule
* Fixed number of matched peaks
* Improved imports
* Reduced warnings
* Updated benchmarking steps

# tima 2.9.6

* Added light-switch thanks to `pkgdown 2.1.0`.
* Attempt to simplify installation
* Fixed library/adducts confusion (#123)
* Fixed some incorrect adduct differences annotations
* Refactored adducts / neutral losses / dimers annotation to allow for more flexibility (#141, #144)

# tima 2.9.5

* Do not re-package if already the latest version
* SIRIUS 6 default and compatible (keeping SIRIUS 5 backward compatibility)
* Updated to Massbank version `2024.06`

# tima 2.9.4

* Automated update
* Added an option to remove ties (#134)
* Added some details for SIRIUS, added manual workspace addition (#132)
* Additional preprocessing (reduction) of noisy spectra
* Dependencies update
* Docker updates (#131)
* Handle cases when same (feature_id, mslevel) pairs are present within an MGF (#133)
* Improved documentation
* New working directory at `$HOME/.tima`
* Updated R and Bioconductor versions

# tima 2.9.3

* Allowed for SIRIUS jobs containing only summaries
* Allowed for underscores in job pattern
* Changed some default values (less stringent)
* Dependencies update
* Migrated app testing to `shinytest2`
* Removed further some inconsistent MS1 annotations
* Removed tests dependencies by default

# tima 2.9.2

* Added Nitrogen rule to filter out some annotations
* Better handling of partial downloads (#118)
* Dependencies update (mainly `targets 1.5.1`, will invalidate previous targets)
* Fixed some port issues in Shiny (#122)
* Removed completely empty columns from final output to avoid confusion (#120)

# tima 2.9.1

* Added [Waystation](https://caltechlibrary.github.io/waystation/) action
* Added structures from spectral libraries to SOP library (#113)
* Exposed all parameters (#107, #108)
* Fixed for Zenodo API
* HMDB structures support
* Optimized grep/gsub by adding `perl=TRUE` or `fixed=TRUE`
* Updated to Massbank version `2023.11`
* Updated SIRIUS preparation (#74, #115)

# tima 2.9.0

* Added compounds names as parameter
* Added MassBank spectral library (#77)
* Allowed files outside `data/source` (#89)
* Added RT library as annotation library (#86)
* Better handling of download errors
* Fixed Docker mount path
* Improved naming (#91)
* Internal variables refactoring
* Multiple Shiny fixes and tests addition (#60)
* Multiple fixes (#71, #81, #82)
* New adducts (#79, #80)
* Refactored adducts, clusters and neutral losses
* Refactored biological and chemical score
* Refactored RT matching (#76)
* Refactored Sirius scores (#92)
* Removed GNPS dependency by default

# tima 2.8.2

* Added spectral entropy
* Added MS1 only possibility
* Added Fluorine adduct
* Changed from pbmclapply to pblapply
* Documentation improvement
* Fixed empty chemical classes
* Fixed not classified taxa
* GitHub Actions improvement
* [renv](https://rstudio.github.io/renv/index.html) removal
* Performance improvement by replacing the [tidyverse](https://www.tidyverse.org) by the [fastverse](https://fastverse.github.io/fastverse) (in progress)
* Reduced warnings (CRAN and jscpd)

# tima 2.8.1

* Adapted tests
* Added `retry` parameter to `get_organism_taxonomy_ott`
* Dependencies update
* Minor fixes
* Moved `/params` and `paths.yaml` to `/inst` as more standard. (see <https://r-pkgs.org/misc.html#other-directories>)
* Performance improvement by replacing the [tidyverse](https://www.tidyverse.org) by the [fastverse](https://fastverse.github.io/fastverse) (in progress)
* Replaced `extdata` loading

# tima 2.8.0

* Added GUI prototype
* Started using [renv](https://rstudio.github.io/renv/index.html)

# tima 2.7.4

* Clearer vocabulary
* ECMDB support
* Edges (mass and spectra-based) and components are generated if not present.
* Fixed case when no GNPS job ID
* Further [Targets](https://books.ropensci.org/targets/) improvements
* Lot of fixes
* Parameters refactoring
* Re-introduced Classyfire support.
* Retention time matching additionally to MS2 if RT present in library
* Steps refactoring

# tima 2.7.3

* Improved calculations over redundant formulas
* Minor fixes
* Parameters refactoring
* Spectral matching update (see <https://github.com/rformassspectrometry/MetaboAnnotation/issues/93>)
* [Targets](https://books.ropensci.org/targets/) implementation

# tima 2.7.2

* Benchmark update (including negative mode)
* Improved parameters documentation
* Minor fixes
* Spectral comparison + intensity filtering update
* Switched r-base Docker image to bioconductor with ARM support

# tima 2.7.1

* Added MONA helpers
* Added parallelization on process_spectra
* Added sqlite storing for spectra
* Improved code documentation
* Improved testing time
* Minor fixes

# tima 2.7.0

* Added HMDB helpers for both taxo and ISDB
* Added MS2 annotation capability (kudos @jorainer for the awesome *Spectra* suite)
* Minor fixes

# tima 2.6.0

* Added Docker container
* Changed data architecture
* Minor fixes

# tima 2.5.6

* Dependencies removal (e.g. metabo-store)
* Minor fixes
* Partial functions cleanup

# tima 2.5.5

* Automation and parameters improvement
* Minor fixes

# tima 2.5.4

* Minor fixes
* Metadata completion improvement
* Molecular formula and adducts formalism improvement

# tima 2.5.3

* Imports improvements
* LOTUS update

# tima 2.5.2

* Packaging improvements

# tima 2.5.1

* Improved support for SIRIUS (with new summaries)

# tima 2.5.0

* LOTUS update
* Minor fixes

# tima 2.4.0

* Added chemical names and xlogp to output (#33)
* Added support for case when no consensus is found (#30)
* Improved output (#34)
* Minor fixes

# tima 2.3.0

* Added support for annotation without MN (#28)
* Added support for multi tool annotations (#27)
* Added support for classical MN GNPS jobs (#25)
* Added support for new version of LOTUS
* General improvements for manual inputs
* Improved tests code coverage
* Minor fixes
* Updated adducts

# tima 2.2.2

* Additional benchmark figure ([Candidates distribution](https://taxonomicallyinformedannotation.github.io/tima/articles/IV-benchmarking.html#candidates-distribution))
* Minor fixes

# tima 2.2.1

* Minor version name fixes

# tima 2.2.0

* Added benchmark ([here](https://taxonomicallyinformedannotation.github.io/tima/articles/IV-benchmarking.html))
* Various fixes

# tima 2.1.0

* Fixes, deletion of binary dependencies.

# tima 2.0.0

* Initial version.
