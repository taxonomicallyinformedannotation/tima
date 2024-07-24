# timaR

# timaR 2.9.7

* Added possibility to add internal libraries through the GUI (#159)
* Added number of peaks in spectrum
* Clearer handling of SIRIUS scores (#146, #147)
* Exposed more parameters to the GUI (#159)
* Fixed adducts and remove nitrogen rule
* Fixed number of matched peaks
* Improved imports
* Reduced warnings
* Updated benchmarking steps

# timaR 2.9.6

* Added light-switch thanks to `pkgdown 2.1.0`.
* Attempt to simplify installation
* Fixed library/adducts confusion (#123)
* Fixed some incorrect adduct differences annotations
* Refactored adducts / neutral losses / dimers annotation to allow for more flexibility (#141, #144)

# timaR 2.9.5

* Do not re-package if already the latest version
* SIRIUS 6 default and compatible (keeping SIRIUS 5 backward compatibility)
* Updated to Massbank version `2024.06`

# timaR 2.9.4

* Automated update
* Added an option to remove ties (#134)
* Added some details for SIRIUS, add manual workspace addition (#132)
* Additional preprocessing (reduction) of noisy spectra
* Dependencies update
* Docker updates (#131)
* Handle cases when same (feature_id, mslevel) pairs are present within an MGF (#133)
* Improved documentation
* New working directory at `$HOME/.tima`
* Updated R and Bioconductor versions

# timaR 2.9.3

* Allowed for SIRIUS jobs containing only summaries
* Allowed for underscores in job pattern
* Changed some default values (less stringent)
* Dependencies update
* Migrated app testing to `shinytest2`
* Removed further some inconsistent MS1 annotations
* Removed tests dependencies by default

# timaR 2.9.2

* Added Nitrogen rule to filter out some annotations
* Better handling of partial downloads (#118)
* Dependencies update (mainly `targets 1.5.1`, will invalidate previous targets)
* Fixed some port issues in Shiny (#122)
* Removed completely empty columns from final output to avoid confusion (#120)

# timaR 2.9.1

* Added [Waystation](https://caltechlibrary.github.io/waystation/) action
* Added structures from spectral libraries to SOP library (#113)
* Exposed all parameters (#107, #108)
* Fixed for Zenodo API
* HMDB structures support
* Optimized grep/gsub by adding `perl=TRUE` or `fixed=TRUE`
* Updated to Massbank version `2023.11`
* Updated SIRIUS preparation (#74, #115)

# timaR 2.9.0

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

# timaR 2.8.2

* Added spectral entropy
* Added MS1 only possibility
* Added Fluorine adduct
* Changed from pbmclapply to pblapply
* Documentation improvement
* Fixed empty chemical classes
* Fixed not classified taxa
* Github Actions improvement
* [renv](https://rstudio.github.io/renv/index.html) removal
* Performance improvement by replacing the [tidyverse](https://www.tidyverse.org) by the [fastverse](https://fastverse.github.io/fastverse) (in progress)
* Reduced warnings (CRAN and jscpd)

# timaR 2.8.1

* Adapted tests
* Added `retry` parameter to `get_organism_taxonomy_ott`
* Dependencies update
* Minor fixes
* Moved `/params` and `paths.yaml` to `/inst` as more standard. (see <https://r-pkgs.org/misc.html#other-directories>)
* Performance improvement by replacing the [tidyverse](https://www.tidyverse.org) by the [fastverse](https://fastverse.github.io/fastverse) (in progress)
* Replaced `extdata` loading

# timaR 2.8.0

* Added GUI prototype
* Started using [renv](https://rstudio.github.io/renv/index.html)

# timaR 2.7.4

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

# timaR 2.7.3

* Improved calculations over redundant formulas
* Minor fixes
* Parameters refactoring
* Spectral matching update (see <https://github.com/rformassspectrometry/MetaboAnnotation/issues/93>)
* [Targets](https://books.ropensci.org/targets/) implementation

# timaR 2.7.2

* Benchmark update (including negative mode)
* Improved parameters documentation
* Minor fixes
* Spectral comparison + intensity filtering update
* Switched r-base Docker image to bioconductor with ARM support

# timaR 2.7.1

* Added MONA helpers
* Added parallelization on process_spectra
* Added sqlite storing for spectra
* Improved code documentation
* Improved testing time
* Minor fixes

# timaR 2.7.0

* Added HMDB helpers for both taxo and ISDB
* Added MS2 annotation capability (kudos @jorainer for the awesome *Spectra* suite)
* Minor fixes

# timaR 2.6.0

* Added Docker container
* Changed data architecture
* Minor fixes

# timaR 2.5.6

* Dependencies removal (e.g. metabo-store)
* Minor fixes
* Partial functions cleanup

# timaR 2.5.5

* Automation and parameters improvement
* Minor fixes

# timaR 2.5.4

* Minor fixes
* Metadata completion improvement
* Molecular formula and adducts formalism improvement

# timaR 2.5.3

* Imports improvements
* LOTUS update

# timaR 2.5.2

* Packaging improvements

# timaR 2.5.1

* Improved support for SIRIUS (with new summaries)

# timaR 2.5.0

* LOTUS update
* Minor fixes

# timaR 2.4.0

* Added chemical names and xlogp to output (#33)
* Added support for case when no consensus is found (#30)
* Improved output (#34)
* Minor fixes

# timaR 2.3.0

* Added support for annotation without MN (#28)
* Added support for multi tool annotations (#27)
* Added support for classical MN GNPS jobs (#25)
* Added support for new version of LOTUS
* General improvements for manual inputs
* Improved tests code coverage
* Minor fixes
* Updated adducts

# timaR 2.2.2

* Additional benchmark figure ([Candidates distribution](https://taxonomicallyinformedannotation.github.io/tima-r/articles/V-actual-performance.html#candidates-distribution))
* Minor fixes

# timaR 2.2.1

* Minor version name fixes

# timaR 2.2.0

* Added benchmark ([here](https://taxonomicallyinformedannotation.github.io/tima-r/articles/V-actual-performance.html))
* Various fixes

# timaR 2.1.0

* Fixes, deletion of binary dependencies.

# timaR 2.0.0

* Initial version.
