# timaR

# timaR 2.9.3

* Allow for SIRIUS jobs containing only summaries
* Allow for underscores in job pattern
* Change some default values (less stringent)
* Dependencies update
* Migrate app testing to `shinytest2`
* Remove further some inconsistent MS1 annotations

# timaR 2.9.2

* Add Nitrogen rule to filter out some annotations
* Better handling of partial downloads (#118)
* Dependencies update (mainly `targets 1.5.1`, will invalidate previous targets)
* Fix some port issues in Shiny (#122)
* Remove completely empty columns from final output to avoid confusion (#120)

# timaR 2.9.1

* Add [Waystation](https://caltechlibrary.github.io/waystation/) action
* Add structures from spectral libraries to SOP library (#113)
* Expose all parameters (#107, #108)
* Fix for Zenodo API
* HMDB structures support
* Optimize grep/gsub by adding `perl=TRUE` or `fixed=TRUE`
* Update to Massbank version `2023.11`
* Update SIRIUS preparation (#74, #115)

# timaR 2.9.0

* Added compounds names as parameter
* Added MassBank spectral library (#77)
* Allow files outside `data/source` (#89)
* Added RT library as annotation library (#86)
* Be less dependent of GNPS by default
* Better handling of download errors
* Fixed Docker mount path
* Improved naming (#91)
* Internal variables refactoring
* New adducts (#79, #80)
* Refactored adducts, clusters and neutral losses
* Refactored biological and chemical score
* Refactored RT matching (#76)
* Refactored Sirius scores (#92)
* Multiple Shiny fixes and tests addition (#60)
* Multiple fixes (#71, #81, #82)

# timaR 2.8.2

* Change from pbmclapply to pblapply
* Added spectral entropy
* Added MS1 only possibility
* Added Fluorine adduct
* Fix empty chemical classes
* Fix not classified taxa
* [renv](https://rstudio.github.io/renv/index.html) removal
* Performance improvement by replacing the [tidyverse](https://www.tidyverse.org) by the [fastverse](https://fastverse.github.io/fastverse) (in progress)
* Github Actions improvement
* Documentation improvement
* Reduced warnings (CRAN and jscpd)

# timaR 2.8.1

* Added `retry` parameter to `get_organism_taxonomy_ott`
* Dependencies update
* Replace `extdata` loading
* Moved `/params` and `paths.yaml` to `/inst` as more standard. (see <https://r-pkgs.org/misc.html#other-directories>)
* Adapted tests
* Performance improvement by replacing the [tidyverse](https://www.tidyverse.org) by the [fastverse](https://fastverse.github.io/fastverse) (in progress)
* Minor fixes

# timaR 2.8.0

* Adding GUI prototype
* Started using [renv](https://rstudio.github.io/renv/index.html)

# timaR 2.7.4

* ECMDB support
* Edges (mass and spectra-based) and components are generated if not present.
* Fix case when no GNPS job ID
* Re-introducing Classyfire support.
* Retention time matching additionally to MS2 if RT present in library
* Parameters refactoring
* Steps refactoring
* Further [Targets](https://books.ropensci.org/targets/) improvements
* Clearer vocabulary
* Lot of fixes

# timaR 2.7.3

* [Targets](https://books.ropensci.org/targets/) implementation
* Parameters refactoring
* Improved calculations over redundant formulas
* Spectral matching update (see <https://github.com/rformassspectrometry/MetaboAnnotation/issues/93>)
* Minor fixes

# timaR 2.7.2

* Benchmark update (including negative mode)
* Spectral comparison + intensity filtering update
* Switched r-base Docker image to bioconductor with ARM support
* Improved parameters documentation
* Minor fixes

# timaR 2.7.1

* Added parallelization on process_spectra
* Added sqlite storing for spectra
* Added MONA helpers
* Improved testing time
* Improved code documentation
* Minor fixes

# timaR 2.7.0

* Added MS2 annotation capability (kudos @jorainer for the awesome *Spectra* suite)
* Added HMDB helpers for both taxo and ISDB
* Minor fixes

# timaR 2.6.0

* Added Docker container
* Changed data architecture
* Minor fixes

# timaR 2.5.6

* Minor fixes
* Partial functions cleanup
* Dependencies removal (e.g. metabo-store)

# timaR 2.5.5

* Minor fixes
* Automation and parameters improvement

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

* Minor fixes
* LOTUS update

# timaR 2.4.0

* Minor fixes
* Improved output (#34)
* Added chemical names and xlogp to output (#33)
* Added support for case when no consensus is found (#30)

# timaR 2.3.0

* Minor fixes
* Added support for annotation without MN (#28)
* Added support for multi tool annotations (#27)
* Added support for classical MN GNPS jobs (#25)
* Added support for new version of LOTUS
* General improvements for manual inputs
* Updated adducts
* Improved tests code coverage

# timaR 2.2.2

* Minor fixes
* Additional benchmark
  figure ([Candidates distribution](https://taxonomicallyinformedannotation.github.io/tima-r/articles/V-actual-performance.html#candidates-distribution)

# timaR 2.2.1

* Minor version name fixes

# timaR 2.2.0

* Various fixes
* Added benchmark ([here](https://taxonomicallyinformedannotation.github.io/tima-r/articles/V-actual-performance.html))

# timaR 2.1.0

* Fixes, deletion of binary dependencies.

# timaR 2.0.0

* Initial version.
