

<!-- README.md is generated from README.qmd. Please edit that file -->

# tima <img src="https://raw.githubusercontent.com/taxonomicallyinformedannotation/tima/main/man/figures/logo.svg" align="right" height="139"/>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/tima.png)](https://CRAN.R-project.org/package=tima)
[![R-CMD-check](https://github.com/taxonomicallyinformedannotation/tima/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/taxonomicallyinformedannotation/tima/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/taxonomicallyinformedannotation/tima/graph/badge.svg)](https://app.codecov.io/gh/taxonomicallyinformedannotation/tima)
[![r-universe
badge](https://taxonomicallyinformedannotation.r-universe.dev/tima/badges/version?&color=blue&style=classic.png)](https://taxonomicallyinformedannotation.r-universe.dev/tima)
[![Docker](https://badgen.net/badge/docker/tima-r?icon&label.png)](https://hub.docker.com/r/adafede/tima-r/)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5797920.svg)](https://doi.org/10.5281/zenodo.5797920)

<!-- badges: end -->

The initial work is available at
<https://doi.org/10.3389/fpls.2019.01329>, with many improvements made
since then. The workflow is illustrated below.

![Workflow](https://raw.githubusercontent.com/taxonomicallyinformedannotation/tima/main/man/figures/tima.svg)  

This repository contains everything needed to perform **T**axonomically
**I**nformed **M**etabolite **A**nnotation.

## Requirements

Here is what you *minimally* need:

- A feature list (.csv) (see [example
  features](https://github.com/taxonomicallyinformedannotation/tima-example-files/blob/main/example_features.csv))
- A spectral file corresponding to the feature list (.mgf) (see [example
  spectra](https://github.com/taxonomicallyinformedannotation/tima-example-files/blob/main/example_spectra_mini.mgf))
- The biological source(s) of the sample(s) you are annotating (.csv)
  (see [example
  metadata](https://github.com/taxonomicallyinformedannotation/tima-example-files/blob/main/example_metadata.tsv))
  (File is optional if only a single organism)

Optionally, you may want to add:

- An in-house structure-organism pairs library (we provide
  [**LOTUS**](https://lotusnprod.github.io/lotus-manuscript/) as
  starting point for each user)
- Your own manual or automated annotations (we currently support
  annotations coming from SIRIUS (with some limitations))

## Installation

As the package is not (yet) available on CRAN, you will need to install
with:

``` r
install.packages(
  "tima",
  repos = c(
    "https://taxonomicallyinformedannotation.r-universe.dev",
    "https://bioc.r-universe.dev",
    "https://cloud.r-project.org"
  )
)
```

Then, you should be able to install the rest with:

``` r
tima::install()
```

Normally, everything you need should then be installed (as tested in
[here](https://github.com/taxonomicallyinformedannotation/tima-r/actions/workflows/external-use.yaml)).
If for some reason, some packages were not installed, try to install
them manually. To avoid such issues, we offer a containerized version
(see [Docker](#docker)).

Once installed, you are ready to go through our
[documentation](https://taxonomicallyinformedannotation.github.io/tima/articles/),
with the major steps detailed.

In case you do not have your data ready, you can obtain some example
data using:

``` r
tima::get_example_files()
```

Once you are done, you can open a small GUI to adapt your parameters and
launch your job:

``` r
tima::run_app()
```

This command will open a small app in your default browser.

### Docker

A container is also available, together with a small compose file. Main
commands are below:

``` bash
docker pull adafede/tima-r
# docker build . -t adafede/tima-r
```

``` bash
docker run --user tima-user -v "$(pwd)/.tima/data:/home/tima-user/.tima/data" -p 3838:3838 adafede/tima-r Rscript -e "tima::run_app()"
# docker run --user tima-user -v "$(pwd)/.tima/data:/home/tima-user/.tima/data" adafede/tima-r Rscript -e "tima::tima_full()"
```

## Main Citations

According to which steps you used, please give credit to the authors of
the tools/resources used.

### TIMA

General: <https://doi.org/10.3389/fpls.2019.01329>

⚠️ Do not forget to cite which version you used:
<https://doi.org/10.5281/zenodo.5797920>

### LOTUS

General: <https://doi.org/10.7554/eLife.70780>

⚠️ Do not forget to cite which version you used:
<https://doi.org/10.5281/zenodo.5794106>

### ISDB

General: <https://doi.org/10.1021/acs.analchem.5b04804>

⚠️ Do not forget to cite which version you used:
<https://doi.org/10.5281/zenodo.5607185>

### GNPS

General: <https://doi.org/10.1038/nbt.3597>

### SIRIUS

General: <https://doi.org/10.1038/s41592-019-0344-8>

- *CSI:FingerId*: <https://doi.org/10.1073/pnas.1509788112>
- *ZODIAC*: <https://doi.org/10.1038/s42256-020-00234-6>
- *CANOPUS*: <https://doi.org/10.1038/s41587-020-0740-8>
- *COSMIC*: <https://doi.org/10.1038/s41587-021-01045-9>

### Others

- ECMDB 2.0: <https://doi.org/10.1093/nar/gkv1060>
- HMDB 5.0: <https://doi.org/10.1093/nar/gkab1062>
- MassBank: <https://doi.org/10.5281/zenodo.3378723>
- Merlin: <https://doi.org/10.5281/zenodo.13911806>
- NPClassifier: <https://doi.org/10.1021/acs.jnatprod.1c00399>

## Additional software credits

| Package         | Version | Citation                                       |
|:----------------|:--------|:-----------------------------------------------|
| archive         | 1.1.12  | @archive                                       |
| base            | 4.5.1   | @base                                          |
| BiocManager     | 1.30.26 | @BiocManager                                   |
| BiocParallel    | 1.42.1  | @BiocParallel                                  |
| BiocVersion     | 3.21.1  | @BiocVersion                                   |
| docopt          | 0.7.2   | @docopt                                        |
| DT              | 0.33    | @DT                                            |
| fs              | 1.6.6   | @fs                                            |
| gt              | 1.0.0   | @gt                                            |
| httr2           | 1.2.1   | @httr2                                         |
| igraph          | 2.1.4   | @igraph2006; @igraph2025                       |
| IRanges         | 2.42.0  | @IRanges                                       |
| knitr           | 1.50    | @knitr2014; @knitr2015; @knitr2025             |
| logger          | 0.4.0   | @logger                                        |
| MetaboCoreUtils | 1.16.1  | @MetaboCoreUtils                               |
| MsBackendMgf    | 1.16.0  | @MsBackendMgf                                  |
| MsBackendMsp    | 1.12.0  | @MsBackendMsp                                  |
| MsCoreUtils     | 1.20.0  | @MsCoreUtils                                   |
| msentropy       | 0.1.4   | @msentropy                                     |
| reticulate      | 1.43.0  | @reticulate                                    |
| rmarkdown       | 2.29    | @rmarkdown2018; @rmarkdown2020; @rmarkdown2024 |
| rotl            | 3.1.0   | @rotl2016; @rotl2019                           |
| shiny           | 1.11.1  | @shiny                                         |
| shinybusy       | 0.3.3   | @shinybusy                                     |
| shinyhelper     | 0.3.2   | @shinyhelper                                   |
| shinyjs         | 2.1.0   | @shinyjs                                       |
| shinytest2      | 0.4.1   | @shinytest2                                    |
| shinyvalidate   | 0.1.3   | @shinyvalidate                                 |
| shinyWidgets    | 0.9.0   | @shinyWidgets                                  |
| Spectra         | 1.18.2  | @Spectra                                       |
| stringi         | 1.8.7   | @stringi                                       |
| targets         | 1.11.3  | @targets                                       |
| testthat        | 3.2.3   | @testthat                                      |
| tidyfst         | 1.8.2   | @tidyfst                                       |
| tidyselect      | 1.2.1   | @tidyselect                                    |
| tidytable       | 0.11.2  | @tidytable                                     |
| tidyverse       | 2.0.0   | @tidyverse                                     |
| tima            | 2.12.0  | @tima2019; @tima2025                           |
| visNetwork      | 2.1.2   | @visNetwork                                    |
| yaml            | 2.3.10  | @yaml                                          |
