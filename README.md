
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tima <img src='man/figures/logo.svg' align="right" height="139" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/tima)](https://CRAN.R-project.org/package=tima)
[![R-CMD-check](https://github.com/taxonomicallyinformedannotation/tima/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/taxonomicallyinformedannotation/tima/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/taxonomicallyinformedannotation/tima/graph/badge.svg)](https://app.codecov.io/gh/taxonomicallyinformedannotation/tima)
[![Docker](https://badgen.net/badge/icon/docker?icon=docker&label)](https://hub.docker.com/r/adafede/tima/)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5797920.svg)](https://doi.org/10.5281/zenodo.5797920)
<!-- badges: end -->

The initial work is available at
<https://doi.org/10.3389/fpls.2019.01329>, with many improvements made
since then. The workflow is illustrated below.

<figure>
<img
src="https://raw.githubusercontent.com/taxonomicallyinformedannotation/tima/main/man/figures/tima.svg"
alt="Workflow" />
<figcaption aria-hidden="true">Workflow</figcaption>
</figure>

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
  **[LOTUS](https://lotusnprod.github.io/lotus-manuscript/)** as
  starting point for each user)
- Your own manual or automated annotations (we currently support
  annotations coming from SIRIUS (with some limitations))

## Installation

As the package is not (yet) available on CRAN, you will need to install
the development version [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("taxonomicallyinformedannotation/tima")
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
data (set of 7,000+ spectra) using:

``` r
tima::get_example_spectra()
tima::get_example_features()
tima::get_example_metadata()
tima::get_example_sirius()
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
docker compose up tima-gui
# docker compose up tima-full
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

- The *RforMassSpectrometry* packages suite for MS2 matching:
  <https://doi.org/10.3390/metabo12020173>
- ECMDB 2.0: <https://doi.org/10.1093/nar/gkv1060>
- HMDB 5.0: <https://doi.org/10.1093/nar/gkab1062>
- MassBank: <https://doi.org/10.5281/zenodo.3378723>
- NPClassifier: <https://doi.org/10.1021/acs.jnatprod.1c00399>
- ROTL: <https://doi.org/10.1111/2041-210X.12593>
- Spectral entropy: <https://doi.org/10.1038/s41592-021-01331-z>
