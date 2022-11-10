# Taxonomically Informed Metabolite Annotation <img src='man/figures/logo.svg' align="right" height="108" />

<!-- badges: start -->
[![Codecov test coverage](https://codecov.io/gh/taxonomicallyinformedannotation/tima-r/branch/main/graph/badge.svg)](https://app.codecov.io/gh/taxonomicallyinformedannotation/tima-r?branch=main)
[![R-CMD-check](https://github.com/taxonomicallyinformedannotation/tima-r/workflows/R-CMD-check/badge.svg)](https://github.com/taxonomicallyinformedannotation/tima-r/actions)
[![Docker](https://badgen.net/badge/icon/docker?icon=docker&label)](https://hub.docker.com/r/adafede/tima-r/)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

The initial work is available at https://doi.org/10.3389/fpls.2019.01329, and many improvements have been made since then.
The workflow is illustrated below.

![Workflow](man/figures/tima.svg)

This repository contains everything needed to perform **T**axonomically **I**nformed **M**etabolite **A**nnotation.

It is provided with an example from well-known pharmacopoeia plants.

Here is what you *minimally* need:

- A feature list with *or without* candidate annotations, if you are using GNPS, it can be your GNPS job ID.
- The source organism of the extract you are annotating, if you are associating metadata within GNPS, it can be your
  GNPS job ID.
- An edge list, if you are using GNPS, it can be your GNPS job ID.

Optionally, you may want to add:

- An in-house structure-organism pairs library (we provide **[LOTUS](https://lotusnprod.github.io/lotus-manuscript/)** as starting point for each user)
- Your own manual or automated annotations (we currently support annotations coming from ISDB and SIRIUS (with some limitations))

## Installation

As the package is not (yet) available on CRAN, you will need to install the development version, therefore:

On *nix systems:
```shell
Rscript -e 'if(!requireNamespace("remotes")){install.packages("remotes")}
remotes::install_github("taxonomicallyinformedannotation/tima-r")'
```

On Windows:
```shell
Rscript -e "if(!requireNamespace('remotes')){install.packages('remotes')}
remotes::install_github('taxonomicallyinformedannotation/tima-r')"
```

or in R directly:

```shell
if(!requireNamespace("remotes", quietly = TRUE))
    install.packages("remotes")
remotes::install_github("taxonomicallyinformedannotation/tima-r")
```

Once installed, you are ready to go through our [documentation](https://taxonomicallyinformedannotation.github.io/tima-r/articles/), with the major steps detailed.

Do not forget to clone the repository (it will allow you to access default architecture, files, and parameters):

```shell
git clone https://github.com/taxonomicallyinformedannotation/tima-r.git
cd tima-r
```

Then, to copy the defaut parameters:

```shell
Rscript inst/scripts/prepare_params.R
```

This command will also be useful in case you want to automate your workflow.
More details are given in the [General comments about the infrastructure](https://taxonomicallyinformedannotation.github.io/tima-r/articles/I-intro.html)

Once done, you can start with an example using:

```shell
Rscript inst/scripts/tima.R
```

### Docker

A container is also available, together with a small compose.
Main commands are below:

```shell
docker build . -t tima-r 
```

```shell
docker compose up prepare_params
docker compose up tima
```


## Main Citations

According to which steps you used, please give credit to the authors of the tools/resources used.

### TIMA 
General: [https://doi.org/10.3389/fpls.2019.01329](https://doi.org/10.3389/fpls.2019.01329)

⚠️ Do not forget to cite which version you used: [https://doi.org/10.5281/zenodo.5797920](https://doi.org/10.5281/zenodo.5797920) 

### LOTUS
General: [https://doi.org/10.7554/eLife.70780](https://doi.org/10.7554/eLife.70780)

⚠️ Do not forget to cite which version you used: [https://doi.org/10.5281/zenodo.5794106](https://doi.org/10.5281/zenodo.5794106) 

### ISDB
General: [https://doi.org/10.1021/acs.analchem.5b04804](https://doi.org/10.1021/acs.analchem.5b04804)

⚠️ Do not forget to cite which version you used: [https://doi.org/10.5281/zenodo.5607185](https://doi.org/10.5281/zenodo.5607185) 

### GNPS
General: [https://doi.org/10.1038/nbt.3597](https://doi.org/10.1038/nbt.3597)

### SIRIUS
General: [https://doi.org/10.1038/s41592-019-0344-8](https://doi.org/10.1038/s41592-019-0344-8)

  - *CSI:FingerId*: [https://doi.org/10.1073/pnas.1509788112](https://www.pnas.org/doi/full/10.1073/pnas.1509788112)
  - *ZODIAC*: [https://doi.org/10.1038/s42256-020-00234-6](https://doi.org/10.1038/s42256-020-00234-6)
  - *CANOPUS*: [https://doi.org/10.1038/s41587-020-0740-8](https://doi.org/10.1038/s41587-020-0740-8)
  - *COSMIC*: [https://doi.org/10.1038/s41587-021-01045-9](https://doi.org/10.1038/s41587-021-01045-9)

### Others
- NPClassifier: [https://doi.org/10.1021/acs.jnatprod.1c00399](https://pubs.acs.org/doi/10.1021/acs.jnatprod.1c00399)
- ROTL: [https://doi.org/10.1111/2041-210X.12593](https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.12593) 
