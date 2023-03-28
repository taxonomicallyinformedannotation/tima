# Taxonomically Informed Metabolite Annotation <img src='https://raw.githubusercontent.com/taxonomicallyinformedannotation/tima-r/main/man/figures/logo.svg' align="right" height="108" />

<!-- badges: start -->
[![Codecov test coverage](https://codecov.io/gh/taxonomicallyinformedannotation/tima-r/branch/main/graph/badge.svg)](https://app.codecov.io/gh/taxonomicallyinformedannotation/tima-r?branch=main)
[![R-CMD-check](https://github.com/taxonomicallyinformedannotation/tima-r/workflows/R-CMD-check/badge.svg)](https://github.com/taxonomicallyinformedannotation/tima-r/actions)
[![Docker](https://badgen.net/badge/icon/docker?icon=docker&label)](https://hub.docker.com/r/adafede/tima-r/)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

The initial work is available at <https://doi.org/10.3389/fpls.2019.01329>, with many improvements made since then.
The workflow is illustrated below.

![Workflow](https://raw.githubusercontent.com/taxonomicallyinformedannotation/tima-r/main/man/figures/tima.svg)

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

```
git clone https://github.com/taxonomicallyinformedannotation/tima-r.git
cd tima-r
Rscript inst/scripts/install.R
```
Once installed, you are ready to go through our [documentation](https://taxonomicallyinformedannotation.github.io/tima-r/articles/), with the major steps detailed.

In case you do not have your data ready, you can obtain some example data using:

```
Rscript inst/scripts/get_gnps_tables.R
```

Once you are done, you can open a small GUI to adapt your parameters and launch your job:

```
Rscript inst/scripts/run_app.R
```

This command will open a small app you can access by entering the following address into your browser: <http://localhost:3838>


### Docker

A container is also available, together with a small compose file.
Main commands are below:

```
docker build . -t adafede/tima-r 
```

```
docker compose up tima-gui
# docker compose up tima-mini
# docker compose up tima-full
```


## Main Citations

According to which steps you used, please give credit to the authors of the tools/resources used.

### TIMA

General: <https://doi.org/10.3389/fpls.2019.01329>

⚠️ Do not forget to cite which version you used: <https://doi.org/10.5281/zenodo.5797920>

### LOTUS

General: <https://doi.org/10.7554/eLife.70780>

⚠️ Do not forget to cite which version you used: <https://doi.org/10.5281/zenodo.5794106>

### ISDB

General: <https://doi.org/10.1021/acs.analchem.5b04804>

⚠️ Do not forget to cite which version you used: <https://doi.org/10.5281/zenodo.5607185>

### GNPS

General: <https://doi.org/10.1038/nbt.3597>

### SIRIUS

General: <https://doi.org/10.1038/s41592-019-0344-8>

  - *CSI:FingerId*: <https://doi.org/10.1073/pnas.1509788112>
  - *ZODIAC*: <https://doi.org/10.1038/s42256-020-00234-6>
  - *CANOPUS*: <https://doi.org/10.1038/s41587-020-0740-8>
  - *COSMIC*: <https://doi.org/10.1038/s41587-021-01045-9>

### Others

- The *RforMassSpectrometry* packages suite for MS2 matching: <https://doi.org/10.3390/metabo12020173>
- ECMDB 2.0: <https://doi.org/10.1093/nar/gkv1060>
- HMDB 5.0: <https://doi.org/10.1093/nar/gkab1062>
- NPClassifier: <https://doi.org/10.1021/acs.jnatprod.1c00399>
- ROTL: <https://doi.org/10.1111/2041-210X.12593>
