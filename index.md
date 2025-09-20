

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

| Package | Version | Citation |
|:---|:---|:---|
| archive | 1.1.12 | Hester and Csárdi (2025) |
| base | 4.5.1 | R Core Team (2025) |
| BiocManager | 1.30.26 | Morgan and Ramos (2025) |
| BiocParallel | 1.42.1 | Wang et al. (2025) |
| BiocVersion | 3.21.1 | Morgan (2025) |
| docopt | 0.7.2 | de Jonge (2025) |
| DT | 0.34.0 | Xie, Cheng, and Tan (2024) |
| fs | 1.6.6 | Hester, Wickham, and Csárdi (2025) |
| gt | 1.0.0 | Iannone et al. (2025) |
| httr2 | 1.2.1 | Wickham (2025) |
| igraph | 2.1.4 | Csardi and Nepusz (2006); Csárdi et al. (2025) |
| IRanges | 2.42.0 | Lawrence et al. (2013) |
| knitr | 1.50 | Xie (2014); Xie (2015); Xie (2025) |
| logger | 0.4.1 | Daróczi and Wickham (2024) |
| MetaboCoreUtils | 1.16.1 | Rainer et al. (2022a) |
| MsBackendMgf | 1.16.0 | Gatto, Rainer, and Gibb (2025) |
| MsBackendMsp | 1.12.0 | Rainer et al. (2022b) |
| MsCoreUtils | 1.20.0 | Rainer et al. (2022c) |
| msentropy | 0.1.4 | Li (2023) |
| reticulate | 1.43.0 | Ushey, Allaire, and Tang (2025) |
| rmarkdown | 2.29 | Xie, Allaire, and Grolemund (2018); Xie, Dervieux, and Riederer (2020); Allaire et al. (2024) |
| rotl | 3.1.0 | Michonneau, Brown, and Winter (2016); OpenTreeOfLife et al. (2019) |
| shiny | 1.11.1 | Chang et al. (2025) |
| shinybusy | 0.3.3 | Meyer and Perrier (2024) |
| shinyhelper | 0.3.2 | Mason-Thom (2019) |
| shinyjs | 2.1.0 | Attali (2021) |
| shinytest2 | 0.4.1 | Schloerke (2025) |
| shinyvalidate | 0.1.3 | Sievert, Iannone, and Cheng (2023) |
| shinyWidgets | 0.9.0 | Perrier, Meyer, and Granjon (2025) |
| Spectra | 1.18.2 | Rainer et al. (2022d) |
| stringi | 1.8.7 | Gagolewski (2022) |
| targets | 1.11.4 | Landau (2021) |
| testthat | 3.2.3 | Wickham (2011) |
| tidyfst | 1.8.2 | Huang and Zhao (2020) |
| tidyselect | 1.2.1 | Henry and Wickham (2024) |
| tidytable | 0.11.2 | Fairbanks (2024) |
| tidyverse | 2.0.0 | Wickham et al. (2019) |
| tima | 2.12.0 | Rutz et al. (2019); Rutz and Allard (2025) |
| visNetwork | 2.1.4 | Almende B.V. and Contributors and Thieurmel (2022) |
| yaml | 2.3.10 | Garbett et al. (2024) |

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-rmarkdown2024" class="csl-entry">

Allaire, JJ, Yihui Xie, Christophe Dervieux, Jonathan McPherson, Javier
Luraschi, Kevin Ushey, Aron Atkins, et al. 2024.
*<span class="nocase">rmarkdown</span>: Dynamic Documents for r*.
<https://github.com/rstudio/rmarkdown>.

</div>

<div id="ref-visNetwork" class="csl-entry">

Almende B.V. and Contributors, and Benoit Thieurmel. 2022.
*<span class="nocase">visNetwork</span>: Network Visualization Using
“<span class="nocase">vis.js</span>” Library*.
<https://doi.org/10.32614/CRAN.package.visNetwork>.

</div>

<div id="ref-shinyjs" class="csl-entry">

Attali, Dean. 2021. *<span class="nocase">shinyjs</span>: Easily Improve
the User Experience of Your Shiny Apps in Seconds*.
<https://doi.org/10.32614/CRAN.package.shinyjs>.

</div>

<div id="ref-shiny" class="csl-entry">

Chang, Winston, Joe Cheng, JJ Allaire, Carson Sievert, Barret Schloerke,
Yihui Xie, Jeff Allen, Jonathan McPherson, Alan Dipert, and Barbara
Borges. 2025. *<span class="nocase">shiny</span>: Web Application
Framework for r*. <https://doi.org/10.32614/CRAN.package.shiny>.

</div>

<div id="ref-igraph2006" class="csl-entry">

Csardi, Gabor, and Tamas Nepusz. 2006. “The Igraph Software Package for
Complex Network Research.” *InterJournal* Complex Systems: 1695.
<https://igraph.org>.

</div>

<div id="ref-igraph2025" class="csl-entry">

Csárdi, Gábor, Tamás Nepusz, Vincent Traag, Szabolcs Horvát, Fabio
Zanini, Daniel Noom, and Kirill Müller. 2025.
*<span class="nocase">igraph</span>: Network Analysis and Visualization
in r*. <https://doi.org/10.5281/zenodo.7682609>.

</div>

<div id="ref-logger" class="csl-entry">

Daróczi, Gergely, and Hadley Wickham. 2024.
*<span class="nocase">logger</span>: A Lightweight, Modern and Flexible
Logging Utility*. <https://doi.org/10.32614/CRAN.package.logger>.

</div>

<div id="ref-docopt" class="csl-entry">

de Jonge, Edwin. 2025. *<span class="nocase">docopt</span>: Command-Line
Interface Specification Language*.
<https://doi.org/10.32614/CRAN.package.docopt>.

</div>

<div id="ref-tidytable" class="csl-entry">

Fairbanks, Mark. 2024. *<span class="nocase">tidytable</span>: Tidy
Interface to “<span class="nocase">data.table</span>”*.
<https://doi.org/10.32614/CRAN.package.tidytable>.

</div>

<div id="ref-stringi" class="csl-entry">

Gagolewski, Marek. 2022. “<span class="nocase">stringi</span>: Fast and
Portable Character String Processing in R.” *Journal of Statistical
Software* 103 (2): 1–59. <https://doi.org/10.18637/jss.v103.i02>.

</div>

<div id="ref-yaml" class="csl-entry">

Garbett, Shawn P, Jeremy Stephens, Kirill Simonov, Yihui Xie, Zhuoer
Dong, Hadley Wickham, Jeffrey Horner, et al. 2024.
*<span class="nocase">yaml</span>: Methods to Convert r Data to YAML and
Back*. <https://doi.org/10.32614/CRAN.package.yaml>.

</div>

<div id="ref-MsBackendMgf" class="csl-entry">

Gatto, Laurent, Johannes Rainer, and Sebastian Gibb. 2025.
*MsBackendMgf: Mass Spectrometry Data Backend for Mascot Generic Format
(Mgf) Files*. <https://doi.org/10.18129/B9.bioc.MsBackendMgf>.

</div>

<div id="ref-tidyselect" class="csl-entry">

Henry, Lionel, and Hadley Wickham. 2024.
*<span class="nocase">tidyselect</span>: Select from a Set of Strings*.
<https://doi.org/10.32614/CRAN.package.tidyselect>.

</div>

<div id="ref-archive" class="csl-entry">

Hester, Jim, and Gábor Csárdi. 2025.
*<span class="nocase">archive</span>: Multi-Format Archive and
Compression Support*. <https://doi.org/10.32614/CRAN.package.archive>.

</div>

<div id="ref-fs" class="csl-entry">

Hester, Jim, Hadley Wickham, and Gábor Csárdi. 2025.
*<span class="nocase">fs</span>: Cross-Platform File System Operations
Based on “<span class="nocase">libuv</span>”*.
<https://doi.org/10.32614/CRAN.package.fs>.

</div>

<div id="ref-tidyfst" class="csl-entry">

Huang, Tian-Yuan, and Bin Zhao. 2020.
“<span class="nocase">tidyfst</span>: Tidy Verbs for Fast Data
Manipulation.” *Journal of Open Source Software* 5 (52): 2388.
<https://doi.org/10.21105/joss.02388>.

</div>

<div id="ref-gt" class="csl-entry">

Iannone, Richard, Joe Cheng, Barret Schloerke, Ellis Hughes, Alexandra
Lauer, JooYoung Seo, Ken Brevoort, and Olivier Roy. 2025.
*<span class="nocase">gt</span>: Easily Create Presentation-Ready
Display Tables*. <https://doi.org/10.32614/CRAN.package.gt>.

</div>

<div id="ref-targets" class="csl-entry">

Landau, William Michael. 2021. “The Targets r Package: A Dynamic
Make-Like Function-Oriented Pipeline Toolkit for Reproducibility and
High-Performance Computing.” *Journal of Open Source Software* 6 (57):
2959. <https://doi.org/10.21105/joss.02959>.

</div>

<div id="ref-IRanges" class="csl-entry">

Lawrence, Michael, Wolfgang Huber, Hervé Pagès, Patrick Aboyoun, Marc
Carlson, Robert Gentleman, Martin Morgan, and Vincent Carey. 2013.
“Software for Computing and Annotating Genomic Ranges.” *PLoS
Computational Biology* 9.
<https://doi.org/10.1371/journal.pcbi.1003118>.

</div>

<div id="ref-msentropy" class="csl-entry">

Li, Yuanyue. 2023. *<span class="nocase">msentropy</span>: Spectral
Entropy for Mass Spectrometry Data*.
<https://doi.org/10.32614/CRAN.package.msentropy>.

</div>

<div id="ref-shinyhelper" class="csl-entry">

Mason-Thom, Chris. 2019. *<span class="nocase">shinyhelper</span>:
Easily Add Markdown Help Files to “<span class="nocase">shiny</span>”
App Elements*. <https://doi.org/10.32614/CRAN.package.shinyhelper>.

</div>

<div id="ref-shinybusy" class="csl-entry">

Meyer, Fanny, and Victor Perrier. 2024.
*<span class="nocase">shinybusy</span>: Busy Indicators and
Notifications for “Shiny” Applications*.
<https://doi.org/10.32614/CRAN.package.shinybusy>.

</div>

<div id="ref-rotl2016" class="csl-entry">

Michonneau, Francois, Joseph W. Brown, and David J. Winter. 2016.
“<span class="nocase">rotl</span>: An r Package to Interact with the
Open Tree of Life Data.” *Methods in Ecology and Evolution* 7 (12):
1476–81. <https://doi.org/10.1111/2041-210X.12593>.

</div>

<div id="ref-BiocVersion" class="csl-entry">

Morgan, Martin. 2025. *BiocVersion: Set the Appropriate Version of
Bioconductor Packages*. <https://doi.org/10.18129/B9.bioc.BiocVersion>.

</div>

<div id="ref-BiocManager" class="csl-entry">

Morgan, Martin, and Marcel Ramos. 2025. *BiocManager: Access the
Bioconductor Project Package Repository*.
<https://doi.org/10.32614/CRAN.package.BiocManager>.

</div>

<div id="ref-rotl2019" class="csl-entry">

OpenTreeOfLife, Benjamin Redelings, Luna Luisa Sanchez Reyes, Karen A.
Cranston, Jim Allman, Mark T. Holder, and Emily Jane McTavish. 2019.
“Open Tree of Life Synthetic Tree.” Zenodo.
<https://doi.org/10.5281/zenodo.3937741>.

</div>

<div id="ref-shinyWidgets" class="csl-entry">

Perrier, Victor, Fanny Meyer, and David Granjon. 2025.
*<span class="nocase">shinyWidgets</span>: Custom Inputs Widgets for
Shiny*. <https://doi.org/10.32614/CRAN.package.shinyWidgets>.

</div>

<div id="ref-base" class="csl-entry">

R Core Team. 2025. *R: A Language and Environment for Statistical
Computing*. Vienna, Austria: R Foundation for Statistical Computing.
<https://www.R-project.org/>.

</div>

<div id="ref-MetaboCoreUtils" class="csl-entry">

Rainer, Johannes, Andrea Vicini, Liesa Salzer, Jan Stanstrup, Josep M.
Badia, Steffen Neumann, Michael A. Stravs, et al. 2022a. “A Modular and
Expandable Ecosystem for Metabolomics Data Annotation in r.”
*Metabolites* 12: 173. <https://doi.org/10.3390/metabo12020173>.

</div>

<div id="ref-MsBackendMsp" class="csl-entry">

———, et al. 2022b. “A Modular and Expandable Ecosystem for Metabolomics
Data Annotation in r.” *Metabolites* 12: 173.
<https://doi.org/10.3390/metabo12020173>.

</div>

<div id="ref-MsCoreUtils" class="csl-entry">

———, et al. 2022c. “A Modular and Expandable Ecosystem for Metabolomics
Data Annotation in r.” *Metabolites* 12: 173.
<https://doi.org/10.3390/metabo12020173>.

</div>

<div id="ref-Spectra" class="csl-entry">

———, et al. 2022d. “A Modular and Expandable Ecosystem for Metabolomics
Data Annotation in r.” *Metabolites* 12: 173.
<https://doi.org/10.3390/metabo12020173>.

</div>

<div id="ref-tima2025" class="csl-entry">

Rutz, Adriano, and Pierre-Marie Allard. 2025.
*<span class="nocase">tima</span>: Taxonomically Informed Metabolite
Annotation*. <https://doi.org/10.5281/zenodo.5797920>.

</div>

<div id="ref-tima2019" class="csl-entry">

Rutz, Adriano, Miwa Dounoue-Kubo, Simon Ollivier, Jonathan Bisson,
Mohsen Bagheri, Tongchai Saesong, Samad Nejad Ebrahimi, Kornkanok
Ingkaninan, Jean-Luc Wolfender, and Pierre-Marie Allard. 2019.
“Taxonomically Informed Scoring Enhances Confidence in Natural Products
Annotation.” *Frontiers in Plant Science* 10.
<https://doi.org/10.3389/FPLS.2019.01329>.

</div>

<div id="ref-shinytest2" class="csl-entry">

Schloerke, Barret. 2025. *Shinytest2: Testing for Shiny Applications*.
<https://doi.org/10.32614/CRAN.package.shinytest2>.

</div>

<div id="ref-shinyvalidate" class="csl-entry">

Sievert, Carson, Richard Iannone, and Joe Cheng. 2023.
*<span class="nocase">shinyvalidate</span>: Input Validation for Shiny
Apps*. <https://doi.org/10.32614/CRAN.package.shinyvalidate>.

</div>

<div id="ref-reticulate" class="csl-entry">

Ushey, Kevin, JJ Allaire, and Yuan Tang. 2025.
*<span class="nocase">reticulate</span>: Interface to “Python”*.
<https://doi.org/10.32614/CRAN.package.reticulate>.

</div>

<div id="ref-BiocParallel" class="csl-entry">

Wang, Jiefei, Martin Morgan, Valerie Obenchain, Michel Lang, Ryan
Thompson, and Nitesh Turaga. 2025. *BiocParallel: Bioconductor
Facilities for Parallel Evaluation*.
<https://doi.org/10.18129/B9.bioc.BiocParallel>.

</div>

<div id="ref-testthat" class="csl-entry">

Wickham, Hadley. 2011. “<span class="nocase">testthat</span>: Get
Started with Testing.” *The R Journal* 3: 5–10.
<https://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf>.

</div>

<div id="ref-httr2" class="csl-entry">

———. 2025. *Httr2: Perform HTTP Requests and Process the Responses*.
<https://doi.org/10.32614/CRAN.package.httr2>.

</div>

<div id="ref-tidyverse" class="csl-entry">

Wickham, Hadley, Mara Averick, Jennifer Bryan, Winston Chang, Lucy
D’Agostino McGowan, Romain François, Garrett Grolemund, et al. 2019.
“Welcome to the <span class="nocase">tidyverse</span>.” *Journal of Open
Source Software* 4 (43): 1686. <https://doi.org/10.21105/joss.01686>.

</div>

<div id="ref-knitr2014" class="csl-entry">

Xie, Yihui. 2014. “<span class="nocase">knitr</span>: A Comprehensive
Tool for Reproducible Research in R.” In *Implementing Reproducible
Computational Research*, edited by Victoria Stodden, Friedrich Leisch,
and Roger D. Peng. Chapman; Hall/CRC.

</div>

<div id="ref-knitr2015" class="csl-entry">

———. 2015. *Dynamic Documents with R and Knitr*. 2nd ed. Boca Raton,
Florida: Chapman; Hall/CRC. <https://yihui.org/knitr/>.

</div>

<div id="ref-knitr2025" class="csl-entry">

———. 2025. *<span class="nocase">knitr</span>: A General-Purpose Package
for Dynamic Report Generation in R*. <https://yihui.org/knitr/>.

</div>

<div id="ref-rmarkdown2018" class="csl-entry">

Xie, Yihui, J. J. Allaire, and Garrett Grolemund. 2018. *R Markdown: The
Definitive Guide*. Boca Raton, Florida: Chapman; Hall/CRC.
<https://bookdown.org/yihui/rmarkdown>.

</div>

<div id="ref-DT" class="csl-entry">

Xie, Yihui, Joe Cheng, and Xianying Tan. 2024. *DT: A Wrapper of the
JavaScript Library “DataTables”*.
<https://doi.org/10.32614/CRAN.package.DT>.

</div>

<div id="ref-rmarkdown2020" class="csl-entry">

Xie, Yihui, Christophe Dervieux, and Emily Riederer. 2020. *R Markdown
Cookbook*. Boca Raton, Florida: Chapman; Hall/CRC.
<https://bookdown.org/yihui/rmarkdown-cookbook>.

</div>

</div>
