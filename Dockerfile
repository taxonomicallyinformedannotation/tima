FROM rocker/r-ver:4.2.2
RUN apt-get update && apt-get install -y  libcurl4-openssl-dev libfribidi-dev libharfbuzz-dev libicu-dev libpng-dev libssl-dev libtiff-dev libxml2-dev make pandoc zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_version("crayon",upgrade="never", version = "1.5.2")'
RUN Rscript -e 'remotes::install_version("tibble",upgrade="never", version = "3.1.8")'
RUN Rscript -e 'remotes::install_version("yaml",upgrade="never", version = "2.3.6")'
RUN Rscript -e 'remotes::install_version("stringr",upgrade="never", version = "1.4.1")'
RUN Rscript -e 'remotes::install_version("knitr",upgrade="never", version = "1.40")'
RUN Rscript -e 'remotes::install_version("curl",upgrade="never", version = "4.3.3")'
RUN Rscript -e 'remotes::install_version("purrr",upgrade="never", version = "0.3.5")'
RUN Rscript -e 'remotes::install_version("rmarkdown",upgrade="never", version = "2.17")'
RUN Rscript -e 'remotes::install_version("dplyr",upgrade="never", version = "1.0.10")'
RUN Rscript -e 'remotes::install_version("testthat",upgrade="never", version = "3.1.5")'
RUN Rscript -e 'remotes::install_version("roxygen2",upgrade="never", version = "7.2.1")'
RUN Rscript -e 'remotes::install_version("pkgdown",upgrade="never", version = "2.0.6")'
RUN Rscript -e 'remotes::install_version("covr",upgrade="never", version = "3.6.1")'
RUN Rscript -e 'remotes::install_version("tidyr",upgrade="never", version = "1.2.1")'
RUN Rscript -e 'remotes::install_version("rotl",upgrade="never", version = "3.0.14")'
RUN Rscript -e 'remotes::install_version("readr",upgrade="never", version = "2.1.3")'
RUN Rscript -e 'remotes::install_version("docopt",upgrade="never", version = "0.7.1")'
RUN Rscript -e 'remotes::install_version("data.table",upgrade="never", version = "1.14.4")'
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
RUN rm -rf /build_zone
