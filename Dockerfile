FROM rocker/r-ver:latest

# Generic install
RUN apt-get update && apt-get install -y  libcurl4-openssl-dev libfribidi-dev libharfbuzz-dev libicu-dev libpng-dev libssl-dev libtiff-dev libxml2-dev make pandoc zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site
RUN R -e 'install.packages(c("remotes"))'

# Generic R install
RUN mkdir /build_dir
ADD . /build_dir
WORKDIR /build_dir
RUN R -e 'remotes::install_local(dependencies=TRUE, upgrade=TRUE)'
RUN rm -rf /build_dir

# Copy needed files
RUN mkdir /work_dir
COPY inst /inst
COPY config/default /config/default
COPY config/default /config/params

WORKDIR /work_dir

#CMD ["--help"]
