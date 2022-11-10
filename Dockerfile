# Small image with multiple arch available
FROM r-base:latest

# Missing (kudos dockerfiller)
RUN apt-get update && apt-get install -y  libcurl4-openssl-dev libfribidi-dev libharfbuzz-dev libicu-dev libpng-dev libssl-dev libtiff-dev libxml2-dev make pandoc zlib1g-dev && rm -rf /var/lib/apt/lists/*

RUN R -e 'install.packages(c("remotes"))'
COPY DESCRIPTION .
RUN R -e 'remotes::install_local()'

COPY paths.yaml ./paths.yaml
COPY inst ./inst
COPY config/default ./config/default
COPY data ./data

#CMD ["--help"]
