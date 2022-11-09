# Small image with multiple arch available
FROM r-base:latest

# Missing (kudos dockerfiller)
RUN apt-get update && apt-get install -y  libcurl4-openssl-dev libfribidi-dev libharfbuzz-dev libicu-dev libpng-dev libssl-dev libtiff-dev libxml2-dev make pandoc zlib1g-dev && rm -rf /var/lib/apt/lists/*

RUN R -e 'install.packages(c("BiocManager","remotes"))'
COPY DESCRIPTION .
# This gets stuck, don't know why
# RUN R -e 'remotes::install_local(dependencies=TRUE, upgrade=TRUE, repos=BiocManager::repositories())'
# Using this instead
RUN R -e 'remotes::install_github("taxonomicallyinformedannotation/tima-r")'

COPY paths.yaml ./paths.yaml
COPY inst ./inst
COPY config/default ./config/default

#CMD ["--help"]
