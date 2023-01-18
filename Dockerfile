# Image
FROM ghcr.io/bioconductor/bioconductor:RELEASE_3_16

# Copy files
COPY paths.yaml ./paths.yaml
COPY config/default ./config/default
COPY data ./data
COPY inst ./inst

ENV BIOCONDUCTOR_USE_CONTAINER_REPOSITORY=FALSE

# R dependencies
RUN R -e 'remotes::install_github(repo = "taxonomicallyinformedannotation/tima-r", upgrade = "always", build_vignettes = FALSE)'

COPY . ./

#CMD ["--help"]
