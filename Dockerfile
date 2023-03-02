# Image
FROM ghcr.io/bioconductor/bioconductor:RELEASE_3_16

# Copy files
COPY DESCRIPTION ./DESCRIPTION
COPY paths.yaml ./paths.yaml
COPY data ./data
COPY inst ./inst
COPY params ./params

# To force bioconductor to install from source
ENV BIOCONDUCTOR_USE_CONTAINER_REPOSITORY=FALSE

# R dependencies
RUN R -e 'remotes::install_github(repo = "taxonomicallyinformedannotation/tima-r", upgrade = "always", build_vignettes = FALSE)'

COPY . ./

#CMD ["--help"]
