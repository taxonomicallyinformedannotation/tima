# Image
FROM ghcr.io/bioconductor/bioconductor:RELEASE_3_16

# Copy files
COPY paths.yaml ./paths.yaml
COPY config/default ./config/default
COPY data ./data
COPY inst ./inst

# R dependencies
RUN R -e 'install.packages(c("BiocManager","remotes"))'
RUN R -e 'remotes::install_github(repo = "taxonomicallyinformedannotation/tima-r", upgrade = "always", build_vignettes = FALSE)'

COPY . ./

#CMD ["--help"]
