# Small image with multiple arch available
FROM ghcr.io/bioconductor/bioconductor:RELEASE_3_16

# Copy files
COPY paths.yaml ./paths.yaml
COPY config/default ./config/default
COPY data ./data
COPY inst ./inst

# R dependencies
RUN R -e 'install.packages(c("BiocManager","remotes"))'
RUN R -e 'remotes::install_github(repo = "taxonomicallyinformedannotation/tima-r", upgrade = "always", dependencies = "hard", repos = BiocManager::repositories(), build_vignettes = FALSE)'

#CMD ["--help"]
