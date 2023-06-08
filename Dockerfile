# Image
FROM bioconductor/bioconductor_docker:RELEASE_3_17

# Copy files
COPY DESCRIPTION ./DESCRIPTION
COPY /data/ ./data/
COPY /inst/ ./inst/
COPY /params/ ./params/
COPY paths.yaml ./paths.yaml

# To force bioconductor to install from source
ENV BIOCONDUCTOR_USE_CONTAINER_REPOSITORY=FALSE

# R dependencies
RUN Rscript inst/scripts/install.R

COPY . ./.

# For Shiny
EXPOSE 3838
EXPOSE 3839

#CMD ["--help"]
