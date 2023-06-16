# Image
FROM bioconductor/bioconductor_docker:3.17-R-4.3.0

# Copy files
COPY DESCRIPTION ./DESCRIPTION
COPY data ./data
COPY inst ./inst
COPY tools ./tools
COPY docker-compose.yml ./docker-compose.yml
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
