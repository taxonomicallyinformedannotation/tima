# Image
FROM ghcr.io/bioconductor/bioconductor:RELEASE_3_16

# To force bioconductor to install from source
ENV BIOCONDUCTOR_USE_CONTAINER_REPOSITORY=FALSE

# Copy files
COPY data data
COPY DESCRIPTION DESCRIPTION
COPY inst inst
COPY params params
COPY paths.yaml paths.yaml

# R install
RUN Rscript inst/scripts/install.R

# Copy the app
COPY . .

# For Shiny
EXPOSE 3838
EXPOSE 3839

#CMD ["--help"]
