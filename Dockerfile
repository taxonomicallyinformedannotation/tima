# Image
FROM bioconductor/bioconductor_docker:3.19-R-4.4.0

# Define workdir and user
WORKDIR /tima
RUN groupadd -r tima && useradd -r -g tima tima-user

# Install R dependencies
# To force bioconductor to install from source
ENV BIOCONDUCTOR_USE_CONTAINER_REPOSITORY=FALSE
COPY DESCRIPTION ./DESCRIPTION
COPY inst/scripts/install.R ./inst/scripts/install.R
RUN Rscript ./inst/scripts/install.R
COPY R ./R
RUN Rscript ./inst/scripts/install.R

# Copy files
COPY inst ./inst
COPY docker-compose.yml ./docker-compose.yml
COPY _targets.yaml ./_targets.yaml

# For Shiny
EXPOSE 3838
EXPOSE 3839

# Change the ownership of the app files to the user
RUN chown -R tima-user:tima ./

# Define user
USER tima-user

HEALTHCHECK NONE

#CMD ["--help"]
