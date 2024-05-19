# Use the specified Bioconductor Docker image
FROM bioconductor/bioconductor_docker:3.19-R-4.4.0

# Define workdir and user
WORKDIR /tima
RUN groupadd -r tima && useradd -r -g tima tima-user

# Install R dependencies
# To force bioconductor to install from source
ENV BIOCONDUCTOR_USE_CONTAINER_REPOSITORY=FALSE

# Create the directory for the package cache and set appropriate permissions
RUN mkdir -p /home/tima-user/.cache/R/pkgcache/pkg && \
    chown -R tima-user:tima /home/tima-user/.cache

# Copy necessary files for dependency installation
COPY DESCRIPTION ./DESCRIPTION
COPY R ./R
COPY _targets.yaml ./_targets.yaml
COPY inst/scripts/install.R ./inst/scripts/install.R

# Run R script to install dependencies as root user
RUN Rscript ./inst/scripts/install.R

# Copy remaining files
COPY inst ./inst
COPY docker-compose.yml ./docker-compose.yml

# For Shiny (assuming ports 3838 and 3839 are used)
EXPOSE 3838
EXPOSE 3839

# Change the ownership of the app files to the user
RUN chown -R tima-user:tima /tima

# Define user and working directory
USER tima-user
WORKDIR /tima

# Disable healthcheck (if you really want to disable it)
HEALTHCHECK NONE

# Define default command (commented out)
# CMD ["Rscript inst/scripts/run_app.R", "Rscript inst/scripts/tima_full.R"]
