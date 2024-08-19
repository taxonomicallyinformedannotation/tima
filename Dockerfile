# Use the specified Bioconductor Docker image
FROM bioconductor/bioconductor_docker:3.19-R-4.4.0

# Update apt and install necessary system dependencies
RUN apt-get update && apt-get install -y --no-install-recommends libcurl4-openssl-dev libharfbuzz-dev libfribidi-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Add a non-root user and create the R library directory
RUN useradd -m tima-user && \
    mkdir -p /home/tima-user/Library/Frameworks/R.framework/Resources/site-library && \
    chown -R tima-user:tima-user /home/tima-user

# Set the R library path to the new directory
ENV R_LIBS_USER=/home/tima-user/Library/Frameworks/R.framework/Resources/site-library

# Switch to the non-root user
USER tima-user
WORKDIR /home/tima-user

# Copy necessary files for dependency installation
COPY --chown=tima-user:tima-user docker-compose.yml ./docker-compose.yml
COPY --chown=tima-user:tima-user inst ./inst
COPY --chown=tima-user:tima-user R ./R

# Run R script to install dependencies
RUN Rscript -e "install.packages('tima', repos = c('https://taxonomicallyinformedannotation.r-universe.dev', 'https://bioc.r-universe.dev', 'https://cloud.r-project.org')); tima::install()"

# Expose the necessary ports for Shiny
EXPOSE 3838

# Disable healthcheck (if you really want to disable it)
HEALTHCHECK NONE

# Define default command (commented out)
# CMD ["Rscript inst/scripts/run_app.R", "Rscript inst/scripts/tima_full.R"]
