# Use the Bioconductor base image
FROM bioconductor/bioconductor_docker:RELEASE_3_22-R-4.5.2

# Add a non-root user and set up the R library directory
RUN useradd -m tima-user && \
    mkdir -p /home/tima-user/Library/Frameworks/R.framework/Resources/site-library && \
    mkdir -p /home/tima-user/.tima/data && \
    chown -R tima-user:tima-user /home/tima-user

# Set the R library path to the new directory
ENV R_LIBS_USER=/home/tima-user/Library/Frameworks/R.framework/Resources/site-library

# Switch to the non-root user
USER tima-user
WORKDIR /home/tima-user

# Install R dependencies
RUN Rscript -e "devtools::install_github('taxonomicallyinformedannotation/tima')" && \
    Rscript -e "tima::install()"

# Expose the necessary ports for Shiny
EXPOSE 3838

# Disable healthcheck
HEALTHCHECK NONE

# Define default command
CMD ["Rscript", "-e", "tima::run_app()"]
