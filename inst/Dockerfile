# Use the Bioconductor base image
FROM bioconductor/bioconductor_docker:RELEASE_3_23-R-4.6.0

# Add a non-root user and set up the R library and working directories
RUN useradd -m tima-user && \
    mkdir -p /home/tima-user/Library/Frameworks/R.framework/Resources/site-library && \
    mkdir -p /home/tima-user/.tima/data && \
    mkdir -p /home/tima-user/.tima/_targets && \
    chown -R tima-user:tima-user /home/tima-user

# Set the R library path to the new directory
ENV R_LIBS_USER=/home/tima-user/Library/Frameworks/R.framework/Resources/site-library

# Switch to the non-root user
USER tima-user
WORKDIR /home/tima-user

# Base dependency installation (cached across builds)
RUN Rscript -e "install.packages('pak')"

# Invalidate layer cache and embed metadata when a new Git commit is pushed
ARG GIT_COMMIT=unknown
ENV GIT_COMMIT=${GIT_COMMIT}
LABEL org.opencontainers.image.revision="${GIT_COMMIT}"

# Copy the repository source code into the container with correct permissions
COPY --chown=tima-user:tima-user . /home/tima-user/app
WORKDIR /home/tima-user/app

# Clean pak cache, install package locally from the current commit, and run post-install setup
RUN Rscript -e "pak::cache_clean(); options(timeout = 30); pak::pkg_install('.', ask = FALSE)" && \
    Rscript -e "tima::install_tima()"

# Expose the necessary ports for Shiny
EXPOSE 3838

# Disable healthcheck
HEALTHCHECK NONE

# Define default command
CMD ["Rscript", "-e", "tima::run_app()"]
