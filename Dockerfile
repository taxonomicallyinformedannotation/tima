# Image
FROM ghcr.io/bioconductor/bioconductor:RELEASE_3_16

# Copy files
COPY DESCRIPTION ./DESCRIPTION
COPY paths.yaml ./paths.yaml
COPY data ./data
COPY inst ./inst
COPY params ./params

# To force bioconductor to install from source
ENV BIOCONDUCTOR_USE_CONTAINER_REPOSITORY=FALSE

# R dependencies using renv
# ENV RENV_VERSION 0.17.2
# ENV RENV_PATHS_LIBRARY renv/library
# COPY .Rprofile .Rprofile
# COPY renv/ ./renv/
# COPY renv.lock ./renv.lock
# RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
# RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"
# RUN R -e "renv::restore()"
# RUN R CMD INSTALL .

# R dependencies
RUN R -e 'remotes::install_github(repo = "taxonomicallyinformedannotation/tima-r", upgrade = "always", build_vignettes = FALSE)'

COPY . ./

# For Shiny
EXPOSE 3838

#CMD ["--help"]
