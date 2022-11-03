FROM bioconductor/bioconductor_docker:devel
RUN apt-get update && \
    apt-get install -y \
    git-core \
    libcurl4-openssl-dev \
    libgit2-dev \
    libicu-dev \
    libssl-dev \
    make pandoc \
    pandoc-citeproc \
    zlib1g-dev \
    xfonts-100dpi \
    xfonts-75dpi \
    biber \
    libsbml5-dev \
    qpdf \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*
# Create a buildzone folder named after the R package 
# BiocCheck requires the buildzone to have the same name as the R package 
ARG PKG
RUN echo $PKG
RUN mkdir -p /$PKG
ADD . /$PKG
WORKDIR /$PKG
# Install dependencies with AnVil (faster)
RUN Rscript -e 'options(download.file.method= "libcurl"); \
                if(!require("BiocManager")) install.packages("BiocManager"); \
                if(!require("AnVIL"))  {BiocManager::install("AnVIL", ask = FALSE)}; \
                AnVIL::install(c("remotes","devtools")); \
                try({remotes::install_github("bergant/rapiclient")}); \
                bioc_ver <- BiocManager::version(); \ 
                options(repos = c(AnVIL::repositories(),\
                                  AnVIL = file.path("https://bioconductordocker.blob.core.windows.net/packages",bioc_ver,"bioc"),\
                                  CRAN = "https://cran.rstudio.com/"),\
                                  download.file.method = "libcurl", Ncpus = 2); \
                deps <- remotes::dev_package_deps(dependencies = TRUE)$package; \
                AnVIL::install(pkgs = deps,  ask = FALSE); \
                deps_left <- deps[!deps %in% rownames(installed.packages())]; \
                if(length(deps_left)>0) devtools::install_dev_deps(dependencies = TRUE, upgrade = "never");'
# Copy directories
COPY . .
COPY ./config/default /config/params
COPY ./config /tests/testhat/config
COPY ./inst /tests/testhat/inst
# Run R CMD check - will fail with any errors or warnings
RUN Rscript -e 'devtools::check()'
# Run Bioconductor's BiocCheck (optional)
#ARG BIOC
#RUN if [ "$BIOC" = "true" ]; then \
#        Rscript -e 'if(!require("BiocCheck")) AnVIL::install("BiocCheck");\
#                    BiocCheck::BiocCheck(`quit-with-status` = TRUE,\
#                    `no-check-R-ver` = TRUE,\
#                    `no-check-bioc-help` = TRUE);'\
#    fi
# Install R package from source 
RUN R -e 'remotes::install_local(upgrade="never")'
RUN rm -rf /$PKG
