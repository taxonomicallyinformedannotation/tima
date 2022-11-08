FROM rocker/r-ver:latest

# R install
RUN mkdir /build_dir
ADD . /build_dir
WORKDIR /build_dir
RUN R -e 'install.packages(c("remotes"))'
RUN R -e 'remotes::install_local(dependencies=TRUE, upgrade=TRUE)'
RUN rm -rf /build_dir

# Copy needed files
RUN mkdir /work_dir
COPY inst /inst
COPY config/default /config/default
COPY config/default /config/params

WORKDIR /work_dir

#CMD ["--help"]
