FROM continuumio/miniconda3

WORKDIR /app

RUN mkdir -p /app
COPY environment.yml /app
RUN conda update -n base -c defaults conda && conda env create -f environment.yml
RUN echo "conda activate tima" >> ~/.bashrc
COPY . /app
SHELL ["/bin/bash", "--login", "-c"]
RUN ./tests/integration_test.sh
