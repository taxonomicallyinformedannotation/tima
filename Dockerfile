FROM continuumio/miniconda3

WORKDIR /app

RUN mkdir -p /app
COPY environment.yml /app
RUN conda update -n base -c defaults conda && conda env create -f environment.yml
RUN echo "conda activate tima" >> ~/.bashrc
RUN apt-get update && apt-get install -y curl
RUN curl -L https://github.com/gnames/gnverifier/releases/download/v0.3.1/gnverifier-v0.3.1-linux.tar.gz | tar xz --directory /usr/bin
RUN curl -L https://github.com/gnames/gnfinder/releases/download/v0.14.1/gnverifier-v0.14.1-linux.tar.gz | tar xz --directory /usr/bin
SHELL ["/bin/bash", "--login", "-c"]
