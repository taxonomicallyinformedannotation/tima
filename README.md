## tl;dr:

```
docker build -t tima . ## optional
docker run -it --rm -v $PWD:/app tima bash ## optional
conda env create -f environment.yml &&
conda activate tima &&
bash src/get-lotus.sh &&
cd src &&
Rscript prepare_lotus.R &&
## Rscript prepare_dnp.R &&
Rscript prepare_library.R &&
Rscript prepare_adducts.R 

## rest to come
```