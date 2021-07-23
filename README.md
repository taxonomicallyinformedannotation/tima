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
Rscript prepare_adducts.R &&
cd .. &&
bash src/get_example_isdb.sh && ## get an example result from new isdb 
cd src &&
## (spectral-lib-matcher, which is only in python. see related repo)
Rscript prepare_gnps.R && ## optional
Rscript prepare_isdb.R &&
## features to do
Rscript prepare_edges.R

## rest to come
```