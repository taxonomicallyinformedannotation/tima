## tl;dr:

```
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