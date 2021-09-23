include config.mk
include paths.mk

all:
.PHONY: copy-params get-lotus
.LOW_RESOLUTION_TIME: get-lotus

.PRECIOUS: %.csv %.gz %.json %.tsv %.yaml %.zip

copy-params: 
	cp -R config/default config/params

get-lotus: ${DATA_INTERIM_PATH}/lotus.csv.gz
${DATA_INTERIM_PATH}/lotus.csv.gz: paths.yaml ${SRC_PATH}/parse_yaml.sh ${SRC_PATH}/warning.sh ${SRC_PATH}/get_lotus.sh 
	bash ${SRC_PATH}/get_lotus.sh
