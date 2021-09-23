export BIN_PATH ?= ${PWD}/bin
export CONFIG_PATH ?= ${PWD}/config
export DATA_PATH ?= ${PWD}/data
export SRC_PATH ?= ${PWD}/src
export TESTS_PATH ?= ${PWD}/tests

export GNVERIFIER_VERSION = v0.3.3

export UNAME := $(shell uname)

PLATFORM := unsupported
ifeq ($(OS),Windows_NT)
	PLATFORM := unsupported
else
	UNAME_S := $(shell uname -s)
    ifeq ($(UNAME_S),Linux)
        PLATFORM := linux
    endif
    ifeq ($(UNAME_S),Darwin)
        PLATFORM := mac
    endif
endif
