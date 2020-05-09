.DEFAULT_GOAL := all

PROJECT := seppen

.PHONY: all
all: build

.PHONY: build
build:
	rebar3 compile

.PHONY: run
run:
	rebar3 shell --apps=$(PROJECT)
