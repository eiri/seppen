.DEFAULT_GOAL := all

PROJECT := seppen

.PHONY: all
all: build

.PHONY: build
build:
	rebar3 compile

.PHONY: test
test:
	rebar3 ct

.PHONY: run
run:
	rebar3 shell --apps=$(PROJECT) --sname seppen --setcookie legNe7mIRCiMEy5X

.PHONY: clean
clean:
	rm -rf $(CURDIR)/_build/test
	rebar3 clean
