.DEFAULT_GOAL := all

PROJECT := seppen

NODE := seppen-0-255@127.0.0.1
COOKIE := legNe7mIRCiMEy5X

.PHONY: all
all: build test

.PHONY: build
build:
	rebar3 compile

.PHONY: test
test: eunit ct

.PHONY: eunit
eunit:
	rebar3 eunit

.PHONY: ct
ct:
	rebar3 ct --name $(NODE) --setcookie $(COOKIE) --suite seppen_SUITE

.PHONY: dist
dist:
	rebar3 ct --name ct_master@127.0.0.1 --setcookie $(COOKIE) --suite seppen_dist_SUITE --readable true -v

.PHONY: dist_clean
dist_clean:
	kill $(shell ps aux | grep '\-name [c]t' | awk '{print $$2}')

.PHONY: run
run:
	rebar3 shell --name $(NODE) --setcookie $(COOKIE) --apps=$(PROJECT)

.PHONY: clean
clean:
	rm -rf $(CURDIR)/_build/test
	rebar3 clean
