.DEFAULT_GOAL := all

PROJECT := seppen

NODE := seppen-0-255@127.0.0.1
COOKIE := snowflake

.PHONY: all
all: format-check build test

.PHONY: format
format:
	rebar3 fmt --write

.PHONY: format-check
format-check:
	rebar3 fmt --check

.PHONY: build
build:
	rebar3 compile

.PHONY: test
test: eunit ct dist

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

.PHONY: release
release:
	rebar3 release

.PHONY: run
run: release
	$(CURDIR)/_build/default/rel/seppen/bin/seppen foreground

.PHONY: shell
shell:
	rebar3 shell --name $(NODE) --setcookie $(COOKIE) --apps=$(PROJECT)

.PHONY: clean
clean:
	rm -rf $(CURDIR)/_build/default/rel
	rm -rf $(CURDIR)/_build/default/lib/seppen
	rm -rf $(CURDIR)/_build/test
	rm -rf $(CURDIR)/_build/prod
	rebar3 clean
