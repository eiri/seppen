.DEFAULT_GOAL := all

PROJECT := seppen

COOKIE := legNe7mIRCiMEy5X

.PHONY: all
all: build

.PHONY: build
build:
	rebar3 compile

.PHONY: test
test:
	rebar3 ct --sname ct --setcookie $(COOKIE) --readable true

.PHONY: run
run:
	rebar3 shell --apps=$(PROJECT) --name seppen@$(shell hostname) --setcookie $(COOKIE)

.PHONY: clean
clean:
	rm -rf $(CURDIR)/_build/test
	rebar3 clean
