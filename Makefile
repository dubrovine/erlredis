  
REBAR3 ?= $(shell test -e `which rebar3` 2>/dev/null && which rebar3 || echo "./rebar3")

ifeq ($(REBAR3),)
REBAR3 = $(CURDIR)/rebar3
endif


all: build


clean:
	$(REBAR3) clean
	rm -rf logs
	rm -rf .eunit
	rm -f test/*.beam


distclean: clean
	git clean -fxd


build:
	$(REBAR3) compile

shell:
	$(REBAR3) shell


eunit:
	$(REBAR3) eunit skip_deps=true


check: build eunit


%.beam: %.erl
	erlc -o test/ $<


.PHONY: all clean distclean depends build etap eunit check