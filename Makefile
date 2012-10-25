# Copyright 2012 Erlware, LLC. All Rights Reserved.
#
# BSD License see COPYING

ERL = $(shell which erl)

ERLFLAGS= -pa $(CURDIR)/.eunit -pa $(CURDIR)/ebin -pa $(CURDIR)/*/ebin

REBAR=$(shell which rebar)

ifeq ($(REBAR),)
$(error "Rebar not available on this system")
endif

ERLWARE_COMMONS_PLT=$(CURDIR)/.erlware_commons_plt

.PHONY: all compile doc clean test dialyzer typer shell distclean pdf get-deps escript

all: compile test doc dialyzer

get-deps:
	$(REBAR) get-deps
	$(REBAR) compile

compile:
	$(REBAR) skip_deps=true compile

doc: compile
	$(REBAR) skip_deps=true doc

test: compile
	$(REBAR) skip_deps=true eunit

$(ERLWARE_COMMONS_PLT):
	@echo Building local plt at $(ERLWARE_COMMONS_PLT)
	@echo
	- dialyzer --fullpath --output_plt $(ERLWARE_COMMONS_PLT) --build_plt \
	   --apps erts kernel stdlib eunit -r deps

dialyzer: $(ERLWARE_COMMONS_PLT)
	dialyzer --fullpath --plt $(ERLWARE_COMMONS_PLT) -Wrace_conditions --src src

typer:
	typer --plt $(ERLWARE_COMMONS_PLT) -r ./src

shell: compile
# You often want *rebuilt* rebar tests to be available to the
# shell you have to call eunit (to get the tests
# rebuilt). However, eunit runs the tests, which probably
# fails (thats probably why You want them in the shell). This
# runs eunit but tells make to ignore the result.
	- @$(REBAR) skip_deps=true eunit
	@$(ERL) $(ERLFLAGS)

clean:
	$(REBAR) skip_deps=true clean
	- rm $(CURDIR)/doc/*.html
	- rm $(CURDIR)/doc/*.css
	- rm $(CURDIR)/doc/*.png
	- rm $(CURDIR)/doc/edoc-info

distclean: clean
	rm -rf $(ERLWARE_COMMONS_PLT)
	rm -rvf $(CURDIR)/deps/*
