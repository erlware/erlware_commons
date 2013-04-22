# Copyright 2012 Erlware, LLC. All Rights Reserved.
#
# BSD License see COPYING

ERL = $(shell which erl)
ERL_VER = $(shell erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().'  -noshell)

ERLFLAGS= -pa $(CURDIR)/.eunit -pa $(CURDIR)/ebin -pa $(CURDIR)/*/ebin

REBAR=$(shell which rebar)

ifeq ($(REBAR),)
$(error "Rebar not available on this system")
endif

ERLWARE_COMMONS_PLT=$(CURDIR)/.erlware_commons_plt

.PHONY: all compile doc clean test shell distclean pdf get-deps rebuild #dialyzer typer #fail on Travis.

all: compile doc test #dialyzer #fail on travis

deps: .DEV_MODE
	$(REBAR) get-deps compile

.DEV_MODE:
	touch $@
	cp priv/ec_semver_parser.peg src

get-deps:
	$(REBAR) get-deps compile

compile: deps
	$(REBAR) skip_deps=true compile


doc: compile
	- $(REBAR) skip_deps=true doc

test: compile
	$(REBAR) skip_deps=true eunit

$(ERLWARE_COMMONS_PLT).$(ERL_VER).erts:
	@echo Building local plt at $(ERLWARE_COMMONS_PLT).$(ERL_VER).base
	@echo

	- dialyzer --fullpath --verbose --output_plt $(ERLWARE_COMMONS_PLT).$(ERL_VER).base --build_plt \
	   --apps erts

$(ERLWARE_COMMONS_PLT).$(ERL_VER).kernel:$(ERLWARE_COMMONS_PLT).$(ERL_VER).erts
	@echo Building local plt at $(ERLWARE_COMMONS_PLT).$(ERL_VER).base
	@echo
	- dialyzer --fullpath --verbose --output_plt $(ERLWARE_COMMONS_PLT).$(ERL_VER).base --build_plt \
	   --apps kernel 

$(ERLWARE_COMMONS_PLT).$(ERL_VER).base:$(ERLWARE_COMMONS_PLT).$(ERL_VER).kernel
	@echo Building local plt at $(ERLWARE_COMMONS_PLT).$(ERL_VER).base
	@echo
	- dialyzer --fullpath --verbose --output_plt $(ERLWARE_COMMONS_PLT).$(ERL_VER).base --build_plt \
	   --apps stdlib 

$(ERLWARE_COMMONS_PLT).$(ERL_VER): $(ERLWARE_COMMONS_PLT).$(ERL_VER).base
	@echo Building local plt at $(ERLWARE_COMMONS_PLT).$(ERL_VER)
	@echo
	- dialyzer --fullpath --verbose --output_plt $(ERLWARE_COMMONS_PLT).$(ERL_VER) --add_to_plt --plt $(ERLWARE_COMMONS_PLT).$(ERL_VER).base \
	   --apps eunit -r deps

dialyzer: $(ERLWARE_COMMONS_PLT).$(ERL_VER)
	dialyzer --fullpath --plt $(ERLWARE_COMMONS_PLT).$(ERL_VER) -Wrace_conditions -r ./ebin

typer: $(ERLWARE_COMMONS_PLT).$(ERL_VER)
	typer --plt $(ERLWARE_COMMONS_PLT).$(ERL_VER) -r ./src

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
	rm -rf $(ERLWARE_COMMONS_PLT).$(ERL_VER)
	rm -rvf $(CURDIR)/deps
	rm -rvf .DEV_MODE

rebuild: distclean get-deps all
