# Copyright 2012 Erlware, LLC. All Rights Reserved.
#
# BSD License see COPYING

ERL = $(shell which erl)
ERL_VER = $(shell erl -eval 'io:format("~s", [erlang:system_info(otp_release)]), halt().'  -noshell)
ERLWARE_COMMONS_PLT=$(CURDIR)/.erlware_commons_plt

ERLFLAGS= -pa $(CURDIR)/.eunit -pa $(CURDIR)/ebin -pa $(CURDIR)/*/ebin

# =============================================================================
# Verify that the programs we need to run are installed on this system
# =============================================================================

REBAR ?= $(shell which rebar)

ifeq ($(REBAR),)
$(error "Rebar not available on this system")
endif

# =============================================================================
# Handle version discovery
# =============================================================================

# We have a problem that we only have 10 minutes to build on travis
# and those travis boxes are quite small. This is ok for the fast
# dialyzer on R15 and above. However on R14 and below we have the
# problem that travis times out. The code below lets us not run
# dialyzer on R14
OTP_VSN=$(shell erl -noshell -eval 'io:format("~p", [erlang:system_info(otp_release)]), erlang:halt(0).' | perl -lne 'print for /R(\d+).*/g')
TRAVIS_SLOW=$(shell expr $(OTP_VSN) \<= 15 )

ifeq ($(TRAVIS_SLOW), 0)
DIALYZER=$(shell which dialyzer)
else
DIALYZER=: not running dialyzer on R14 or R15
endif

# =============================================================================
# Rules to build the system
# =============================================================================

.PHONY: all compile doc clean test shell distclean pdf get-deps rebuild dialyzer typer

all: compile doc test

rebuild: distclean deps compile dialyzer test

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

	- $(DIALYZER) --fullpath --verbose --output_plt $(ERLWARE_COMMONS_PLT).$(ERL_VER).erts --build_plt \
	   --apps erts

$(ERLWARE_COMMONS_PLT).$(ERL_VER).kernel:$(ERLWARE_COMMONS_PLT).$(ERL_VER).erts
	@echo Building local plt at $(ERLWARE_COMMONS_PLT).$(ERL_VER).base
	@echo
	- $(DIALYZER) --fullpath --verbose --output_plt $(ERLWARE_COMMONS_PLT).$(ERL_VER).kernel --build_plt \
	   --apps kernel

$(ERLWARE_COMMONS_PLT).$(ERL_VER).base:$(ERLWARE_COMMONS_PLT).$(ERL_VER).kernel
	@echo Building local plt at $(ERLWARE_COMMONS_PLT).$(ERL_VER).base
	@echo
	- $(DIALYZER) --fullpath --verbose --output_plt $(ERLWARE_COMMONS_PLT).$(ERL_VER).base --build_plt \
	   --apps stdlib

$(ERLWARE_COMMONS_PLT).$(ERL_VER): $(ERLWARE_COMMONS_PLT).$(ERL_VER).base
	@echo Building local plt at $(ERLWARE_COMMONS_PLT).$(ERL_VER)
	@echo
	- $(DIALYZER) --fullpath --verbose --output_plt $(ERLWARE_COMMONS_PLT).$(ERL_VER) --add_to_plt --plt $(ERLWARE_COMMONS_PLT).$(ERL_VER).base \
	   --apps eunit -r deps

dialyzer: compile $(ERLWARE_COMMONS_PLT).$(ERL_VER)
	$(DIALYZER) --fullpath --plt $(ERLWARE_COMMONS_PLT).$(ERL_VER) -Wrace_conditions -r ./ebin

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
