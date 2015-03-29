ERL=erl
BEAMDIR=./deps/*/ebin ./ebin
REBAR=./rebar

all: clean get-deps update-deps compile xref

update-deps:
	@$(REBAR) update-deps

get-deps:
	@$(REBAR) get-deps

compile:
	@$(REBAR) compile

xref:
	@$(REBAR) xref skip_deps=true

clean: 
	@ $(REBAR) clean

eunit:
	@rm -rf .eunit
	@mkdir -p .eunit
	@ERL_FLAGS="-config ./test/test.config" $(REBAR) skip_deps=true eunit

ct:
	@ERL_FLAGS="-config ./test/test.config" $(REBAR) skip_deps=true ct

test: eunit ct

edoc:
	@$(REBAR) skip_deps=true doc
