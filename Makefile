ERL=erl
BEAMDIR=./deps/*/ebin ./ebin
REBAR=./rebar3

all: clean compile xref

compile:
	@$(REBAR) compile

xref:
	@$(REBAR) xref skip_deps=true

clean: 
	@ $(REBAR) clean

eunit:
	@rm -rf .eunit
	@mkdir -p .eunit
	@ERL_FLAGS="-config ./test/test.config" $(REBAR) eunit

ct:
	@ERL_FLAGS="-config ./test/test.config" $(REBAR) ct

test: eunit ct

edoc:
	@$(REBAR) skip_deps=true doc
