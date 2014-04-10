APPS = kernel stdlib sasl erts ssl tools os_mon runtime_tools crypto inets \
	public_key mnesia syntax_tools compiler
COMBO_PLT = $(HOME)/.riak_core_combo_dialyzer_plt
.PHONY: all compile deps test eunit clean distclean

all: compile

compile:
	./rebar get-deps compile

deps:
	./rebar get-deps

eunit: test

test: compile
	./rebar eunit skip_deps=true

distclean: clean
	./rebar delete-deps

clean:
	./rebar clean

build_plt: compile
	dialyzer --build_plt --output_plt $(COMBO_PLT) --apps $(APPS) \
		deps/*/ebin

check_plt: compile
	dialyzer --check_plt --plt $(COMBO_PLT) --apps $(APPS) \
		deps/*/ebin

dialyzer: compile
	@echo
	@echo Use "'make check_plt'" to check PLT prior to using this target.
	@echo Use "'make build_plt'" to build PLT prior to using this target.
	@echo
	dialyzer --plt $(COMBO_PLT) ebin

compile test clean: rebar

rebar:
	wget -c http://cloud.github.com/downloads/basho/rebar/rebar
	chmod +x $@
