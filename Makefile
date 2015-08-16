REBAR = $(shell pwd)/rebar3

.PHONY: test

test:
	REBAR_PROFILE=test $(REBAR) compile && erl -noshell -pa _build/test/lib/poolmachine/ebin -pa _build/test/lib/proper/ebin -s poolmachine_test -s erlang halt
