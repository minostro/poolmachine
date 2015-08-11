REBAR = $(shell pwd)/rebar3

testsuite:
	REBAR_PROFILE=milton $(REBAR) eqc
