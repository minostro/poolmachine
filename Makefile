REBAR = $(shell pwd)/rebar3

testsuite:
	REBAR_PROFILE=test $(REBAR) compile
