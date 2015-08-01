REBAR = $(shell pwd)/rebar3

eqc-ci:
	$(REBAR) as quickcheck_ci compile

testsuite:
	REBAR_PROFILE=test $(REBAR) eqc
