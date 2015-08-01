REBAR = $(shell pwd)/rebar3

eqc-ci:
	$(REBAR) as quickcheck_ci compile
