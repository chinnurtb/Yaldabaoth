.PHONY: all compile generate test clean

all: compile generate

compile: rebar
	@if [ -e ./rebar ]; then ./rebar get-deps compile ; else rebar get-deps compile ; fi

generate:
	@if [ -e ./rebar ]; then ./rebar generate -f ; else rebar generate -f ; fi

test: rebar compile
	@if [ -e ./rebar ]; then ./rebar skip_deps=true eunit ; else rebar skip_deps=true eunit ; fi

clean: rebar
	@if [ -e ./rebar ]; then ./rebar clean ; else rebar clean ; fi

rebar:
	@which rebar 1>/dev/null 2>/dev/null ; if [ $$? -eq 1 ]; then wget http://cloud.github.com/downloads/basho/rebar/rebar ; chmod u+x rebar ; fi
