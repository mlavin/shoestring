clean:
	rm -f erl_crash.dump
	rebar clean

build:
	rebar compile

server: build
	erl -pa ebin deps/*/ebin -boot start_sasl -s shoestring

.PHONY: clean build server
