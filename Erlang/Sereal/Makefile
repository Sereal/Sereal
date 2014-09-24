C_OUT=c_src/*.o
DEBUG=0

all: deps eunit

clean:
	rm -f $(C_OUT)
	rebar clean

deps:
	rebar get-deps

compile: clean 
	rebar compile -D DEBUG=$(DEBUG) 

eunit: compile
	rebar eunit

.PHONY: clean all
