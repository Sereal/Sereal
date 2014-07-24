C_OUT=c_src/*.o

all: deps eunit

clean:
	rm -f $(C_OUT)
	rebar clean

deps:
	rebar get-deps

compile: clean 
	rebar compile  

eunit: compile
	rebar eunit

.PHONY: clean all
