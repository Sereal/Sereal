C_OUT=c_src/*.o
DEBUG=0
MAPS=0

all: deps eunit

clean:
	rm -f $(C_OUT)
	rebar clean

deps:
	rebar get-deps

compile: clean 
	rebar compile -D DEBUG=$(DEBUG) -D SEREAL_MAP_SUPPORT=$(MAPS)

eunit: compile
	rebar eunit

.PHONY: clean all
