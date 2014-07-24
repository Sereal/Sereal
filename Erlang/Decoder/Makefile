C_OUT=c_src/*.o

all: deps run

clean:
	rm -f $(C_OUT)
	rebar clean

deps:
	rebar get-deps

compile: clean 
	rebar compile 
   
run: compile
	erl -pa ./ebin -noshell -eval 'sereal_decoder:test(), init:stop().'


.PHONY: clean all
