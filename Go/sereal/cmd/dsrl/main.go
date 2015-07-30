package main

import (
	"flag"
	"io/ioutil"
	"log"
	"os"

	"github.com/Sereal/Sereal/Go/sereal"
	"github.com/davecgh/go-spew/spew"
)

func process(fname string, b []byte) {

	var i interface{}
	d := sereal.Decoder{}

	err := d.Unmarshal(b, &i)

	if err != nil {
		log.Fatalf("error processing %s: %s", fname, err)
	}

	spew.Dump(i)
}

func main() {

	flag.Parse()

	if flag.NArg() == 0 {
		b, _ := ioutil.ReadAll(os.Stdin)
		process("stdin", b)
		return
	}

	for _, arg := range flag.Args() {
		b, _ := ioutil.ReadFile(arg)
		process(arg, b)
	}
}
