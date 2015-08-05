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
		switch e := err.(type) {
		case sereal.ErrCorrupt:
			log.Fatalf("error processing %s: %s (%s)", fname, e, e.Err)
		default:
			log.Fatalf("error processing %s: %s", fname, e)
		}
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
