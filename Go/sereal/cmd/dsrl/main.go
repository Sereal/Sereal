package main

/*
TODO:
   dsrl as standard unit tool:
      dsrl   <-- read from stdin
      dsrl foo.txt <-- read from foo.txt
      dsrl foo.txt bar.txt <-- dump foo.txt, then bar.txt
*/

import (
	"flag"
	"fmt"
	"github.com/Sereal/Sereal/Go/sereal"
	"github.com/davecgh/go-spew/spew"
	"io/ioutil"
	"log"
)

func main() {

	var fname = flag.String("f", "", "file name to read")

	flag.Parse()

	b, _ := ioutil.ReadFile(*fname)

	var i interface{}
	d := sereal.Decoder{}

	err := d.Unmarshal(b, &i)

	if err != nil {
		log.Fatal("err=", err)
	}

	fmt.Printf("%s\n", spew.Sdump(i))

}
