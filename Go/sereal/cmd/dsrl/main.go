package main

import (
	"encoding/binary"
	"encoding/hex"
	"flag"
	"io/ioutil"
	"log"
	"os"
	"runtime"

	"github.com/Sereal/Sereal/Go/sereal"
	"github.com/davecgh/go-spew/spew"
	"github.com/dchest/siphash"
	"github.com/dgryski/go-ddmin"
)

func sipuintptr(s []uintptr) uint64 {

	b := make([]byte, len(s)*8)

	for i, v := range s {
		binary.LittleEndian.PutUint64(b[i*8:], uint64(v))
	}

	return siphash.Hash(0, 0, b)
}

func unmarshal(b []byte) (intf map[string]interface{}, crash uint64, err error) {

	defer func() {
		if p := recover(); p != nil {
			var stack [5]uintptr
			runtime.Callers(4, stack[:])
			crash = sipuintptr(stack[:])
			err = p.(error)
		}
	}()

	d := sereal.Decoder{}

	err = d.Unmarshal(b, &intf)
	if err != nil {
		return nil, 0, err
	}

	return intf, 0, nil
}

func process(fname string, b []byte) {

	intf, _, err := unmarshal(b)

	if err != nil {
		switch e := err.(type) {
		case sereal.ErrCorrupt:
			log.Fatalf("error processing %s: %s (%s)", fname, e, e.Err)
		default:
			log.Fatalf("error processing %s: %s", fname, e)
		}
	}

	spew.Dump(intf)
}

func minimize(b []byte, crashWant uint64, errWant error) []byte {
	return ddmin.Minimize(b, func(b []byte) ddmin.Result {
		_, crash, got := unmarshal(b)

		if got == nil {
			return ddmin.Pass
		}

		if crashWant == crash && got.Error() == errWant.Error() {
			return ddmin.Fail
		}

		return ddmin.Unresolved
	})
}

func main() {

	optMinimize := flag.Bool("minimize", false, "minimize test input")

	flag.Parse()

	if *optMinimize {
		b, _ := ioutil.ReadAll(os.Stdin)
		log.Println("data to minimize length", len(b))
		_, crashWant, errWant := unmarshal(b)
		if errWant == nil {
			log.Fatal("no error received while unmarshalling")
		}
		log.Printf("crash=%x errWant=%+v\n", crashWant, errWant)
		m := minimize(b, crashWant, errWant)
		_, crashGot, errGot := unmarshal(m)
		log.Printf("crash=%x errGot=%+v\n", crashGot, errGot)
		log.Println("minimized length", len(m))
		log.Println("\n" + hex.Dump(m))
		os.Stdout.Write(m)
		return
	}

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
