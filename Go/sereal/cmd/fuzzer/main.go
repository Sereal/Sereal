package main

import (
	crand "crypto/rand"
	"encoding/hex"
	"fmt"
	"github.com/Sereal/Sereal/Go/sereal"
	mrand "math/rand"
)

func main() {

	srlHeader, _ := hex.DecodeString("3d73726c0100")

        var decoder sereal.Decoder
        decoder.PerlCompat = true

	for {
            l := len(srlHeader) + mrand.Intn(200)
            b := make([]byte, l)
            crand.Read(b)
            doc := make([]byte, l + len(srlHeader))
            copy(doc, srlHeader)
            copy(doc[6:], b)
            fmt.Println(hex.Dump(doc))
            var m interface{}
            err := decoder.Unmarshal(doc, &m)
            fmt.Println("err=", err)
	}

}
