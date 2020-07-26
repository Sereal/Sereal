// +build gofuzz

package sereal

import (
	"github.com/google/go-cmp/cmp"
	"reflect"
)

func Fuzz(data []byte) int {
	var m interface{}

	header, err := readHeader(data)
	if err != nil {
		return 0
	}

	bodyStart := headerSize + header.suffixSize

	if bodyStart > len(data) || bodyStart < 0 {
		return 0
	}

	switch header.version {
	case 1, 2, 3:
		break
	default:
		return 0
	}

	switch header.doctype {
	case serealRaw:
		break
	case serealSnappy, serealSnappyIncremental, serealZlib:
		// ignore compressed data
		return 0
	}

	if err := Unmarshal(data, &m); err != nil {
		return 0
	}

	var enc []byte
	if enc, err = Marshal(m); err != nil {
		panic("unable to marshal")

	}

	var m2 interface{}
	if err := Unmarshal(enc, &m2); err != nil {
		panic("unmarshalling marshalled data")

	}

	if !reflect.DeepEqual(m, m2) {
		panic("failed to roundtrip")
	}

	return 1
}

type S struct {
	A int
	B string
	C float64
	D bool
	E uint8
	F []byte
	G interface{}
	H map[string]interface{}
	I map[string]string
	J []interface{}
	K []string
	L S1
	M *S1
	N *int
	O **int
}

type S1 struct {
	A int
	B string
}

func FuzzStructure(data []byte) int {
	var s S

	header, err := readHeader(data)
	if err != nil {
		return 0
	}

	bodyStart := headerSize + header.suffixSize

	if bodyStart > len(data) || bodyStart < 0 {
		return 0
	}

	switch header.version {
	case 1, 2, 3:
		break
	default:
		return 0
	}

	switch header.doctype {
	case serealRaw:
		break
	case serealSnappy, serealSnappyIncremental, serealZlib:
		// ignore compressed data
		return 0
	}

	if err := Unmarshal(data, &s); err != nil {
		return 0
	}

	var enc []byte
	if enc, err = Marshal(s); err != nil {
		panic("unable to marshal")

	}

	var s2 S
	if err := Unmarshal(enc, &s2); err != nil {
		panic("unmarshalling marshalled data")

	}

	if !reflect.DeepEqual(s, s2) {
		s := cmp.Diff(s, s2)
		panic("failed to roundtrip: " + s)
	}

	return 1
}
