package sereal

import (
	"reflect"
	"testing"
)

var roundtrips = []interface{}{
	true,
	false,
	1,
	10,
	100,
	200,
	300,
	"hello",
	"hello, world",
	"twas brillig and the slithy toves and gyre and gimble in the wabe",
	2.2,
	9891234567890.098,
	//        []interface{}{1,100,1000,2000,0xdeadbeef},
}

func TestRoundtrip(t *testing.T) {

	for _, v := range roundtrips {
		b, err := Marshal(v)
		if err != nil {
			t.Errorf("failed marshalling : %v\n", v)
		}
		var unp interface{}
		Unmarshal(b, &unp)
		if !reflect.DeepEqual(v, unp) {
			t.Errorf("failed roundtripping: %#v: got %#v\n", v, unp)
		}
	}
}
