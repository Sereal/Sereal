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
	0,
	-1,
	-15,
	15,
	-16,
	16,
	17,
	-17,
	"hello",
	"hello, world",
	"twas brillig and the slithy toves and gyre and gimble in the wabe",
	float32(2.2),
	float32(9891234567890.098),
	float64(2.2),
	float64(9891234567890.098),
	[]interface{}{1, 100, 1000, 2000, 0xdeadbeef, float32(2.2), "hello, world", map[string]interface{}{"foo": []interface{}{1, 2, 3}}},
	map[string]interface{}{"foo": 1, "bar": 2, "baz": "qux"},
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
