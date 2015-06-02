package sereal

import (
	"encoding/hex"
	"reflect"
	"testing"
)

func TestV2(t *testing.T) {

	h := map[string]interface{}{
		"type":  "web",
		"count": []interface{}{12, 14, 12},
	}

	b := map[string]interface{}{
		"hello": "world",
		"foo": []interface{}{
			map[string]interface{}{"bar": 1},
			map[string]interface{}{"bar": 2},
		},
	}

	e := NewEncoderV2()

	enc, err := e.MarshalWithHeader(h, b)
	if err != nil {
		t.Fatal(err)
	}

	d := NewDecoder()

	var dh map[string]interface{}
	var db map[string]interface{}

	err = d.UnmarshalHeaderBody(enc, &dh, &db)
	if err != nil {
		t.Fatal(err)
	}

	if !reflect.DeepEqual(h, dh) {
		t.Errorf("failed to decode header:\ngot   : %#v\nexpect: %#v\n", dh, h)
	}

	if !reflect.DeepEqual(b, db) {
		t.Errorf("failed to decode body:\ngot   : %#v\nexpect: %#v\n", db, b)
	}

}

func TestV2Compat(t *testing.T) {

	// ugly because the hash values aren't typed as strings but as SHORT_BINARY
	h := map[string]interface{}{"type": []uint8{0x77, 0x65, 0x62}, "counts": []interface{}{12, 14, 12}}
	b := map[string]interface{}{"hello": []uint8{0x77, 0x6f, 0x72, 0x6c, 0x64}, "foo": []interface{}{map[string]interface{}{"bar": 1}, map[string]interface{}{"bar": 2}}}

	enc, _ := hex.DecodeString("3d73726c0216015264747970656377656266636f756e7473430c0e0c526568656c6c6f65776f726c6463666f6f42516362617201512f1402")

	var dh map[string]interface{}
	var db map[string]interface{}

	d := NewDecoder()

	err := d.UnmarshalHeaderBody(enc, &dh, &db)
	if err != nil {
		t.Fatal(err)
	}

	if !reflect.DeepEqual(h, dh) {
		t.Errorf("failed to decode header:\ngot   : %#v\nexpect: %#v\n", dh, h)
	}

	if !reflect.DeepEqual(b, db) {
		t.Errorf("failed to decode body:\ngot   : %#v\nexpect: %#v\n", db, b)
	}
}
