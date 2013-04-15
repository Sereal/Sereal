package sereal

import (
	"encoding/hex"
	"github.com/davecgh/go-spew/spew"
	"io/ioutil"
	"path/filepath"
	"reflect"
	"strconv"
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

	e := &Encoder{}
	d := &Decoder{}

	for _, v := range roundtrips {
		b, err := e.Marshal(v)
		if err != nil {
			t.Errorf("failed marshalling : %v\n", v)
		}
		var unp interface{}
		d.Unmarshal(b, &unp)
		if !reflect.DeepEqual(v, unp) {
			t.Errorf("failed roundtripping: %#v: got %#v\n", v, unp)
		}
	}
}

/*
 * To make the corpus of test files:
 * perl -I Perl/shared/t/lib/ -MSereal::TestSet -MSereal::Encoder -e'Sereal::TestSet::write_test_files("test_dir")'
 *
 * This runs the Decoder/Encoder over every file in the supplied directory and tells you when the bytes the encoder
 * outputs do not match the bytes in the test file. The purpose is to check if roundtripping to Perl type
 * datastructures works.
 *
 * If you pass a file as parameter it will do the same but do more detailed logging.
 *
 */
func TestCorpus(t *testing.T) {

	e := &Encoder{PerlCompat: true}
	d := &Decoder{PerlCompat: true}

	_ = e

	debug := false

	corpusFiles, err := filepath.Glob("test_dir/test_data_?????")

	//	corpusFiles, err = filepath.Glob("test_dir/test_data_00028")
	//	debug = true

	if err != nil {
		t.Errorf("error opening test_dir: %v", err)
		return
	}

	for _, corpusFile := range corpusFiles {

		t.Log("processing:", corpusFile)
		contents, err := ioutil.ReadFile(corpusFile)

		if err != nil {
			t.Errorf("error opening test_dir/%s: %v", corpusFile, err)
			return
		}

		var value interface{}

		if debug {
			t.Log("unmarshalling..")
			t.Log(hex.Dump(contents))
		}
		err = d.Unmarshal(contents, &value)

		if debug {
			t.Log("done")
		}

		if err != nil {
			t.Errorf("unpacking %s generated an error: %v", corpusFile, err)
			continue
		}

		if debug {
			t.Log("marshalling")
			t.Log("value=", spew.Sdump(value))
			t.Logf("    =%#v\n", value)
		}
		b, err := e.Marshal(value)

		if debug {
			t.Log("done")
			t.Log(hex.Dump(b))
		}

		if err != nil {
			t.Errorf("packing %s generated an error: %v", corpusFile, err)
			continue
		}

		ioutil.WriteFile(corpusFile+"-go.out", b, 0600)
	}
}

func TestSnappyArray(t *testing.T) {

	e := &Encoder{}
	d := &Decoder{}

	// test many duplicated strings -- this uses both the string table and snappy compressiong
	// this ensures we're not messing up the offsets when decoding

	manydups := make([]string, 2048)
	for i := 0; i < len(manydups); i++ {
		manydups[i] = "hello, world " + strconv.Itoa(i%10)
	}

	encoded, err := e.Marshal(manydups)
	if err != nil {
		t.Errorf("encoding a large array generated an error: %v", err)
		return
	}

	e.UseSnappy = true
	e.SnappyThreshold = 0 // always compress
	snencoded, err := e.Marshal(manydups)

	if err != nil {
		t.Fatalf("snappy encoding a large array generated an error: %v", err)
	}

	if len(encoded) <= len(snencoded) {
		t.Fatalf("snappy failed to compress redundant array: encoded=%d snappy=%d\n", len(encoded), len(snencoded))
	}

	var decoded []string
	d.Unmarshal(snencoded, &decoded)

	if len(decoded) != 2048 {
		t.Fatalf("got wrong number of elements back: wanted=%d got=%d\n", len(manydups), len(decoded))
	}

	for i := 0; i < 2048; i++ {
		s := decoded[i]
		expected := "hello, world " + strconv.Itoa(i%10)
		if s != expected {
			t.Errorf("failed decompressing many-dup string: s=%s expected=%s", s, expected)
		}
	}
}

func TestStructs(t *testing.T) {

	type A struct {
		Name     string
		Phone    string
		Siblings int
		Spouse   bool
		Money    float64
	}

	// some people
	Afoo := A{"mr foo", "12345", 10, true, 123.45}
	Abar := A{"mr bar", "54321", 5, false, 321.45}
	Abaz := A{"mr baz", "15243", 20, true, 543.21}
	pAbaz := &Abaz

	type B struct {
		Person1 A
		Person2 *A
		Person3 **A
	}

	type C struct {
		FieldB B
	}

	type private struct {
		pbool bool
		pstr  string
		pint  int
	}

	type semiprivate struct {
		Bool   bool
		pbool  bool
		String string
		pstr   string
		pint   int
	}

	tests := []struct {
		what     string
		input    interface{}
		outvar   interface{}
		expected interface{}
	}{
		{
			"struct with fields",
			&Afoo,
			&A{},
			&Afoo,
		},
		{
			"struct with fields into map",
			Afoo,
			&map[string]interface{}{},
			&map[string]interface{}{
				"Name":     "mr foo",
				"Phone":    "12345",
				"Siblings": 10,
				"Spouse":   true,
				"Money":    123.45,
			},
		},
		{
			"struct with private fields",
			private{false, "hello", 3},
			&private{}, // zero value for struct
			&private{},
		},
		{
			"semi-private struct",
			semiprivate{Bool: true, pbool: false, String: "world", pstr: "hello", pint: 3},
			&semiprivate{},
			&semiprivate{Bool: true, String: "world"},
		},
		{
			"array of structs",
			[]A{Afoo, Abar, Abaz},
			&[]A{},
			&[]A{Afoo, Abar, Abaz},
		},
		{
			"references",
			B{Afoo, &Abar, &pAbaz},
			&B{},
			&B{Afoo, &Abar, &pAbaz},
		},
		{
			"nested structs",
			C{B{Person1: Afoo}},
			&C{},
			&C{B{Person1: Afoo}},
		},
	}

	e := &Encoder{}
	d := &Decoder{}

	for _, v := range tests {

		rinput := reflect.ValueOf(v.input)

		x, err := e.Marshal(rinput.Interface())
		if err != nil {
			t.Errorf("error marshalling %s: %s\n", v.what, err)
		}

		routvar := reflect.ValueOf(&v.outvar)

		err = d.Unmarshal(x, routvar.Elem().Interface())
		if err != nil {
			t.Errorf("error unmarshalling %s: %s\n", v.what, err)
		}

		if !reflect.DeepEqual(v.outvar, v.expected) {
			t.Errorf("roundtrip mismatch for %s: got: %#v expected: %#v\n", v.what, v.outvar, v.expected)
		}
	}
}
