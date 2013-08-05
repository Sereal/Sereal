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
	-2613115362782646504,
	uint(0xdbbc596c24396f18),
	"hello",
	"hello, world",
	"twas brillig and the slithy toves and gyre and gimble in the wabe",
	float32(2.2),
	float32(9891234567890.098),
	float64(2.2),
	float64(9891234567890.098),
	[]interface{}{0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14},
	[]interface{}{0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15},
	[]interface{}{0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16},
	[]interface{}{1, 100, 1000, 2000, 0xdeadbeef, float32(2.2), "hello, world", map[string]interface{}{"foo": []interface{}{1, 2, 3}}},
	map[string]interface{}{"foo": 1, "bar": 2, "baz": "qux"},
}

func TestRoundtripGo(t *testing.T) {
	testRoundtrip(t, false)
}

func TestRoundtripPerl(t *testing.T) {
	testRoundtrip(t, true)
}

func testRoundtrip(t *testing.T, perlCompat bool) {

	e := &Encoder{PerlCompat: perlCompat}
	d := &Decoder{PerlCompat: false}

	for _, v := range roundtrips {
		b, err := e.Marshal(v)
		if err != nil {
			t.Errorf("failed marshalling with perlCompat=%t : %v\n", perlCompat, v)
		}
		var unp interface{}

		err = d.Unmarshal(b, &unp)
		if err != nil {
			t.Errorf("perl compat: error during unmarshall: %s\n", err)
		}
		if !reflect.DeepEqual(v, unp) {
			t.Errorf("failed roundtripping with perlCompat=%t: %#v: got %#v\n", perlCompat, v, unp)
		}
	}
}

func TestRoundtripCompat(t *testing.T) {

	input := []interface{}{map[string]interface{}{"foo": []interface{}{1, 2, 3}}}
	expectGo := []interface{}{map[string]interface{}{"foo": []interface{}{1, 2, 3}}}
	expectPerlCompat := []interface{}{&map[string]interface{}{"foo": &[]interface{}{1, 2, 3}}}

	e := &Encoder{}
	d := &Decoder{}

	noCompat, _ := e.Marshal(input)

	e.PerlCompat = true
	perlCompat, _ := e.Marshal(input)

	// no perl compat on encode, no perl compat on decode
	var nono interface{}
	err := d.Unmarshal(noCompat, &nono)
	if err != nil {
		t.Errorf("perl compat: error during unmarshall: %s\n", err)
	}
	if !reflect.DeepEqual(expectGo, nono) {
		t.Errorf("perl compat: no no failed: got %#v\n", nono)
	}

	// perl compat on encode, no perl compat on decode
	var yesno interface{}
	err = d.Unmarshal(perlCompat, &yesno)
	if err != nil {
		t.Errorf("perl compat: error during unmarshall: %s\n", err)
	}
	if !reflect.DeepEqual(expectGo, yesno) {
		t.Errorf("perl compat: yes no failed: got %#v\n", yesno)
	}

	d.PerlCompat = true

	// no perl compat on encode, perl compat on decode
	var noyes interface{}
	err = d.Unmarshal(noCompat, &noyes)
	if err != nil {
		t.Errorf("perl compat: error during unmarshall: %s\n", err)
	}

	if !reflect.DeepEqual(expectGo, noyes) {
		t.Errorf("perl compat: no yes failed: got %#v\n", noyes)
	}

	// perl compat on encode, yes perl compat on decode
	var yesyes interface{}

	err = d.Unmarshal(perlCompat, &yesyes)
	if err != nil {
		t.Errorf("perl compat: error during unmarshall: %s\n", err)
	}

	if !reflect.DeepEqual(expectPerlCompat, yesyes) {
		t.Errorf("perl compat: yes yes failed: got %#v\n", yesyes)
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
	err = d.Unmarshal(snencoded, &decoded)
	if err != nil {
		t.Fatalf("snappy decoding generated error: %v", err)
	}

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

	type nested1 struct {
		Person A
	}

	type nested struct {
		Nested1 nested1
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

	type ATags struct {
		Name     string `sereal:"Phone"`
		Phone    string `sereal:"Name"`
		Siblings int    // no tag, isn't unpacked
	}

	tests := []struct {
		what     string
		input    interface{}
		outvar   interface{}
		expected interface{}
	}{
		{
			"struct with fields",
			Afoo,
			A{},
			Afoo,
		},
		{
			"struct with fields into map",
			Afoo,
			map[string]interface{}{},
			map[string]interface{}{
				"Name":     "mr foo",
				"Phone":    "12345",
				"Siblings": 10,
				"Spouse":   true,
				"Money":    123.45,
			},
		},
		{
			"decode struct with tags",
			Afoo,
			ATags{},
			ATags{Name: "12345", Phone: "mr foo", Siblings: 0},
		},
		{
			"encode struct with tags",
			ATags{Name: "12345", Phone: "mr foo", Siblings: 10},
			A{},
			A{Name: "mr foo", Phone: "12345"},
		},
		{
			"struct with private fields",
			private{false, "hello", 3},
			private{}, // zero value for struct
			private{},
		},
		{
			"semi-private struct",
			semiprivate{Bool: true, pbool: false, String: "world", pstr: "hello", pint: 3},
			semiprivate{},
			semiprivate{Bool: true, String: "world"},
		},
		{
			"nil slice of structs",
			[]A{Afoo, Abar, Abaz},
			[]A(nil),
			[]A{Afoo, Abar, Abaz},
		},
		{
			"0-length slice of structs",
			[]A{Afoo, Abar, Abaz},
			[]A{},
			[]A{Afoo, Abar, Abaz},
		},
		{
			"1-length slice of structs",
			[]A{Afoo, Abar, Abaz},
			[]A{A{}},
			[]A{Afoo},
		},
		{
			"nested",
			nested{nested1{Afoo}},
			nested{},
			nested{nested1{Afoo}},
		},
	}

	e := &Encoder{}
	d := &Decoder{}

	for _, v := range tests {

		rinput := reflect.ValueOf(v.input)

		x, err := e.Marshal(rinput.Interface())
		if err != nil {
			t.Errorf("error marshalling %s: %s\n", v.what, err)
			continue
		}

		routvar := reflect.New(reflect.TypeOf(v.outvar))
		routvar.Elem().Set(reflect.ValueOf(v.outvar))

		err = d.Unmarshal(x, routvar.Interface())
		if err != nil {
			t.Errorf("error unmarshalling %s: %s\n", v.what, err)
			continue
		}

		if !reflect.DeepEqual(routvar.Elem().Interface(), v.expected) {
			t.Errorf("roundtrip mismatch for %s: got: %#v expected: %#v\n", v.what, routvar.Elem().Interface(), v.expected)
		}
	}
}
