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

func TestSnappyEndToEndString(t *testing.T) {

	e := &Encoder{}
	d := &Decoder{}

	hugeString := ""

	for i := 0; i < 2048; i++ {
		hugeString += "a"
	}

	encoded, err := e.Marshal(hugeString)

	if err != nil {
		t.Errorf("encoding a huge string generated an error: %v", err)
		return
	}

	var decoded string
	err = d.Unmarshal(encoded, &decoded)

	if err != nil {
		t.Errorf("decoding a huge string generated an error: %v", err)
		return
	}

	if hugeString != decoded {
		t.Errorf("end-to-end encoding + decoding of huge string didn't work")
		return
	}
}

func TestSnappyEndToEndArray(t *testing.T) {

	e := &Encoder{}
	d := &Decoder{}

	hugeArray := make([]interface{}, 2048)

	for i := 0; i < 2048; i++ {
		hugeArray[i] = 4
	}

	encoded, err := e.Marshal(hugeArray)

	if err != nil {
		t.Errorf("encoding a huge array generated an error: %v", err)
		return
	}

	var decoded []interface{}
	err = d.Unmarshal(encoded, &decoded)

	if err != nil {
		t.Errorf("decoding a huge array generated an error: %v", err)
		return
	}

	if !reflect.DeepEqual(hugeArray, decoded) {
		t.Errorf("end-to-end encoding + decoding of huge array didn't work")
		return
	}
}

func TestSnappyArray(t *testing.T) {

	e := &Encoder{}
	d := &Decoder{}

	hugeString := ""

	for i := 0; i < 2048; i++ {
		hugeString += "a"
	}

	hugeArray := []string{hugeString, hugeString}

	encoded, err := e.Marshal(hugeArray)

	if err != nil {
		t.Errorf("encoding a huge array generated an error: %v", err)
		return
	}

	var decoded []interface{}
	err = d.Unmarshal(encoded, &decoded)

	if err != nil {
		t.Errorf("decoding a huge array generated an error: %v", err)
		return
	}

	// XXX I think these should be the same object in memory as well
	if decoded[0].(string) != decoded[1].(string) {
		t.Errorf("decoding an array of two identical strings resulted in two different strings")
	}
	// XXX we should also test two structurally identical (but identically different) strings

	// test many duplicated strings -- this uses both the string table and snappy compressiong
	// this ensures we're not messing up the offsets when decoding
	manydups := make([]string, 2048)
	for i := 0; i < len(manydups); i++ {
		manydups[i] = "hello, world " + strconv.Itoa(i%10)
	}

	for i := range decoded {
		decoded[i] = nil
	}

	encoded, _ = e.Marshal(manydups)
	var decodedHW []interface{}
	d.Unmarshal(encoded, &decodedHW)

	if len(decodedHW) != 2048 {
		t.Errorf("got wrong number of elements back: wanted=%d got=%d\n", 2048, len(decodedHW))
	} else {
		for i := 0; i < 2048; i++ {
			s, ok := decodedHW[i].(string)
			expected := "hello, world " + strconv.Itoa(i%10)
			if !ok || s != expected {
				t.Errorf("failed decompressing many-dup string: ok=%v s=%s expected=%s", ok, s, expected)
			}
		}
	}
}

func TestNoCompatStruct(t *testing.T) {

	type A struct {
		Name     string
		Phone    string
		Siblings int
		Spouse   bool
		Money    float64
	}

	a := A{
		"mr foo",
		"12345",
		10,
		true,
		123.45,
	}

	e := &Encoder{}
	d := &Decoder{}

	x, err := e.Marshal(&a)
	if err != nil {
		panic(err)
	}

	var ua A
	err = d.Unmarshal(x, &ua)
	if err != nil {
		t.Errorf("struct unmarshalling failed: %s\n", err)
	}

	if !reflect.DeepEqual(a, ua) {
		t.Errorf("struct unmarshalling mismatch: got:%#v expected: %#v\n", ua, a)
	}

	// check we can unmarshal into a map too
	m := make(map[string]interface{})
	m["Name"] = a.Name
	m["Phone"] = a.Phone
	m["Siblings"] = a.Siblings
	m["Spouse"] = a.Spouse
	m["Money"] = a.Money

	var um map[string]interface{}

	err = d.Unmarshal(x, &um)
	if err != nil {
		t.Errorf("map unmarshalling failed: %s\n", err)
	}

	if !reflect.DeepEqual(m, um) {
		t.Errorf("map unmarshalling mismatch: got:%#v expected: %#v\n", um, m)
	}

}
