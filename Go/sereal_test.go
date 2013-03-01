package sereal

import (
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
	corpusFiles, err := filepath.Glob("test_dir/test_data_?????")
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

		err = Unmarshal(contents, &value)

		if err != nil {
			t.Errorf("unpacking %s generated an error: %v", corpusFile, err)
			continue
		}

		b, err := Marshal(value)

		if err != nil {
			t.Errorf("packing %s generated an error: %v", corpusFile, err)
			continue
		}

		ioutil.WriteFile(corpusFile+"-go.out", b, 0600)
	}
}

func TestSnappyEndToEndString(t *testing.T) {
	hugeString := ""

	for i := 0; i < 2048; i++ {
		hugeString += "a"
	}

	encoded, err := Marshal(hugeString)

	if err != nil {
		t.Errorf("encoding a huge string generated an error: %v", err)
		return
	}

	var decoded string
	err = Unmarshal(encoded, &decoded)

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
	hugeArray := make([]interface{}, 2048)

	for i := 0; i < 2048; i++ {
		hugeArray[i] = 4
	}

	encoded, err := Marshal(hugeArray)

	if err != nil {
		t.Errorf("encoding a huge array generated an error: %v", err)
		return
	}

	var decoded []interface{}
	err = Unmarshal(encoded, &decoded)

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
	hugeString := ""

	for i := 0; i < 2048; i++ {
		hugeString += "a"
	}

	hugeArray := []string{hugeString, hugeString}

	encoded, err := Marshal(hugeArray)

	if err != nil {
		t.Errorf("encoding a huge array generated an error: %v", err)
		return
	}

	var decoded []interface{}
	err = Unmarshal(encoded, &decoded)

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

	encoded, _ = Marshal(manydups)
	Unmarshal(encoded, &decoded)

	for i := 0; i < 2048; i++ {
		s, ok := decoded[i].(string)
		expected := "hello, world " + strconv.Itoa(i%10)
		if !ok || s != expected {
			t.Errorf("failed decompressing many-dup string: ok=%v s=%s expected=%s", ok, s, expected)
		}
	}
}
