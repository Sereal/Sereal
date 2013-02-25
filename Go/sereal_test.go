package sereal

import (
	"errors"
	"io/ioutil"
	"path/filepath"
	"fmt"
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

func unmarshalSafely(contents []byte, dest interface{}) (err error) {
	defer func() {
		if r := recover(); r != nil {
			err = errors.New(fmt.Sprintf("Recovered %v", r))
		}
	}()

	err = Unmarshal(contents, dest)
	return err
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
	corpusFiles, err := filepath.Glob("test_dir/test_data_*")
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

		err = unmarshalSafely(contents, &value)

		if err != nil {
			t.Errorf("unpacking %s generated an error: %v", corpusFile, err)
		}
	}
}

func TestSnappyEndToEndString(t *testing.T) {
	hugeString := ""

	for i := 0; i < 2048; i++ {
		hugeString  += "a"
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

/*
func TestSnappyEndToEndArray(t *testing.T) {
	hugeArray  := make([]int, 2048)

	for i := 0; i < 2048; i++ {
		hugeArray[i] = 4
	}

	encoded, err := Marshal(hugeArray)

	if err != nil {
		t.Errorf("encoding a huge array generated an error: %v", err)
		return
	}

	var decoded []int
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
*/

func TestSnappyArray(t *testing.T) {
	hugeString := ""

	for i := 0; i < 2048; i++ {
		hugeString  += "a"
	}

	hugeArray   := make([]string, 2)
	hugeArray[0] = hugeString
	hugeArray[1] = hugeString

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
}
