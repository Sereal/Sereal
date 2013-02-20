package sereal

import (
	"errors"
	"io/ioutil"
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
	corpusFiles, err := ioutil.ReadDir("test_dir")
	if err != nil {
	    t.Errorf("error opening test_dir: %v", err)
	    return
	}

	for _, corpusFile := range corpusFiles {
		contents, err := ioutil.ReadFile("test_dir/" + corpusFile.Name())

		if err != nil {
			t.Errorf("error opening test_dir/%s: %v", corpusFile.Name(), err)
			return
		}
		var value interface{}

		err = unmarshalSafely(contents, &value)

		if err != nil {
			t.Errorf("unpacking %s generated an error: %v", corpusFile.Name(), err)
		}
	}
}
