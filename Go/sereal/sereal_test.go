package sereal

import (
	"bytes"
	"encoding/hex"
	"errors"
	"io/ioutil"
	"os"
	"path/filepath"
	"reflect"
	"strconv"
	"testing"
	"time"

	"github.com/davecgh/go-spew/spew"
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
	testRoundtrip(t, false, 1)
	testRoundtrip(t, false, 2)
}

func TestRoundtripPerl(t *testing.T) {
	testRoundtrip(t, true, 1)
	testRoundtrip(t, true, 2)
}

func testRoundtrip(t *testing.T, perlCompat bool, version int) {

	e := &Encoder{PerlCompat: perlCompat, version: version}
	d := &Decoder{PerlCompat: false}

	for _, v := range roundtrips {
		b, err := e.Marshal(v)
		if err != nil {
			t.Errorf("failed marshalling with perlCompat=%t : %v: %s\n", perlCompat, v, err)
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
	expectPerlCompat := &[]interface{}{&map[string]interface{}{"foo": &[]interface{}{1, 2, 3}}}

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

func TestSnappyArray(t *testing.T) { testCompressedArray(t, "snappy", SnappyCompressor{true}) }
func TestZlibArray(t *testing.T)   { testCompressedArray(t, "zlib", ZlibCompressor{}) }

func testCompressedArray(t *testing.T, name string, compression compressor) {
	defer func() {
		if r := recover(); r != nil {
			// may happen due to bounds check
			t.Fatalf("shouldn't panic with bounds error: recovered %v", r)
		}
	}()

	e := &Encoder{}
	d := &Decoder{}

	// test many duplicated strings -- this uses both the string table and snappy compressiong
	// this ensures we're not messing up the offsets when decoding

	n := 2048
	manydups := make([]string, n)
	for i := 0; i < len(manydups); i++ {
		manydups[i] = "hello, world " + strconv.Itoa(i%10)
	}

	encoded, err := e.Marshal(manydups)
	if err != nil {
		t.Errorf("encoding a large array generated an error: %v", err)
		return
	}

	e.Compression = compression
	e.CompressionThreshold = 0 // always compress
	snencoded, err := e.Marshal(manydups)

	if err != nil {
		t.Fatalf("%s encoding a large array generated an error: %v", name, err)
	}

	if len(encoded) <= len(snencoded) {
		t.Fatalf("%s failed to compress redundant array: encoded=%d snappy=%d\n", name, len(encoded), len(snencoded))
	}

	var decoded []string
	err = d.Unmarshal(snencoded, &decoded)
	if err != nil {
		t.Fatalf("%s decoding generated error: %v", name, err)
	}

	if len(decoded) != n {
		t.Fatalf("got wrong number of elements back: wanted=%d got=%d\n", len(manydups), len(decoded))
	}

	for i := 0; i < n; i++ {
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
		Siblings int    `sereal:"-"` // don't serialize
	}

	type ALowerTags struct {
		Name  string `sereal:"name"`
		Phone string `sereal:"phone"`
	}

	type BInt int
	type AInt struct {
		B BInt
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
			"decode struct with lower-case field names",
			ALowerTags{Name: "mr foo", Phone: "12345"},
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
		{
			"struct with int field",
			AInt{B: BInt(3)},
			AInt{},
			AInt{B: BInt(3)},
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

func TestDecodeToStruct(t *testing.T) {
	type obj struct {
		ValueStr   string
		ValueByte  []byte
		ValueInt   int
		ValueSlice []float32
		ValueHash  map[string][]byte
	}

	exp := make([]obj, 3)
	exp[0] = obj{
		ValueStr:   "string as string value which actually should be 32+ characters",
		ValueByte:  []byte("string as binary value"),
		ValueInt:   10,
		ValueSlice: []float32{1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 17.0},
		ValueHash: map[string][]byte{
			"key1": []byte("unique value"),
			"key2": []byte("duplicate value"),
			"key3": []byte("deplicate value"),
		},
	}

	exp[1] = obj{
		ValueStr:   "another string as string value which actually should be 32+ characters",
		ValueByte:  []byte("another string as binary value"),
		ValueInt:   -10,
		ValueSlice: []float32{18.0, 19.0, 20.0},
		ValueHash: map[string][]byte{
			"key1": []byte("unique value"),
			"key2": []byte("duplicate value"),
			"key3": []byte("deplicate value"),
		},
	}

	exp[2] = exp[0]

	filename := "test_dir/test-decode-struct.srl"
	content, err := ioutil.ReadFile(filename)

	if err != nil {
		t.Skip("run 'make test_files' and try again")
		return
	}

	var slice []obj
	d := NewDecoder()

	if err := d.Unmarshal(content, &slice); err != nil {
		t.Errorf("error unmarshalling: %s", err)
	}

	if !reflect.DeepEqual(exp, slice) {
		t.Errorf("failed decode into struct:\n\nexp: %#v:\n\ngot %#v\n", exp, slice)
	}
}

func TestStructsWithPtrs(t *testing.T) {
	type First struct{ I int }
	type Second struct{ S string }
	type NestedPtr struct {
		A *First
		B *Second
	}
	tests := []struct {
		what     string
		input    interface{}
		outvar   interface{}
		expected interface{}
	}{
		{
			"struct with two fields of different types",
			NestedPtr{&First{1}, &Second{"two"}},
			NestedPtr{},
			NestedPtr{&First{1}, &Second{"two"}},
		},
		{
			"struct with two nils of different types",
			NestedPtr{},
			NestedPtr{},
			NestedPtr{},
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

		for i := 0; i < routvar.Elem().NumField(); i++ {
			outfield := routvar.Elem().Field(i)
			outfield.Interface()
			expfield := reflect.ValueOf(v.expected).Field(i)

			if !reflect.DeepEqual(outfield.Interface(), expfield.Interface()) {
				t.Errorf("roundtrip mismatch for %s: got: %#v expected: %#v\n", v.what, outfield.Interface(), expfield.Interface())
			}
		}
	}
}

type ErrorBinaryUnmarshaler int

var errUnmarshaler = errors.New("error binary unmarshaler")

func (e *ErrorBinaryUnmarshaler) UnmarshalBinary(data []byte) error {
	return errUnmarshaler
}

func TestBinaryMarshaller(t *testing.T) {

	// our data
	now := time.Now()

	e := &Encoder{}
	d := &Decoder{}

	x, err := e.Marshal(now)
	if err != nil {
		t.Errorf("error marshalling %s", err)
	}

	var tm time.Time

	// unpack into something that expects the bytes
	err = d.Unmarshal(x, &tm)

	if err != nil {
		t.Errorf("error unmarshalling: %s", err)
	}

	if !now.Equal(tm) {
		t.Errorf("failed unpacking: got=%v wanted=%v\n", tm, now)
	}

	// unpack into something that produces an error
	var errunmarshaler ErrorBinaryUnmarshaler
	err = d.Unmarshal(x, &errunmarshaler)
	if err == nil {
		t.Errorf("failed propagating error from unmarshaler")
	}

	// unpack into something that isn't a marshaller
	var i int
	err = d.Unmarshal(x, &i)
	if err == nil {
		t.Errorf("failed to generate error trying to unpack into non-slice/unmashaler")
		return
	}

	// unpack into a byte slice
	bdata, _ := now.MarshalBinary()

	var data []byte
	err = d.Unmarshal(x, &data)
	if err != nil {
		t.Errorf("error unpacking: %v", err)
		return
	}

	if !bytes.Equal(bdata, data) {
		t.Errorf("failed unpacking into byte-slice: got=%v wanted=%v\n", tm, now)
	}

	// unpack into a nil interface
	var intf interface{}
	err = d.Unmarshal(x, &intf)
	if err != nil {
		t.Errorf("error unpacking: %v", err)
		return
	}

	var pfreeze *PerlFreeze
	var ok bool

	if pfreeze, ok = intf.(*PerlFreeze); !ok {
		t.Errorf("failed unpacking into nil interface : got=%v", intf)
	}

	if pfreeze.Class != "time.Time" || !bytes.Equal(pfreeze.Data, bdata) {
		t.Errorf("failed unpacking into nil interface : got=%v", pfreeze)
	}

	// check that registering a type works
	var registerTime time.Time
	d.RegisterName("time.Time", &registerTime)

	// unpack into a nil interface should return a time.Time
	var tintf interface{}
	err = d.Unmarshal(x, &tintf)
	if err != nil {
		t.Errorf("error unpacking registered type: %s", err)
	}

	var rtime *time.Time
	if rtime, ok = tintf.(*time.Time); ok {
		if !now.Equal(*rtime) {
			t.Errorf("failed unpacking registered type: got=%v wanted=%v\n", rtime, now)
		}
	} else {
		t.Errorf("failed unpacking registered nil interface : got=%v", tintf)
	}

	// overwrite with our error type
	d.RegisterName("time.Time", &errunmarshaler)
	var eintf interface{}

	err = d.Unmarshal(x, &eintf)
	if err != errUnmarshaler {
		t.Errorf("failed to error unpacking registered error type: %s", err)
	}
}

func TestUnmarshalHeaderError(t *testing.T) {

	testcases := []struct {
		docHex string
		err    error
	}{
		// Garbage
		{"badbadbadbad", ErrBadHeader},
		// Version 1 and 2, "=srl"
		{"3d73726c0100", nil},
		{"3d73726c0200", nil},
		// Version 3, "=srl" with a high-bit-set-on-the-"s"
		{"3df3726c0300", nil},
		// Version 3, "=srl" corrupted by accidental UTF8 encoding
		{"3dc3b3726c0300", ErrBadHeaderUTF8},
		// Forbidden version 2 and high-bit-set-on-the-"s" combination
		{"3df3726c0200", ErrBadHeader},
		// Forbidden version 3 and obsolete "=srl" magic string
		{"3d73726c0300", ErrBadHeader},
		// Non-existing (yet) version 5, "=srl" with a high-bit-set-on-the-"s"
		{"3df3726c0500", errors.New("document version '5' not yet supported")},
	}

	d := NewDecoder()

	for i, tc := range testcases {
		doc, err := hex.DecodeString(tc.docHex)
		if err != nil {
			t.Error(err)
			continue
		}

		got := d.UnmarshalHeaderBody(doc, nil, nil)
		wanted := tc.err

		ok := false
		ok = ok || (got == nil && wanted == nil)
		ok = ok || (got != nil && wanted != nil && got.Error() == wanted.Error())
		if !ok {
			t.Errorf("test case #%v:\ngot   : %v\nwanted: %v", i, got, wanted)
			continue
		}
	}
}

func TestPrepareFreezeRoundtrip(t *testing.T) {
	_, err := os.Stat("test_freeze")
	if os.IsNotExist(err) {
		return
	}

	now := time.Now()

	type StructWithTime struct{ time.Time }

	tests := []struct {
		what     string
		input    interface{}
		outvar   interface{}
		expected interface{}
	}{

		{
			"Time",
			now,
			time.Time{},
			now,
		},
		{
			"Time_ptr",
			&now,
			&time.Time{},
			&now,
		},
		{
			"struct_Time",
			StructWithTime{now},
			StructWithTime{},
			StructWithTime{now},
		},
		{
			"struct_Time_ptr",
			&StructWithTime{now},
			&StructWithTime{},
			&StructWithTime{now},
		},
	}

	for _, compat := range []bool{false, true} {
		for _, v := range tests {
			e := Encoder{PerlCompat: compat}
			d := Decoder{}

			var name string
			if compat {
				name = "compat_" + v.what
			} else {
				name = v.what
			}

			rinput := reflect.ValueOf(v.input)

			x, err := e.Marshal(rinput.Interface())
			if err != nil {
				t.Errorf("error marshalling %s: %s\n", v.what, err)
				continue
			}

			err = ioutil.WriteFile("test_freeze/"+name+"-go.out", x, 0600)
			if err != nil {
				t.Error(err)
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
}

func TestFreezeRoundtrip(t *testing.T) {
	if os.Getenv("RUN_FREEZE") == "1" {
		d := Decoder{}

		buf, err := ioutil.ReadFile("test_freeze/Time-go.out")
		if err != nil {
			t.Error(err)
		}
		var then time.Time
		d.Unmarshal(buf, &then)

		type StructWithTime struct{ time.Time }
		tests := []struct {
			what     string
			outvar   interface{}
			expected interface{}
		}{

			{
				"Time",
				time.Time{},
				then,
			},
			{
				"Time_ptr",
				&time.Time{},
				&then,
			},
			{
				"struct_Time",
				StructWithTime{},
				StructWithTime{then},
			},
			{
				"struct_Time_ptr",
				&StructWithTime{},
				&StructWithTime{then},
			},
		}

		for _, v := range tests {
			for _, compat := range []string{"", "compat_"} {
				x, err := ioutil.ReadFile("test_freeze/" + compat + v.what + "-perl.out")
				if err != nil {
					t.Error(err)
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

	}
}

func TestIssue130(t *testing.T) {
	t.Skip("Issue 130")

	type AStructType struct {
		EmptySlice  []*AStructType
		EmptySlice2 []AStructType
	}

	t1 := &AStructType{}

	b, err := Marshal(t1)
	if err != nil {
		t.Fatal("failed to marshal:", err)
	}

	t12 := &AStructType{}
	err = Unmarshal(b, &t12)
	if err != nil {
		t.Fatal("failed to unmarshal:", err)
	}

	if !reflect.DeepEqual(t1, t12) {
		t.Errorf("roundtrip slice pointers failed\nwant\n%#v\ngot\n%#v", t1, t12)
	}
}

func TestIssue131(t *testing.T) {
	type A struct {
		T *time.Time
	}

	t0 := time.Now()
	a := A{T: &t0}

	b, err := Marshal(&a)
	if err != nil {
		t.Fatal(err)
	}

	var decoded A
	err = Unmarshal(b, &decoded)
	if err != nil {
		t.Fatal(err)
	}
}

func TestIssue135(t *testing.T) {
	type A struct {
		M map[string][]int
	}

	u := A{M: make(map[string][]int)}

	u.M["k99"] = []int{1, 2, 3}

	b, err := Marshal(&u)
	if err != nil {
		t.Fatal(err)
	}

	var decoded A
	err = Unmarshal(b, &decoded)
	if err != nil {
		t.Fatal(err)
	}
}

type errorsInMarshal struct{}

func (errorsInMarshal) MarshalBinary() ([]byte, error) {
	return nil, errors.New("this object refuses to serialize")
}

func TestIssue150(t *testing.T) {
	defer func() {
		if r := recover(); r != nil {
			// may happen due to bounds check
			t.Fatalf("shouldn't panic with bounds error: recovered %v", r)
		}
	}()

	b, err := Marshal(errorsInMarshal{})

	if b != nil {
		t.Fatal("Should not have serialized anything")
	}

	if err == nil || err.Error() != "this object refuses to serialize" {
		t.Fatalf("should get error from inner marshal call, got %v", err)
	}
}
