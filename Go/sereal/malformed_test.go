package sereal

import (
	"bytes"
	"errors"
	"fmt"
	"reflect"
	"runtime"
	"testing"
)

// callNoPanic runs fn and turns a panic escaping the library into a test
// failure, so that one malformed input does not abort the whole run
func callNoPanic(t *testing.T, what string, fn func() error) (err error) {
	t.Helper()
	defer func() {
		if r := recover(); r != nil {
			t.Errorf("%s: panic: %v", what, r)
			err = fmt.Errorf("panic: %v", r)
		}
	}()
	return fn()
}

type malformedS struct {
	A int
	B string
	J []interface{}
}

// decodeAllWays decodes doc into an interface{} (plain and PerlCompat), a
// struct and the header, and returns the errors keyed by path
func decodeAllWays(t *testing.T, doc []byte) map[string]error {
	t.Helper()
	errs := map[string]error{}
	errs["iface"] = callNoPanic(t, "iface", func() error {
		var v interface{}
		return Unmarshal(doc, &v)
	})
	errs["perl"] = callNoPanic(t, "perl", func() error {
		d := NewDecoder()
		d.PerlCompat = true
		var v interface{}
		return d.Unmarshal(doc, &v)
	})
	errs["struct"] = callNoPanic(t, "struct", func() error {
		var s malformedS
		return Unmarshal(doc, &s)
	})
	errs["header"] = callNoPanic(t, "header", func() error {
		var h interface{}
		return NewDecoder().UnmarshalHeader(doc, &h)
	})
	return errs
}

func TestMalformedHeader(t *testing.T) {
	tests := []struct {
		name string
		doc  string
		// paths that must fail; the others may succeed
		paths []string
	}{
		// header suffix length 2^63-1: headerSize+suffixSize wraps negative
		{"suffix length overflow", "3df3726c03f2ffffffffffffff7f01", []string{"iface", "perl", "struct", "header"}},
		// suffix length 0x7f with only one byte left
		{"suffix length past end", "3df3726c037f01", []string{"iface", "perl", "struct", "header"}},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			errs := decodeAllWays(t, mustHex(t, tt.doc))
			for _, p := range tt.paths {
				if errs[p] == nil {
					t.Errorf("%s: decoding %s succeeded, want an error", p, tt.doc)
				}
			}
		})
	}

	for _, doc := range []string{"3df3726c037f01", "3df3726c03f2ffffffffffffff7f01"} {
		err := callNoPanic(t, "merge", func() error {
			_, err := NewMerger().Append(mustHex(t, doc))
			return err
		})
		if err == nil {
			t.Errorf("Merger.Append(%s) succeeded, want an error", doc)
		}
	}
}

func TestMalformedHeaderEmptySuffix(t *testing.T) {
	// an empty suffix spelt as the padded varint 80 00: the suffix size is
	// two bytes, but there is no flags byte
	for _, doc := range []string{"3df3726c048000", "3d73726c028000"} {
		var h interface{}
		err := callNoPanic(t, doc, func() error { return NewDecoder().UnmarshalHeader(mustHex(t, doc), &h) })
		if err != nil || h != nil {
			t.Errorf("UnmarshalHeader(%s) = %v, %v, want no header and no error", doc, h, err)
		}
	}
}

func TestMalformedOffsets(t *testing.T) {
	tests := []struct {
		name string
		doc  string
	}{
		// COPY offset 0 of a v3 body is the header suffix flags byte
		{"copy into header", "3df3726c0301072f00"},
		// COPY offset 4 of a v1 document (absolute offsets) is the version byte
		{"v1 copy into header", "3d73726c01002f04"},
		// OBJECTV naming a class string that follows it
		{"objectv forward", "3df3726c03002d040163616263"},
		{"objectv_freeze forward", "3df3726c03003306282b0163616263"},
		// OBJECTV offset 0 is the last header suffix byte, a SHORT_BINARY_1
		{"objectv into header", "3df3726c03030000612d0001"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			errs := decodeAllWays(t, mustHex(t, tt.doc))
			for _, p := range []string{"iface", "perl", "struct"} {
				if errs[p] == nil {
					t.Errorf("%s: decoding %s succeeded, want an error", p, tt.doc)
				}
			}
		})
	}
}

func TestHeaderOffsetsRelativeToSuffix(t *testing.T) {
	// the encoder writes header offsets relative to the suffix flags byte
	hdr := []interface{}{map[string]interface{}{"key": 1}, map[string]interface{}{"key": 2}}
	for _, e := range []*Encoder{NewEncoderV2(), NewEncoderV3(), NewEncoderV6()} {
		b, err := e.MarshalWithHeader(hdr, 1)
		if err != nil {
			t.Fatal(err)
		}
		var h interface{}
		if err := NewDecoder().UnmarshalHeader(b, &h); err != nil || !reflect.DeepEqual(h, hdr) {
			t.Errorf("UnmarshalHeader(%x) = %#v, %v, want %#v", b, h, err, hdr)
		}
	}
}

// allocated returns the number of bytes allocated while running fn
func allocated(fn func()) uint64 {
	var before, after runtime.MemStats
	runtime.ReadMemStats(&before)
	fn()
	runtime.ReadMemStats(&after)
	return after.TotalAlloc - before.TotalAlloc
}

func TestMalformedCompressedLength(t *testing.T) {
	tests := []struct {
		name string
		doc  string
		// the error must be ErrMaxUncompressedSize
		tooLarge bool
	}{
		// zlib: declared uncompressed length 2^62, then a valid empty stream
		{"zlib length 2^62", "3df3726c330080808080808080804009789c63040000020002", true},
		// zlib: declared uncompressed length -2^63
		{"zlib length negative", "3df3726c33008080808080808080800109789c63040000020002", false},
		// zlib: declared uncompressed length 8 GiB
		{"zlib length 8 GiB", "3df3726c3300808080802009789c63040000020002", true},
		// snappy: declared decoded length 4 GiB - 1
		{"snappy length 4 GiB", "3df3726c230007ffffffff0f0001", true},
		// zstd: frame content size 512 MiB in a 22-byte frame
		{"zstd frame content size 512 MiB", "3df3726c44001028b52ffde00000002000000000010000", false},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			doc := mustHex(t, tt.doc)
			check := func(what string, err error) {
				t.Helper()
				if err == nil {
					t.Errorf("%s: succeeded, want an error", what)
				} else if tt.tooLarge && !errors.Is(err, ErrMaxUncompressedSize) {
					t.Errorf("%s: error %v, want ErrMaxUncompressedSize", what, err)
				}
			}

			var err error
			n := allocated(func() {
				err = callNoPanic(t, "unmarshal", func() error {
					var v interface{}
					return Unmarshal(doc, &v)
				})
			})
			check("Unmarshal", err)
			if n > 64<<20 {
				t.Errorf("Unmarshal allocated %d MiB", n>>20)
			}

			n = allocated(func() {
				err = callNoPanic(t, "merge", func() error {
					_, err := NewMerger().Append(doc)
					return err
				})
			})
			check("Merger.Append", err)
			if n > 64<<20 {
				t.Errorf("Merger.Append allocated %d MiB", n>>20)
			}

			n = allocated(func() {
				err = callNoPanic(t, "decompress", func() error {
					_, err := DecompressDocument(nil, doc)
					return err
				})
			})
			check("DecompressDocument", err)
			if n > 64<<20 {
				t.Errorf("DecompressDocument allocated %d MiB", n>>20)
			}
		})
	}
}

func TestDecompressDocumentEmptyBody(t *testing.T) {
	// a snappy document whose body decompresses to nothing, decompressed
	// into a non-empty buffer that is too short to be reused
	doc := mustHex(t, "3df3726c23000100")
	var got []byte
	err := callNoPanic(t, "decompress", func() (err error) {
		got, err = DecompressDocument(make([]byte, 4), doc)
		return err
	})
	if err != nil || !bytes.Equal(got, mustHex(t, "3df3726c0300")) {
		t.Errorf("DecompressDocument(%x) = %x, %v, want 3df3726c0300", doc, got, err)
	}
}

func TestMaxUncompressedSize(t *testing.T) {
	body := bytes.Repeat([]byte("sereal"), 200000) // 1.2 MB, compresses well

	for _, comp := range []compressor{SnappyCompressor{Incremental: true}, ZlibCompressor{}, ZstdCompressor{}} {
		e := NewEncoderV4()
		e.Compression = comp
		doc, err := e.Marshal(body)
		if err != nil {
			t.Fatal(err)
		}
		if len(doc) > len(body)/10 {
			t.Fatalf("%T: document of %d bytes is not compressed", comp, len(doc))
		}

		for _, tt := range []struct {
			limit int
			ok    bool
		}{{1 << 20, false}, {2 << 20, true}, {0, true}, {-1, true}} {
			d := NewDecoder()
			d.MaxUncompressedSize = tt.limit
			var got []byte
			err := d.Unmarshal(doc, &got)
			if tt.ok && (err != nil || !bytes.Equal(got, body)) {
				t.Errorf("%T: Unmarshal with MaxUncompressedSize %d: %v", comp, tt.limit, err)
			} else if !tt.ok && !errors.Is(err, ErrMaxUncompressedSize) {
				t.Errorf("%T: Unmarshal with MaxUncompressedSize %d: error %v, want ErrMaxUncompressedSize", comp, tt.limit, err)
			}

			m := NewMerger()
			m.MaxUncompressedSize = tt.limit
			_, err = m.Append(doc)
			if tt.ok && err != nil {
				t.Errorf("%T: Merger.Append with MaxUncompressedSize %d: %v", comp, tt.limit, err)
			} else if !tt.ok && !errors.Is(err, ErrMaxUncompressedSize) {
				t.Errorf("%T: Merger.Append with MaxUncompressedSize %d: error %v, want ErrMaxUncompressedSize", comp, tt.limit, err)
			}
		}
	}
}

// deepDoc returns a v3 document whose body is prefix repeated n times
// followed by leaf
func deepDoc(prefix []byte, n int, leaf ...byte) []byte {
	doc := []byte{0x3d, 0xf3, 0x72, 0x6c, 0x03, 0x00}
	doc = append(doc, bytes.Repeat(prefix, n)...)
	return append(doc, leaf...)
}

func TestMaxRecursionDepth(t *testing.T) {
	refn := []byte{typeREFN}
	arrayref1 := []byte{typeARRAYREF_0 + 1}
	array1 := []byte{typeARRAY, 0x01}
	hashref1 := []byte{typeHASHREF_0 + 1, typeSHORT_BINARY_0 + 1, 'k'}
	weaken := []byte{typeWEAKEN, typeREFN}

	decoders := map[string]func(d *Decoder, doc []byte) error{
		"iface": func(d *Decoder, doc []byte) error {
			var v interface{}
			return d.Unmarshal(doc, &v)
		},
		"perl": func(d *Decoder, doc []byte) error {
			d.PerlCompat = true
			var v interface{}
			return d.Unmarshal(doc, &v)
		},
		"struct": func(d *Decoder, doc []byte) error {
			var s malformedS
			return d.Unmarshal(doc, &s)
		},
		"slice": func(d *Decoder, doc []byte) error {
			var s []interface{}
			return d.Unmarshal(doc, &s)
		},
	}

	for _, nest := range []struct {
		name   string
		prefix []byte
		leaf   []byte
		paths  []string
	}{
		{"refn", refn, []byte{typeHASHREF_0}, []string{"iface", "perl", "struct"}},
		{"arrayref", arrayref1, []byte{0x01}, []string{"iface", "perl", "slice"}},
		{"array", array1, []byte{0x01}, []string{"iface", "perl", "slice"}},
		{"hashref", hashref1, []byte{0x01}, []string{"iface", "perl", "struct"}},
		{"weaken", weaken, []byte{0x01}, []string{"iface", "perl"}},
	} {
		for _, path := range nest.paths {
			dec := decoders[path]

			for _, tt := range []struct {
				limit, depth int
				ok           bool
			}{
				{0, 20000, false},
				{100, 150, false},
				{100, 40, true},
				{-1, 20000, true},
			} {
				d := NewDecoder()
				d.MaxRecursionDepth = tt.limit
				doc := deepDoc(nest.prefix, tt.depth, nest.leaf...)
				err := callNoPanic(t, nest.name+"/"+path, func() error { return dec(d, doc) })
				if tt.ok && err != nil {
					t.Errorf("%s/%s: depth %d, MaxRecursionDepth %d: %v", nest.name, path, tt.depth, tt.limit, err)
				} else if !tt.ok && !errors.Is(err, ErrMaxRecursionLimit) {
					t.Errorf("%s/%s: depth %d, MaxRecursionDepth %d: error %v, want ErrMaxRecursionLimit", nest.name, path, tt.depth, tt.limit, err)
				}
			}
		}
	}

	// user header data
	hdr := []byte{0x3d, 0xf3, 0x72, 0x6c, 0x03}
	suffix := append([]byte{0x01}, bytes.Repeat(refn, 20000)...)
	suffix = append(suffix, 0x01)
	hdr = varint(hdr, uint(len(suffix)))
	hdr = append(append(hdr, suffix...), 0x01)
	var h interface{}
	if err := callNoPanic(t, "header", func() error { return NewDecoder().UnmarshalHeader(hdr, &h) }); !errors.Is(err, ErrMaxRecursionLimit) {
		t.Errorf("UnmarshalHeader of 20000 nested REFN: error %v, want ErrMaxRecursionLimit", err)
	}
}

func TestMaxRecursionDepthStack(t *testing.T) {
	if testing.Short() {
		t.Skip("builds a 10 MB document")
	}
	// ten million nested REFN would overflow the goroutine stack
	body, err := ZlibCompressor{}.compress(append(bytes.Repeat([]byte{typeREFN}, 10000000), 0x01))
	if err != nil {
		t.Fatal(err)
	}
	doc := append([]byte{0x3d, 0xf3, 0x72, 0x6c, 0x33, 0x00}, body...)
	for _, perl := range []bool{false, true} {
		d := NewDecoder()
		d.PerlCompat = perl
		var v interface{}
		if err := d.Unmarshal(doc, &v); !errors.Is(err, ErrMaxRecursionLimit) {
			t.Errorf("PerlCompat %v: error %v, want ErrMaxRecursionLimit", perl, err)
		}
	}
}

func TestMaxRecursionDepthPerlRefnMemory(t *testing.T) {
	// each PerlCompat REFN level creates a new pointer type, which reflect
	// keeps forever: the depth limit bounds that
	doc := deepDoc([]byte{typeREFN}, 20000, 0x01)
	var err error
	n := allocated(func() {
		d := NewDecoder()
		d.PerlCompat = true
		var v interface{}
		err = callNoPanic(t, "perl", func() error { return d.Unmarshal(doc, &v) })
	})
	if !errors.Is(err, ErrMaxRecursionLimit) {
		t.Errorf("error %v, want ErrMaxRecursionLimit", err)
	}
	if n > 200<<20 {
		t.Errorf("allocated %d MiB", n>>20)
	}
}

func TestMergerMaxRecursionDepth(t *testing.T) {
	for _, tt := range []struct {
		limit, depth int
		ok           bool
	}{
		{0, 20000, false},
		{100, 150, false},
		{100, 40, true},
		{-1, 20000, true},
	} {
		for _, prefix := range [][]byte{{typeARRAYREF_0 + 1}, {typeHASHREF_0 + 1, typeSHORT_BINARY_0 + 1, 'k'}} {
			m := NewMerger()
			m.MaxRecursionDepth = tt.limit
			doc := deepDoc(prefix, tt.depth, 0x01)
			err := callNoPanic(t, "merge", func() error { _, err := m.Append(doc); return err })
			if tt.ok && err != nil {
				t.Errorf("depth %d, MaxRecursionDepth %d: %v", tt.depth, tt.limit, err)
			} else if !tt.ok && !errors.Is(err, ErrMaxRecursionLimit) {
				t.Errorf("depth %d, MaxRecursionDepth %d: error %v, want ErrMaxRecursionLimit", tt.depth, tt.limit, err)
			}
		}
	}
}
