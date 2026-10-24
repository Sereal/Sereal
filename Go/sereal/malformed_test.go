package sereal

import (
	"fmt"
	"reflect"
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
