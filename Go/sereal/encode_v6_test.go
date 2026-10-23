package sereal

import (
	"bytes"
	"encoding/hex"
	"math"
	"reflect"
	"strings"
	"testing"
)

// encodedBody returns the body of an unsigned document without header user
// data
func encodedBody(t *testing.T, e *Encoder, v interface{}) ([]byte, error) {
	t.Helper()
	b, err := e.Marshal(v)
	if err != nil {
		return nil, err
	}
	return b[headerSize+1:], nil
}

func TestEncodeV6Header(t *testing.T) {
	b, err := NewEncoderV6().Marshal(7)
	if err != nil {
		t.Fatal(err)
	}
	if got := hex.EncodeToString(b); got != "3df3726c060007" {
		t.Errorf("Marshal(7) = %s, want 3df3726c060007", got)
	}
}

func TestEncodeNegVarint(t *testing.T) {
	tests := []struct {
		v    interface{}
		body string
	}{
		{-17, "3c11"},
		{-1000, "3ce807"},
		{int64(math.MinInt64), "3c80808080808080808001"},
		{-16, "10"},
		{-1, "1f"},
		{int8(-17), "3c11"},
		{int16(-300), "3cac02"},
		{int32(-17), "3c11"},
		{uint64(math.MaxUint64), "20ffffffffffffffffff01"},
	}

	for _, tt := range tests {
		body, err := encodedBody(t, NewEncoderV6(), tt.v)
		if err != nil {
			t.Errorf("Marshal(%T %v) error: %v", tt.v, tt.v, err)
			continue
		}
		if got := hex.EncodeToString(body); got != tt.body {
			t.Errorf("Marshal(%T %v) body = %s, want %s", tt.v, tt.v, got, tt.body)
		}
	}

	// reflection path: a named integer type
	type myInt int
	body, err := encodedBody(t, NewEncoderV6(), []myInt{-17})
	if err != nil || hex.EncodeToString(body) != "2b013c11" {
		t.Errorf("Marshal([]myInt{-17}) body = %x, %v, want 2b013c11", body, err)
	}

	// protocol 5 keeps using ZIGZAG
	body, err = encodedBody(t, NewEncoderV5(), -17)
	if err != nil || hex.EncodeToString(body) != "2121" {
		t.Errorf("v5 Marshal(-17) body = %x, %v, want 2121", body, err)
	}

	// roundtrip through the decoder
	for _, v := range []int{-17, -1000, math.MinInt64, math.MinInt64 + 1} {
		b, err := NewEncoderV6().Marshal(v)
		if err != nil {
			t.Fatal(err)
		}
		var got int
		if err := Unmarshal(b, &got); err != nil || got != v {
			t.Errorf("roundtrip %d = %d, %v", v, got, err)
		}
	}
}

func TestEncodeVString(t *testing.T) {
	tests := []struct {
		v    interface{}
		body string
	}{
		{PerlVString{Spelling: "v1.2.3"}, "3e260676312e322e33"},
		{&PerlVString{Spelling: "v300.1"}, "3e2706763330302e31"},
		{PerlVString{Spelling: "v127"}, "3e260476313237"},
		{PerlVString{Spelling: "v128"}, "3e270476313238"},
	}

	for _, tt := range tests {
		body, err := encodedBody(t, NewEncoderV6(), tt.v)
		if err != nil {
			t.Errorf("Marshal(%#v) error: %v", tt.v, err)
			continue
		}
		if got := hex.EncodeToString(body); got != tt.body {
			t.Errorf("Marshal(%#v) body = %s, want %s", tt.v, got, tt.body)
		}
	}

	for _, v := range []interface{}{PerlVString{Spelling: "v1.2.3x"}, &PerlVString{Spelling: ""}, []interface{}{PerlVString{Spelling: "1."}}} {
		if _, err := NewEncoderV6().Marshal(v); err != ErrBadVString {
			t.Errorf("Marshal(%#v) error = %v, want ErrBadVString", v, err)
		}
	}

	if _, err := NewEncoderV5().Marshal(PerlVString{Spelling: "v1.2.3"}); err != ErrVStringVersion {
		t.Errorf("v5 Marshal(v-string) error = %v, want ErrVStringVersion", err)
	}

	b, err := NewEncoderV6().Marshal(PerlVString{Spelling: "v1.2.3"})
	if err != nil {
		t.Fatal(err)
	}
	var got interface{}
	if err := (&Decoder{PerlCompat: true}).Unmarshal(b, &got); err != nil || !reflect.DeepEqual(got, &PerlVString{Spelling: "v1.2.3"}) {
		t.Errorf("roundtrip = %#v, %v", got, err)
	}
}

func TestEncodeChecksumMatchesPerl(t *testing.T) {
	e := &Encoder{version: 6, PerlCompat: true, ChecksumV2Key: vecKey(t)}
	b, err := e.Marshal(7)
	if err != nil {
		t.Fatal(err)
	}
	if got := hex.EncodeToString(b); got != vecRawV2Key7 {
		t.Errorf("Marshal(7) = %s, want %s", got, vecRawV2Key7)
	}
}

func TestEncodeChecksumRoundtrip(t *testing.T) {
	key := vecKey(t)
	var other [16]byte
	copy(other[:], "AAAAAAAAAAAAAAAA")

	body := map[string]interface{}{
		"name": "sereal",
		"n":    -1000,
		"list": []interface{}{1, -17, 1 << 40, strings.Repeat("x", 40)},
		"v":    &PerlVString{Spelling: "v1.2.3"},
	}
	header := map[string]interface{}{"hdr": "meta", "n": 7}

	compressors := []struct {
		c       compressor
		doctype documentType
	}{
		{nil, serealRaw},
		{SnappyCompressor{Incremental: true}, serealSnappyIncremental},
		{ZlibCompressor{}, serealZlib},
		{ZstdCompressor{}, serealZstd},
	}

	for _, comp := range compressors {
		for typ := byte(0); typ <= 2; typ++ {
			for _, withHeader := range []bool{false, true} {
				e := &Encoder{version: 6, Compression: comp.c, CompressionThreshold: 0}
				d := &Decoder{ChecksumPolicy: ChecksumDie}
				switch typ {
				case 0:
					e.ChecksumV0Key, d.ChecksumV0Key = key, key
				case 1:
					e.ChecksumV1Key, d.ChecksumV1Key = key, key
				case 2:
					e.ChecksumV2Key, d.ChecksumV2Key = key, key
				}

				var hdr interface{}
				if withHeader {
					hdr = header
				}

				b, err := e.MarshalWithHeader(hdr, body)
				name := func() string { return reflect.TypeOf(comp.c).String() + " type " + string('0'+typ) }
				if err != nil {
					t.Errorf("%s: Marshal error: %v", name(), err)
					continue
				}

				if b[4]&checksumFlag == 0 || documentType((b[4]>>4)&doctypeMask) != comp.doctype || b[4]&0x0f != 6 {
					t.Errorf("%s: version-type byte = %#x", name(), b[4])
				}

				tag, err := GetChecksum(b)
				if err != nil || len(tag) != checksumTagLength(typ) {
					t.Errorf("%s: GetChecksum = %x, %v", name(), tag, err)
				}

				if ok, err := ValidateChecksum(b, key); !ok || err != nil {
					t.Errorf("%s: ValidateChecksum = %v, %v", name(), ok, err)
				}
				if ok, err := ValidateChecksum(b, &other); ok || err != nil {
					t.Errorf("%s: ValidateChecksum(other key) = %v, %v", name(), ok, err)
				}

				var gotHeader map[string]interface{}
				var gotBody map[string]interface{}
				if err := d.UnmarshalHeaderBody(b, &gotHeader, &gotBody); err != nil {
					t.Errorf("%s: Unmarshal error: %v", name(), err)
					continue
				}
				if !reflect.DeepEqual(gotBody, body) {
					t.Errorf("%s: body = %#v, want %#v", name(), gotBody, body)
				}
				if withHeader && !reflect.DeepEqual(gotHeader, header) {
					t.Errorf("%s: header = %#v, want %#v", name(), gotHeader, header)
				}
			}
		}
	}
}

func TestEncodeChecksumErrors(t *testing.T) {
	key := vecKey(t)

	e := NewEncoderV6()
	e.ChecksumV0Key, e.ChecksumV2Key = key, key
	if _, err := e.Marshal(7); err != ErrChecksumMultipleKeys {
		t.Errorf("two keys: error = %v, want ErrChecksumMultipleKeys", err)
	}

	e = NewEncoderV5()
	e.ChecksumV1Key = key
	if _, err := e.Marshal(7); err != ErrChecksumKeyVersion {
		t.Errorf("v5 with key: error = %v, want ErrChecksumKeyVersion", err)
	}

	e = NewEncoderV6()
	e.ChecksumPolicy = ChecksumRequireKey
	if _, err := e.Marshal(7); err != ErrChecksumKeyRequired {
		t.Errorf("require_key without key: error = %v, want ErrChecksumKeyRequired", err)
	}

	e = &Encoder{ChecksumV2Key: key}
	if _, err := e.Marshal(7); err != ErrChecksumKeyVersion {
		t.Errorf("zero-value encoder with key: error = %v, want ErrChecksumKeyVersion", err)
	}

	// require_key with a key signs as usual
	e = NewEncoderV6()
	e.ChecksumPolicy, e.ChecksumV2Key = ChecksumRequireKey, key
	b, err := e.Marshal(7)
	if err != nil || !bytes.Equal(b, mustHex(t, vecRawV2Key7)) {
		t.Errorf("require_key with key = %x, %v, want %s", b, err, vecRawV2Key7)
	}
}
