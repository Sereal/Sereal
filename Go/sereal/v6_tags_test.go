package sereal

import (
	"math"
	"reflect"
	"strings"
	"testing"
)

func TestParseVString(t *testing.T) {
	// accepted and rejected spellings verified against perl's scan_vstring
	// through Sereal::Decoder
	for _, s := range []string{"v1.2.3", "1.2.3", "1.2", "1", "v", ".1", "v.1", "v1_0.2", "01.002.003", "v_1", "v1_", "_", "_.1", "1_.2", "1.1_", "1.2.3.4.5.6"} {
		if _, ok := parseVString([]byte(s)); !ok {
			t.Errorf("parseVString(%q) rejected, want accepted", s)
		}
	}

	for _, s := range []string{"v1..2", "1.", "x", "v1.2.3x", "", "V1", "v.", ".", "..", "1._", "1._1", "1.__", "._", "1.2._3"} {
		if _, ok := parseVString([]byte(s)); ok {
			t.Errorf("parseVString(%q) accepted, want rejected", s)
		}
	}

	utf8 := map[string]bool{
		"v127":                     false,
		"v128":                     true,
		"v1.300":                   true,
		"v99999999999999999999999": true,  // wraps, still > 127
		"v18446744073709551621":    false, // wraps to 5
		"v1.2.3":                   false,
		"v":                        false,
	}
	for s, want := range utf8 {
		got, ok := parseVString([]byte(s))
		if !ok || got != want {
			t.Errorf("parseVString(%q) = %v, %v, want %v, true", s, got, ok, want)
		}
		if (PerlVString{Spelling: s}).IsUTF8() != want {
			t.Errorf("PerlVString{%q}.IsUTF8() != %v", s, want)
		}
	}

	if (PerlVString{Spelling: "v1.2.3x"}).IsUTF8() {
		t.Errorf("IsUTF8 of an invalid spelling is true")
	}
}

func TestDecodeNegVarint(t *testing.T) {
	tests := []struct {
		doc  string
		want int
		err  error
	}{
		{"3df3726c06003c11", -17, nil},
		{"3df3726c06003ce807", -1000, nil},
		{"3df3726c06003c80808080808080808001", math.MinInt64, nil},
		{"3df3726c06003c00", 0, ErrCorrupt{errBadNegVarint}},
		{"3df3726c06003c81808080808080808001", 0, ErrCorrupt{errBadNegVarint}},
		// a 10-byte varint above 2^64 must not wrap (perl: "varint too big")
		{"3df3726c06003c80808080808080808003", 0, ErrCorrupt{errBadVarint}},
		{"3df3726c05003c11", 0, ErrCorrupt{errNegVarintVersion}},
		// v5 encoders use ZIGZAG, which v6 decoders still accept
		{"3df3726c05002121", -17, nil},
	}

	for _, tt := range tests {
		var got interface{}
		err := Unmarshal(mustHex(t, tt.doc), &got)
		if err != tt.err {
			t.Errorf("Unmarshal(%s) error = %v, want %v", tt.doc, err, tt.err)
			continue
		}
		if err == nil && got != tt.want {
			t.Errorf("Unmarshal(%s) = %#v, want %d", tt.doc, got, tt.want)
		}
	}

	var i64 int64
	if err := Unmarshal(mustHex(t, "3df3726c06003c11"), &i64); err != nil || i64 != -17 {
		t.Errorf("Unmarshal into int64 = %d, %v, want -17", i64, err)
	}

	var i32 int32
	if err := Unmarshal(mustHex(t, "3df3726c06003c11"), &i32); err != nil || i32 != -17 {
		t.Errorf("Unmarshal into int32 = %d, %v, want -17", i32, err)
	}

	if err := Unmarshal(mustHex(t, "3df3726c06003c00"), &i64); err != (ErrCorrupt{errBadNegVarint}) {
		t.Errorf("Unmarshal magnitude 0 into int64 error = %v", err)
	}
}

func TestDecodeVString(t *testing.T) {
	tests := []struct {
		doc      string
		spelling string
		utf8     bool
	}{
		{"3df3726c06003e260676312e322e33", "v1.2.3", false},
		{"3df3726c06003e26097636352e36362e3637", "v65.66.67", false},
		{"3df3726c06003e2706763330302e31", "v300.1", true},
		{"3df3726c06003e260476313237", "v127", false},
		{"3df3726c06003e270476313238", "v128", true},
	}

	for _, tt := range tests {
		for _, perlCompat := range []bool{false, true} {
			var got interface{}
			d := &Decoder{PerlCompat: perlCompat}
			if err := d.Unmarshal(mustHex(t, tt.doc), &got); err != nil {
				t.Errorf("Unmarshal(%s) error: %v", tt.doc, err)
				continue
			}
			v, ok := got.(*PerlVString)
			if !ok || v.Spelling != tt.spelling || v.IsUTF8() != tt.utf8 {
				t.Errorf("Unmarshal(%s) = %#v, want &PerlVString{%q}", tt.doc, got, tt.spelling)
			}
		}
	}

	doc := mustHex(t, tests[0].doc)

	var v PerlVString
	if err := Unmarshal(doc, &v); err != nil || v.Spelling != "v1.2.3" {
		t.Errorf("Unmarshal into PerlVString = %#v, %v", v, err)
	}

	var pv *PerlVString
	if err := Unmarshal(doc, &pv); err != nil || pv == nil || pv.Spelling != "v1.2.3" {
		t.Errorf("Unmarshal into *PerlVString = %#v, %v", pv, err)
	}

	var s string
	if err := Unmarshal(doc, &s); err != nil || s != "v1.2.3" {
		t.Errorf("Unmarshal into string = %q, %v", s, err)
	}

	var i int
	if err := Unmarshal(doc, &i); err == nil {
		t.Errorf("Unmarshal into int succeeded: %d", i)
	}

	errs := []struct {
		doc string
		err error
	}{
		{"3df3726c06003e2600", ErrCorrupt{errBadVString}},
		{"3df3726c06003e26026162", ErrCorrupt{errBadVString}},
		{"3df3726c06003e260476313238", ErrCorrupt{errVStringEncoding}},
		{"3df3726c06003e270476313237", ErrCorrupt{errVStringEncoding}},
		{"3df3726c06003e26067631", ErrTruncated},
		{"3df3726c06003e26", ErrTruncated},
		{"3df3726c06003e", ErrTruncated},
	}

	for _, tt := range errs {
		var got interface{}
		if err := Unmarshal(mustHex(t, tt.doc), &got); err != tt.err {
			t.Errorf("Unmarshal(%s) error = %v, want %v", tt.doc, err, tt.err)
		}

		var typed PerlVString
		if err := Unmarshal(mustHex(t, tt.doc), &typed); err != tt.err {
			t.Errorf("Unmarshal(%s) into PerlVString error = %v, want %v", tt.doc, err, tt.err)
		}
	}
}

func TestDecodeUndefDeadly(t *testing.T) {
	for _, policy := range []string{"", "undef", "warn", "die"} {
		for _, perlCompat := range []bool{false, true} {
			var warnings []string
			d := &Decoder{PerlCompat: perlCompat, UndefDeadlyPolicy: policy,
				UndefDeadlyWarningHandler: func(msg string) { warnings = append(warnings, msg) }}
			var got interface{}
			err := d.Unmarshal(mustHex(t, "3df3726c0600423eff3eff"), &got)
			if policy == "die" {
				if err != ErrUndefDeadlyTag {
					t.Fatalf("die: %v", err)
				}
				continue
			}
			if err != nil {
				t.Fatal(err)
			}
			want := []interface{}{nil, nil}
			if perlCompat {
				want = []interface{}{&PerlUndef{}, &PerlUndef{}}
			}
			if perlCompat {
				got = *(got.(*[]interface{}))
			}
			if !reflect.DeepEqual(got, want) {
				t.Errorf("%q compat=%v: got %#v, want %#v", policy, perlCompat, got, want)
			}
			count := 0
			if policy == "warn" {
				count = 2
			}
			if len(warnings) != count {
				t.Errorf("%q: warnings = %v", policy, warnings)
			}
		}
	}
	// Reflection must treat this exactly like an ordinary UNDEF, too.
	for _, target := range []interface{}{new(string), new(map[string]int), new([]int), new(*int)} {
		if err := Unmarshal(mustHex(t, "3df3726c06003eff"), target); err != nil {
			t.Errorf("%T: %v", target, err)
		}
		if err := (&Decoder{UndefDeadlyPolicy: "die"}).Unmarshal(mustHex(t, "3df3726c06003eff"), target); err != ErrUndefDeadlyTag {
			t.Errorf("%T die: %v", target, err)
		}
	}
	var got interface{}
	for _, bad := range []string{"ignore", "UNDEF", "0"} {
		if err := (&Decoder{UndefDeadlyPolicy: bad}).Unmarshal(mustHex(t, "3df3726c060007"), &got); err == nil {
			t.Errorf("accepted policy %q", bad)
		}
	}
	// These bytes inside a string are data, not a tag.
	if err := (&Decoder{UndefDeadlyPolicy: "die"}).Unmarshal(mustHex(t, "3df3726c0600623eff"), &got); err != nil {
		t.Fatal(err)
	}
	if err := Unmarshal(mustHex(t, "3df3726c06003e00"), &got); err != (ErrCorrupt{errUnknownExtend}) {
		t.Errorf("unassigned extension 0: %v", err)
	}
	if err := Unmarshal(mustHex(t, "3df3726c05003eff"), &got); err != (ErrCorrupt{errExtendVersion}) {
		t.Errorf("v5: %v", err)
	}
}

func TestDecodeExtendUnknownID(t *testing.T) {
	var got interface{}
	if err := Unmarshal(mustHex(t, "3df3726c06003e05"), &got); err != (ErrCorrupt{errUnknownExtend}) {
		t.Errorf("error = %v, want ErrCorrupt{%s}", err, errUnknownExtend)
	}
}

func TestDecodeExtendRequiresV6(t *testing.T) {
	var got interface{}
	if err := Unmarshal(mustHex(t, "3df3726c05003e260676312e322e33"), &got); err != (ErrCorrupt{errExtendVersion}) {
		t.Errorf("error = %v, want ErrCorrupt{%s}", err, errExtendVersion)
	}
}

func TestDecodeV6SampleBody(t *testing.T) {
	var got map[string]interface{}
	if err := Unmarshal(mustHex(t, vecPlainV6), &got); err != nil {
		t.Fatal(err)
	}

	want := map[string]interface{}{
		"name": []byte("sereal"),
		"n":    -1000,
		// perl's 2**40 is an NV, which the encoder stores as a FLOAT
		"list": []interface{}{1, -17, float32(1099511627776), []byte(strings.Repeat("x", 40))},
		"v":    &PerlVString{Spelling: "v1.2.3"},
	}
	if !reflect.DeepEqual(got, want) {
		t.Errorf("Unmarshal(plain_v6) =\n%#v\nwant\n%#v", got, want)
	}
}

// moved here from the envelope tests: the perl sample bodies use NEG_VARINT
// and EXTEND VSTRING
func TestDecodeChecksumSampleBodies(t *testing.T) {
	var w warnCollector
	d := keyedDecoder(vecKey(t), ChecksumDie, &w)

	var want interface{}
	if err := d.Unmarshal(mustHex(t, vecPlainV6), &want); err != nil {
		t.Fatal(err)
	}

	for _, p := range []string{vecRawV0Key, vecRawV1Key, vecRawV2Key, vecZlibV2Key, vecZstdV2Key, vecSnappyIncrV0Key, vecRawV1KeyWithHeader} {
		var got interface{}
		if err := d.Unmarshal(mustHex(t, p), &got); err != nil {
			t.Errorf("Unmarshal(%s...) error: %v", p[:24], err)
			continue
		}
		if !reflect.DeepEqual(got, want) {
			t.Errorf("Unmarshal(%s...) = %#v, want %#v", p[:24], got, want)
		}
	}

	var header map[string]interface{}
	var body interface{}
	if err := d.UnmarshalHeaderBody(mustHex(t, vecRawV1KeyWithHeader), &header, &body); err != nil {
		t.Fatal(err)
	}
	if want := (map[string]interface{}{"hdr": []byte("meta"), "n": 7}); !reflect.DeepEqual(header, want) {
		t.Errorf("header = %#v, want %#v", header, want)
	}
	if !reflect.DeepEqual(body, want) {
		t.Errorf("body = %#v, want %#v", body, want)
	}

	if len(w.msgs) != 0 {
		t.Errorf("unexpected warnings: %q", w.msgs)
	}
}
