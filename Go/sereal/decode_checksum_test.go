package sereal

import (
	"bytes"
	"errors"
	"io"
	"os"
	"reflect"
	"strings"
	"testing"
)

type warnCollector struct{ msgs []string }

func (w *warnCollector) handler(msg string) { w.msgs = append(w.msgs, msg) }

// keyedDecoder returns a PerlCompat decoder holding key for all three
// checksum types and collecting warnings into w
func keyedDecoder(key *[16]byte, policy ChecksumPolicy, w *warnCollector) *Decoder {
	return &Decoder{
		PerlCompat:             true,
		ChecksumV0Key:          key,
		ChecksumV1Key:          key,
		ChecksumV2Key:          key,
		ChecksumPolicy:         policy,
		ChecksumWarningHandler: w.handler,
	}
}

func TestDecodeChecksumSamplePackets(t *testing.T) {
	var w warnCollector

	d := keyedDecoder(vecKey915Bytes(), ChecksumVerifyIfAvailable, &w)
	for _, p := range []string{vecLegacySnappyType0, vecIncrSnappyType2, vecRawType1Short} {
		var got interface{}
		if err := d.Unmarshal(mustHex(t, p), &got); err != nil {
			t.Errorf("Unmarshal(%s) error: %v", p, err)
			continue
		}
		if !reflect.DeepEqual(got, []byte("k")) {
			t.Errorf("Unmarshal(%s) = %#v, want []byte(\"k\")", p, got)
		}
	}

	d = keyedDecoder(vecKey(t), ChecksumVerifyIfAvailable, &w)
	var got interface{}
	b := append(mustHex(t, vecRawV2Key7), "garbage"...)
	if err := d.Unmarshal(b, &got); err != nil || got != 7 {
		t.Errorf("Unmarshal(raw_v2key_7+garbage) = %v, %v, want 7", got, err)
	}

	if len(w.msgs) != 0 {
		t.Errorf("unexpected warnings: %q", w.msgs)
	}
}

// signedGoPacket encodes body (and header) at protocol version 5 with the
// given compressor, relabels it as version 6 and signs it
func signedGoPacket(t *testing.T, header, body interface{}, c compressor, typ byte, key *[16]byte) []byte {
	t.Helper()
	e := &Encoder{PerlCompat: true, version: 5, Compression: c, CompressionThreshold: 0}
	b, err := e.MarshalWithHeader(header, body)
	if err != nil {
		t.Fatal(err)
	}
	b[4] = b[4]&0xf0 | 6

	h, err := readHeader(b)
	if err != nil {
		t.Fatal(err)
	}
	return protectPacket(b, headerSize+h.suffixSize, typ, key)
}

func TestDecodeChecksumCompressedGoPackets(t *testing.T) {
	key := vecKey(t)
	body := []interface{}{"hello", strings.Repeat("x", 100), 42}
	header := map[string]interface{}{"hdr": "meta"}

	for _, c := range []compressor{nil, SnappyCompressor{Incremental: true}, ZlibCompressor{}, ZstdCompressor{}} {
		for typ := byte(0); typ <= 2; typ++ {
			b := signedGoPacket(t, header, body, c, typ, key)

			var w warnCollector
			d := keyedDecoder(key, ChecksumDie, &w)
			d.PerlCompat = false
			var gotBody []interface{}
			var gotHeader map[string]interface{}
			if err := d.UnmarshalHeaderBody(b, &gotHeader, &gotBody); err != nil {
				t.Errorf("%T type %d: error %v", c, typ, err)
				continue
			}
			if !reflect.DeepEqual(gotBody, body) {
				t.Errorf("%T type %d: body = %#v", c, typ, gotBody)
			}
			if !reflect.DeepEqual(gotHeader, header) {
				t.Errorf("%T type %d: header = %#v", c, typ, gotHeader)
			}

			// one byte appended inside the payload: still validly signed,
			// but the compression framing no longer ends at the payload end
			if c != nil {
				h, _ := readHeader(b)
				env, err := readChecksumEnvelope(b, headerSize+h.suffixSize)
				if err != nil {
					t.Fatal(err)
				}
				doc := append(append([]byte{}, b[:headerSize+h.suffixSize]...), b[env.payloadStart:env.payloadEnd]...)
				doc[4] &^= checksumFlag
				doc = append(doc, 0x00)
				long := protectPacket(doc, headerSize+h.suffixSize, typ, key)
				if err := d.UnmarshalHeaderBody(long, nil, &gotBody); err != ErrChecksumPayloadLength {
					t.Errorf("%T type %d + trailing byte: error = %v, want ErrChecksumPayloadLength", c, typ, err)
				}
			}
		}
	}
}

func TestDecodeChecksumPolicies(t *testing.T) {
	var wrong [16]byte
	copy(wrong[:], "AAAAAAAAAAAAAAAA")

	tests := []struct {
		name   string
		packet string
		key    *[16]byte
		policy ChecksumPolicy
		err    error
		warn   string
	}{
		{"no key, default", vecRawV2Key7, nil, ChecksumVerifyIfAvailable, nil, checksumWarnNoKey},
		{"no key, die", vecRawV2Key7, nil, ChecksumDie, ErrChecksumKeyMissing, ""},
		{"wrong key, default", vecRawV2Key7, &wrong, ChecksumVerifyIfAvailable, ErrChecksumMismatch, ""},
		{"wrong key, die", vecRawV2Key7, &wrong, ChecksumDie, ErrChecksumMismatch, ""},
		{"wrong key, ignore", vecRawV2Key7, &wrong, ChecksumIgnore, nil, ""},
		{"wrong key, ignore-warn", vecRawV2Key7, &wrong, ChecksumIgnoreWarn, nil, checksumWarnIgnored},
		{"right key, ignore-warn", vecRawV2Key7, vecKey(t), ChecksumIgnoreWarn, nil, checksumWarnIgnored},
		{"right key, die", vecRawV2Key7, vecKey(t), ChecksumDie, nil, ""},
		{"unknown type, default", vecUnknownType3, vecKey(t), ChecksumVerifyIfAvailable, nil, checksumWarnNoKey},
		{"unknown type, die", vecUnknownType3, vecKey(t), ChecksumDie, ErrChecksumUnknownType, ""},
		{"unknown type, ignore", vecUnknownType3, vecKey(t), ChecksumIgnore, nil, ""},
		{"unsigned, die", "3df3726c060007", vecKey(t), ChecksumDie, nil, ""},
	}

	for _, tt := range tests {
		var w warnCollector
		d := keyedDecoder(tt.key, tt.policy, &w)

		var got interface{}
		err := d.Unmarshal(mustHex(t, tt.packet), &got)
		if err != tt.err {
			t.Errorf("%s: error = %v, want %v", tt.name, err, tt.err)
			continue
		}
		if err == nil && got != 7 {
			t.Errorf("%s: got %#v, want 7", tt.name, got)
		}

		var wantWarn []string
		if tt.warn != "" {
			wantWarn = []string{tt.warn}
		}
		if !reflect.DeepEqual(w.msgs, wantWarn) {
			t.Errorf("%s: warnings = %q, want %q", tt.name, w.msgs, wantWarn)
		}
	}
}

func TestDecodeChecksumRequireKey(t *testing.T) {
	var w warnCollector
	var got interface{}

	d := &Decoder{ChecksumPolicy: ChecksumRequireKey, ChecksumWarningHandler: w.handler}
	if err := d.Unmarshal(mustHex(t, vecRawV2Key7), &got); err != ErrChecksumKeyRequired {
		t.Errorf("no keys: error = %v, want ErrChecksumKeyRequired", err)
	}

	d = &Decoder{ChecksumPolicy: ChecksumRequireKey, ChecksumV2Key: vecKey(t), ChecksumWarningHandler: w.handler}
	if err := d.Unmarshal(mustHex(t, vecPlainV6), &got); err != ErrChecksumRequired {
		t.Errorf("unsigned document: error = %v, want ErrChecksumRequired", err)
	}
	if err := d.Unmarshal(mustHex(t, vecRawV2Key7), &got); err != nil || got != 7 {
		t.Errorf("signed document: %v, %v, want 7", got, err)
	}

	// require_key needs the key of the document's own type
	d = &Decoder{ChecksumPolicy: ChecksumRequireKey, ChecksumV0Key: vecKey(t), ChecksumWarningHandler: w.handler}
	if err := d.Unmarshal(mustHex(t, vecRawV2Key7), &got); err != ErrChecksumKeyMissing {
		t.Errorf("key for another type: error = %v, want ErrChecksumKeyMissing", err)
	}

	if len(w.msgs) != 0 {
		t.Errorf("unexpected warnings: %q", w.msgs)
	}
}

func TestDecodeChecksumTamper(t *testing.T) {
	var w warnCollector
	d := keyedDecoder(vecKey(t), ChecksumVerifyIfAvailable, &w)

	b := mustHex(t, vecRawV2Key)
	b[len(b)-1] ^= 0x01
	var got interface{}
	if err := d.Unmarshal(b, &got); err != ErrChecksumMismatch {
		t.Errorf("tampered payload: error = %v, want ErrChecksumMismatch", err)
	}

	// the header user data is authenticated before it is decoded
	b = mustHex(t, vecRawV1KeyWithHeader)
	i := bytes.Index(b, []byte("meta"))
	b[i] = 'b'
	var header interface{}
	if err := d.UnmarshalHeader(b, &header); err != ErrChecksumMismatch {
		t.Errorf("tampered header: error = %v, want ErrChecksumMismatch", err)
	}
	if header != nil {
		t.Errorf("tampered header was decoded: %#v", header)
	}
}

func TestDecodeChecksumPayloadLengthMismatch(t *testing.T) {
	var w warnCollector
	d := keyedDecoder(vecKey(t), ChecksumVerifyIfAvailable, &w)

	for _, p := range []string{vecTrailingByte, vecTrailingPad} {
		var got interface{}
		if err := d.Unmarshal(mustHex(t, p), &got); err != ErrChecksumPayloadLength {
			t.Errorf("Unmarshal(%s) = %v, %v, want ErrChecksumPayloadLength", p, got, err)
		}

		var typed int
		if err := d.Unmarshal(mustHex(t, p), &typed); err != ErrChecksumPayloadLength {
			t.Errorf("Unmarshal(%s) into int = %v, %v, want ErrChecksumPayloadLength", p, typed, err)
		}
	}

	var got interface{}
	if err := d.Unmarshal(mustHex(t, vecPayloadLen0), &got); err != ErrTruncated {
		t.Errorf("Unmarshal(payload_len_0) = %v, %v, want ErrTruncated", got, err)
	}
}

func TestDecodeChecksumIgnoreStillValidatesFraming(t *testing.T) {
	var w warnCollector
	d := keyedDecoder(nil, ChecksumIgnore, &w)

	b := mustHex(t, vecRawV2Key7)
	b[7] = 0x08
	var got interface{}
	if err := d.Unmarshal(b, &got); err != (ErrCorrupt{errBadChecksumLength}) {
		t.Errorf("error = %v, want ErrCorrupt{%s}", err, errBadChecksumLength)
	}
}

func TestDecodeLegacySnappyNeedsEnvelope(t *testing.T) {
	var got interface{}
	if err := Unmarshal(mustHex(t, "3df3726c16000204616b"), &got); err != ErrBadSnappy {
		t.Errorf("error = %v, want ErrBadSnappy", err)
	}
}

func TestDecodeChecksumDefaultWarningHandler(t *testing.T) {
	r, wr, err := os.Pipe()
	if err != nil {
		t.Fatal(err)
	}
	stderr := os.Stderr
	os.Stderr = wr
	defer func() { os.Stderr = stderr }()

	var got interface{}
	err = NewDecoder().Unmarshal(mustHex(t, vecRawV2Key7), &got)
	os.Stderr = stderr
	wr.Close()
	out, _ := io.ReadAll(r)

	if err != nil || got != 7 {
		t.Errorf("Unmarshal = %v, %v, want 7", got, err)
	}
	if want := "sereal: " + checksumWarnNoKey + "\n"; string(out) != want {
		t.Errorf("stderr = %q, want %q", out, want)
	}
}

func TestLooksLikeSerealChecksum(t *testing.T) {
	if !LooksLikeSereal(mustHex(t, vecRawV2Key7)) {
		t.Errorf("LooksLikeSereal(raw_v2key_7) = false")
	}
}

func TestDecompressDocumentRefusesChecksummed(t *testing.T) {
	if _, err := DecompressDocument(nil, mustHex(t, vecZlibV2Key)); err != ErrChecksummedDocument {
		t.Errorf("error = %v, want ErrChecksummedDocument", err)
	}
}

func TestMergerRefusesChecksummed(t *testing.T) {
	m := NewMergerV3()
	if _, err := m.Append(mustHex(t, vecZlibV2Key)); !errors.Is(err, ErrChecksummedDocument) {
		t.Errorf("error = %v, want ErrChecksummedDocument", err)
	}
}
