package sereal

import (
	"bytes"
	"encoding/hex"
	"errors"
	"strings"
	"testing"
)

func TestPrepareChecksumKeyVectors(t *testing.T) {
	tests := []struct {
		key, optKey string
		want        string
	}{
		{"", "", "37836edd9413d706d1067ef2e57a180f"},
		{"secret", "", "83182fefa01acd13bd33da94acac43b9"},
		{"secret", "salt", "7000483230ace57b62bcce4437cae6ea"},
		{"0123456789abcdef", "", "f5127cc67a45860d6f10630d765afb2f"},
		{strings.Repeat("x", 100), strings.Repeat("y", 40), "b46131cea89b4a55baf8305215f8c939"},
	}

	for _, tt := range tests {
		got := PrepareChecksumKey([]byte(tt.key), []byte(tt.optKey))
		if hex.EncodeToString(got[:]) != tt.want {
			t.Errorf("PrepareChecksumKey(%q, %q) = %x, want %s", tt.key, tt.optKey, got, tt.want)
		}
	}

	if got := PrepareChecksumKey(nil, nil); hex.EncodeToString(got[:]) != tests[0].want {
		t.Errorf("PrepareChecksumKey(nil, nil) = %x, want %s", got, tests[0].want)
	}
}

func TestGetChecksumSamplePackets(t *testing.T) {
	tests := []struct {
		name, packet, tag string
	}{
		{"raw_v0key", vecRawV0Key, "1c67b8b325a786ee"},
		{"raw_v1key", vecRawV1Key, "6008cd6c486fcbd2e0251c7a7735d072"},
		{"raw_v2key", vecRawV2Key, "5b9794d91f2b16954839b7ea1fbc4a3d"},
		{"zlib_v2key", vecZlibV2Key, "ef9d187819d8dac6edbbe02fd9da8593"},
		{"zstd_v2key", vecZstdV2Key, "fbfc6d7144dded95cf92911e216a4125"},
		{"snappy_incr_v0key", vecSnappyIncrV0Key, "08b05c3bbb1817af"},
		{"raw_v1key_with_header", vecRawV1KeyWithHeader, "2c00434ab6b2e66aab0fd29d29f44cca"},
		{"raw_v2key_7", vecRawV2Key7, "0f8ddc80c103af28700d68f7d168aa3b"},
		{"unknown_type3", vecUnknownType3, hex.EncodeToString([]byte(strings.Repeat("x", 16)))},
	}

	for _, tt := range tests {
		tag, err := GetChecksum(mustHex(t, tt.packet))
		if err != nil {
			t.Errorf("%s: GetChecksum error: %v", tt.name, err)
			continue
		}
		if hex.EncodeToString(tag) != tt.tag {
			t.Errorf("%s: GetChecksum = %x, want %s", tt.name, tag, tt.tag)
		}
	}

	tag, err := GetChecksum(mustHex(t, vecPlainV6))
	if tag != nil || err != nil {
		t.Errorf("GetChecksum(plain_v6) = %x, %v, want nil, nil", tag, err)
	}
}

func TestValidateChecksumSamplePackets(t *testing.T) {
	key := vecKey(t)
	for _, p := range []string{vecRawV0Key, vecRawV1Key, vecRawV2Key, vecZlibV2Key, vecZstdV2Key, vecSnappyIncrV0Key, vecRawV1KeyWithHeader, vecRawV2Key7} {
		ok, err := ValidateChecksum(mustHex(t, p), key)
		if !ok || err != nil {
			t.Errorf("ValidateChecksum(%s...) = %v, %v, want true, nil", p[:24], ok, err)
		}

		var wrong [16]byte
		copy(wrong[:], "AAAAAAAAAAAAAAAA")
		ok, err = ValidateChecksum(mustHex(t, p), &wrong)
		if ok || err != nil {
			t.Errorf("ValidateChecksum(%s..., wrong key) = %v, %v, want false, nil", p[:24], ok, err)
		}
	}

	for _, p := range []string{vecLegacySnappyType0, vecIncrSnappyType2, vecRawType1Short} {
		ok, err := ValidateChecksum(mustHex(t, p), vecKey915Bytes())
		if !ok || err != nil {
			t.Errorf("ValidateChecksum(%s) = %v, %v, want true, nil", p, ok, err)
		}
	}

	if _, err := ValidateChecksum(mustHex(t, vecRawV2Key), nil); err != ErrChecksumKeyMissing {
		t.Errorf("ValidateChecksum(nil key) error = %v, want ErrChecksumKeyMissing", err)
	}

	if ok, err := ValidateChecksum(mustHex(t, vecPlainV6), key); ok || err != nil {
		t.Errorf("ValidateChecksum(plain_v6) = %v, %v, want false, nil", ok, err)
	}

	if _, err := ValidateChecksum(mustHex(t, vecUnknownType3), key); err != ErrChecksumUnknownType {
		t.Errorf("ValidateChecksum(unknown_type3) error = %v, want ErrChecksumUnknownType", err)
	}

	// a valid MAC over a payload that is not exactly one value: the
	// payload length is the decoder's problem, not the MAC's
	if ok, err := ValidateChecksum(mustHex(t, vecTrailingByte), key); !ok || err != nil {
		t.Errorf("ValidateChecksum(trailing_byte) = %v, %v, want true, nil", ok, err)
	}

	// bytes after the declared payload are not authenticated
	b := append(mustHex(t, vecRawV2Key7), "garbage"...)
	if ok, err := ValidateChecksum(b, key); !ok || err != nil {
		t.Errorf("ValidateChecksum(raw_v2key_7+garbage) = %v, %v, want true, nil", ok, err)
	}

	// the envelope bit on a v5 header
	b = mustHex(t, vecRawV2Key7)
	b[4] = 0x85
	if _, err := ValidateChecksum(b, key); err != ErrChecksumVersion {
		t.Errorf("ValidateChecksum(v5 with checksum bit) error = %v, want ErrChecksumVersion", err)
	}
}

func TestReadChecksumEnvelopeMalformed(t *testing.T) {
	corrupt := func(s string) error { return ErrCorrupt{s} }
	withByte := func(s string, idx int, v byte) []byte {
		b := mustHex(t, s)
		b[idx] = v
		return b
	}

	tests := []struct {
		name string
		b    []byte
		err  error
	}{
		{"truncated after type", mustHex(t, "3df3726c860002"), ErrTruncated},
		{"tag length past the end", append(mustHex(t, "3df3726c860002ac02"), make([]byte, 20)...), corrupt(errBadChecksumLength)},
		{"type 2 with length 8", withByte(vecRawV2Key7, 7, 0x08), corrupt(errBadChecksumLength)},
		{"type 0 with length 16", withByte(vecRawV2Key7, 6, 0x00), corrupt(errBadChecksumLength)},
		{"payload length past the end", withByte(vecRawV2Key7, 24, 0x05), corrupt(errBadPayloadLength)},
	}

	for _, tt := range tests {
		if _, err := readChecksumEnvelope(tt.b, 6); err != tt.err {
			t.Errorf("%s: readChecksumEnvelope error = %v, want %v", tt.name, err, tt.err)
		}
	}

	env, err := readChecksumEnvelope(mustHex(t, vecUnknownType3), 6)
	if err != nil {
		t.Fatalf("unknown_type3: %v", err)
	}
	if env.known || env.typ != 3 || env.tagEnd-env.tagStart != 16 || env.payloadEnd-env.payloadStart != 1 {
		t.Errorf("unknown_type3: envelope = %+v", env)
	}

	env, err = readChecksumEnvelope(mustHex(t, vecRawV2Key7), 6)
	if err != nil {
		t.Fatalf("raw_v2key_7: %v", err)
	}
	want := checksumEnvelope{typ: 2, known: true, tagStart: 8, tagEnd: 24, payloadStart: 25, payloadEnd: 26}
	if env != want {
		t.Errorf("raw_v2key_7: envelope = %+v, want %+v", env, want)
	}
}

func TestChecksumMatches(t *testing.T) {
	b := mustHex(t, vecRawV2Key7)
	env, err := readChecksumEnvelope(b, 6)
	if err != nil {
		t.Fatal(err)
	}
	if !checksumMatches(b, env, vecKey(t)) {
		t.Errorf("checksumMatches(raw_v2key_7) = false")
	}

	b[25] = 0x08
	if checksumMatches(b, env, vecKey(t)) {
		t.Errorf("checksumMatches(tampered payload) = true")
	}
}

func TestProtectPacketMatchesPerl(t *testing.T) {
	plain := mustHex(t, vecPlainV6)
	for typ, want := range map[byte]string{0: vecRawV0Key, 1: vecRawV1Key, 2: vecRawV2Key} {
		got := protectPacket(plain, 6, typ, vecKey(t))
		if !bytes.Equal(got, mustHex(t, want)) {
			t.Errorf("protectPacket(plain_v6, type %d) =\n%x\nwant\n%s", typ, got, want)
		}
	}

	if !bytes.Equal(plain, mustHex(t, vecPlainV6)) {
		t.Errorf("protectPacket modified its input")
	}
}

func TestCompressedPayloadLength(t *testing.T) {
	payload := func(s string) []byte {
		b := mustHex(t, s)
		env, err := readChecksumEnvelope(b, 6)
		if err != nil {
			t.Fatal(err)
		}
		return b[env.payloadStart:env.payloadEnd]
	}

	tests := []struct {
		name    string
		doctype documentType
		payload []byte
	}{
		{"zlib", serealZlib, payload(vecZlibV2Key)},
		{"zstd", serealZstd, payload(vecZstdV2Key)},
		{"snappy incremental", serealSnappyIncremental, payload(vecSnappyIncrV0Key)},
		{"snappy legacy", serealSnappy, payload(vecLegacySnappyType0)},
	}

	for _, tt := range tests {
		n, err := compressedPayloadLength(tt.doctype, tt.payload)
		if err != nil || n != len(tt.payload) {
			t.Errorf("%s: compressedPayloadLength = %d, %v, want %d, nil", tt.name, n, err, len(tt.payload))
		}

		if tt.doctype == serealSnappy {
			continue // legacy snappy has no framing: it is always the whole payload
		}

		extra := append(append([]byte{}, tt.payload...), 0x00)
		n, err = compressedPayloadLength(tt.doctype, extra)
		if err != nil || n != len(extra)-1 {
			t.Errorf("%s + trailing byte: compressedPayloadLength = %d, %v, want %d, nil", tt.name, n, err, len(extra)-1)
		}

		_, err = compressedPayloadLength(tt.doctype, tt.payload[:len(tt.payload)-1])
		var corrupt ErrCorrupt
		if !errors.As(err, &corrupt) {
			t.Errorf("%s truncated: compressedPayloadLength error = %v, want ErrCorrupt", tt.name, err)
		}
	}
}
