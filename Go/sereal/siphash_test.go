package sereal

import (
	"encoding/binary"
	"encoding/hex"
	"testing"
)

var sipTestKey = [16]byte{0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15}

// vectors generated with Sereal::Decoder::siphash24, siphash24_128 and
// siphash36 from the perl build
var sipHashVectors = []struct {
	data   string
	sip24  string
	sip128 string
	sip36  string
}{
	{"", "310e0edd47db6f72", "a3817f04ba25a8e66df67214c7550293", "37228a2b7a1e505b60ae19c48695eb27"},
	{"a", "ca4811a7e9e8a32b", "0ea4f2fb02ad21d92d5fc91684f26426", "702e5fd82fb0b0c875cb8123c5b4282b"},
	{"abcdefgh", "7044d491a3dd29c3", "8c93e201ac8e8396897e3133e447eda3", "3f6cb86db4351c887760e15c72ed0ac6"},
	{"abcdefghi", "a4ec9d6dc4e1a9b2", "247f7b6adc31aa1c1ec5cb1076ab0bd2", "5df18ae5838274bdd01091d4c6df0cdf"},
	{"The quick brown fox jumps over the lazy dog", "e46f1fdc05612752", "7628c9301aa4412555e65227cd31964e", "b343e2a6f2cc0ddadd51e16b5851a1ef"},
}

func TestSipHashVectors(t *testing.T) {
	for _, v := range sipHashVectors {
		var b64 [8]byte
		binary.LittleEndian.PutUint64(b64[:], sipHash24(&sipTestKey, []byte(v.data)))
		if got := hex.EncodeToString(b64[:]); got != v.sip24 {
			t.Errorf("sipHash24(%q) = %s, want %s", v.data, got, v.sip24)
		}

		h128 := sipHash24_128(&sipTestKey, []byte(v.data))
		if got := hex.EncodeToString(h128[:]); got != v.sip128 {
			t.Errorf("sipHash24_128(%q) = %s, want %s", v.data, got, v.sip128)
		}

		h36 := sipHash36_128(&sipTestKey, []byte(v.data))
		if got := hex.EncodeToString(h36[:]); got != v.sip36 {
			t.Errorf("sipHash36_128(%q) = %s, want %s", v.data, got, v.sip36)
		}
	}
}

func TestSipHashMultiPart(t *testing.T) {
	h := sipHash36_128(&sipTestKey, []byte("abcd"), []byte("efgh"))
	if got := hex.EncodeToString(h[:]); got != sipHashVectors[2].sip36 {
		t.Errorf("sipHash36_128(abcd, efgh) = %s, want %s", got, sipHashVectors[2].sip36)
	}

	h = sipHash36_128(&sipTestKey, []byte("abcdefgh"), []byte("i"))
	if got := hex.EncodeToString(h[:]); got != sipHashVectors[3].sip36 {
		t.Errorf("sipHash36_128(abcdefgh, i) = %s, want %s", got, sipHashVectors[3].sip36)
	}

	// odd splits crossing block boundaries in every algorithm
	data := []byte(sipHashVectors[4].data)
	for split := 0; split <= len(data); split++ {
		var b64 [8]byte
		binary.LittleEndian.PutUint64(b64[:], sipHash24(&sipTestKey, data[:split], nil, data[split:]))
		if got := hex.EncodeToString(b64[:]); got != sipHashVectors[4].sip24 {
			t.Fatalf("sipHash24 split at %d = %s, want %s", split, got, sipHashVectors[4].sip24)
		}

		h = sipHash24_128(&sipTestKey, data[:split], data[split:])
		if got := hex.EncodeToString(h[:]); got != sipHashVectors[4].sip128 {
			t.Fatalf("sipHash24_128 split at %d = %s, want %s", split, got, sipHashVectors[4].sip128)
		}
	}
}

func TestChecksumTagLengths(t *testing.T) {
	for typ, want := range map[byte]int{0: 8, 1: 16, 2: 16} {
		if got := len(checksumTag(typ, &sipTestKey, []byte("abc"))); got != want {
			t.Errorf("len(checksumTag(%d)) = %d, want %d", typ, got, want)
		}
	}

	if tag := checksumTag(3, &sipTestKey, []byte("abc")); tag != nil {
		t.Errorf("checksumTag(3) = %x, want nil", tag)
	}

	// the tag bytes are the little-endian hash output
	h := sipHash36_128(&sipTestKey, []byte("abcdefgh"))
	if got := checksumTag(2, &sipTestKey, []byte("abcd"), []byte("efgh")); string(got) != string(h[:]) {
		t.Errorf("checksumTag(2) = %x, want %x", got, h)
	}
}
