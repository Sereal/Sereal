package sereal

// Native fuzz targets for the decoder, the merger and the header/checksum
// helpers. Any panic that escapes the library is a bug, and so is one the
// library recovers itself (ErrDecodePanic): malformed input must be caught
// by a check first. Each input must also finish within a time and
// allocation budget.
//
//	go test -run='^$' -fuzz='^FuzzUnmarshal$' -fuzztime=5m .
//	go test -run='^$' -fuzz='^FuzzMerge$' -fuzztime=5m .
//	go test -run='^$' -fuzz='^FuzzHeader$' -fuzztime=5m .

import (
	"encoding/hex"
	"errors"
	"os"
	"path/filepath"
	"runtime"
	"testing"
	"time"
)

// fuzzCheck fails t when err is a panic the library recovered
func fuzzCheck(t *testing.T, err error) {
	t.Helper()
	if errors.Is(err, ErrDecodePanic) {
		t.Fatal(err)
	}
}

const (
	fuzzMaxDuration = 2 * time.Second
	fuzzMaxAlloc    = 512 << 20
)

type fuzzAuditS1 struct {
	A int
	B string
}

type fuzzAuditS struct {
	A int
	B string
	C float64
	D bool
	E uint8
	F []byte
	G interface{}
	H map[string]interface{}
	I map[string]string
	J []interface{}
	K []string
	L fuzzAuditS1
	M *fuzzAuditS1
	N *int
	O **int
	P [4]byte
	Q map[string]*fuzzAuditS1
	R []fuzzAuditS1
	V PerlVString
}

// fuzzBudget runs fn and fails t when it takes too long or allocates too much
func fuzzBudget(t *testing.T, fn func()) {
	t.Helper()
	var before, after runtime.MemStats
	runtime.ReadMemStats(&before)
	start := time.Now()
	fn()
	elapsed := time.Since(start)
	runtime.ReadMemStats(&after)
	if elapsed > fuzzMaxDuration {
		t.Fatalf("input took %v", elapsed)
	}
	if alloc := after.TotalAlloc - before.TotalAlloc; alloc > fuzzMaxAlloc {
		t.Fatalf("input allocated %d bytes", alloc)
	}
}

func fuzzSeeds(f *testing.F) {
	values := []interface{}{
		nil, 1, -1, 300, -300, int64(-1 << 62), uint64(1 << 63), 1.5, float32(2.5),
		"hello", []byte("bytes"), true, false,
		[]interface{}{1, "a", []interface{}{2, 3}},
		map[string]interface{}{"a": 1, "b": []interface{}{"x", "x", "x"}},
		&PerlVString{Spelling: "v1.2.3"},
		fuzzAuditS{A: 1, B: "b", K: []string{"k", "k"}, M: &fuzzAuditS1{A: 2, B: "c"}},
	}
	shared := []interface{}{"dup", "dup"}
	values = append(values, map[string]interface{}{"x": shared, "y": shared})

	key := PrepareChecksumKey([]byte("fuzz"), nil)
	encoders := []*Encoder{NewEncoder(), NewEncoderV2(), NewEncoderV3(), NewEncoderV4(), NewEncoderV5(), NewEncoderV6()}
	for _, base := range encoders {
		for _, comp := range []compressor{nil, SnappyCompressor{Incremental: true}, ZlibCompressor{}, ZstdCompressor{}} {
			for _, perl := range []bool{false, true} {
				e := *base
				e.PerlCompat = perl
				e.Compression = comp
				e.CompressionThreshold = 0
				if e.version == 6 && perl {
					e.ChecksumV1Key = &key
				}
				for _, v := range values {
					if b, err := e.Marshal(v); err == nil {
						f.Add(b)
					}
				}
				if b, err := e.MarshalWithHeader(map[string]interface{}{"h": 1}, values[13]); err == nil {
					f.Add(b)
				}
			}
		}
	}

	for _, h := range []string{vecPlainV6, vecRawV0Key, vecRawV1Key, vecRawV2Key, vecZlibV2Key, vecZstdV2Key,
		vecSnappyIncrV0Key, vecRawV1KeyWithHeader, vecRawV2Key7, vecTrailingByte, vecTrailingPad,
		vecPayloadLen0, vecUnknownType3, vecLegacySnappyType0} {
		if b, err := hex.DecodeString(h); err == nil {
			f.Add(b)
		}
	}

	for _, pattern := range []string{"test_dir/test_data_?????", "../../Erlang/Sereal/test/cases/*/*.srl",
		"../../JavaScript/example/srl/*.srl", "../../ruby/test/*.srl", "../../objC/*.srl"} {
		files, _ := filepath.Glob(pattern)
		for _, file := range files {
			if b, err := os.ReadFile(file); err == nil && len(b) < 1<<16 {
				f.Add(b)
			}
		}
	}
}

func FuzzUnmarshal(f *testing.F) {
	fuzzSeeds(f)
	f.Fuzz(func(t *testing.T, data []byte) {
		key := PrepareChecksumKey([]byte("fuzz"), nil)
		fuzzBudget(t, func() {
			d := NewDecoder()
			d.ChecksumWarningHandler = func(string) {}
			var v interface{}
			fuzzCheck(t, d.Unmarshal(data, &v))
		})
		fuzzBudget(t, func() {
			d := NewDecoder()
			d.PerlCompat = true
			d.ChecksumV1Key = &key
			d.ChecksumWarningHandler = func(string) {}
			var h, v interface{}
			fuzzCheck(t, d.UnmarshalHeaderBody(data, &h, &v))
		})
		fuzzBudget(t, func() {
			d := NewDecoder()
			d.DisableReferentialIntegrity = true
			d.ChecksumWarningHandler = func(string) {}
			var s fuzzAuditS
			fuzzCheck(t, d.Unmarshal(data, &s))
		})
		fuzzBudget(t, func() {
			d := NewDecoder()
			d.ChecksumWarningHandler = func(string) {}
			var m map[string]interface{}
			var h fuzzAuditS1
			fuzzCheck(t, d.UnmarshalHeaderBody(data, &h, &m))
		})
	})
}

func FuzzMerge(f *testing.F) {
	fuzzSeeds(f)
	f.Fuzz(func(t *testing.T, data []byte) {
		for _, cfg := range []struct {
			top         topLevelElementType
			flat, dedup bool
		}{{TopLevelArrayRef, false, false}, {TopLevelArrayRef, true, true}, {TopLevelArray, true, false}} {
			fuzzBudget(t, func() {
				m := NewMerger()
				m.TopLevelElement = cfg.top
				m.KeepFlat = cfg.flat
				m.DedupeStrings = cfg.dedup
				if _, err := m.Append(data); err != nil {
					fuzzCheck(t, err)
					return
				}
				_, err := m.Append(data)
				fuzzCheck(t, err)
				out, err := m.Finish()
				if err != nil {
					fuzzCheck(t, err)
					return
				}
				var v interface{}
				fuzzCheck(t, Unmarshal(out, &v))
			})
		}
	})
}

func FuzzHeader(f *testing.F) {
	fuzzSeeds(f)
	f.Fuzz(func(t *testing.T, data []byte) {
		key := PrepareChecksumKey([]byte("fuzz"), nil)
		fuzzBudget(t, func() {
			_ = LooksLikeSereal(data)
			_, err := GetChecksum(data)
			fuzzCheck(t, err)
			_, err = ValidateChecksum(data, &key)
			fuzzCheck(t, err)
			_, err = DecompressDocument(nil, data)
			fuzzCheck(t, err)
			_, err = DecompressDocument(make([]byte, len(data)*2), data)
			fuzzCheck(t, err)
			_, err = DecompressDocument(make([]byte, len(data)/2), data)
			fuzzCheck(t, err)
			d := NewDecoder()
			d.ChecksumWarningHandler = func(string) {}
			var h interface{}
			fuzzCheck(t, d.UnmarshalHeader(data, &h))
		})
	})
}
