package sereal

import (
	"bytes"
	"fmt"
	"testing"

	"github.com/klauspost/compress/zstd"
)

// Perl requires Frame_Content_Size, which the pure-Go encoder used to omit
// below 256 bytes. Check the actual frame rather than just a Go round trip.
// Run this with and without -tags clibs to cover both compression backends.
func TestZstdFrameContentSize(t *testing.T) {
	for _, size := range []int{1, 255, 256, 1 << 20} {
		t.Run(fmt.Sprint(size), func(t *testing.T) {
			body := bytes.Repeat([]byte{7}, size)
			compressed, err := (ZstdCompressor{}).compress(body)
			if err != nil {
				t.Fatal(err)
			}
			length, prefix, err := varintdecode(compressed)
			if err != nil || length != len(compressed)-prefix {
				t.Fatalf("invalid compressed length: %d, %d, %v", length, prefix, err)
			}
			var header zstd.Header
			if err := header.Decode(compressed[prefix:]); err != nil {
				t.Fatal(err)
			}
			if !header.HasFCS || header.FrameContentSize != uint64(size) {
				t.Fatalf("HasFCS=%v FrameContentSize=%d; want true, %d", header.HasFCS, header.FrameContentSize, size)
			}
			decoded, err := (ZstdCompressor{}).decompress(nil, compressed, DefaultMaxUncompressedSize)
			if err != nil || !bytes.Equal(decoded, body) {
				t.Fatalf("Zstd round trip failed: %v", err)
			}
		})
	}
}

func TestZstdSmallDocument(t *testing.T) {
	for _, version := range []int{4, 5, 6} {
		t.Run(fmt.Sprint(version), func(t *testing.T) {
			e := &Encoder{version: version, Compression: ZstdCompressor{}, CompressionThreshold: 0}
			doc, err := e.Marshal(7)
			if err != nil {
				t.Fatal(err)
			}
			header, err := readHeader(doc)
			if err != nil || header.doctype != serealZstd {
				t.Fatalf("expected a Zstd document: %+v, %v", header, err)
			}
			body := doc[headerSize+header.suffixSize:]
			_, prefix, err := varintdecode(body)
			if err != nil {
				t.Fatal(err)
			}
			var frame zstd.Header
			if err := frame.Decode(body[prefix:]); err != nil {
				t.Fatal(err)
			}
			if !frame.HasFCS || frame.FrameContentSize != 1 {
				t.Fatalf("small document omits its one-byte content size: %+v", frame)
			}
			var got interface{}
			if err := Unmarshal(doc, &got); err != nil || got != 7 {
				t.Fatalf("round trip = %v, %v; want 7", got, err)
			}
		})
	}
}
