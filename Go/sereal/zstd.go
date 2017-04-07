package sereal

import (
	"math"
)

// ZstdCompressor compresses a Sereal document using the zstd format.
type ZstdCompressor struct {
	Level int // compression level, set to ZstdDefaultCompression by default
}

// Zstd constants
const (
	ZstdBestSpeed          = 1
	ZstdBestCompression    = 20
	ZstdDefaultCompression = 3
)

func (c ZstdCompressor) compress(buf []byte) ([]byte, error) {
	// Prepend a compressed block with its length, i.e.:
	//
	// <Varint><Zstd Blob>
	// 1st varint indicates the length of the compressed document.
	//
	// XXX It's the naive implementation, better to rework as described in the spec:
	// https://github.com/Sereal/Sereal/blob/master/sereal_spec.pod#encoding-the-length-of-compressed-documents

	if c.Level == 0 {
		c.Level = ZstdDefaultCompression
	}

	tail, err := zstdEncode(buf, c.Level)
	if err != nil {
		return nil, err
	}

	var head []byte
	head = varint(head, uint(len(tail)))
	head = append(head, tail...)

	return head, nil
}

func (c ZstdCompressor) decompress(buf []byte) ([]byte, error) {
	// Read the claimed length of the compressed document
	ln, sz, err := varintdecode(buf)
	if err != nil {
		return nil, err
	}

	if ln < 0 || ln > math.MaxInt32 || sz+ln > len(buf) {
		return nil, ErrCorrupt{errBadOffset}
	}

	buf = buf[sz : sz+ln]

	return zstdDecode(buf)
}
