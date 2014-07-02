package sereal

import (
	"code.google.com/p/snappy-go/snappy"
)

type SnappyCompressor struct {
	Incremental bool
}

func (c SnappyCompressor) compress(b []byte) ([]byte, error) {
	// XXX this could be more efficient!  I'm creating a new buffer to
	//     store the compressed document, which isn't necessary.  You
	//     could probably write directly to the slice after the header
	//     and after the varint holding the length
	compressed, err := snappy.Encode(nil, b)
	if err != nil {
		return nil, err
	}

	// shrink down b to reuse the allocated buffer
	b = b[:0]
	b = varint(b, uint(len(compressed)))
	b = append(b, compressed...)

	return b, nil
}

func (c SnappyCompressor) decompress(b []byte) ([]byte, error) {
	if c.Incremental {
		ln, sz := varintdecode(b)
		b = b[sz : sz+ln]
	}

	decompressed, err := snappy.Decode(nil, b)
	if err != nil {
		return nil, err
	}

	return decompressed, nil
}
