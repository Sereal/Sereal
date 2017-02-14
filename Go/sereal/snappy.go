package sereal

import (
	"math"

	"github.com/golang/snappy"
)

// SnappyCompressor compresses a Sereal document using the Snappy format.
type SnappyCompressor struct {
	Incremental bool // enable incremental parsing
}

func (c SnappyCompressor) compress(b []byte) ([]byte, error) {
	// XXX this could be more efficient!  I'm creating a new buffer to
	//     store the compressed document, which isn't necessary.  You
	//     could probably write directly to the slice after the header
	//     and after the varint holding the length

	if len(b) >= math.MaxUint32 {
		return nil, ErrTooLarge
	}

	compressed := snappy.Encode(nil, b)

	if c.Incremental {
		// shrink down b to reuse the allocated buffer
		b = b[:0]
		b = varint(b, uint(len(compressed)))
		b = append(b, compressed...)
	} else {
		b = compressed
	}

	return b, nil
}

func (c SnappyCompressor) decompress(b []byte) ([]byte, error) {
	if c.Incremental {
		ln, sz, err := varintdecode(b)
		if err != nil {
			return nil, err
		}

		if ln < 0 || sz+ln > len(b) || ln > math.MaxInt32 {
			return nil, ErrCorrupt{errBadOffset}
		}
		b = b[sz : sz+ln]
	}

	decompressed, err := snappy.Decode(nil, b)
	if err != nil {
		return nil, err
	}

	return decompressed, nil
}
