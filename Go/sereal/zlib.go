package sereal

import "compress/zlib"

// ZlibCompressor compresses a Sereal document using the zlib format.
type ZlibCompressor struct {
	Level int // compression level, set to ZlibDefaultCompression by default
}

const (
	ZlibBestSpeed          = zlib.BestSpeed
	ZlibBestCompression    = zlib.BestCompression
	ZlibDefaultCompression = zlib.DefaultCompression
)

func (c ZlibCompressor) compress(buf []byte) ([]byte, error) {
	// Prepend a compressed block with its length, i.e.:
	//
	// <Varint><Varint><Zlib Blob>
	// 1st varint indicates the length of the uncompressed document,
	// 2nd varint indicates the length of the compressed document.
	//
	// XXX It's the naive implementation, better to rework as described in the spec:
	// https://github.com/Sereal/Sereal/blob/master/sereal_spec.pod#encoding-the-length-of-compressed-documents

	if c.Level == 0 {
		c.Level = ZlibDefaultCompression
	}

	tail, err := zlibEncode(buf, c.Level)
	if err != nil {
		return nil, err
	}

	var head []byte
	head = varint(head, uint(len(buf)))
	head = varint(head, uint(len(tail)))

	return append(head, tail...), nil
}

func (c ZlibCompressor) decompress(buf []byte) ([]byte, error) {
	// Read the claimed length of the uncompressed document
	uln, usz := varintdecode(buf)
	buf = buf[usz:]

	// Read the claimed length of the compressed document
	cln, csz := varintdecode(buf)
	buf = buf[csz : csz+cln]

	// XXX Perhaps check if len(buf) == cln

	return zlibDecode(uln, buf)
}
