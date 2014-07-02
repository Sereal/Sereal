package sereal

import (
	"bytes"
	"compress/zlib"
)

// ZlibCompressor compresses a Sereal document using the zlib format.
type ZlibCompressor struct {
	Level int // compression level
}

const (
	ZlibNoCompression      = zlib.NoCompression
	ZlibBestSpeed          = zlib.BestSpeed
	ZlibBestCompression    = zlib.BestCompression
	ZlibDefaultCompression = zlib.DefaultCompression
)

func (c ZlibCompressor) compress(buf []byte) ([]byte, error) {
	var comp bytes.Buffer

	zw, err := zlib.NewWriterLevel(&comp, c.Level)
	if err != nil {
		return nil, err
	}

	_, err = zw.Write(buf)
	if err != nil {
		return nil, err
	}

	err = zw.Close()
	if err != nil {
		return nil, err
	}

	// Prepend a compressed block with its length, i.e.:
	//
	// <Varint><Varint><Zlib Blob>
	// 1st varint indicates the length of the uncompressed document,
	// 2nd varint indicates the length of the compressed document.
	//
	// XXX It's the naive implementation, better to rework as described in the spec:
	// https://github.com/Sereal/Sereal/blob/master/sereal_spec.pod#encoding-the-length-of-compressed-documents
	var head []byte
	tail := comp.Bytes()
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

	zr, err := zlib.NewReader(bytes.NewReader(buf))
	if err != nil {
		return nil, err
	}
	defer zr.Close()

	dec := bytes.NewBuffer(make([]byte, 0, uln))
	_, err = dec.ReadFrom(zr)
	if err != nil {
		return nil, err
	}

	// XXX Perhaps check if the number of read bytes == uln

	return dec.Bytes(), nil
}
