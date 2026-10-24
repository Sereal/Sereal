//go:build clibs
// +build clibs

package sereal

import (
	"bytes"
	"io"

	"github.com/DataDog/zstd"
)

func zstdEncode(buf []byte, level int) ([]byte, error) {
	dst, err := zstd.CompressLevel(nil, buf, level)
	return dst, err
}

func zstdDecode(d, buf []byte, limit int) ([]byte, error) {
	if limit < 0 {
		return zstd.Decompress(d, buf)
	}

	// stream so that a frame without a content size cannot inflate past limit
	zr := zstd.NewReader(bytes.NewReader(buf))
	defer zr.Close()

	dec := bytes.NewBuffer(d[:0])
	if _, err := dec.ReadFrom(io.LimitReader(zr, int64(limit)+1)); err != nil {
		return nil, err
	}
	if dec.Len() > limit {
		return nil, ErrMaxUncompressedSize
	}

	return dec.Bytes(), nil
}
