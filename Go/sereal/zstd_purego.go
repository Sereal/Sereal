//go:build !clibs
// +build !clibs

package sereal

import (
	"errors"
	"sync"

	"github.com/klauspost/compress/zstd"
)

func zstdEncode(buf []byte, level int) ([]byte, error) {
	encoder, err := zstd.NewWriter(nil, zstd.WithEncoderLevel(zstd.EncoderLevelFromZstd(level)))
	if err != nil {
		return nil, err
	}

	return encoder.EncodeAll(buf, nil), nil
}

// zstdDecoders holds a shared decoder per decompressed size limit
var zstdDecoders sync.Map

func zstdDecoder(limit int) (*zstd.Decoder, error) {
	if dec, ok := zstdDecoders.Load(limit); ok {
		return dec.(*zstd.Decoder), nil
	}

	opts := []zstd.DOption{zstd.WithDecoderConcurrency(0)}
	if limit >= 0 {
		opts = append(opts, zstd.WithDecoderMaxMemory(uint64(limit)))
	}

	dec, err := zstd.NewReader(nil, opts...)
	if err != nil {
		return nil, err
	}

	if prev, loaded := zstdDecoders.LoadOrStore(limit, dec); loaded {
		dec.Close()
		return prev.(*zstd.Decoder), nil
	}
	return dec, nil
}

func zstdDecode(d, buf []byte, limit int) ([]byte, error) {
	dec, err := zstdDecoder(limit)
	if err != nil {
		return nil, err
	}

	res, err := dec.DecodeAll(buf, d)
	if errors.Is(err, zstd.ErrDecoderSizeExceeded) {
		return nil, ErrMaxUncompressedSize
	}
	return res, err
}
