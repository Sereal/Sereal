//go:build !clibs
// +build !clibs

package sereal

import "github.com/klauspost/compress/zstd"

func zstdEncode(buf []byte, level int) ([]byte, error) {
	encoder, err := zstd.NewWriter(nil, zstd.WithEncoderLevel(zstd.EncoderLevelFromZstd(level)))
	if err != nil {
		return nil, err
	}

	return encoder.EncodeAll(buf, nil), nil
}

var decoder, _ = zstd.NewReader(nil, zstd.WithDecoderConcurrency(0))

func zstdDecode(d, buf []byte) ([]byte, error) {
	return decoder.DecodeAll(buf, d)
}
