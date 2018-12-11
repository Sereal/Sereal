// +build clibs

package sereal

import (
	"github.com/DataDog/zstd"
)

func zstdEncode(buf []byte, level int) ([]byte, error) {
	dst, err := zstd.CompressLevel(nil, buf, level)
	return dst, err
}

func zstdDecode(d, buf []byte) ([]byte, error) {
	dst, err := zstd.Decompress(d, buf)
	return dst, err
}
