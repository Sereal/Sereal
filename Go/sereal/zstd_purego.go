// +build !clibs

package sereal

import (
	"errors"
)

var errNoZstd = errors.New("sereal: zstd not supported in pure-Go build")

func zstdEncode(buf []byte, level int) ([]byte, error) {
	return nil, errNoZstd
}

func zstdDecode(buf []byte) ([]byte, error) {
	return nil, errNoZstd
}
