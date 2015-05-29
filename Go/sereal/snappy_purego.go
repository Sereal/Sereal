// +build !clibs

package sereal

import "github.com/golang/snappy/snappy"

func snappyEncode(dst, src []byte) ([]byte, error) { return snappy.Encode(dst, src) }

func snappyDecode(dst, src []byte) ([]byte, error) { return snappy.Decode(dst, src) }
