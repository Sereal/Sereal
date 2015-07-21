// +build !clibs

package sereal

import "github.com/golang/snappy"

func snappyEncode(dst, src []byte) []byte { return snappy.Encode(dst, src) }

func snappyDecode(dst, src []byte) ([]byte, error) { return snappy.Decode(dst, src) }
