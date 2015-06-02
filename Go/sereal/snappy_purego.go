// +build !clibs

package sereal

import "code.google.com/p/snappy-go/snappy"

func snappyEncode(dst, src []byte) ([]byte, error) { return snappy.Encode(dst, src) }

func snappyDecode(dst, src []byte) ([]byte, error) { return snappy.Decode(dst, src) }
