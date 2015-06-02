// +build clibs

package sereal

import snappy "github.com/dgryski/go-csnappy"

func snappyEncode(dst, src []byte) ([]byte, error) { return snappy.Encode(dst, src) }

func snappyDecode(dst, src []byte) ([]byte, error) { return snappy.Decode(dst, src) }
