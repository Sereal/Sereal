// +build clibs

package sereal

/*
#cgo LDFLAGS: -lz

#include <zlib.h>

*/
import "C"

import (
	"errors"
	"unsafe"
)

func zlibEncode(buf []byte, level int) ([]byte, error) {

	dLen := C.compressBound(C.uLong(len(buf)))

	dst := make([]byte, dLen)

	err := C.compress2((*C.Bytef)(unsafe.Pointer(&dst[0])), (*C.uLongf)(unsafe.Pointer(&dLen)),
		(*C.Bytef)(unsafe.Pointer(&buf[0])), C.uLong(len(buf)),
		C.int(level))

	// compression failed :(
	if err != C.Z_OK {
		return nil, errors.New("zlib error")
	}

	return dst[:dLen], nil
}

func zlibDecode(uln int, buf []byte) ([]byte, error) {

	dst := make([]byte, uln)

	dLen := uln

	err := C.uncompress((*C.Bytef)(unsafe.Pointer(&dst[0])), (*C.uLongf)(unsafe.Pointer(&dLen)),
		(*C.Bytef)(unsafe.Pointer(&buf[0])), C.uLong(len(buf)))

	// compression failed :(
	if err != C.Z_OK || uln != dLen {
		return nil, errors.New("zlib error")
	}

	return dst, nil
}
