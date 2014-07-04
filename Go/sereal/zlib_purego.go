// +build !clibs

package sereal

import (
	"bytes"
	"compress/zlib"
)

func zlibEncode(buf []byte, level int) ([]byte, error) {

	var comp bytes.Buffer

	zw, err := zlib.NewWriterLevel(&comp, level)
	if err != nil {
		return nil, err
	}

	_, err = zw.Write(buf)
	if err != nil {
		return nil, err
	}

	err = zw.Close()
	if err != nil {
		return nil, err
	}

	return comp.Bytes(), nil
}

func zlibDecode(uln int, buf []byte) ([]byte, error) {
	zr, err := zlib.NewReader(bytes.NewReader(buf))
	if err != nil {
		return nil, err
	}
	defer zr.Close()

	dec := bytes.NewBuffer(make([]byte, 0, uln))
	_, err = dec.ReadFrom(zr)
	if err != nil {
		return nil, err
	}

	// XXX Perhaps check if the number of read bytes == uln
	return dec.Bytes(), nil
}
