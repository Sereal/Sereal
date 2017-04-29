package sereal

import (
	"bytes"
	"compress/zlib"
	"fmt"
	"sync"
)

var zlibWriterPools = make(map[int]*sync.Pool)

func init() {
	// -1 => 9
	for i := zlib.DefaultCompression; i <= zlib.BestCompression; i++ {
		level := i
		zlibWriterPools[i] = &sync.Pool{
			New: func() interface{} {
				zw, _ := zlib.NewWriterLevel(nil, level)
				return zw
			},
		}
	}
}

func zlibEncode(buf []byte, level int) ([]byte, error) {
	pool := zlibWriterPools[level]
	if pool == nil {
		return nil, fmt.Errorf("unknown level %d", level)
	}

	var comp bytes.Buffer
	zw := pool.Get().(*zlib.Writer)
	defer pool.Put(zw)
	zw.Reset(&comp)

	_, err := zw.Write(buf)
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
