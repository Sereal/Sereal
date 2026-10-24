package sereal

import (
	"bytes"
	"compress/zlib"
	"fmt"
	"io"
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

// zlibMaxRatio bounds the inflated size of deflate data
const zlibMaxRatio = 1032

// zlibDecode inflates buf, which must hold exactly uln (>= 0) bytes
func zlibDecode(uln int, buf []byte) ([]byte, error) {
	zr, err := zlib.NewReader(bytes.NewReader(buf))
	if err != nil {
		return nil, err
	}
	defer zr.Close()

	// uln is untrusted: preallocate no more than buf can inflate to and
	// read at most one byte more than uln
	prealloc := uln
	if prealloc/zlibMaxRatio > len(buf) {
		prealloc = zlibMaxRatio * len(buf)
	}

	dec := bytes.NewBuffer(make([]byte, 0, prealloc))
	_, err = dec.ReadFrom(io.LimitReader(zr, int64(uln)+1))
	if err != nil {
		return nil, err
	}

	if dec.Len() != uln {
		return nil, ErrCorrupt{errBadUncompressedSize}
	}

	return dec.Bytes(), nil
}
