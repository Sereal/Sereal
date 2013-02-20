package sereal

import (
	"code.google.com/p/snappy-go/snappy"
	"encoding/binary"
	"errors"
	"fmt"
	"math"
	"reflect"
	"runtime"
	"strconv"
)

func getDocumentTypeAndVersion(b byte) (VersionType, byte) {
	return VersionType(b >> 4), b & 0xF
}

func handleHeader(b []byte) int {
	// no op for now
	// actually parse out header size
	return 0
}

func Unmarshal(b []byte, v interface{}) (err error) {
	defer func() {
		if r := recover(); r != nil {
			if _, ok := r.(runtime.Error); ok {
				panic(r)
			}

			if s, ok := r.(string); ok {
				err = errors.New(s)
			} else {
				err = r.(error)
			}
		}
	}()

	vPtrValue := reflect.ValueOf(v)

	if binary.LittleEndian.Uint32(b[:4]) != Magic {
		return errors.New("bad header")
	}

	docType, version := getDocumentTypeAndVersion(b[4])
	headerLength     := handleHeader(b)

	var idx int

	/* XXX instead of creating an uncompressed copy of the document,
	 *     it would be more flexible to use a sort of "Reader" interface */
	switch docType {

	case VersionRaw:
		idx = 6 + headerLength
	case VersionSnappy:
		b, err = snappy.Decode(nil, b[6 + headerLength:])
		idx    = 0

		if err != nil {
			return err
		}
	case VersionSnappyLength:
		fallthrough
	default:
		return errors.New(fmt.Sprintf("Document type '%v' not yet supported", docType))

	}

	if version != 1 {
		return errors.New(fmt.Sprintf("Document version '%d' not yet supported", version))
	}

	if reflect.TypeOf(v).Kind() != reflect.Ptr {
		return errors.New("expected pointer")
	}

	// just unpack everything into an interface{} for now -- worry about schema stuff later

	tracked := make(map[int]reflect.Value)

	ptr, _, err := decode(b, idx, tracked)

	if err != nil {
		return err
	}

	vPtrValue.Elem().Set(ptr.Elem())

	return nil

}

func indent(idx int) {
	for i := 1; i < idx; i++ {
		fmt.Print("~~~")
	}

	fmt.Println("^")
}

func decode(b []byte, idx int, tracked map[int]reflect.Value) (reflect.Value, int, error) {

	startIdx := idx

	/*
		fmt.Printf("b=   % x\n", b)
		fmt.Printf("  ")
		indent(2 + idx)
		fmt.Printf("tag=%x\n", b[idx])
	*/

	var ptr reflect.Value

	tag := b[idx]

	trackme := (tag & TrackFlag) == TrackFlag

	tag &^= TrackFlag

	switch {
	case tag < TypeVARINT:
		idx++
		neg := (tag & 0x10) == 0x10
		ptr = reflect.New(reflect.TypeOf(int(0)))
		i := int(tag)
		if neg {
			i -= 32
		}
		ptr.Elem().SetInt(int64(i))
	case tag == TypeVARINT, tag == TypeZIGZAG:
		ptr = reflect.New(reflect.TypeOf(int(0)))
		idx++
		i, sz := varintdecode(b[idx:])
		idx += int(sz)
		if tag == TypeZIGZAG {
			i = -(1 + (i >> 1)) // un-zigzag
		}

		ptr.Elem().SetInt(int64(i))

	case tag == TypeFLOAT:
		idx++

		var f float32

		ptr = reflect.New(reflect.TypeOf(f))
		bits := uint32(b[idx]) | uint32(b[idx+1])<<8 | uint32(b[idx+2])<<16 | uint32(b[idx+3])<<24
		f = math.Float32frombits(bits)
		idx += 4
		ptr.Elem().SetFloat(float64(f))

	case tag == TypeDOUBLE:
		idx++

		var f float64

		ptr = reflect.New(reflect.TypeOf(f))
		bits := uint64(b[idx]) | uint64(b[idx+1])<<8 | uint64(b[idx+2])<<16 | uint64(b[idx+3])<<24 | uint64(b[idx+4])<<32 | uint64(b[idx+5])<<40 | uint64(b[idx+6])<<48 | uint64(b[idx+7])<<56
		f = math.Float64frombits(bits)
		idx += 8
		ptr.Elem().SetFloat(f)

	case tag == TypeBINARY:

		idx++
		ln, sz := varintdecode(b[idx:])
		idx += int(sz)

		ptr = reflect.MakeSlice(reflect.TypeOf([]byte{0}), ln, ln)
		reflect.Copy(ptr.Elem(), reflect.ValueOf(b[idx:idx+ln]))

		idx += ln

	case tag == TypeSTR_UTF8:

		idx++
		ln, sz := varintdecode(b[idx:])
		idx += int(sz)

		ptr = reflect.New(reflect.TypeOf(""))
		ptr.Elem().SetString(string(b[idx : idx+ln]))

		idx += ln

	case tag == TypeREFN:
		idx++
		e, sz, _ := decode(b, idx, tracked)
		idx += sz

		// FIXME: this is not technically correct
		ptr = e

	case tag == TypeREFP:
		idx++

		offs, sz := varintdecode(b[idx:])
		idx += int(sz)

		ptr = tracked[offs]

	case tag == TypeHASH:

		idx++

		ln, sz := varintdecode(b[idx:])
		idx += int(sz)

		m := make(map[string]interface{})
		ptr = reflect.ValueOf(&m)

		for i := 0; i < ln; i++ {
			// key
			k, sz, _ := decode(b, idx, tracked)
			idx += sz
			v, sz, _ := decode(b, idx, tracked)
			idx += sz

			s := stringOf(k)
			ptr.Elem().SetMapIndex(reflect.ValueOf(s), v.Elem())
		}

	case tag == TypeARRAY:

		idx++
		ln, sz := varintdecode(b[idx:])
		idx += int(sz)

		a := make([]interface{}, ln)

		ptr = reflect.ValueOf(&a)

		for i := 0; i < ln; i++ {
			e, sz, _ := decode(b, idx, tracked)
			idx += sz
			ptr.Elem().Index(i).Set(e.Elem())
		}

	case tag == TypeTRUE, tag == TypeFALSE:
		idx++
		ptr = reflect.New(reflect.TypeOf(false))
		ptr.Elem().SetBool(tag == TypeTRUE)

	case tag >= TypeARRAYREF_0 && tag < TypeARRAYREF_0+16:

		idx++
		ln := int(tag & 0x0f)

		a := make([]interface{}, ln)

		ptr = reflect.ValueOf(&a)

		for i := 0; i < ln; i++ {
			e, sz, _ := decode(b, idx, tracked)
			idx += sz
			ptr.Elem().Index(i).Set(e.Elem())
		}

	case tag >= TypeHASHREF_0 && tag < TypeHASHREF_0+16:

		idx++
		ln := int(tag & 0x0f)

		m := make(map[string]interface{})
		ptr = reflect.ValueOf(&m)

		for i := 0; i < ln; i++ {
			// key
			k, sz, _ := decode(b, idx, tracked)
			idx += sz
			v, sz, _ := decode(b, idx, tracked)
			idx += sz
			s := stringOf(k.Elem())
			ptr.Elem().SetMapIndex(reflect.ValueOf(s), v.Elem())
		}

	case tag >= TypeSHORT_BINARY_0 && tag < TypeSHORT_BINARY_0+32:
		ln := int(tag & 0x1F) // get length from tag
		idx++

		// assume these are strings

		ptr = reflect.New(reflect.TypeOf(""))
		ptr.Elem().SetString(string(b[idx : idx+ln]))

		idx += ln

	case tag == TypeALIAS:
		idx++

		offs, sz := varintdecode(b[idx:])
		idx += int(sz)

		ptr = tracked[offs]

	case tag == TypeCOPY:
		idx++

		offs, sz := varintdecode(b[idx:])
		idx += int(sz)

		p, _, _ := decode(b, offs, tracked)
		ptr = p

	default:
		panic("unknown tag byte: " + strconv.Itoa(int(tag)))
	}

	if trackme {
		tracked[startIdx] = ptr
	}

	return ptr, idx - startIdx, nil

}

func stringOf(v reflect.Value) string {

	if v.Type().Kind() == reflect.Ptr {
		return stringOf(v.Elem())
	}

	if v.Type().Kind() == reflect.String {
		return v.String()
	}

	if (v.Type().Kind() == reflect.Array || v.Type().Kind() == reflect.Slice) && (v.Type().Elem().Kind() == reflect.Uint8) {
		return string(v.Bytes())
	}

	panic("bad value for stringOf")
}

func varintdecode(by []byte) (n int, sz int) {

	s := uint(0) // shift count
	for i, b := range by {
		n |= int(b&0x7f) << s
		s += 7

		if (b & 0x80) == 0 {
			return n, i + 1
		}
	}

	// byte without continuation bit
	panic("bad varint")
}
