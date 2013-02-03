package sereal

import (
	"bytes"
	"errors"
	"fmt"
	"math"
	"reflect"
	"strconv"
)

func Unmarshal(b []byte, v interface{}) error {

	// header must match
	header := []byte{'=', 's', 'r', 'l', 1 /* version */, 0 /* header size */}

	vPtrValue := reflect.ValueOf(v)

	if !bytes.Equal(header, b[:6]) {
		return errors.New("bad header")
	}

	if reflect.TypeOf(v).Kind() != reflect.Ptr {
		return errors.New("expected pointer")
	}

	// just unpack everything into an interface{} for now -- worry about schema stuff later

	idx := 6

	ptr, _, err := decode(b, idx)

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

func decode(b []byte, idx int) (reflect.Value, int, error) {

	startIdx := idx

	/*
		fmt.Printf("b=   % x\n", b)
		fmt.Printf("  ")
		indent(2 + idx)
		fmt.Printf("tag=%x\n", b[idx])
	*/

	var ptr reflect.Value

	switch tag := b[idx]; {
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

	case tag == TypeDOUBLE:
		idx++

		var f float64

		ptr = reflect.New(reflect.TypeOf(f))
		bits := uint64(b[idx]) | uint64(b[idx+1])<<8 | uint64(b[idx+2])<<16 | uint64(b[idx+3])<<24 | uint64(b[idx+4])<<32 | uint64(b[idx+5])<<40 | uint64(b[idx+6])<<48 | uint64(b[idx+7])<<56
		f = math.Float64frombits(bits)
		idx += 8

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

	case tag == TypeREFN:
		idx++
		e, sz, _ := decode(b, idx)
		idx += sz

		// FIXME: this is not technically correct
		ptr = e

	case tag == TypeHASH:

		idx++

		ln, sz := varintdecode(b[idx:])
		idx += int(sz)

		m := make(map[string]interface{})
		ptr = reflect.ValueOf(&m)

		for i := 0; i < ln; i++ {
			// key
			k, sz, _ := decode(b, idx)
			idx += sz
			v, sz, _ := decode(b, idx)
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
			e, sz, _ := decode(b, idx)
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
			e, sz, _ := decode(b, idx)
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
			k, sz, _ := decode(b, idx)
			idx += sz
			v, sz, _ := decode(b, idx)
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

	default:
		panic("unknown tag byte: " + strconv.Itoa(int(tag)))
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

func varintdecode(by []byte) (n int, sz uint) {

	s := uint(0) // shift count
	for i, b := range by {
		n |= int(b&0x7f) << s
		s += 7

		if (b & 0x80) == 0 {
			return n, uint(i) + 1
		}
	}

	// byte without continuation bit
	panic("bad varint")
}
