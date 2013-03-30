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

func getDocumentTypeAndVersion(b byte) (versionType, byte) {
	return versionType(b >> 4), b & 0xF
}

func handleHeader(b []byte) int {
	// no op for now
	ln, sz := varintdecode(b[5:])
	return ln + sz
}

type Decoder struct {
	// empty, for now
}

// Unmarshal parses the Sereal-encoded buffer b and stores the result in the value pointed to by v
func (d *Decoder) Unmarshal(b []byte, v interface{}) (err error) {
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

	if binary.LittleEndian.Uint32(b[:4]) != magicHeaderBytes {
		return errors.New("bad header")
	}

	docType, version := getDocumentTypeAndVersion(b[4])
	headerLength := handleHeader(b)

	idx := 5 + headerLength // where the data starts

	/* XXX instead of creating an uncompressed copy of the document,
	 *     it would be more flexible to use a sort of "Reader" interface */
	switch docType {

	case versionRaw:
		// nothing
	case versionSnappy:
		decoded, err := snappy.Decode(nil, b[5+headerLength:])

		if err != nil {
			return err
		}

		d := make([]byte, 0, len(decoded)+5+headerLength)
		d = append(d, b[:5+headerLength]...)
		d = append(d, decoded...)
		b = d

	case versionSnappyLength:
		ln, sz := varintdecode(b[5+headerLength:])
		decoded, err := snappy.Decode(nil, b[5+headerLength+sz:5+headerLength+sz+ln])

		if err != nil {
			return err
		}

		// we want to treat the passed-in buffer as read-only here
		// if we just used append, we'd overwrite any data past the end of the underlying array, which wouldn't be nice
		d := make([]byte, 0, len(decoded)+5+headerLength)
		d = append(d, b[:5+headerLength]...)
		d = append(d, decoded...)
		b = d

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

	ptr, _, err := d.decode(b, idx, tracked)

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

func (d *Decoder) decode(b []byte, idx int, tracked map[int]reflect.Value) (reflect.Value, int, error) {

	startIdx := idx
	/*
		fmt.Printf("b=   % x\n", b)
		fmt.Printf("  ")
		indent(2 + idx)
		fmt.Printf("tag=%x\n", b[idx])
	*/

	var ptr reflect.Value

	tag := b[idx]

	// skip over any padding bytes
	for tag == typePAD {
		idx++
		tag = b[idx]
	}

	trackme := (tag & trackFlag) == trackFlag

	tag &^= trackFlag

	switch {
	case tag < typeVARINT:
		idx++
		neg := (tag & 0x10) == 0x10
		ptr = reflect.New(reflect.TypeOf(int(0)))
		i := int(tag)
		if neg {
			i -= 32
		}
		ptr.Elem().SetInt(int64(i))
	case tag == typeVARINT, tag == typeZIGZAG:
		ptr = reflect.New(reflect.TypeOf(int(0)))
		idx++
		i, sz := varintdecode(b[idx:])
		idx += sz
		if tag == typeZIGZAG {
			i = -(1 + (i >> 1)) // un-zigzag
		}

		ptr.Elem().SetInt(int64(i))

	case tag == typeFLOAT:
		idx++

		var f float32

		ptr = reflect.New(reflect.TypeOf(f))
		bits := uint32(b[idx]) | uint32(b[idx+1])<<8 | uint32(b[idx+2])<<16 | uint32(b[idx+3])<<24
		f = math.Float32frombits(bits)
		idx += 4
		ptr.Elem().SetFloat(float64(f))

	case tag == typeDOUBLE:
		idx++

		var f float64

		ptr = reflect.New(reflect.TypeOf(f))
		bits := uint64(b[idx]) | uint64(b[idx+1])<<8 | uint64(b[idx+2])<<16 | uint64(b[idx+3])<<24 | uint64(b[idx+4])<<32 | uint64(b[idx+5])<<40 | uint64(b[idx+6])<<48 | uint64(b[idx+7])<<56
		f = math.Float64frombits(bits)
		idx += 8
		ptr.Elem().SetFloat(f)

	case tag == typeUNDEF:
		idx++

		u := &PerlUndef{}

		ptr = reflect.ValueOf(u)

	case tag == typeBINARY:

		idx++
		ln, sz := varintdecode(b[idx:])
		idx += sz

		e := reflect.MakeSlice(reflect.TypeOf([]byte{0}), ln, ln)
		reflect.Copy(e.Slice(0, ln), reflect.ValueOf(b[idx:idx+ln]))

		ptr = reflect.New(e.Type())
		ptr.Elem().Set(e)

		idx += ln

	case tag == typeSTR_UTF8:

		idx++
		ln, sz := varintdecode(b[idx:])
		idx += sz

		ptr = reflect.New(reflect.TypeOf(""))
		ptr.Elem().SetString(string(b[idx : idx+ln]))

		idx += ln

	case tag == typeREFN:
		idx++
		e, sz, _ := d.decode(b, idx, tracked)
		idx += sz

		ptr = reflect.New(e.Type())
		ptr.Elem().Set(e)

	case tag == typeREFP:
		idx++

		offs, sz := varintdecode(b[idx:])
		idx += sz

		e := tracked[offs]

		ptr = reflect.New(e.Type())
		ptr.Elem().Set(e)

	case tag == typeHASH:

		idx++

		ln, sz := varintdecode(b[idx:])
		idx += sz

		m := make(map[string]interface{})
		ptr = reflect.ValueOf(&m)

		if trackme {
			tracked[startIdx] = ptr
		}

		for i := 0; i < ln; i++ {
			// key
			k, sz, _ := d.decode(b, idx, tracked)
			idx += sz
			v, sz, _ := d.decode(b, idx, tracked)
			idx += sz

			s := stringOf(k)
			ptr.Elem().SetMapIndex(reflect.ValueOf(s), v.Elem())
		}

	case tag == typeARRAY:

		idx++
		ln, sz := varintdecode(b[idx:])
		idx += sz

		a := make([]interface{}, ln)
		ptr = reflect.ValueOf(&a)

		if trackme {
			tracked[startIdx] = ptr
		}

		for i := 0; i < ln; i++ {
			e, sz, _ := d.decode(b, idx, tracked)
			idx += sz
			a[i] = e.Elem().Interface()
		}

	case tag == typeOBJECT:
		idx++

		// FIXME: track before recurse?
		className, sz, _ := d.decode(b, idx, tracked)
		idx += sz
		ref, sz, _ := d.decode(b, idx, tracked)
		idx += sz

		s := stringOf(className)
		o := &PerlObject{s, ref.Elem().Interface()}
		ptr = reflect.ValueOf(o)

	case tag == typeOBJECTV:
		idx++
		offs, sz := varintdecode(b[idx:])
		idx += sz
		className, _, _ := d.decode(b, offs, tracked)
		ref, sz, _ := d.decode(b, idx, tracked)
		idx += sz

		s := stringOf(className)
		o := &PerlObject{s, ref.Elem().Interface()}
		ptr = reflect.ValueOf(o)

	case tag == typeTRUE, tag == typeFALSE:
		idx++
		ptr = reflect.New(reflect.TypeOf(false))
		ptr.Elem().SetBool(tag == typeTRUE)

	case tag >= typeARRAYREF_0 && tag < typeARRAYREF_0+16:

		idx++
		ln := int(tag & 0x0f)

		a := make([]interface{}, ln)

		e := reflect.ValueOf(a)
		pe := reflect.New(e.Type())
		pe.Elem().Set(e)
		ptr = reflect.New(reflect.PtrTo(e.Type()))
		ptr.Elem().Set(pe)

		if trackme {
			tracked[startIdx] = ptr
		}

		for i := 0; i < ln; i++ {
			e, sz, _ := d.decode(b, idx, tracked)
			idx += sz
			a[i] = e.Elem().Interface()
		}

	case tag >= typeHASHREF_0 && tag < typeHASHREF_0+16:

		idx++
		ln := int(tag & 0x0f)

		m := make(map[string]interface{})

		e := reflect.ValueOf(m)
		pe := reflect.New(e.Type())
		pe.Elem().Set(e)
		ptr = reflect.New(reflect.PtrTo(e.Type()))
		ptr.Elem().Set(pe)

		if trackme {
			tracked[startIdx] = ptr
		}

		for i := 0; i < ln; i++ {
			// FIXME: track before recurse?
			k, sz, _ := d.decode(b, idx, tracked)
			idx += sz
			v, sz, _ := d.decode(b, idx, tracked)
			idx += sz
			s := stringOf(k.Elem())
			m[s] = v.Elem().Interface()
		}

	case tag >= typeSHORT_BINARY_0 && tag < typeSHORT_BINARY_0+32:
		ln := int(tag & 0x1F) // get length from tag
		idx++

		a := make([]byte, ln)
		copy(a, b[idx:idx+ln])
		ptr = reflect.ValueOf(&a)

		idx += ln

	case tag == typeALIAS:
		idx++

		offs, sz := varintdecode(b[idx:])
		idx += sz

		ptr = tracked[offs]

	case tag == typeCOPY:
		idx++

		offs, sz := varintdecode(b[idx:])
		idx += sz

		p, _, _ := d.decode(b, offs, tracked)
		ptr = p

	case tag == typeWEAKEN:
		idx++

		r, sz, _ := d.decode(b, idx, tracked)
		idx += sz
		w := PerlWeakRef{r}
		ptr = reflect.ValueOf(w)

	case tag == typeREGEXP:
		idx++
		// FIXME: track before recurse?
		pattern, sz, _ := d.decode(b, idx, tracked)
		idx += sz
		modifiers, sz, _ := d.decode(b, idx, tracked)
		idx += sz

		re := &PerlRegexp{pattern.Elem().Bytes(), modifiers.Elem().Bytes()}

		ptr = reflect.ValueOf(re)

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
