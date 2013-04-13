package sereal

import (
	"code.google.com/p/snappy-go/snappy"
	"encoding/binary"
	"errors"
	"fmt"
	"math"
	"reflect"
	"runtime"
	"unsafe"
)

func reflectValueOf(v interface{}) reflect.Value {

	rv, ok := v.(reflect.Value)
	if !ok {
		rv = reflect.ValueOf(v)
	}
	return rv

}

func snappify(b []byte) ([]byte, error) {
	b[4] |= (2 << 4) // set the document type to '2' (incr snappy)

	optHeaderLength, optHeaderSize := varintdecode(b[5:])
	optHeaderLength += optHeaderSize

	// XXX this could be more efficient!  I'm creating a new buffer to
	//     store the compressed document, which isn't necessary.  You
	//     could probably write directly to the slice after the header
	//     and after the varint holding the length
	compressed, err := snappy.Encode(nil, b[5+optHeaderLength:])
	if err != nil {
		return nil, err
	}

	// resize b to just the original header
	b = b[:5+optHeaderLength]
	b = varint(b, uint(len(compressed)))
	b = append(b, compressed...)

	return b, nil
}

type Encoder struct {
	PerlCompat bool
}

// Marshal returns the Sereal encoding of v
func (e *Encoder) Marshal(v interface{}) (b []byte, err error) {
	defer func() {
		//return
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

	headerLength := 6
	b = make([]byte, headerLength, 32)

	binary.LittleEndian.PutUint32(b[:4], magicHeaderBytes)
	b[4] = 1 /* version */
	b[5] = 0 /* header size */

	rv := reflectValueOf(v)

	strTable := make(map[string]int)
	ptrTable := make(map[uintptr]int)

	encoded, err := e.encode(b, rv, strTable, ptrTable)

	if err != nil {
		return nil, err
	}

	if len(encoded) >= snappyThreshold+headerLength {
		encoded, err = snappify(encoded)

		if err != nil {
			return nil, err
		}
	}

	return encoded, nil
}

func (e *Encoder) encode(b []byte, rv reflect.Value, strTable map[string]int, ptrTable map[uintptr]int) ([]byte, error) {

	switch rk := rv.Kind(); rk {

	case reflect.Bool:
		b = e.encodeBool(b, rv.Bool())
	case reflect.Int, reflect.Int8, reflect.Int64, reflect.Int32, reflect.Int16:
		b = e.encodeInt(b, rv.Int())
	case reflect.Uint8, reflect.Uint64, reflect.Uint, reflect.Uint32, reflect.Uint16:
		b = e.encodeInt(b, int64(rv.Uint()))
	case reflect.String:
		b = e.encodeString(b, rv.String(), strTable)
	case reflect.Array, reflect.Slice:
		if rv.Type().Elem().Kind() == reflect.Uint8 {
			b = e.encodeBytes(b, rv.Bytes(), strTable)
		} else {
			b = e.encodeArray(b, rv, strTable, ptrTable)
		}

	case reflect.Map:
		b = e.encodeMap(b, rv, strTable, ptrTable)

	case reflect.Struct:
		b = e.encodeStruct(b, rv, strTable, ptrTable)

	case reflect.Float32:
		b = e.encodeFloat(b, float32(rv.Float()))

	case reflect.Float64:
		b = e.encodeDouble(b, float64(rv.Float()))

	case reflect.Interface:
		// recurse until we get a concrete type
		// could be optmized into a tail call
		var err error
		b, err = e.encode(b, rv.Elem(), strTable, ptrTable)
		if err != nil {
			return nil, err
		}

	case reflect.Ptr:

		if rv.Elem().Kind() == reflect.Struct {

			switch rv.Elem().Interface().(type) {
			case PerlRegexp:
				b = e.encodeStruct(b, rv.Elem(), strTable, ptrTable)
				return b, nil
			case PerlUndef:
				b = e.encodeStruct(b, rv.Elem(), strTable, ptrTable)
				return b, nil
			case PerlObject:
				b = e.encodeStruct(b, rv.Elem(), strTable, ptrTable)
				return b, nil
			case PerlWeakRef:
				b = e.encodeStruct(b, rv.Elem(), strTable, ptrTable)
				return b, nil
			}
		}

		rvptr := rv.Pointer()
		rvptr2 := getPointer(rv.Elem())

		offs, ok := ptrTable[rvptr]

		if !ok && rvptr2 != 0 {
			offs, ok = ptrTable[rvptr2]
			if ok {
				rvptr = rvptr2
			} else {
			}
		}

		if ok { // seen this before
			b = append(b, typeREFP)
			b = varint(b, uint(offs))
			b[offs] |= trackFlag // original offset now tracked
		} else {

			lenbOrig := len(b)

			b = append(b, typeREFN)

			ptrTable[rvptr] = lenbOrig

			var err error

			b, err = e.encode(b, rv.Elem(), strTable, ptrTable)
			if err != nil {
				return nil, err
			}

			if rvptr2 != 0 {
				// The thing this this points to starts one after the current pointer
				ptrTable[rvptr2] = lenbOrig + 1
			}
		}

	default:
		panic(fmt.Sprintf("no support for type '%s'", rk.String()))
	}

	return b, nil
}

func getPointer(rv reflect.Value) uintptr {

	var rvptr uintptr

	switch rv.Kind() {
	case reflect.Map, reflect.Slice:
		rvptr = rv.Pointer()
	case reflect.Interface:
		// FIXME: still needed?
		return getPointer(rv.Elem())
	case reflect.Ptr:
		rvptr = rv.Pointer()
	case reflect.String:
		ps := (*reflect.StringHeader)(unsafe.Pointer(rv.UnsafeAddr()))
		rvptr = ps.Data
	}

	return rvptr
}

func varint(by []byte, n uint) []uint8 {

	for n >= 0x80 {
		b := byte(n) | 0x80
		by = append(by, b)
		n >>= 7
	}

	return append(by, byte(n))
}

func (e *Encoder) encodeArrayRef(by []byte, arr reflect.Value, strTable map[string]int, ptrTable map[uintptr]int) []byte {
	l := arr.Len()

	if l >= 16 {
		by = append(by, typeREFN)
		return e.encodeArray(by, arr, strTable, ptrTable)
	}

	by = append(by, typeARRAYREF_0+byte(l))

	for i := 0; i < l; i++ {
		if e.PerlCompat {
			by = e.encodeScalar(by, arr.Index(i), strTable, ptrTable)
		} else {
			by, _ = e.encode(by, arr.Index(i), strTable, ptrTable)
		}
	}

	return by
}

func (e *Encoder) encodeArray(by []byte, arr reflect.Value, strTable map[string]int, ptrTable map[uintptr]int) []byte {

	l := arr.Len()

	by = append(by, typeARRAY)
	by = varint(by, uint(l))

	for i := 0; i < l; i++ {
		v := arr.Index(i)
		if e.PerlCompat {
			by = e.encodeScalar(by, v, strTable, ptrTable)
		} else {
			by, _ = e.encode(by, v, strTable, ptrTable)
		}
	}

	return by

}

func (e *Encoder) encodeBool(by []byte, bo bool) []byte {

	if bo {
		by = append(by, typeTRUE)
	} else {

		by = append(by, typeFALSE)
	}

	return by
}

func (e *Encoder) encodeBytes(by []byte, byt []byte, strTable map[string]int) []byte {

	l := len(byt)

	if l < 32 {

		if strTable != nil {

			// track short byte strTable

			s := string(byt)

			copy_offs, ok := strTable[s]

			if ok {
				by = append(by, typeCOPY)
				by = varint(by, uint(copy_offs))
				return by
			}

			// save for later
			strTable[s] = len(by)
		}

		by = append(by, typeSHORT_BINARY_0+byte(l))
	} else {
		by = append(by, typeBINARY)
		by = varint(by, uint(l))
	}

	by = append(by, byt...)

	return by
}

func (e *Encoder) encodeDouble(by []byte, f float64) []byte {

	var u uint64
	u = math.Float64bits(f)
	by = append(by, typeDOUBLE)
	by = append(by, byte(u))
	by = append(by, byte(u>>8))
	by = append(by, byte(u>>16))
	by = append(by, byte(u>>24))
	by = append(by, byte(u>>32))
	by = append(by, byte(u>>40))
	by = append(by, byte(u>>48))
	by = append(by, byte(u>>56))
	return by
}

func (e *Encoder) encodeFloat(by []byte, f float32) []byte {

	var u uint32
	u = math.Float32bits(f)
	by = append(by, typeFLOAT)
	by = append(by, byte(u))
	by = append(by, byte(u>>8))
	by = append(by, byte(u>>16))
	by = append(by, byte(u>>24))
	return by
}

func (e *Encoder) encodeInt(by []byte, i int64) []byte {

	switch {
	case 0 <= i && i <= 15:
		by = append(by, byte(i)&0x0f)
	case -16 <= i && i < 0:
		by = append(by, 0x010|(byte(i)&0x0f))
	case i > 15:
		by = append(by, typeVARINT)
		by = varint(by, uint(i))
	case i < 0:
		by = append(by, typeZIGZAG)
		n := (i << 1) ^ (i >> 63)
		by = varint(by, uint(n))
	}

	return by
}

func (e *Encoder) encodeMapRef(by []byte, m reflect.Value, strTable map[string]int, ptrTable map[uintptr]int) []byte {
	keys := m.MapKeys()

	l := len(keys)

	if l >= 16 {
		by = append(by, typeREFN)
		return e.encodeMap(by, m, strTable, ptrTable)
	}

	by = append(by, typeHASHREF_0+byte(l))

	for _, k := range keys {
		// FIXME: key must be a string type, or coercible to one
		// Do we coerce or simply force all maps to be map[string]interface{} ?

		by, _ = e.encode(by, k, strTable, ptrTable)
		v := m.MapIndex(k)
		if e.PerlCompat {
			by = e.encodeScalar(by, v, strTable, ptrTable)
		} else {
			by, _ = e.encode(by, v, strTable, ptrTable)
		}
	}

	return by
}

func (e *Encoder) encodeScalar(by []byte, rv reflect.Value, strTable map[string]int, ptrTable map[uintptr]int) []byte {

	switch rv.Kind() {
	case reflect.Array, reflect.Slice:
		if rv.Type().Elem().Kind() == reflect.Uint8 {
			by = e.encodeBytes(by, rv.Bytes(), strTable)
		} else {
			by = e.encodeArrayRef(by, rv, strTable, ptrTable)
		}
	case reflect.Map:
		by = e.encodeMapRef(by, rv, strTable, ptrTable)
	case reflect.Interface:
		by = e.encodeScalar(by, rv.Elem(), strTable, ptrTable)
	default:
		by, _ = e.encode(by, rv, strTable, ptrTable)
	}

	return by

}

func (e *Encoder) encodeMap(by []byte, m reflect.Value, strTable map[string]int, ptrTable map[uintptr]int) []byte {

	keys := m.MapKeys()

	l := len(keys)

	by = append(by, typeHASH)
	by = varint(by, uint(l))

	for _, k := range keys {
		by, _ = e.encode(by, k, strTable, ptrTable)
		v := m.MapIndex(k)
		if e.PerlCompat {
			// only scalars allowed in arrays
			by = e.encodeScalar(by, v, strTable, ptrTable)
		} else {
			by, _ = e.encode(by, v, strTable, ptrTable)
		}
	}

	return by
}

func (e *Encoder) encodeString(by []byte, s string, strTable map[string]int) []byte {

	copy_offs, ok := strTable[s]

	if ok {
		by = append(by, typeCOPY)
		by = varint(by, uint(copy_offs))
		return by
	}

	// save for later
	strTable[s] = len(by)

	by = append(by, typeSTR_UTF8)
	by = varint(by, uint(len(s)))
	by = append(by, []byte(s)...)

	return by
}

func (e *Encoder) encodeStruct(by []byte, st reflect.Value, strTable map[string]int, ptrTable map[uintptr]int) []byte {

	typ := st.Type()

	// first test if it's one of our internal SerealPerl structs
	switch val := st.Interface().(type) {
	case PerlUndef:
		_ = val
		by = append(by, typeUNDEF)
		return by
	case PerlObject:
		by = append(by, typeOBJECT)
		// nil strTable because classnames need their own namespace
		by = e.encodeBytes(by, []byte(val.Class), nil)
		by, _ = e.encode(by, reflect.ValueOf(val.Reference), strTable, ptrTable)
		return by
	case PerlRegexp:
		by = append(by, typeREGEXP)
		by = e.encodeBytes(by, []byte(val.Pattern), strTable)
		by = e.encodeBytes(by, []byte(val.Modifiers), strTable)
		return by
	case PerlWeakRef:
		by = append(by, typeWEAKEN)
		by, _ = e.encode(by, reflect.ValueOf(val.Reference), strTable, ptrTable)
		return by
	}

	by = append(by, typeOBJECT)

	by = e.encodeBytes(by, []byte(typ.Name()), strTable)

	l := typ.NumField()

	if l < 16 {
		by = append(by, typeHASHREF_0+byte(l))
	} else {
		by = append(by, typeHASH)
		by = varint(by, uint(l))
	}

	for i := 0; i < l; i++ {
		fty := typ.Field(i)
		if fty.PkgPath != "" {
			continue // skip unexported names
		}
		by = e.encodeString(by, fty.Name, strTable)
		by, _ = e.encode(by, st.Field(i), strTable, ptrTable)
	}

	return by
}
