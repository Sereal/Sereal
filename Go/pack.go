package sereal

import (
	"code.google.com/p/snappy-go/snappy"
	"encoding/binary"
	"errors"
	"fmt"
	"math"
	"reflect"
	"runtime"
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

func Marshal(v interface{}) (b []byte, err error) {
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

	headerLength := 6
	b = make([]byte, headerLength, 32)

	binary.LittleEndian.PutUint32(b[:4], Magic)
	b[4] = 1 /* version */
	b[5] = 0 /* header size */

	rv := reflectValueOf(v)

	strTable := make(map[string]int)
	ptrTable := make(map[uintptr]int)

	encoded, err := encode(b, rv, strTable, ptrTable)

	if err != nil {
		return nil, err
	}

	if len(encoded) >= SnappyThreshold+headerLength {
		encoded, err = snappify(encoded)

		if err != nil {
			return nil, err
		}
	}

	return encoded, nil
}

func encode(b []byte, rv reflect.Value, strTable map[string]int, ptrTable map[uintptr]int) ([]byte, error) {

	switch rk := rv.Kind(); rk {

	case reflect.Bool:
		b = encodeBool(b, rv.Bool())
	case reflect.Int, reflect.Int8, reflect.Int64, reflect.Int32, reflect.Int16:
		b = encodeInt(b, rv.Int())
	case reflect.Uint8, reflect.Uint64, reflect.Uint, reflect.Uint32, reflect.Uint16:
		b = encodeInt(b, int64(rv.Uint()))
	case reflect.String:
		b = encodeString(b, rv.String(), strTable)
	case reflect.Array, reflect.Slice:
		if rv.Type().Elem().Kind() == reflect.Uint8 {
			b = encodeBytes(b, rv.Bytes(), strTable)
		} else {
			b = encodeArray(b, rv, strTable, ptrTable)
		}

	case reflect.Map:
		b = encodeMap(b, rv, strTable, ptrTable)

	case reflect.Struct:
		b = encodeStruct(b, rv, strTable, ptrTable)

	case reflect.Float32:
		b = encodeFloat(b, float32(rv.Float()))

	case reflect.Float64:
		b = encodeDouble(b, float64(rv.Float()))

	case reflect.Interface:
		// recurse until we get a concrete type
		// could be optmized into a tail call
		var err error
		b, err = encode(b, rv.Elem(), strTable, ptrTable)
		if err != nil {
			return nil, err
		}

	case reflect.Ptr:
		offs, ok := ptrTable[rv.Pointer()]

		if ok { // seen this before
			b = append(b, TypeREFP)
			b = varint(b, uint(offs))
			b[offs] |= TrackFlag // original offset now tracked
		} else {
			b = append(b, TypeREFN)
			ptrTable[rv.Pointer()] = len(b)
			var err error
			b, err = encode(b, rv.Elem(), strTable, ptrTable)
			if err != nil {
				return nil, err
			}
		}

	default:
		panic(fmt.Sprintf("no support for type '%s'", rk.String()))
	}

	return b, nil
}

func varint(by []byte, n uint) []uint8 {

	for n >= 0x80 {
		b := byte(n) | 0x80
		by = append(by, b)
		n >>= 7
	}

	return append(by, byte(n))
}

func encodeArrayRef(by []byte, arr reflect.Value, strTable map[string]int, ptrTable map[uintptr]int) []byte {
	l := arr.Len()

	if l >= 16 {
		by = append(by, TypeREFN)
		return encodeArray(by, arr, strTable, ptrTable)
	}

	by = append(by, TypeARRAYREF_0+byte(l))

	for i := 0; i < l; i++ {
		by, _ = encode(by, arr.Index(i), strTable, ptrTable)
	}

	return by
}

func encodeArray(by []byte, arr reflect.Value, strTable map[string]int, ptrTable map[uintptr]int) []byte {

	l := arr.Len()

	by = append(by, TypeARRAY)
	by = varint(by, uint(l))

	for i := 0; i < l; i++ {
		by, _ = encode(by, arr.Index(i), strTable, ptrTable)
	}

	return by

}

func encodeBool(by []byte, bo bool) []byte {

	if bo {
		by = append(by, TypeTRUE)
	} else {

		by = append(by, TypeFALSE)
	}

	return by
}

func encodeBytes(by []byte, byt []byte, strTable map[string]int) []byte {

	l := len(byt)

	if l < 32 {

		// track short byte strTable

		s := string(byt)

		copy_offs, ok := strTable[s]

		if ok {
			by = append(by, TypeCOPY)
			by = varint(by, uint(copy_offs))
			return by
		}

		// save for later
		strTable[s] = len(by)

		by = append(by, TypeSHORT_BINARY_0+byte(l))
	} else {
		by = append(by, TypeBINARY)
		by = varint(by, uint(l))
	}

	by = append(by, byt...)

	return by
}

func encodeDouble(by []byte, f float64) []byte {

	var u uint64
	u = math.Float64bits(f)
	by = append(by, TypeDOUBLE)
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

func encodeFloat(by []byte, f float32) []byte {

	var u uint32
	u = math.Float32bits(f)
	by = append(by, TypeFLOAT)
	by = append(by, byte(u))
	by = append(by, byte(u>>8))
	by = append(by, byte(u>>16))
	by = append(by, byte(u>>24))
	return by
}

func encodeInt(by []byte, i int64) []byte {

	switch {
	case 0 <= i && i <= 15:
		by = append(by, byte(i)&0x0f)
	case -16 <= i && i < 0:
		by = append(by, 0x010|(byte(i)&0x0f))
	case i > 15:
		by = append(by, TypeVARINT)
		by = varint(by, uint(i))
	case i < 0:
		by = append(by, TypeZIGZAG)
		n := (i << 1) ^ (i >> 63)
		by = varint(by, uint(n))
	}

	return by
}

func encodeMapRef(by []byte, m reflect.Value, strTable map[string]int, ptrTable map[uintptr]int) []byte {
	keys := m.MapKeys()

	l := len(keys)

	if l >= 16 {
		by = append(by, TypeREFN)
		return encodeMap(by, m, strTable, ptrTable)
	}

	by = append(by, TypeHASHREF_0+byte(l))

	for _, k := range keys {
		// FIXME: key must be a string type, or coercible to one
		// Do we coerce or simply force all maps to be map[string]interface{} ?

		by, _ = encode(by, k, strTable, ptrTable)
		v := m.MapIndex(k)
		by, _ = encode(by, v, strTable, ptrTable)
	}

	return by
}

func encodeMap(by []byte, m reflect.Value, strTable map[string]int, ptrTable map[uintptr]int) []byte {

	keys := m.MapKeys()

	l := len(keys)

	by = append(by, TypeHASH)
	by = varint(by, uint(l))

	for _, k := range keys {
		by, _ = encode(by, k, strTable, ptrTable)
		v := m.MapIndex(k)
		by, _ = encode(by, v, strTable, ptrTable)
	}

	return by
}

func encodeString(by []byte, s string, strTable map[string]int) []byte {

	copy_offs, ok := strTable[s]

	if ok {
		by = append(by, TypeCOPY)
		by = varint(by, uint(copy_offs))
		return by
	}

	// save for later
	strTable[s] = len(by)

	by = append(by, TypeSTR_UTF8)
	by = varint(by, uint(len(s)))
	by = append(by, []byte(s)...)

	return by
}

func encodeStruct(by []byte, st reflect.Value, strTable map[string]int, ptrTable map[uintptr]int) []byte {

	typ := st.Type()

	// first test if it's one of our internal SerealPerl structs
	switch val := st.Interface().(type) {
	case PerlUndef:
		_ = val
		by = append(by, TypeUNDEF)
		return by
	case PerlObject:
		by = append(by, TypeOBJECT)
		by = encodeBytes(by, []byte(val.Class), strTable)
		by, _ = encode(by, reflect.ValueOf(val.Reference), strTable, ptrTable)
		return by
	case PerlRegexp:
		by = append(by, TypeREGEXP)
		by = encodeBytes(by, []byte(val.Pattern), strTable)
		by = encodeBytes(by, []byte(val.Modifiers), strTable)
		return by
	}

	by = append(by, TypeOBJECT)

	by = encodeBytes(by, []byte(typ.Name()), strTable)

	l := typ.NumField()

	if l < 16 {
		by = append(by, TypeARRAYREF_0+byte(l))
	} else {
		by = append(by, TypeARRAY)
		by = varint(by, uint(l))
	}

	for i := 0; i < l; i++ {
		by, _ = encode(by, st.Field(i), strTable, ptrTable)
	}

	return by
}
