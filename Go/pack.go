package sereal

import (
	"math"
	"code.google.com/p/snappy-go/snappy"
	"encoding/binary"
	"fmt"
	"reflect"
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
	compressed, err  := snappy.Encode(nil, b[5 + optHeaderLength:])
	if err != nil {
		return nil, err
	}
        // XXX I'm sure that this could be using a slice of b rather than nil
        //     so we don't need to copy, but my Go-fu is too low to do it.
        compressedLength := varint(nil, uint(len(compressed)))
        copy(b[5 + optHeaderLength:], compressedLength)

	bytesCopied := copy(b[5 + optHeaderLength + len(compressedLength):], compressed)

	// XXX should we verify that bytesCopied == len(compressed)?

	return b[0:bytesCopied], nil
}

func Marshal(v interface{}) (b []byte, err error) {

	headerLength := 6
	b             = make([]byte, headerLength, 32)

	binary.LittleEndian.PutUint32(b[:4], Magic)
	b[4] = 1 /* version */
	b[5] = 0 /* header size */

	rv := reflectValueOf(v)

	strTable := make(map[string]int)

	encoded, err := encode(b, rv, strTable)

	if err != nil {
		return nil, err
	}

/*
	if len(encoded) >= SnappyThreshold + headerLength {
		encoded, err = snappify(encoded)

		if err != nil {
			return nil, err
		}
	}
*/

	return encoded, nil
}

func encode(b []byte, rv reflect.Value, strTable map[string]int) ([]byte, error) {

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
		if rv.Len() == 0 && rv.Type().Elem().Kind() == reflect.Uint8 {
			b = encodeBytes(b, rv.Bytes(), strTable)
		} else if rv.Index(0).Kind() == reflect.Uint8 {
			b = encodeBytes(b, rv.Bytes(), strTable)
		} else {
			b = encodeArray(b, rv, strTable)
		}

	case reflect.Map:
		b = encodeMap(b, rv, strTable)

	case reflect.Struct:
		b = encodeStruct(b, rv, strTable)

	case reflect.Float32:
		b = encodeFloat(b, float32(rv.Float()))

	case reflect.Float64:
		b = encodeDouble(b, float64(rv.Float()))

	case reflect.Interface:
		// recurse until we get a concrete type
		// could be optmized into a tail call
		var err error
		b, err = encode(b, rv.Elem(), strTable)
		if err != nil {
			return nil, err
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

func encodeArray(by []byte, arr reflect.Value, strTable map[string]int) []byte {

	l := arr.Len()

	if l < 16 {
		by = append(by, TypeARRAYREF_0+byte(l))
	} else {
		by = append(by, TypeARRAY)
		by = varint(by, uint(l))
	}

	for i := 0; i < l; i++ {
		by, _ = encode(by, arr.Index(i), strTable)
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

func encodeMap(by []byte, m reflect.Value, strTable map[string]int) []byte {

	l := m.Len()

	if l < 16 {
		by = append(by, TypeHASHREF_0+byte(l))
	} else {
		by = append(by, TypeHASH)
		by = varint(by, uint(l))
	}

	keys := m.MapKeys()

	for _, k := range keys {
		// FIXME: key must be a string type, or coercible to one
		// Do we coerce or simply force all maps to be map[string]interface{} ?
		by, _ = encode(by, k, strTable)
		v := m.MapIndex(k)
		by, _ = encode(by, v, strTable)
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

func encodeStruct(by []byte, st reflect.Value, strTable map[string]int) []byte {

	by = append(by, TypeOBJECT)

	typ := st.Type()

	by = encodeString(by, typ.Name(), strTable)

	l := typ.NumField()

	if l < 16 {
		by = append(by, TypeARRAYREF_0+byte(l))
	} else {
		by = append(by, TypeARRAY)
		by = varint(by, uint(l))
	}

	for i := 0; i < l; i++ {
		by, _ = encode(by, st.Field(i), strTable)
	}

	return by
}
