package sereal

import (
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

func Marshal(v interface{}) (b []byte, err error) {

	b = make([]byte, 6, 32)

	binary.LittleEndian.PutUint32(b[:4], Magic)
	b[4] = 1 /* version */
	b[5] = 0 /* header size */

	rv := reflectValueOf(v)

	strTable := make(map[string]int)

	return encode(b, rv, strTable)

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
