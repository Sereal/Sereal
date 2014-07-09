package sereal

import (
	"encoding"
	"encoding/binary"
	"errors"
	"fmt"
	"math"
	"reflect"
	"runtime"
	"unsafe"
)

// An Encoder encodes Go data structures into Sereal byte streams
type Encoder struct {
	PerlCompat           bool       // try to mimic Perl's structure as much as possible
	Compression          compressor // optionally compress the main payload of the document using SnappyCompressor or ZlibCompressor
	CompressionThreshold int        // threshold in bytes above which compression is attempted: 1024 bytes by default
	DisableDedup         bool       // should we disable deduping of class names and hash keys
	DisableFREEZE        bool       // should we disable the FREEZE tag, which calls MarshalBinary
	version              int        // default version to encode
}

type compressor interface {
	compress(b []byte) ([]byte, error)
}

// NewEncoder returns a new Encoder struct with default values
func NewEncoder() *Encoder {
	return &Encoder{
		PerlCompat:           false,
		CompressionThreshold: 1024,
		version:              1,
	}
}

// NewEncoderV2 returns a new Encoder that encodes version 2
func NewEncoderV2() *Encoder {
	return &Encoder{
		PerlCompat:           false,
		CompressionThreshold: 1024,
		version:              2,
	}
}

// NewEncoderV3 returns a new Encoder that encodes version 3
func NewEncoderV3() *Encoder {
	return &Encoder{
		PerlCompat:           false,
		CompressionThreshold: 1024,
		version:              3,
	}
}

var defaultEncoder = NewEncoderV3()

// Marshal encodes body with the default encoder
func Marshal(body interface{}) ([]byte, error) {
	return defaultEncoder.MarshalWithHeader(nil, body)
}

// Marshal returns the Sereal encoding of body
func (e *Encoder) Marshal(body interface{}) (b []byte, err error) {
	return e.MarshalWithHeader(nil, body)
}

// MarshalWithHeader returns the Sereal encoding of body with header data
func (e *Encoder) MarshalWithHeader(header interface{}, body interface{}) (b []byte, err error) {
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

	// uninitialized encoder? set to the most recent supported protocol version
	if e.version == 0 {
		e.version = ProtocolVersion
	}

	encHeader := make([]byte, headerSize, 32)

	if e.version < 3 {
		binary.LittleEndian.PutUint32(encHeader[:4], magicHeaderBytes)
	} else {
		binary.LittleEndian.PutUint32(encHeader[:4], magicHeaderBytesHighBit)
	}

	// Set the <version-type> component in the header
	encHeader[4] = byte(e.version) | byte(serealRaw)<<4

	if header != nil && e.version >= 2 {
		strTable := make(map[string]int)
		ptrTable := make(map[uintptr]int)
		rv := reflectValueOf(header)
		// this is both the flag byte (== "there is user data") and also a hack to make 1-based offsets work
		henv := []byte{0x01} // flag byte == "there is user data"
		encHeaderSuffix, err := e.encode(henv, rv, false, strTable, ptrTable)

		if err != nil {
			return nil, err
		}

		encHeader = varint(encHeader, uint(len(encHeaderSuffix)))
		encHeader = append(encHeader, encHeaderSuffix...)
	} else {
		/* header size */
		encHeader = append(encHeader, 0)
	}

	strTable := make(map[string]int)
	ptrTable := make(map[uintptr]int)

	var encBody []byte

	switch e.version {
	case 1:
		encBody, err = e.encode(encBody, body, false, strTable, ptrTable)
	case 2, 3:
		encBody = append(encBody, 0) // hack for 1-based offsets
		encBody, err = e.encode(encBody, body, false, strTable, ptrTable)
		encBody = encBody[1:] // trim hacky first byte
	}

	if err != nil {
		return nil, err
	}

	if e.Compression != nil && (e.CompressionThreshold == 0 || len(encBody) >= e.CompressionThreshold) {
		encBody, err = e.Compression.compress(encBody)
		if err != nil {
			return nil, err
		}

		var doctype documentType

		switch c := e.Compression.(type) {
		case SnappyCompressor:
			if e.version > 1 && !c.Incremental {
				return nil, errors.New("non-incremental snappy compression only valid for v1 documents")
			}
			if e.version == 1 {
				doctype = serealSnappy
			} else {
				doctype = serealSnappyIncremental
			}
		case ZlibCompressor:
			if e.version < 3 {
				return nil, errors.New("zlib compression only valid for v3 documents and up")
			}
			doctype = serealZlib
		default:
			// Defensive programming: this point should never be
			// reached in production code because the compressor
			// interface is not exported, hence no way to pass in
			// an unknown thing. But it may happen during
			// development when a new compressor is implemented,
			// but a relevant document type is not defined.
			panic("undefined compression")
		}

		encHeader[4] |= byte(doctype) << 4
	}

	return append(encHeader, encBody...), nil
}

/*************************************
 * Encode via static types - fast path
 *************************************/
func (e *Encoder) encode(b []byte, v interface{}, isKeyOrClass bool, strTable map[string]int, ptrTable map[uintptr]int) ([]byte, error) {
	var err error

	// TODO probably need a recursion here
	// make sure that we're looking at real value
	if reflectValue, ok := v.(reflect.Value); ok {
		if reflectValue.Kind() != reflect.Invalid {
			v = reflectValue.Interface()
		} else {
			v = nil
		}
	}

	switch value := v.(type) {
	case nil:
		b = append(b, typeUNDEF)

	case bool:
		if value {
			b = append(b, typeTRUE)
		} else {
			b = append(b, typeFALSE)
		}

	case int:
		b = e.encodeInt(b, reflect.Int, int64(value))
	case int8:
		b = e.encodeInt(b, reflect.Int, int64(value))
	case int16:
		b = e.encodeInt(b, reflect.Int, int64(value))
	case int32:
		b = e.encodeInt(b, reflect.Int, int64(value))
	case int64:
		b = e.encodeInt(b, reflect.Int, int64(value))

	case uint:
		b = e.encodeInt(b, reflect.Uint, int64(value))
	case uint8:
		b = e.encodeInt(b, reflect.Uint, int64(value))
	case uint16:
		b = e.encodeInt(b, reflect.Uint, int64(value))
	case uint32:
		b = e.encodeInt(b, reflect.Uint, int64(value))
	case uint64:
		b = e.encodeInt(b, reflect.Uint, int64(value))

	case float32:
		b = e.encodeFloat(b, value)
	case float64:
		b = e.encodeDouble(b, value)

	case string:
		b = e.encodeString(b, value, isKeyOrClass, strTable)

	case []uint8:
		b = e.encodeBytes(b, value, isKeyOrClass, strTable)

	case []interface{}:
		b = e.encodeIntfArray(b, value, strTable, ptrTable)

	case map[string]interface{}:
		b = e.encodeStrMap(b, value, strTable, ptrTable)

	case PerlUndef:
		if value.canonical {
			b = append(b, typeCANONICAL_UNDEF)
		} else {
			b = append(b, typeUNDEF)
		}

	case PerlObject:
		b = append(b, typeOBJECT)
		b = e.encodeBytes(b, []byte(value.Class), true, strTable)
		b, err = e.encode(b, value.Reference, false, strTable, ptrTable)

	case PerlRegexp:
		b = append(b, typeREGEXP)
		b = e.encodeBytes(b, value.Pattern, false, strTable)
		b = e.encodeBytes(b, value.Modifiers, false, strTable)

	case PerlWeakRef:
		b = append(b, typeWEAKEN)
		b, err = e.encode(b, value.Reference, false, strTable, ptrTable)

	//case interface{}:
	//TODO can it be here????
	//case *interface{}:
	//TODO handle here if easy

	default:
		b, err = e.encodeViaReflection(b, reflectValueOf(value), isKeyOrClass, strTable, ptrTable)
	}

	if err != nil {
		return nil, err
	}

	return b, nil
}

func (e *Encoder) encodeInt(by []byte, k reflect.Kind, i int64) []byte {
	switch {
	case 0 <= i && i <= 15:
		by = append(by, byte(i)&0x0f)
	case -16 <= i && i < 0 && k == reflect.Int:
		by = append(by, 0x010|(byte(i)&0x0f))
	case i > 15:
		by = append(by, typeVARINT)
		by = varint(by, uint(i))
	case i < 0:
		n := uint(i)
		if k == reflect.Int {
			by = append(by, typeZIGZAG)
			n = uint((i << 1) ^ (i >> 63))
		} else {
			by = append(by, typeVARINT)
		}

		by = varint(by, uint(n))
	}

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

func (e *Encoder) encodeString(by []byte, s string, isKeyOrClass bool, strTable map[string]int) []byte {
	if !e.DisableDedup && isKeyOrClass {
		if copyOffs, ok := strTable[s]; ok {
			by = append(by, typeCOPY)
			by = varint(by, uint(copyOffs))
			return by
		} else {
			strTable[s] = len(by)
		}
	}

	by = append(by, typeSTR_UTF8)
	by = varint(by, uint(len(s)))
	return append(by, s...)
}

func (e *Encoder) encodeBytes(by []byte, byt []byte, isKeyOrClass bool, strTable map[string]int) []byte {
	l := len(byt)

	if l < 32 {
		if !e.DisableDedup && isKeyOrClass {
			// track short byte strTable
			if copyOffs, ok := strTable[string(byt)]; ok {
				by = append(by, typeCOPY)
				by = varint(by, uint(copyOffs))
				return by
			} else {
				// save for later
				strTable[string(byt)] = len(by)
			}
		}

		by = append(by, typeSHORT_BINARY_0+byte(l))
	} else {
		// TODO dedup here as well
		by = append(by, typeBINARY)
		by = varint(by, uint(l))
	}

	return append(by, byt...)
}

func (e *Encoder) encodeIntfArray(by []byte, arr []interface{}, strTable map[string]int, ptrTable map[uintptr]int) []byte {
	if e.PerlCompat {
		by = append(by, typeREFN)
	}

	l := len(arr)
	by = append(by, typeARRAY)
	by = varint(by, uint(l))

	for i := 0; i < l; i++ {
		by, _ = e.encode(by, arr[i], false, strTable, ptrTable)
		// TODO check error
	}

	return by
}

func (e *Encoder) encodeStrMap(by []byte, m map[string]interface{}, strTable map[string]int, ptrTable map[uintptr]int) []byte {
	if e.PerlCompat {
		by = append(by, typeREFN)
	}

	by = append(by, typeHASH)
	by = varint(by, uint(len(m)))

	for k, v := range m {
		by = e.encodeString(by, k, true, strTable)
		by, _ = e.encode(by, v, false, strTable, ptrTable)
		// TODO check error
	}

	return by
}

/*************************************
 * Encode via reflection
 *************************************/
func (e *Encoder) encodeViaReflection(b []byte, rv reflect.Value, isKeyOrClass bool, strTable map[string]int, ptrTable map[uintptr]int) ([]byte, error) {
	var err error
	if !e.DisableFREEZE && rv.Kind() != reflect.Invalid {
		if m, ok := rv.Interface().(encoding.BinaryMarshaler); ok {
			by, err := m.MarshalBinary()
			if err != nil {
				return nil, err
			}

			b = append(b, typeOBJECT_FREEZE)
			b = e.encodeString(b, concreteName(rv), true, strTable)
			return e.encode(b, reflect.ValueOf(by), false, strTable, ptrTable)
		}
	}

	// make sure we're looking at a real type and not an interface
	for rv.Kind() == reflect.Interface {
		rv = rv.Elem()
	}

	switch rk := rv.Kind(); rk {
	case reflect.Slice:
		// uint8 case is handled in encode()
		fallthrough

	case reflect.Array:
		b = e.encodeArray(b, rv, strTable, ptrTable)

	case reflect.Map:
		b = e.encodeMap(b, rv, strTable, ptrTable)

	case reflect.Struct:
		b = e.encodeStruct(b, rv, strTable, ptrTable)

	case reflect.Ptr:
		b, err = e.encodePointer(b, rv, strTable, ptrTable)
		// TODO make all function return ([]byte, error)

	case reflect.Interface:
		// TODO can it be here???
		// recurse until we get a concrete type
		// could be optmized into a tail call
		b, err = e.encode(b, rv.Elem(), isKeyOrClass, strTable, ptrTable)
		if err != nil {
			return nil, err
		}

	case reflect.Invalid:
		b = append(b, typeUNDEF)

	default:
		panic(fmt.Sprintf("no support for type '%s' (%s)", rk.String(), rv.Type()))
	}

	if err != nil {
		return nil, err
	}

	return b, nil
}

func (e *Encoder) encodeArray(by []byte, arr reflect.Value, strTable map[string]int, ptrTable map[uintptr]int) []byte {
	if e.PerlCompat {
		by = append(by, typeREFN)
	}

	l := arr.Len()
	by = append(by, typeARRAY)
	by = varint(by, uint(l))

	for i := 0; i < l; i++ {
		by, _ = e.encode(by, arr.Index(i), false, strTable, ptrTable)
	}

	return by
}

func (e *Encoder) encodeMap(by []byte, m reflect.Value, strTable map[string]int, ptrTable map[uintptr]int) []byte {
	if e.PerlCompat {
		by = append(by, typeREFN)
	}

	keys := m.MapKeys()
	by = append(by, typeHASH)
	by = varint(by, uint(len(keys)))

	if e.PerlCompat {
		for _, k := range keys {
			by = e.encodeString(by, k.String(), true, strTable)
			by, _ = e.encode(by, m.MapIndex(k), false, strTable, ptrTable)
		}
	} else {
		for _, k := range keys {
			by, _ = e.encode(by, k, true, strTable, ptrTable)
			by, _ = e.encode(by, m.MapIndex(k), false, strTable, ptrTable)
		}
	}

	return by
}

func (e *Encoder) encodeStruct(by []byte, st reflect.Value, strTable map[string]int, ptrTable map[uintptr]int) []byte {
	tags := getStructTags(st)

	by = append(by, typeOBJECT)
	by = e.encodeBytes(by, []byte(st.Type().Name()), true, strTable)

	if e.PerlCompat {
		// must be a reference
		by = append(by, typeREFN)
	}

	by = append(by, typeHASH)
	by = varint(by, uint(len(tags)))

	if tags != nil {
		for f, i := range tags {
			by = e.encodeString(by, f, true, strTable)
			by, _ = e.encode(by, st.Field(i), false, strTable, ptrTable)
		}
	}

	return by
}

func (e *Encoder) encodePointer(by []byte, rv reflect.Value, strTable map[string]int, ptrTable map[uintptr]int) ([]byte, error) {
	// ikruglov
	// I don't fully understand this logic, so leave it as is :-)

	if rv.Elem().Kind() == reflect.Struct {
		switch rv.Elem().Interface().(type) {
		case PerlRegexp:
			return e.encode(by, rv.Elem(), false, strTable, ptrTable)
		case PerlUndef:
			return e.encode(by, rv.Elem(), false, strTable, ptrTable)
		case PerlObject:
			return e.encode(by, rv.Elem(), false, strTable, ptrTable)
		case PerlWeakRef:
			return e.encode(by, rv.Elem(), false, strTable, ptrTable)
		}
	}

	rvptr := rv.Pointer()
	rvptr2 := getPointer(rv.Elem())

	offs, ok := ptrTable[rvptr]

	if !ok && rvptr2 != 0 {
		offs, ok = ptrTable[rvptr2]
		if ok {
			rvptr = rvptr2
		}
	}

	if ok { // seen this before
		by = append(by, typeREFP)
		by = varint(by, uint(offs))
		by[offs] |= trackFlag // original offset now tracked
	} else {

		lenbOrig := len(by)

		by = append(by, typeREFN)

		ptrTable[rvptr] = lenbOrig

		var err error

		by, err = e.encode(by, rv.Elem(), false, strTable, ptrTable)
		if err != nil {
			return nil, err
		}

		if rvptr2 != 0 {
			// The thing this this points to starts one after the current pointer
			ptrTable[rvptr2] = lenbOrig + 1
		}
	}

	return by, nil
}

func varint(by []byte, n uint) []uint8 {
	for n >= 0x80 {
		b := byte(n) | 0x80
		by = append(by, b)
		n >>= 7
	}

	return append(by, byte(n))
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

func concreteName(value reflect.Value) string {
	return value.Type().PkgPath() + "." + value.Type().Name()
}

func reflectValueOf(v interface{}) reflect.Value {
	rv, ok := v.(reflect.Value)
	if !ok {
		rv = reflect.ValueOf(v)
	}

	return rv
}
