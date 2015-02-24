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
	ExpectedSize         uint       // give a hint to encoder about expected size of encoded data
	version              int        // default version to encode
	tcache               tagsCache
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
		// this is both the flag byte (== "there is user data") and also a hack to make 1-based offsets work
		henv := []byte{0x01} // flag byte == "there is user data"
		encHeaderSuffix, err := e.encode(henv, header, false, false, strTable, ptrTable)

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

	encBody := make([]byte, 0, e.ExpectedSize)

	switch e.version {
	case 1:
		encBody, err = e.encode(encBody, body, false, false, strTable, ptrTable)
	case 2, 3:
		encBody = append(encBody, 0) // hack for 1-based offsets
		encBody, err = e.encode(encBody, body, false, false, strTable, ptrTable)
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
func (e *Encoder) encode(b []byte, v interface{}, isKeyOrClass bool, isRefNext bool, strTable map[string]int, ptrTable map[uintptr]int) ([]byte, error) {
	var err error

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
		b, err = e.encodeIntfArray(b, value, isRefNext, strTable, ptrTable)

	case map[string]interface{}:
		b, err = e.encodeStrMap(b, value, isRefNext, strTable, ptrTable)

	case reflect.Value:
		if value.Kind() == reflect.Invalid {
			b = append(b, typeUNDEF)
		} else {
			// could be optimized to tail call
			b, err = e.encode(b, value.Interface(), false, isRefNext, strTable, ptrTable)
		}

	case PerlUndef:
		if value.canonical {
			b = append(b, typeCANONICAL_UNDEF)
		} else {
			b = append(b, typeUNDEF)
		}

	case PerlObject:
		b = append(b, typeOBJECT)
		b = e.encodeBytes(b, []byte(value.Class), true, strTable)
		b, err = e.encode(b, value.Reference, false, false, strTable, ptrTable)

	case PerlRegexp:
		b = append(b, typeREGEXP)
		b = e.encodeBytes(b, value.Pattern, false, strTable)
		b = e.encodeBytes(b, value.Modifiers, false, strTable)

	case PerlWeakRef:
		b = append(b, typeWEAKEN)
		b, err = e.encode(b, value.Reference, false, false, strTable, ptrTable)

	//case *interface{}:
	//TODO handle here if easy

	//case interface{}:
	// http://blog.golang.org/laws-of-reflection
	// One important detail is that the pair inside an interface always has the form (value, concrete type)
	// and cannot have the form (value, interface type). Interfaces do not hold interface values.
	//panic("interface cannot hold an interface")

	// ikruglov
	// in theory this block should no be commented,
	// but in practise type *interface{} somehow manages to match interface{}
	// if one manages to properly implement *interface{} case, this block should be uncommented

	default:
		b, err = e.encodeViaReflection(b, reflect.ValueOf(value), isKeyOrClass, isRefNext, strTable, ptrTable)
	}

	return b, err
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
	u := math.Float32bits(f)
	by = append(by, typeFLOAT)
	by = append(by, byte(u))
	by = append(by, byte(u>>8))
	by = append(by, byte(u>>16))
	by = append(by, byte(u>>24))
	return by
}

func (e *Encoder) encodeDouble(by []byte, f float64) []byte {
	u := math.Float64bits(f)
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
	if !e.DisableDedup && isKeyOrClass {
		if copyOffs, ok := strTable[string(byt)]; ok {
			by = append(by, typeCOPY)
			by = varint(by, uint(copyOffs))
			return by
		} else {
			// save for later
			strTable[string(byt)] = len(by)
		}
	}

	if l := len(byt); l < 32 {
		by = append(by, typeSHORT_BINARY_0+byte(l))
	} else {
		by = append(by, typeBINARY)
		by = varint(by, uint(l))
	}

	return append(by, byt...)
}

func (e *Encoder) encodeIntfArray(by []byte, arr []interface{}, isRefNext bool, strTable map[string]int, ptrTable map[uintptr]int) ([]byte, error) {
	if e.PerlCompat && !isRefNext {
		by = append(by, typeREFN)
	}

	// TODO implement ARRAYREF for small arrays

	l := len(arr)
	by = append(by, typeARRAY)
	by = varint(by, uint(l))

	var err error
	for i := 0; i < l; i++ {
		if by, err = e.encode(by, arr[i], false, false, strTable, ptrTable); err != nil {
			return nil, err
		}
	}

	return by, nil
}

func (e *Encoder) encodeStrMap(by []byte, m map[string]interface{}, isRefNext bool, strTable map[string]int, ptrTable map[uintptr]int) ([]byte, error) {
	if e.PerlCompat && !isRefNext {
		by = append(by, typeREFN)
	}

	// TODO implement HASHREF for small maps

	by = append(by, typeHASH)
	by = varint(by, uint(len(m)))

	var err error
	for k, v := range m {
		by = e.encodeString(by, k, true, strTable)
		if by, err = e.encode(by, v, false, false, strTable, ptrTable); err != nil {
			return by, err
		}
	}

	return by, nil
}

/*************************************
 * Encode via reflection
 *************************************/
func (e *Encoder) encodeViaReflection(b []byte, rv reflect.Value, isKeyOrClass bool, isRefNext bool, strTable map[string]int, ptrTable map[uintptr]int) ([]byte, error) {
	if !e.DisableFREEZE && rv.Kind() != reflect.Invalid {
		if m, ok := rv.Interface().(encoding.BinaryMarshaler); ok {
			by, err := m.MarshalBinary()
			if err != nil {
				return nil, err
			}

			b = append(b, typeOBJECT_FREEZE)
			b = e.encodeString(b, concreteName(rv), true, strTable)
			return e.encode(b, reflect.ValueOf(by), false, false, strTable, ptrTable)
		}
	}

	// make sure we're looking at a real type and not an interface
	for rv.Kind() == reflect.Interface {
		rv = rv.Elem()
	}

	var err error
	switch rk := rv.Kind(); rk {
	case reflect.Slice:
		// uint8 case is handled in encode()
		fallthrough

	case reflect.Array:
		b, err = e.encodeArray(b, rv, isRefNext, strTable, ptrTable)

	case reflect.Map:
		b, err = e.encodeMap(b, rv, isRefNext, strTable, ptrTable)

	case reflect.Struct:
		b, err = e.encodeStruct(b, rv, strTable, ptrTable)

	case reflect.Ptr:
		b, err = e.encodePointer(b, rv, strTable, ptrTable)

	default:
		panic(fmt.Sprintf("no support for type '%s' (%s)", rk.String(), rv.Type()))
	}

	return b, err
}

func (e *Encoder) encodeArray(by []byte, arr reflect.Value, isRefNext bool, strTable map[string]int, ptrTable map[uintptr]int) ([]byte, error) {
	if e.PerlCompat && !isRefNext {
		by = append(by, typeREFN)
	}

	l := arr.Len()
	by = append(by, typeARRAY)
	by = varint(by, uint(l))

	var err error
	for i := 0; i < l; i++ {
		if by, err = e.encode(by, arr.Index(i), false, false, strTable, ptrTable); err != nil {
			return nil, err
		}
	}

	return by, nil
}

func (e *Encoder) encodeMap(by []byte, m reflect.Value, isRefNext bool, strTable map[string]int, ptrTable map[uintptr]int) ([]byte, error) {
	if e.PerlCompat && !isRefNext {
		by = append(by, typeREFN)
	}

	keys := m.MapKeys()
	by = append(by, typeHASH)
	by = varint(by, uint(len(keys)))

	if e.PerlCompat {
		var err error
		for _, k := range keys {
			by = e.encodeString(by, k.String(), true, strTable)
			if by, err = e.encode(by, m.MapIndex(k), false, false, strTable, ptrTable); err != nil {
				return by, err
			}
		}
	} else {
		var err error
		for _, k := range keys {
			if by, err = e.encode(by, k, true, false, strTable, ptrTable); err != nil {
				return by, err
			}

			if by, err = e.encode(by, m.MapIndex(k), false, false, strTable, ptrTable); err != nil {
				return by, err
			}
		}
	}

	return by, nil
}

func (e *Encoder) encodeStruct(by []byte, st reflect.Value, strTable map[string]int, ptrTable map[uintptr]int) ([]byte, error) {
	tags := e.tcache.Get(st)

	by = append(by, typeOBJECT)
	by = e.encodeBytes(by, []byte(st.Type().Name()), true, strTable)

	if e.PerlCompat {
		// must be a reference
		by = append(by, typeREFN)
	}

	by = append(by, typeHASH)
	by = varint(by, uint(len(tags)))

	var err error
	for f, i := range tags {
		by = e.encodeString(by, f, true, strTable)
		if by, err = e.encode(by, st.Field(i), false, false, strTable, ptrTable); err != nil {
			return nil, err
		}
	}

	return by, nil
}

func (e *Encoder) encodePointer(by []byte, rv reflect.Value, strTable map[string]int, ptrTable map[uintptr]int) ([]byte, error) {
	// ikruglov
	// I don't fully understand this logic, so leave it as is :-)

	if rv.Elem().Kind() == reflect.Struct {
		switch rv.Elem().Interface().(type) {
		case PerlRegexp:
			return e.encode(by, rv.Elem(), false, false, strTable, ptrTable)
		case PerlUndef:
			return e.encode(by, rv.Elem(), false, false, strTable, ptrTable)
		case PerlObject:
			return e.encode(by, rv.Elem(), false, false, strTable, ptrTable)
		case PerlWeakRef:
			return e.encode(by, rv.Elem(), false, false, strTable, ptrTable)
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
		by, err = e.encode(by, rv.Elem(), false, true, strTable, ptrTable)
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
