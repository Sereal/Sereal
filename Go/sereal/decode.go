package sereal

import (
	"encoding"
	"encoding/binary"
	//"errors"
	"fmt"
	"math"
	"reflect"
	//"runtime"
	//"strconv"
	"strings"
	"sync"
)

type serealHeader struct {
	doctype     documentType
	version     byte
	suffixStart int
	suffixSize  int
	suffixFlags uint8
}

func readHeader(b []byte) (serealHeader, error) {
	first4Bytes := binary.LittleEndian.Uint32(b[:4])

	var h serealHeader

	h.doctype = documentType(b[4] >> 4)
	h.version = b[4] & 0x0f

	validHeader := false

	switch first4Bytes {
	case magicHeaderBytes:
		if 1 <= h.version && h.version <= 2 {
			validHeader = true
		}
	case magicHeaderBytesHighBit:
		if h.version >= 3 {
			validHeader = true
		}
	case magicHeaderBytesHighBitUTF8:
		return serealHeader{}, ErrBadHeaderUTF8
	}

	if !validHeader {
		return serealHeader{}, ErrBadHeader
	}

	ln, sz := varintdecode(b[5:])
	h.suffixSize = ln + sz
	h.suffixStart = headerSize + sz

	return h, nil
}

// A Decoder reads and decodes Sereal objects from an input buffer
type Decoder struct {
	tracked   map[int]reflect.Value
	copyDepth int

	PerlCompat bool
}

type decompressor interface {
	decompress(b []byte) ([]byte, error)
}

// NewDecoder returns a decoder with default flags
func NewDecoder() *Decoder {
	return &Decoder{}
}

// Unmarshal decodes b into body with the default decoder
func Unmarshal(b []byte, body interface{}) error {
	decoder := &Decoder{}
	return decoder.UnmarshalHeaderBody(b, nil, body)
}

// UnmarshalHeader parses the Sereal-v2-encoded buffer b and stores the header data into the variable pointed to by vheader
func (d *Decoder) UnmarshalHeader(b []byte, vheader interface{}) (err error) {
	return d.UnmarshalHeaderBody(b, vheader, nil)
}

// Unmarshal parses the Sereal-encoded buffer b and stores the result in the value pointed to by vbody
func (d *Decoder) Unmarshal(b []byte, vbody interface{}) (err error) {
	return d.UnmarshalHeaderBody(b, nil, vbody)
}

// UnmarshalHeaderBody parses the Sereal-encoded buffer b extracts the header and body data into vheader and vbody, respectively
func (d *Decoder) UnmarshalHeaderBody(b []byte, vheader interface{}, vbody interface{}) (err error) {
	//	defer func() {
	//		if r := recover(); r != nil {
	//			if _, ok := r.(runtime.Error); ok {
	//				panic(r)
	//			}
	//
	//			if s, ok := r.(string); ok {
	//				err = errors.New(s)
	//			} else {
	//				err = r.(error)
	//			}
	//		}
	//	}()

	header, err := readHeader(b)

	if err != nil {
		return err
	}

	bodyStart := headerSize + header.suffixSize

	switch header.version {
	case 1:
		break
	case 2:
		break
	case 3:
		break
	default:
		return fmt.Errorf("document version '%d' not yet supported", header.version)
	}

	var decomp decompressor

	switch header.doctype {
	case serealRaw:
		// nothing

	case serealSnappy:
		if header.version != 1 {
			return ErrBadSnappy
		}
		decomp = SnappyCompressor{Incremental: false}

	case serealSnappyIncremental:
		decomp = SnappyCompressor{Incremental: true}

	case serealZlib:
		if header.version < 3 {
			return ErrBadZlibV3
		}
		decomp = ZlibCompressor{}

	default:
		return fmt.Errorf("document type '%d' not yet supported", header.doctype)
	}

	/* XXX instead of creating an uncompressed copy of the document,
	 *     it would be more flexible to use a sort of "Reader" interface */
	if decomp != nil {
		decompBody, err := decomp.decompress(b[bodyStart:])
		if err != nil {
			return err
		}

		// shrink down b to reuse the allocated buffer
		b = b[:0]
		b = append(b, b[:bodyStart]...)
		b = append(b, decompBody...)
	}

	//	if vheader != nil && header.suffixSize != 1 {
	//		tracked := make(map[int]reflect.Value)
	//		if reflect.TypeOf(vheader).Kind() != reflect.Ptr {
	//			return ErrHeaderPointer
	//		}
	//
	//		headerPtrValue := reflect.ValueOf(vheader)
	//
	//		header.suffixFlags = b[header.suffixStart]
	//
	//		if header.suffixFlags&1 == 1 {
	//			_, err = d.decode(b[header.suffixStart+1:bodyStart], 0, tracked, headerPtrValue.Elem())
	//
	//			if err != nil {
	//				return err
	//			}
	//
	//		}
	//	}

	if err == nil && vbody != nil {
		d.tracked = make(map[int]reflect.Value)
		defer func() { d.tracked = nil }()

		bodyValue := reflect.ValueOf(vbody)
		if bodyValue.Kind() != reflect.Ptr {
			return ErrBodyPointer
		}

		if header.version == 1 {
			if ptr, ok := vbody.(*interface{}); ok && *ptr == nil {
				_, err = d.decode(b, bodyStart, ptr)
			} else {
				_, err = d.decodeViaReflection(b, bodyStart, bodyValue.Elem())
			}
		} else {
			// serealv2 documents have 1-based offsets :/
			if ptr, ok := vbody.(*interface{}); ok && *ptr == nil {
				_, err = d.decode(b[bodyStart-1:], 1, ptr)
			} else {
				_, err = d.decodeViaReflection(b[bodyStart-1:], 1, bodyValue.Elem())
			}
		}
	}

	return err
}

/****************************************************************
 * Decode document of unknown structure (i.e. without reflection)
 ****************************************************************/
func (d *Decoder) decode(by []byte, idx int, ptr *interface{}) (int, error) {
	if idx < 0 || idx >= len(by) {
		return 0, ErrTruncated
	}

	var err error
	tag := by[idx]

	// skip over any padding bytes
	for tag == typePAD || tag == typePAD|trackFlag {
		idx++
		if idx >= len(by) {
			return 0, ErrTruncated
		}

		tag = by[idx]
	}

	trackme := (tag & trackFlag) == trackFlag
	if trackme {
		tag &^= trackFlag
		d.tracked[idx] = reflect.ValueOf(ptr)
	}

	idx++

	//fmt.Printf("start decode: tag %d (0x%x)\n", int(tag), int(tag))

	switch {
	case tag < typeVARINT:
		*ptr = d.decodeInt(tag)

	case tag == typeVARINT:
		var val int
		if val, idx = d.decodeVarint(by, idx); val < 0 {
			*ptr = uint(val)
		} else {
			*ptr = val
		}

	case tag == typeZIGZAG:
		*ptr, idx = d.decodeZigzag(by, idx)

	case tag == typeFLOAT:
		*ptr, idx, err = d.decodeFloat(by, idx)

	case tag == typeDOUBLE:
		*ptr, idx, err = d.decodeDouble(by, idx)

	case tag == typeTRUE:
		*ptr = true

	case tag == typeFALSE:
		*ptr = false

	case tag == typeHASH:
		// see commends at top of the function
		ln, sz := varintdecode(by[idx:])
		idx, err = d.decodeHash(by, idx+sz, ln, ptr, false)

	case tag >= typeHASHREF_0 && tag < typeHASHREF_0+16:
		idx, err = d.decodeHash(by, idx, int(tag&0x0f), ptr, d.PerlCompat)

	case tag == typeARRAY:
		// see commends at top of the function
		ln, sz := varintdecode(by[idx:])
		idx, err = d.decodeArray(by, idx+sz, ln, ptr, false)

	case tag >= typeARRAYREF_0 && tag < typeARRAYREF_0+16:
		idx, err = d.decodeArray(by, idx, int(tag&0x0f), ptr, d.PerlCompat)

	case tag == typeSTR_UTF8:
		var val []byte
		ln, sz := varintdecode(by[idx:])
		if val, idx, err = d.decodeBinary(by, idx+sz, ln); err == nil {
			*ptr = string(val)
		}

	case tag == typeBINARY:
		ln, sz := varintdecode(by[idx:])
		*ptr, idx, err = d.decodeBinary(by, idx+sz, ln)

	case tag >= typeSHORT_BINARY_0 && tag < typeSHORT_BINARY_0+32:
		*ptr, idx, err = d.decodeBinary(by, idx, int(tag&0x1f))

	case tag == typeUNDEF, tag == typeCANONICAL_UNDEF:
		if d.PerlCompat && tag == typeCANONICAL_UNDEF {
			*ptr = perlCanonicalUndef
		} else if d.PerlCompat {
			*ptr = &PerlUndef{}
		} else {
			*ptr = nil
		}

	case tag == typeREFN:
		if d.PerlCompat {
			var iface interface{}
			*ptr = &iface

			if idx, err = d.decode(by, idx, &iface); !trackme && err == nil {
				// if REFN is not tracked, build a pointer to concrete type
				// otherwise let ptr be pointer to something (i.e. *interface{})
				riface := reflect.ValueOf(iface)

				// reflect.New create value of type *riface.Type())
				val := reflect.New(riface.Type())
				val.Elem().Set(riface)
				*ptr = val.Interface()
			}
		} else {
			idx, err = d.decode(by, idx, ptr)
		}

	case tag == typeREFP, tag == typeALIAS:
		offs, sz := varintdecode(by[idx:])
		idx += sz

		if offs < 0 || offs >= idx {
			return 0, ErrCorrupt{errBadOffset}
		}

		rv, ok := d.tracked[offs]
		if !ok {
			return 0, ErrCorrupt{errUntrackedOffsetREFP}
		}

		if tag == typeREFP {
			// rv contains *interface{},
			// rv.Elem() will be an interface
			// rv.Elem().Elem() should be the data inside interface
			rvData := rv.Elem().Elem()

			// reflect.New create value of type *riface.Type())
			val := reflect.New(rvData.Type())
			val.Elem().Set(rvData)
			*ptr = val.Interface()
		} else {
			*ptr = rv.Elem().Interface()
		}

	case tag == typeWEAKEN:
		if d.PerlCompat {
			pweak := PerlWeakRef{}
			*ptr = &pweak
			idx, err = d.decode(by, idx, &pweak.Reference)
		} else {
			idx, err = d.decode(by, idx, ptr)
		}

	case tag == typeOBJECT:
		var className []byte
		if className, idx, err = d.decodeStringish(by, idx); err != nil {
			return idx, err
		}

		if d.PerlCompat {
			pobj := PerlObject{Class: string(className)}
			*ptr = &pobj
			idx, err = d.decode(by, idx, &pobj.Reference)
		} else {
			idx, err = d.decode(by, idx, ptr)
		}

	case tag == typeREGEXP:
		var pattern []byte
		if pattern, idx, err = d.decodeStringish(by, idx); err != nil {
			return idx, err
		}

		var modifiers []byte
		if modifiers, idx, err = d.decodeStringish(by, idx); err != nil {
			return idx, err
		}

		// TODO perhaps, copy values
		*ptr = &PerlRegexp{pattern, modifiers}

	default:
		return 0, fmt.Errorf("unknown tag byte: %d (0x%x)", int(tag), int(tag))
	}

	//fmt.Printf("stop decode: tag %d (0x%x)\n", int(tag), int(tag))
	return idx, err
}

func (d *Decoder) decodeInt(tag byte) int {
	if (tag & 0x10) == 0x10 {
		return int(tag) - 32 // negative number
	} else {
		return int(tag)
	}
}

func (d *Decoder) decodeVarint(by []byte, idx int) (int, int) {
	i, sz := varintdecode(by[idx:])
	return i, idx + sz
}

func (d *Decoder) decodeZigzag(by []byte, idx int) (int, int) {
	i, sz := varintdecode(by[idx:])
	return int(-(1 + (uint64(i) >> 1))), idx + sz
}

func (d *Decoder) decodeFloat(by []byte, idx int) (float32, int, error) {
	if idx+3 >= len(by) {
		return 0, 0, ErrTruncated
	}

	bits := uint32(by[idx]) | uint32(by[idx+1])<<8 | uint32(by[idx+2])<<16 | uint32(by[idx+3])<<24
	return math.Float32frombits(bits), idx + 4, nil
}

func (d *Decoder) decodeDouble(by []byte, idx int) (float64, int, error) {
	if idx+7 >= len(by) {
		return 0, 0, ErrTruncated
	}

	bits := uint64(by[idx]) | uint64(by[idx+1])<<8 | uint64(by[idx+2])<<16 | uint64(by[idx+3])<<24 | uint64(by[idx+4])<<32 | uint64(by[idx+5])<<40 | uint64(by[idx+6])<<48 | uint64(by[idx+7])<<56
	return math.Float64frombits(bits), idx + 8, nil
}

func (d *Decoder) decodeHash(by []byte, idx int, ln int, ptr *interface{}, isRef bool) (int, error) {
	if ln < 0 || ln > math.MaxInt32 {
		return 0, ErrCorrupt{errBadHashSize}
	}

	var err error
	hash := make(map[string]interface{}, ln)

	if isRef {
		*ptr = &hash
	} else {
		*ptr = hash
	}

	for i := 0; i < ln; i++ {
		var key []byte
		key, idx, err = d.decodeStringish(by, idx)
		if err != nil {
			return 0, err
		}

		var value interface{}
		idx, err = d.decode(by, idx, &value)
		if err != nil {
			return 0, err
		}

		hash[string(key)] = value
	}

	return idx, nil
}

func (d *Decoder) decodeArray(by []byte, idx int, ln int, ptr *interface{}, isRef bool) (int, error) {
	if ln < 0 || ln > math.MaxInt32 {
		return 0, ErrCorrupt{errBadSliceSize}
	}

	var err error
	var slice []interface{}

	if ln == 0 {
		// FIXME this is not optimal
		slice = make([]interface{}, 0, 1)
	} else {
		slice = make([]interface{}, ln, ln)
	}

	if isRef {
		*ptr = &slice
	} else {
		*ptr = slice
	}

	for i := 0; i < ln; i++ {
		idx, err = d.decode(by, idx, &slice[i])
		if err != nil {
			return 0, err
		}
	}

	return idx, nil
}

func (d *Decoder) decodeBinary(by []byte, idx int, ln int) ([]byte, int, error) {
	if ln < 0 {
		return nil, 0, ErrCorrupt{errBadStringSize}
	} else if idx+ln > len(by) {
		return nil, 0, ErrTruncated
	}

	return by[idx : idx+ln], idx + ln, nil
}

func (d *Decoder) decodeStringish(by []byte, idx int) ([]byte, int, error) {
	if idx < 0 || idx >= len(by) {
		return nil, 0, ErrTruncated
	}

	//TODO trackme

	var err error
	var res []byte

	tag := by[idx]
	for tag == typePAD || tag == typePAD|trackFlag {
		idx++
		if idx >= len(by) {
			return nil, 0, ErrTruncated
		}

		tag = by[idx]
	}

	tag &^= trackFlag
	idx++

	//fmt.Printf("decodeStringish: tag %d (0x%x)\n", int(tag), int(tag))

	switch {
	case tag == typeBINARY, tag == typeSTR_UTF8:
		ln, sz := varintdecode(by[idx:])
		idx += sz

		if ln < 0 {
			return nil, 0, ErrCorrupt{errBadStringSize}
		} else if idx+ln > len(by) {
			return nil, 0, ErrTruncated
		}

		res = by[idx : idx+ln]
		idx += ln

	case tag >= typeSHORT_BINARY_0 && tag < typeSHORT_BINARY_0+32:
		ln := int(tag & 0x1F) // get length from tag
		res = by[idx : idx+ln]
		idx += ln

	case tag == typeCOPY:
		offs, sz := varintdecode(by[idx:])
		idx += sz

		// TODO nested copy
		if offs < 0 || offs >= len(by) {
			return nil, 0, ErrCorrupt{errBadOffset}
		}

		res, _, err = d.decodeStringish(by, offs)
		if err != nil {
			return nil, 0, err
		}

	default:
		return nil, 0, fmt.Errorf("expect stringhish at offset %d but got %d (0x%x)", idx, int(tag), int(tag))
	}

	//fmt.Println(string(*ptr))
	return res, idx, nil
}

/********************************************************************
 * Decode document with predefined structure (have to use reflection)
 ********************************************************************/
func (d *Decoder) decodeViaReflection(by []byte, idx int, ptr reflect.Value) (int, error) {
	if idx < 0 || idx >= len(by) {
		return 0, ErrTruncated
	}

	var err error
	var iface interface{}
	ptrKind := ptr.Kind()

	// at this point structure of decoding document is uknown, make a shortcut
	if ptrKind == reflect.Interface && ptr.IsNil() {
		idx, err = d.decode(by, idx, &iface)
		ptr.Set(reflect.ValueOf(iface))
		return idx, err
	}

	tag := by[idx]
	for tag == typePAD || tag == typePAD|trackFlag {
		idx++
		if idx >= len(by) {
			return 0, ErrTruncated
		}

		tag = by[idx]
	}

	if (tag & trackFlag) == trackFlag {
		tag &^= trackFlag
		d.tracked[idx] = ptr
	}

	idx++

	switch {
	case tag < typeVARINT:
		setInt(ptr, d.decodeInt(tag))

	case tag == typeVARINT:
		var val int
		val, idx = d.decodeVarint(by, idx)
		setInt(ptr, val)

	case tag == typeZIGZAG:
		var val int
		val, idx = d.decodeZigzag(by, idx)
		setInt(ptr, val)

	case tag == typeFLOAT:
		var val float32
		if val, idx, err = d.decodeFloat(by, idx); err == nil {
			ptr.SetFloat(float64(val))
		}

	case tag == typeDOUBLE:
		var val float64
		if val, idx, err = d.decodeDouble(by, idx); err == nil {
			ptr.SetFloat(float64(val))
		}

	case tag == typeTRUE, tag == typeFALSE:
		ptr.SetBool(tag == typeTRUE)

	case tag == typeBINARY:
		var val []byte
		ln, sz := varintdecode(by[idx:])
		if val, idx, err = d.decodeBinary(by, idx+sz, ln); err == nil {
			setBinary(ptr, val)
		}

	case tag >= typeSHORT_BINARY_0 && tag < typeSHORT_BINARY_0+32:
		var val []byte
		if val, idx, err = d.decodeBinary(by, idx, int(tag&0x1f)); err == nil {
			setBinary(ptr, val)
		}

	case tag == typeSTR_UTF8:
		var val []byte
		ln, sz := varintdecode(by[idx:])
		if val, idx, err = d.decodeBinary(by, idx+sz, ln); err == nil {
			ptr.SetString(string(val))
		}

	case tag == typeHASH:
		ln, sz := varintdecode(by[idx:])
		idx, err = d.decodeHashViaReflection(by, idx+sz, ln, ptr)

	case tag >= typeHASHREF_0 && tag < typeHASHREF_0+16:
		idx, err = d.decodeHashViaReflection(by, idx, int(tag&0x0f), ptr) // TODO handle reference

	case tag == typeARRAY:
		ln, sz := varintdecode(by[idx:])
		idx, err = d.decodeArrayViaReflection(by, idx+sz, ln, ptr)

	case tag >= typeARRAYREF_0 && tag < typeARRAYREF_0+16:
		idx, err = d.decodeArrayViaReflection(by, idx, int(tag&0x0f), ptr) // TODO handle reference

	case tag == typeUNDEF, tag == typeCANONICAL_UNDEF:
		if d.PerlCompat && tag == typeCANONICAL_UNDEF {
			ptr.Set(reflect.ValueOf(perlCanonicalUndef))
		} else if d.PerlCompat {
			ptr.Set(reflect.ValueOf(&PerlUndef{}))
		} else {
			if ptrKind == reflect.Ptr || ptrKind == reflect.Map || ptrKind == reflect.Slice {
				ptr.Set(reflect.Zero(ptr.Type()))
			} //TODO else panic
		}

	default:
		return 0, fmt.Errorf("unknown tag byte: %d (0x%x)", int(tag), int(tag))
	}

	return idx, err
}

func (d *Decoder) decodeArrayViaReflection(by []byte, idx int, ln int, ptr reflect.Value) (int, error) {
	if ln < 0 || ln > math.MaxInt32 {
		return 0, ErrCorrupt{errBadSliceSize}
	}

	switch ptr.Kind() {
	case reflect.Slice:
		if ptr.IsNil() || ptr.Len() == 0 {
			ptr.Set(reflect.MakeSlice(ptr.Type(), ln, ln))
		}

	case reflect.Array:
		// do nothing

	default:
		panic("unhandled type: " + ptr.Kind().String())
	}

	var err error
	ptrLen := ptr.Len()

	for i := 0; i < ln; i++ {
		if i < ptrLen {
			idx, err = d.decodeViaReflection(by, idx, ptr.Index(i))
		} else {
			// we went outside of array length, so ignore folowwing content
			var iface interface{}
			idx, err = d.decode(by, idx, &iface) // TODO make this process to be efficient
		}

		if err != nil {
			return 0, err
		}
	}

	return idx, nil
}

func (d *Decoder) decodeHashViaReflection(by []byte, idx int, ln int, ptr reflect.Value) (int, error) {
	if ln < 0 || ln > math.MaxInt32 {
		return 0, ErrCorrupt{errBadHashSize}
	}

	var err error

	switch ptr.Kind() {
	case reflect.Map:
		// TODO confirm the same behaivour with JSON
		for i := 0; i < ln; i++ {
			var key []byte
			key, idx, err = d.decodeStringish(by, idx)
			if err != nil {
				return 0, err
			}

			keyValue := reflect.ValueOf(string(key))
			if value := ptr.MapIndex(keyValue); value.IsValid() {
				// strkey exists in map, replace its content but respect structure
				idx, err = d.decodeViaReflection(by, idx, value)
			} else {
				// there is strkey in map, crete a new one
				var iface interface{}
				idx, err = d.decode(by, idx, &iface)
				if err != nil {
					return 0, err
				}

				ptr.SetMapIndex(keyValue, reflect.ValueOf(iface))
			}

			if err != nil {
				return 0, err
			}
		}

	case reflect.Struct:
		tags := getStructTags(ptr)
		for i := 0; i < ln; i++ {
			var key []byte
			key, idx, err = d.decodeStringish(by, idx)
			if err != nil {
				return 0, err
			}

			found := false
			strkey := string(key)

			if tags == nil {
				// do nothing
			} else if i, found := tags[strkey]; found {
				idx, err = d.decodeViaReflection(by, idx, ptr.Field(i))
			} else if i, found := tags[strings.Title(strkey)]; found {
				idx, err = d.decodeViaReflection(by, idx, ptr.Field(i))
			}

			if !found {
				// struct doesn't contain field with strkey name
				var iface interface{}
				idx, err = d.decode(by, idx, &iface) // TODO make this process to be efficient
			}

			if err != nil {
				return 0, err
			}
		}

	default:
		panic("unhandled type: " + ptr.Kind().String())
	}

	return idx, nil
}

func setInt(ptr reflect.Value, i int) {
	switch ptr.Kind() {
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		ptr.SetInt(int64(i))

	case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64:
		ptr.SetUint(uint64(i))

	default:
		panic("bad type for setInt: " + ptr.Kind().String())
	}
}

func setBinary(ptr reflect.Value, val []byte) {
	switch ptr.Kind() {
	case reflect.Slice:
		if ptr.Type().Elem().Kind() == reflect.Uint8 && ptr.IsNil() {
			slice := make([]byte, len(val), len(val))
			ptr.Set(reflect.ValueOf(slice))
		}

		reflect.Copy(ptr, reflect.ValueOf(val))

	case reflect.Array:
		reflect.Copy(ptr.Slice(0, ptr.Len()), reflect.ValueOf(val))

	case reflect.String:
		ptr.SetString(string(val))

	default:
		panic("bad type for setBinary: " + ptr.Kind().String())
	}
}

func varintdecode(by []byte) (n int, sz int) {
	s := uint(0) // shift count
	for i, b := range by {
		n |= int(b&0x7f) << s
		s += 7

		if (b & 0x80) == 0 {
			return n, i + 1
		}

		if s > 63 {
			// too many continuation bits
			panic("bad varint")
		}
	}

	// byte without continuation bit
	panic("bad varint")
}

//func (d *Decoder) decode(b []byte, idx int, tracked map[int]reflect.Value, ptr reflect.Value) (int, error) {
//
//	if idx < 0 || idx >= len(b) {
//		return 0, ErrTruncated
//	}
//
//	startIdx := idx
//
//	tag := b[idx]
//
//	// skip over any padding bytes
//	for tag == typePAD || tag == typePAD|trackFlag {
//		idx++
//
//		if idx >= len(b) {
//			return 0, ErrTruncated
//		}
//
//		tag = b[idx]
//	}
//
//	trackme := (tag & trackFlag) == trackFlag
//
//	tag &^= trackFlag
//
//	switch {
//	case tag < typeVARINT:
//		idx++
//		neg := (tag & 0x10) == 0x10
//		i := int(tag)
//		if neg {
//			i -= 32
//		}
//		setInt(ptr, reflect.Int, i)
//
//	case tag == typeVARINT, tag == typeZIGZAG:
//		idx++
//		i, sz := varintdecode(b[idx:])
//		idx += sz
//		if tag == typeVARINT {
//			// varints are unsigned, but we returned a signed int if possible
//			if i < 0 {
//				setInt(ptr, reflect.Uint, i)
//			} else {
//				setInt(ptr, reflect.Int, i)
//			}
//		} else {
//			// zigzag
//			i = int(-(1 + (uint64(i) >> 1))) // un-zigzag
//			setInt(ptr, reflect.Int, i)
//		}
//
//	case tag == typeFLOAT:
//		idx++
//
//		if idx+3 >= len(b) {
//			return 0, ErrTruncated
//		}
//
//		bits := uint32(b[idx]) | uint32(b[idx+1])<<8 | uint32(b[idx+2])<<16 | uint32(b[idx+3])<<24
//		f := math.Float32frombits(bits)
//		idx += 4
//		setFloat(ptr, reflect.Float32, float64(f))
//
//	case tag == typeDOUBLE:
//		idx++
//
//		if idx+7 >= len(b) {
//			return 0, ErrTruncated
//		}
//
//		bits := uint64(b[idx]) | uint64(b[idx+1])<<8 | uint64(b[idx+2])<<16 | uint64(b[idx+3])<<24 | uint64(b[idx+4])<<32 | uint64(b[idx+5])<<40 | uint64(b[idx+6])<<48 | uint64(b[idx+7])<<56
//		f := math.Float64frombits(bits)
//		idx += 8
//		setFloat(ptr, reflect.Float64, float64(f))
//
//	case tag == typeUNDEF, tag == typeCANONICAL_UNDEF:
//		idx++
//
//		if d.PerlCompat {
//			if tag == typeCANONICAL_UNDEF {
//				ptr.Set(reflect.ValueOf(perlCanonicalUndef))
//			} else {
//				ptr.Set(reflect.ValueOf(&PerlUndef{}))
//			}
//		} else {
//			switch ptr.Kind() {
//			case reflect.Interface, reflect.Ptr, reflect.Map, reflect.Slice:
//				ptr.Set(reflect.Zero(ptr.Type()))
//			}
//		}
//
//	case tag == typeBINARY:
//
//		idx++
//		ln, sz := varintdecode(b[idx:])
//
//		if ln < 0 || ln > math.MaxInt32 {
//			return 0, ErrCorrupt{errBadSliceSize}
//		}
//
//		idx += sz
//
//		var slice reflect.Value
//
//		switch {
//
//		case ptr.Kind() == reflect.Interface && ptr.IsNil():
//			e := make([]byte, ln, ln)
//			slice = reflect.ValueOf(e)
//			ptr.Set(slice)
//
//		case ptr.Kind() == reflect.Slice && ptr.Type().Elem().Kind() == reflect.Uint8 && ptr.IsNil():
//			e := make([]byte, ln, ln)
//			slice = reflect.ValueOf(e)
//			ptr.Set(slice)
//
//		case ptr.Kind() == reflect.Slice || ptr.Kind() == reflect.Array || ptr.Kind() == reflect.String:
//			slice = ptr
//		}
//
//		if idx+ln > len(b) {
//			return 0, ErrTruncated
//		}
//
//		setString(slice, b[idx:idx+ln])
//		idx += ln
//
//	case tag == typeSTR_UTF8:
//
//		idx++
//		ln, sz := varintdecode(b[idx:])
//		idx += sz
//
//		if ln < 0 {
//			return 0, ErrCorrupt{errBadStringSize}
//		}
//
//		if idx+ln > len(b) {
//			return 0, ErrTruncated
//		}
//
//		s := string(b[idx : idx+ln])
//		idx += ln
//
//		p := reflect.ValueOf(s)
//
//		switch {
//		case ptr.Kind() == reflect.Interface && ptr.IsNil():
//			ptr.Set(p)
//		case ptr.Kind() == reflect.String:
//			ptr.SetString(s)
//		default:
//			panic("bad type for string: " + ptr.Kind().String())
//		}
//
//		if trackme {
//			tracked[startIdx] = p
//		}
//
//	case tag == typeREFN:
//		idx++
//
//		if d.PerlCompat {
//			var e interface{}
//			re := reflect.ValueOf(&e)
//
//			var p reflect.Value
//
//			if trackme {
//				// we're tracked, but we don't yet know what it is we are
//				// so, create a pointer to 'something' and store it for later
//				p = reflect.New(re.Elem().Type())
//				p.Elem().Set(re)
//				tracked[startIdx] = p
//			}
//
//			sz, _ := d.decode(b, idx, tracked, re.Elem())
//			idx += sz
//
//			// replace p with a more accurate pointer type
//			if !p.IsValid() {
//				p = reflect.New(re.Elem().Elem().Type())
//			}
//			p.Elem().Set(re.Elem().Elem())
//			ptr.Set(p)
//			if trackme {
//				tracked[startIdx] = p
//			}
//		} else {
//			// references are flattened, same as gob
//			sz, _ := d.decode(b, idx, tracked, ptr)
//			idx += sz
//		}
//
//	case tag == typeREFP:
//		idx++
//
//		offs, sz := varintdecode(b[idx:])
//		idx += sz
//
//		if offs < 0 || offs > len(b) {
//			return 0, ErrCorrupt{errBadOffset}
//		}
//
//		e, ok := tracked[offs]
//
//		if !ok {
//			return 0, ErrCorrupt{errUntrackedOffsetREFP}
//		}
//
//		p := reflect.New(e.Type())
//		p.Elem().Set(e)
//		ptr.Set(p)
//
//	case tag == typeHASH:
//
//		idx++
//
//		ln, sz := varintdecode(b[idx:])
//		idx += sz
//
//		if ln < 0 || ln > math.MaxInt32 {
//			return 0, ErrCorrupt{errBadHashSize}
//		}
//
//		if 2*ln > len(b[idx:]) {
//			// not enough sereal tags remaining
//			return 0, ErrTruncated
//		}
//
//		if ptr.Kind() == reflect.Interface && ptr.IsNil() {
//			m := make(map[string]interface{})
//			ptr.Set(reflect.ValueOf(m))
//		}
//
//		tags := getStructTags(ptr)
//
//		if trackme {
//			tracked[startIdx] = ptr
//		}
//
//		for i := 0; i < ln; i++ {
//			var key string
//			rkey := reflect.ValueOf(&key)
//			sz, err := d.decode(b, idx, tracked, rkey.Elem())
//			if err != nil {
//				return 0, err
//			}
//
//			idx += sz
//			rval, _ := getValue(ptr, key, tags)
//			sz, err = d.decode(b, idx, tracked, rval)
//			if err != nil {
//				return 0, err
//			}
//			idx += sz
//			setKeyValue(ptr, key, rval, tags)
//		}
//
//	case tag == typeARRAY:
//
//		idx++
//		ln, sz := varintdecode(b[idx:])
//		idx += sz
//
//		if ln < 0 || ln > math.MaxInt32 {
//			return 0, ErrCorrupt{errBadSliceSize}
//		}
//
//		if ln > len(b[idx:]) {
//			// not enough sereal tags remaining
//			return 0, ErrTruncated
//		}
//
//		var slice reflect.Value
//
//		switch {
//
//		case ptr.Kind() == reflect.Interface && ptr.IsNil():
//			var e []interface{}
//			if ln == 0 {
//				e = make([]interface{}, 0, 1)
//			} else {
//				e = make([]interface{}, ln, ln)
//			}
//			slice = reflect.ValueOf(e)
//			ptr.Set(slice)
//
//		case ptr.Kind() == reflect.Slice && (ptr.IsNil() || ptr.Len() == 0):
//			slice = reflect.MakeSlice(ptr.Type(), ln, ln)
//			ptr.Set(slice)
//
//		case (ptr.Kind() == reflect.Slice && ptr.Len() > 0) || ptr.Kind() == reflect.Array:
//			slice = ptr
//		default:
//			panic("unhandled type: " + ptr.Kind().String())
//		}
//
//		if trackme {
//			tracked[startIdx] = ptr
//		}
//
//		for i := 0; i < ln; i++ {
//			var e reflect.Value
//			if i < slice.Len() {
//				e = slice.Index(i)
//			} else {
//				var iface interface{}
//				e = reflect.ValueOf(&iface).Elem()
//			}
//			sz, err := d.decode(b, idx, tracked, e)
//			if err != nil {
//				return 0, err
//			}
//
//			idx += sz
//		}
//
//	case tag == typeOBJECT:
//		idx++
//
//		// FIXME: track before recurse?
//		var s string
//		className := reflect.ValueOf(&s)
//		if !isStringish(b, idx) {
//			return 0, ErrCorrupt{errStringish}
//		}
//		sz, err := d.decode(b, idx, tracked, className.Elem())
//		if err != nil {
//			return 0, err
//		}
//		idx += sz
//
//		if d.PerlCompat {
//			var ref interface{}
//			rref := reflect.ValueOf(&ref)
//			sz, err := d.decode(b, idx, tracked, rref.Elem())
//			if err != nil {
//				return 0, err
//			}
//			idx += sz
//
//			s := stringOf(className)
//			o := &PerlObject{s, ref}
//			ptr.Set(reflect.ValueOf(o))
//		} else {
//			sz, err := d.decode(b, idx, tracked, ptr)
//			if err != nil {
//				return 0, err
//			}
//
//			idx += sz
//
//			// FIXME: stuff className somewhere if map/struct?
//		}
//
//	case tag == typeOBJECTV:
//		idx++
//		offs, sz := varintdecode(b[idx:])
//		if offs >= len(b) {
//			return 0, ErrCorrupt{errBadOffset}
//		}
//		idx += sz
//		var s string
//		className := reflect.ValueOf(&s)
//		if !isStringish(b, offs) {
//			return 0, ErrCorrupt{errStringish}
//		}
//		sz, err := d.decode(b, offs, tracked, className.Elem())
//		if err != nil {
//			return 0, err
//		}
//
//		if d.PerlCompat {
//			var ref interface{}
//			rref := reflect.ValueOf(&ref)
//			sz, err := d.decode(b, idx, tracked, rref.Elem())
//			if err != nil {
//				return 0, err
//			}
//			idx += sz
//
//			s := stringOf(className)
//			o := &PerlObject{s, ref}
//			ptr.Set(reflect.ValueOf(o))
//		} else {
//			sz, err := d.decode(b, idx, tracked, ptr)
//			if err != nil {
//				return 0, err
//			}
//			idx += sz
//		}
//
//	case tag == typeTRUE, tag == typeFALSE:
//		idx++
//		bol := tag == typeTRUE
//
//		if ptr.Kind() == reflect.Interface && ptr.IsNil() {
//			ptr.Set(reflect.ValueOf(bol))
//		} else {
//			ptr.SetBool(bol)
//		}
//
//	case tag >= typeARRAYREF_0 && tag < typeARRAYREF_0+16:
//
//		idx++
//		ln := int(tag & 0x0f)
//
//		var slice reflect.Value
//
//		switch {
//
//		case ptr.Kind() == reflect.Interface && ptr.IsNil():
//			var e []interface{}
//			if ln == 0 {
//				e = make([]interface{}, 0, 1)
//			} else {
//				e = make([]interface{}, ln, ln)
//			}
//			slice = reflect.ValueOf(e)
//
//			if d.PerlCompat {
//				p := reflect.New(reflect.TypeOf(e))
//				p.Elem().Set(slice)
//				ptr.Set(p)
//			} else {
//				ptr.Set(slice)
//			}
//
//		case ptr.Kind() == reflect.Slice && ptr.IsNil():
//			slice = reflect.MakeSlice(ptr.Type(), ln, ln)
//			ptr.Set(slice)
//
//		case ptr.Kind() == reflect.Slice || ptr.Kind() == reflect.Array:
//			slice = ptr
//
//		default:
//			panic("unhandled type: " + ptr.Kind().String())
//		}
//
//		if trackme {
//			tracked[startIdx] = ptr
//		}
//
//		for i := 0; i < ln; i++ {
//			var e reflect.Value
//			if i < slice.Len() {
//				e = slice.Index(i)
//			} else {
//				var iface interface{}
//				e = reflect.ValueOf(&iface).Elem()
//			}
//			sz, err := d.decode(b, idx, tracked, e)
//			if err != nil {
//				return 0, err
//			}
//			idx += sz
//		}
//
//	case tag >= typeHASHREF_0 && tag < typeHASHREF_0+16:
//
//		idx++
//		ln := int(tag & 0x0f)
//
//		// FIXME:
//		// 1) this is now identical to the typeHASH case
//		// 2) how does this affect PerlCompat mode?
//
//		var href reflect.Value
//
//		if ptr.Kind() == reflect.Interface && ptr.IsNil() {
//			m := make(map[string]interface{})
//			rm := reflect.ValueOf(m)
//
//			if d.PerlCompat {
//				p := reflect.New(rm.Type())
//				p.Elem().Set(rm)
//				ptr.Set(p)
//			} else {
//				ptr.Set(rm)
//			}
//
//			href = rm
//		} else {
//			href = ptr
//		}
//
//		if trackme {
//			tracked[startIdx] = ptr
//		}
//
//		structTags := getStructTags(ptr)
//
//		for i := 0; i < ln; i++ {
//			var key string
//			rkey := reflect.ValueOf(&key)
//			sz, err := d.decode(b, idx, tracked, rkey.Elem())
//			if err != nil {
//				return 0, err
//			}
//			idx += sz
//			rval, _ := getValue(ptr, key, structTags)
//			sz, err = d.decode(b, idx, tracked, rval)
//			if err != nil {
//				return 0, err
//			}
//			idx += sz
//			setKeyValue(href, key, rval, structTags)
//		}
//
//	case tag >= typeSHORT_BINARY_0 && tag < typeSHORT_BINARY_0+32:
//		ln := int(tag & 0x1F) // get length from tag
//		idx++
//
//		// identical to BINARY
//		// very similar to ARRAY
//		var slice reflect.Value
//
//		switch {
//
//		case ptr.Kind() == reflect.Interface && ptr.IsNil():
//			e := make([]byte, ln, ln)
//			slice = reflect.ValueOf(e)
//			ptr.Set(slice)
//
//		case ptr.Kind() == reflect.Slice && ptr.Type().Elem().Kind() == reflect.Uint8 && ptr.IsNil():
//			e := make([]byte, ln, ln)
//			slice = reflect.ValueOf(e)
//			ptr.Set(slice)
//
//		case ptr.Kind() == reflect.Slice || ptr.Kind() == reflect.Array || ptr.Kind() == reflect.String:
//			slice = ptr
//		}
//
//		if idx+ln > len(b) {
//			return 0, ErrTruncated
//		}
//
//		setString(slice, b[idx:idx+ln])
//		idx += ln
//
//	case tag == typeALIAS:
//		idx++
//
//		offs, sz := varintdecode(b[idx:])
//		idx += sz
//
//		if offs < 0 || offs >= len(b) {
//			return 0, ErrCorrupt{errBadOffset}
//		}
//
//		e, ok := tracked[offs]
//		if !ok {
//			return 0, ErrCorrupt{errUntrackedOffsetAlias}
//		}
//
//		// FIXME: not technically correct, but better than nothing
//		// also, better than panicking
//
//		ptr.Set(e)
//
//	case tag == typeCOPY:
//		idx++
//
//		// FIXME: copyDepth isn't reset properly on decoder start -- move it somewhere else?
//		d.copyDepth++
//		defer func() { d.copyDepth-- }()
//
//		offs, sz := varintdecode(b[idx:])
//		idx += sz
//
//		if offs < 0 || offs >= len(b) {
//			return 0, ErrCorrupt{errBadOffset}
//		}
//
//		if d.copyDepth > 0 && !isStringish(b, offs) {
//			return 0, ErrCorrupt{errNestedCOPY}
//		}
//
//		sz, err := d.decode(b, offs, tracked, ptr)
//		if err != nil {
//			return 0, err
//		}
//
//	case tag == typeWEAKEN:
//		idx++
//
//		var r interface{}
//		rr := reflect.ValueOf(&r).Elem()
//
//		// FIXME: track before recurse?, as with REFN?
//
//		sz, _ := d.decode(b, idx, tracked, rr)
//		idx += sz
//		if d.PerlCompat {
//			w := PerlWeakRef{r}
//			ptr.Set(reflect.ValueOf(w))
//		} else {
//			ptr.Set(rr.Elem())
//		}
//
//	case tag == typeREGEXP:
//		idx++
//		// FIXME: track before recurse?
//		var pat string
//		rpat := reflect.ValueOf(&pat)
//		sz, _ := d.decode(b, idx, tracked, rpat.Elem())
//		idx += sz
//		var mod []byte
//		rmod := reflect.ValueOf(&mod)
//		sz, _ = d.decode(b, idx, tracked, rmod.Elem())
//		idx += sz
//
//		re := &PerlRegexp{[]byte(pat), mod}
//
//		rre := reflect.ValueOf(re)
//
//		if trackme {
//			tracked[startIdx] = rre
//		}
//
//		ptr.Set(rre)
//
//	case tag == typeOBJECT_FREEZE:
//		idx++
//
//		var class string
//		rclass := reflect.ValueOf(&class)
//		sz, err := d.decode(b, idx, tracked, rclass.Elem())
//		if err != nil {
//			return 0, err
//		}
//		idx += sz
//
//		// spec says 'any object', but we only support byte slices
//		var data []byte
//		rdata := reflect.ValueOf(&data)
//		sz, err = d.decode(b, idx, tracked, rdata.Elem())
//		if err != nil {
//			return 0, err
//		}
//		idx += sz
//
//		var rfreeze reflect.Value
//
//		if d.PerlCompat {
//			freeze := &PerlFreeze{class, data}
//			rfreeze = reflect.ValueOf(freeze)
//		} else {
//
//			if obj, ok := findUnmarshaler(ptr); ok {
//				err := obj.UnmarshalBinary(data)
//				if err != nil {
//					return 0, err
//				}
//				rfreeze = ptr
//			} else {
//
//				switch {
//
//				case ptr.Kind() == reflect.Interface && ptr.IsNil():
//
//					// do we have a registered handler for this type?
//					registerLock.Lock()
//					concreteClass, ok := nameToType[class]
//					registerLock.Unlock()
//
//					if ok {
//						rzero := instantiateZero(concreteClass)
//						obj, ok := findUnmarshaler(rzero)
//
//						if !ok {
//							// only things that have an unmarshaler should have been put into the map
//							panic(fmt.Sprintf("unable to find unmarshaler for %s", rzero))
//						}
//
//						err := obj.UnmarshalBinary(data)
//						if err != nil {
//							return 0, err
//						}
//
//						rfreeze = reflect.ValueOf(obj)
//					} else {
//						rfreeze = reflect.ValueOf(&PerlFreeze{class, data})
//					}
//
//				case ptr.Kind() == reflect.Slice && ptr.Type().Elem().Kind() == reflect.Uint8 && ptr.IsNil():
//					rfreeze = reflect.ValueOf(data)
//
//				default:
//					return 0, fmt.Errorf("can't unpack FROZEN object into %v", ptr.Type())
//				}
//			}
//		}
//
//		if trackme {
//			tracked[startIdx] = rfreeze
//		}
//
//		ptr.Set(rfreeze)
//
//	default:
//		return 0, errors.New("unknown tag byte: " + strconv.Itoa(int(tag)))
//	}
//
//	if _, ok := tracked[startIdx]; !ok && trackme {
//		tracked[startIdx] = ptr
//	}
//
//	return idx - startIdx, nil
//
//}
//
//func setInt(v reflect.Value, k reflect.Kind, i int) {
//	if v.Kind() == reflect.Interface && v.IsNil() {
//		switch k {
//		case reflect.Uint:
//			v.Set(reflect.ValueOf(uint(i)))
//		case reflect.Int:
//			v.Set(reflect.ValueOf(i))
//		}
//		return
//	}
//
//	switch v.Kind() {
//	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
//		v.SetInt(int64(i))
//	case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64:
//		v.SetUint(uint64(i))
//	default:
//		panic("bad type for setInt: " + v.Kind().String())
//	}
//}
//
//func setFloat(v reflect.Value, k reflect.Kind, f float64) {
//
//	if v.Kind() == reflect.Interface && v.IsNil() {
//		switch k {
//		case reflect.Float32:
//			v.Set(reflect.ValueOf(float32(f)))
//		case reflect.Float64:
//			v.Set(reflect.ValueOf(float64(f)))
//		}
//		return
//	}
//
//	v.SetFloat(f)
//}

var structTagsCache = make(map[reflect.Type]map[string]int) // TODO fix data race

func getStructTags(ptr reflect.Value) map[string]int {
	if ptr.Kind() != reflect.Struct {
		return nil
	}

	t := ptr.Type()

	if m, ok := structTagsCache[t]; ok {
		return m
	}

	m := make(map[string]int)

	l := t.NumField()
	numTags := 0
	for i := 0; i < l; i++ {
		field := t.Field(i).Tag.Get("sereal")
		if field != "" {
			m[field] = i
			numTags++
		}
	}

	if numTags != 0 {
		structTagsCache[t] = m
		return m
	}

	// build one from the public names
	for i := 0; i < l; i++ {
		pkgpath := t.Field(i).PkgPath
		if pkgpath == "" { // exported
			field := t.Field(i).Name
			m[field] = i
			numTags++
		}
	}

	if numTags != 0 {
		structTagsCache[t] = m
		return m
	}

	structTagsCache[t] = nil
	return nil
}

//func getValue(ptr reflect.Value, key string, tags map[string]int) (reflect.Value, bool) {
//	if ptr.Kind() == reflect.Map {
//		return reflect.New(ptr.Type().Elem()).Elem(), true
//	}
//
//	if ptr.Kind() == reflect.Struct {
//
//		if tags == nil {
//			// struct has no public fields
//			var iface interface{}
//			return reflect.ValueOf(&iface).Elem(), false
//		}
//
//		if i, ok := tags[key]; ok {
//			return ptr.Field(i), true
//		}
//
//		tkey := strings.Title(key)
//		if i, ok := tags[tkey]; ok {
//			return ptr.Field(i), true
//		}
//
//		// unknown field name
//		var iface interface{}
//		return reflect.ValueOf(&iface).Elem(), false
//	}
//
//	var iface interface{}
//
//	return reflect.ValueOf(&iface).Elem(), false
//}
//
//func setKeyValue(ptr reflect.Value, key string, val reflect.Value, tags map[string]int) {
//
//	if ptr.Kind() == reflect.Map {
//		if ptr.IsNil() {
//			ptr.Set(reflect.MakeMap(ptr.Type()))
//		}
//		ptr.SetMapIndex(reflect.ValueOf(key), val)
//		return
//	}
//
//	if ptr.Kind() == reflect.Struct {
//
//		if tags == nil {
//			// no public fields, nothing to set
//			return
//		}
//
//		// look for the key we know
//		if i, ok := tags[key]; ok {
//			f := ptr.Field(i)
//			f.Set(val)
//			return
//		}
//
//		// look for the title-cased key
//		tkey := strings.Title(key)
//		if i, ok := tags[tkey]; ok {
//			f := ptr.Field(i)
//			f.Set(val)
//			return
//		}
//
//		// not found
//		return
//	}
//
//	if ptr.Kind() == reflect.Interface && ptr.Elem().Kind() == reflect.Map {
//		ptr.Elem().SetMapIndex(reflect.ValueOf(key), val)
//		return
//	}
//
//	panic("unknown type for setKeyValue: " + ptr.Kind().String())
//
//}
//
//func setString(slice reflect.Value, b []byte) {
//
//	switch slice.Kind() {
//
//	case reflect.Array, reflect.Slice:
//		reflect.Copy(slice.Slice(0, slice.Len()), reflect.ValueOf(b))
//	case reflect.String:
//		slice.SetString(string(b))
//	default:
//		panic("bad type for setString: " + slice.Kind().String())
//	}
//}
//
//func stringOf(v reflect.Value) string {
//
//	if v.Type().Kind() == reflect.Ptr {
//		return stringOf(v.Elem())
//	}
//
//	if v.Type().Kind() == reflect.String {
//		return v.String()
//	}
//
//	if (v.Type().Kind() == reflect.Array || v.Type().Kind() == reflect.Slice) && (v.Type().Elem().Kind() == reflect.Uint8) {
//		return string(v.Bytes())
//	}
//
//	panic("bad value for stringOf")
//}
//
//func isStringish(b []byte, idx int) bool {
//
//	if idx < 0 || idx >= len(b) {
//		return false
//	}
//
//	tag := b[idx]
//
//	// skip over any padding bytes
//	for tag == typePAD || tag == typePAD|trackFlag {
//		idx++
//		if idx >= len(b) {
//			return false
//		}
//		tag = b[idx]
//	}
//
//	tag &^= trackFlag
//
//	if tag == typeCOPY {
//		idx++
//
//		offs, sz := varintdecode(b[idx:])
//		idx += sz
//
//		if offs < 0 || offs >= len(b) {
//			return false
//		}
//		return isStringish(b, offs)
//	}
//
//	return tag == typeBINARY || tag == typeSTR_UTF8 || (tag >= typeSHORT_BINARY_0 && tag <= typeSHORT_BINARY_0+31)
//}
//
//func varintdecode(by []byte) (n int, sz int) {
//
//	s := uint(0) // shift count
//	for i, b := range by {
//		n |= int(b&0x7f) << s
//		s += 7
//
//		if (b & 0x80) == 0 {
//			return n, i + 1
//		}
//
//		if s > 63 {
//			// too many continuation bits
//			panic("bad varint")
//		}
//	}
//
//	// byte without continuation bit
//	panic("bad varint")
//}
//
//func findUnmarshaler(ptr reflect.Value) (encoding.BinaryUnmarshaler, bool) {
//
//	if obj, ok := ptr.Interface().(encoding.BinaryUnmarshaler); ok {
//		return obj, true
//	}
//
//	pptr := ptr.Addr()
//
//	if obj, ok := pptr.Interface().(encoding.BinaryUnmarshaler); ok {
//		return obj, true
//	}
//
//	return nil, false
//}
//
var nameToType = make(map[string]reflect.Type)
var registerLock sync.Mutex

// RegisterName registers the named class with an instance of 'value'.  When the
// decoder finds a FREEZE tag with the given class, the binary data will be
// passed to value's UnmarshalBinary method.
func RegisterName(name string, value interface{}) {
	registerLock.Lock()
	defer registerLock.Unlock()

	rv := reflect.ValueOf(value)

	if _, ok := rv.Interface().(encoding.BinaryUnmarshaler); ok {
		nameToType[name] = rv.Type()
		return
	}

	prv := rv.Addr()
	if _, ok := prv.Interface().(encoding.BinaryUnmarshaler); ok {
		nameToType[name] = prv.Type()
		return
	}

	panic(fmt.Sprintf("unable to register type %s: not encoding.BinaryUnmarshaler", rv.Type()))
}

//
//func instantiateZero(typ reflect.Type) reflect.Value {
//
//	if typ.Kind() == reflect.Ptr {
//		return reflect.New(typ.Elem())
//	}
//
//	v := reflect.New(typ)
//	return v.Addr()
//}
