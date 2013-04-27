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
	"strings"
)

type serealHeader struct {
	doctype    documentType
	version    byte
	suffixSize int
}

func readHeader(b []byte) (serealHeader, error) {

	if binary.LittleEndian.Uint32(b[:4]) != magicHeaderBytes {
		return serealHeader{}, errors.New("bad header")
	}

	var h serealHeader

	h.doctype = documentType(b[4] >> 4)
	h.version = b[4] & 0x0f

	ln, sz := varintdecode(b[5:])
	h.suffixSize = ln + sz

	return h, nil
}

type Decoder struct {
	PerlCompat bool
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

	header, err := readHeader(b)

	if err != nil {
		return err
	}

	bodyStart := headerSize + header.suffixSize

	/* XXX instead of creating an uncompressed copy of the document,
	 *     it would be more flexible to use a sort of "Reader" interface */
	switch header.doctype {

	case serealRaw:
		// nothing
	case serealSnappy:
		decoded, err := snappy.Decode(nil, b[bodyStart:])

		if err != nil {
			return err
		}

		d := make([]byte, 0, len(decoded)+bodyStart)
		d = append(d, b[:bodyStart]...)
		d = append(d, decoded...)
		b = d

	case serealSnappyLength:
		ln, sz := varintdecode(b[bodyStart:])
		decoded, err := snappy.Decode(nil, b[bodyStart+sz:bodyStart+sz+ln])

		if err != nil {
			return err
		}

		// we want to treat the passed-in buffer as read-only here
		// if we just used append, we'd overwrite any data past the end of the underlying array, which wouldn't be nice
		d := make([]byte, 0, len(decoded)+bodyStart)
		d = append(d, b[:bodyStart]...)
		d = append(d, decoded...)
		b = d

	default:
		return errors.New(fmt.Sprintf("Document type '%d' not yet supported", header.doctype))

	}

	if header.version != 1 {
		return errors.New(fmt.Sprintf("Document version '%d' not yet supported", header.version))
	}

	if reflect.TypeOf(v).Kind() != reflect.Ptr {
		return errors.New("expected pointer")
	}

	tracked := make(map[int]reflect.Value)

	_, err = d.decode(b, bodyStart, tracked, vPtrValue.Elem())

	if err != nil {
		return err
	}

	return nil

}

func (d *Decoder) decode(b []byte, idx int, tracked map[int]reflect.Value, ptr reflect.Value) (int, error) {

	startIdx := idx

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
		i := int(tag)
		if neg {
			i -= 32
		}
		setInt(ptr, reflect.Int, i)

	case tag == typeVARINT, tag == typeZIGZAG:
		idx++
		i, sz := varintdecode(b[idx:])
		idx += sz
		if tag == typeVARINT {
			// varints are unsigned, but we returned a signed int if possible
			if i < 0 {
				setInt(ptr, reflect.Uint, i)
			} else {
				setInt(ptr, reflect.Int, i)
			}
		} else {
			// zigzag
			i = -(1 + (i >> 1)) // un-zigzag
			setInt(ptr, reflect.Int, i)
		}

	case tag == typeFLOAT:
		idx++

		bits := uint32(b[idx]) | uint32(b[idx+1])<<8 | uint32(b[idx+2])<<16 | uint32(b[idx+3])<<24
		f := math.Float32frombits(bits)
		idx += 4
		setFloat(ptr, reflect.Float32, float64(f))

	case tag == typeDOUBLE:
		idx++

		bits := uint64(b[idx]) | uint64(b[idx+1])<<8 | uint64(b[idx+2])<<16 | uint64(b[idx+3])<<24 | uint64(b[idx+4])<<32 | uint64(b[idx+5])<<40 | uint64(b[idx+6])<<48 | uint64(b[idx+7])<<56
		f := math.Float64frombits(bits)
		idx += 8
		setFloat(ptr, reflect.Float64, float64(f))

	case tag == typeUNDEF:
		idx++

		if d.PerlCompat {
			ptr.Set(reflect.ValueOf(&PerlUndef{}))
		} else {
			switch ptr.Kind() {
			case reflect.Interface, reflect.Ptr, reflect.Map, reflect.Slice:
				ptr.Set(reflect.Zero(ptr.Type()))
			}
		}

	case tag == typeBINARY:

		idx++
		ln, sz := varintdecode(b[idx:])
		idx += sz

		var slice reflect.Value

		switch {

		case ptr.Kind() == reflect.Interface && ptr.IsNil():
			e := make([]byte, ln, ln)
			slice = reflect.ValueOf(e)
			ptr.Set(slice)

		case ptr.Kind() == reflect.Slice && ptr.Type().Elem().Kind() == reflect.Uint8 && ptr.IsNil():
			e := make([]byte, ln, ln)
			slice = reflect.ValueOf(e)
			ptr.Set(slice)

		case ptr.Kind() == reflect.Slice || ptr.Kind() == reflect.Array || ptr.Kind() == reflect.String:
			slice = ptr
		}

		setString(slice, b[idx:idx+ln])
		idx += ln

	case tag == typeSTR_UTF8:

		idx++
		ln, sz := varintdecode(b[idx:])
		idx += sz

		s := string(b[idx : idx+ln])
		idx += ln

		p := reflect.ValueOf(s)

		switch {
		case ptr.Kind() == reflect.Interface && ptr.IsNil():
			ptr.Set(p)
		case ptr.Kind() == reflect.String:
			ptr.SetString(s)
		default:
			panic("bad type for string: " + ptr.Kind().String())
		}

		if trackme {
			tracked[startIdx] = p
		}

	case tag == typeREFN:
		idx++

		if d.PerlCompat {
			var e interface{}
			re := reflect.ValueOf(&e)

			var p reflect.Value

			if trackme {
				// we're tracked, but we don't yet know what it is we are
				// so, create a pointer to 'something' and store it for later
				p = reflect.New(re.Elem().Type())
				p.Elem().Set(re)
				tracked[startIdx] = p
			}

			sz, _ := d.decode(b, idx, tracked, re.Elem())
			idx += sz

			// replace p with a more accurate pointer type
			if !p.IsValid() {
				p = reflect.New(re.Elem().Elem().Type())
			}
			p.Elem().Set(re.Elem().Elem())
			ptr.Set(p)
			if trackme {
				tracked[startIdx] = p
			}
		} else {
			// references are flattened, same as gob
			sz, _ := d.decode(b, idx, tracked, ptr)
			idx += sz
		}

	case tag == typeREFP:
		idx++

		offs, sz := varintdecode(b[idx:])
		idx += sz

		e := tracked[offs]

		p := reflect.New(e.Type())
		p.Elem().Set(e)
		ptr.Set(p)

	case tag == typeHASH:

		idx++

		ln, sz := varintdecode(b[idx:])
		idx += sz

		if ptr.Kind() == reflect.Interface && ptr.IsNil() {
			m := make(map[string]interface{})
			ptr.Set(reflect.ValueOf(m))
		}

		structTags := getStructTags(ptr)

		if trackme {
			tracked[startIdx] = ptr
		}

		for i := 0; i < ln; i++ {
			var key string
			rkey := reflect.ValueOf(&key)
			sz, _ := d.decode(b, idx, tracked, rkey.Elem())
			idx += sz
			rval, _ := getValue(ptr, key, structTags)
			sz, _ = d.decode(b, idx, tracked, rval)
			idx += sz
			setKeyValue(ptr, key, rval, structTags)
		}

	case tag == typeARRAY:

		idx++
		ln, sz := varintdecode(b[idx:])
		idx += sz

		var slice reflect.Value

		switch {

		case ptr.Kind() == reflect.Interface && ptr.IsNil():
			var e []interface{}
			if ln == 0 {
				e = make([]interface{}, 0, 1)
			} else {
				e = make([]interface{}, ln, ln)
			}
			slice = reflect.ValueOf(e)
			ptr.Set(slice)

		case ptr.Kind() == reflect.Slice && (ptr.IsNil() || ptr.Len() == 0):
			slice = reflect.MakeSlice(ptr.Type(), ln, ln)
			ptr.Set(slice)

		case (ptr.Kind() == reflect.Slice && ptr.Len() > 0) || ptr.Kind() == reflect.Array:
			slice = ptr
		default:
			panic("unhandled type: " + ptr.Kind().String())
		}

		if trackme {
			tracked[startIdx] = ptr
		}

		for i := 0; i < ln; i++ {
			var e reflect.Value
			if i < slice.Len() {
				e = slice.Index(i)
			} else {
				var iface interface{}
				e = reflect.ValueOf(&iface).Elem()
			}
			sz, _ := d.decode(b, idx, tracked, e)

			idx += sz
		}

	case tag == typeOBJECT:
		idx++

		// FIXME: track before recurse?
		var s string
		className := reflect.ValueOf(&s)
		sz, _ := d.decode(b, idx, tracked, className.Elem())
		idx += sz

		if d.PerlCompat {
			var ref interface{}
			rref := reflect.ValueOf(&ref)
			sz, _ := d.decode(b, idx, tracked, rref.Elem())
			idx += sz

			s := stringOf(className)
			o := &PerlObject{s, ref}
			ptr.Set(reflect.ValueOf(o))
		} else {
			sz, _ := d.decode(b, idx, tracked, ptr)
			idx += sz

			// FIXME: stuff className somewhere if map/struct?
		}

	case tag == typeOBJECTV:
		idx++
		offs, sz := varintdecode(b[idx:])
		idx += sz
		var s string
		className := reflect.ValueOf(&s)
		sz, _ = d.decode(b, offs, tracked, className.Elem())

		if d.PerlCompat {
			var ref interface{}
			rref := reflect.ValueOf(&ref)
			sz, _ := d.decode(b, idx, tracked, rref.Elem())
			idx += sz

			s := stringOf(className)
			o := &PerlObject{s, ref}
			ptr.Set(reflect.ValueOf(o))
		} else {
			sz, _ := d.decode(b, idx, tracked, ptr)
			idx += sz
		}

	case tag == typeTRUE, tag == typeFALSE:
		idx++
		bol := tag == typeTRUE

		if ptr.Kind() == reflect.Interface && ptr.IsNil() {
			ptr.Set(reflect.ValueOf(bol))
		} else {
			ptr.SetBool(bol)
		}

	case tag >= typeARRAYREF_0 && tag < typeARRAYREF_0+16:

		idx++
		ln := int(tag & 0x0f)

		var slice reflect.Value

		switch {

		case ptr.Kind() == reflect.Interface && ptr.IsNil():
			var e []interface{}
			if ln == 0 {
				e = make([]interface{}, 0, 1)
			} else {
				e = make([]interface{}, ln, ln)
			}
			slice = reflect.ValueOf(e)

			if d.PerlCompat {
				p := reflect.New(reflect.TypeOf(e))
				p.Elem().Set(slice)
				ptr.Set(p)
			} else {
				ptr.Set(slice)
			}

		case ptr.Kind() == reflect.Slice && ptr.IsNil():
			slice = reflect.MakeSlice(ptr.Type(), ln, ln)
			ptr.Set(slice)

		case ptr.Kind() == reflect.Slice || ptr.Kind() == reflect.Array:
			slice = ptr

		default:
		}

		if trackme {
			tracked[startIdx] = ptr
		}

		for i := 0; i < ln; i++ {
			var e reflect.Value
			if i < slice.Len() {
				e = slice.Index(i)
			} else {
				var iface interface{}
				e = reflect.ValueOf(&iface).Elem()
			}
			sz, _ := d.decode(b, idx, tracked, e)
			idx += sz
		}

	case tag >= typeHASHREF_0 && tag < typeHASHREF_0+16:

		idx++
		ln := int(tag & 0x0f)

		// FIXME:
		// 1) this is now identical to the typeHASH case
		// 2) how does this affect PerlCompat mode?

		var href reflect.Value

		if ptr.Kind() == reflect.Interface && ptr.IsNil() {
			m := make(map[string]interface{})
			rm := reflect.ValueOf(m)

			if d.PerlCompat {
				p := reflect.New(rm.Type())
				p.Elem().Set(rm)
				ptr.Set(p)
			} else {
				ptr.Set(rm)
			}

			href = rm
		} else {
			href = ptr
		}

		if trackme {
			tracked[startIdx] = ptr
		}

		structTags := getStructTags(ptr)

		for i := 0; i < ln; i++ {
			var key string
			rkey := reflect.ValueOf(&key)
			sz, _ := d.decode(b, idx, tracked, rkey.Elem())
			idx += sz
			rval, _ := getValue(ptr, key, structTags)
			sz, _ = d.decode(b, idx, tracked, rval)
			idx += sz
			setKeyValue(href, key, rval, structTags)
		}

	case tag >= typeSHORT_BINARY_0 && tag < typeSHORT_BINARY_0+32:
		ln := int(tag & 0x1F) // get length from tag
		idx++

		// identical to BINARY
		// very similar to ARRAY
		var slice reflect.Value

		switch {

		case ptr.Kind() == reflect.Interface && ptr.IsNil():
			e := make([]byte, ln, ln)
			slice = reflect.ValueOf(e)
			ptr.Set(slice)

		case ptr.Kind() == reflect.Slice && ptr.Type().Elem().Kind() == reflect.Uint8 && ptr.IsNil():
			e := make([]byte, ln, ln)
			slice = reflect.ValueOf(e)
			ptr.Set(slice)

		case ptr.Kind() == reflect.Slice || ptr.Kind() == reflect.Array || ptr.Kind() == reflect.String:
			slice = ptr
		}

		setString(slice, b[idx:idx+ln])
		idx += ln

	case tag == typeALIAS:
		idx++

		offs, sz := varintdecode(b[idx:])
		idx += sz

		// FIXME: not technically correct, but better than nothing
		// also, better than panicking

		e := tracked[offs]
		ptr.Set(e)

	case tag == typeCOPY:
		idx++

		offs, sz := varintdecode(b[idx:])
		idx += sz

		d.decode(b, offs, tracked, ptr)

	case tag == typeWEAKEN:
		idx++

		var r interface{}
		rr := reflect.ValueOf(&r).Elem()

		// FIXME: track before recurse?, as with REFN?

		sz, _ := d.decode(b, idx, tracked, rr)
		idx += sz
		if d.PerlCompat {
			w := PerlWeakRef{r}
			ptr.Set(reflect.ValueOf(w))
		} else {
			ptr.Set(rr.Elem())
		}

	case tag == typeREGEXP:
		idx++
		// FIXME: track before recurse?
		var pat string
		rpat := reflect.ValueOf(&pat)
		sz, _ := d.decode(b, idx, tracked, rpat.Elem())
		idx += sz
		var mod []byte
		rmod := reflect.ValueOf(&mod)
		sz, _ = d.decode(b, idx, tracked, rmod.Elem())
		idx += sz

		re := &PerlRegexp{[]byte(pat), mod}

		rre := reflect.ValueOf(re)

		if trackme {
			tracked[startIdx] = rre
		}

		ptr.Set(rre)

	default:
		panic("unknown tag byte: " + strconv.Itoa(int(tag)))
	}

	if _, ok := tracked[startIdx]; !ok && trackme {
		tracked[startIdx] = ptr
	}

	return idx - startIdx, nil

}

func setInt(v reflect.Value, k reflect.Kind, i int) {
	if v.Kind() == reflect.Interface && v.IsNil() {
		switch k {
		case reflect.Uint:
			v.Set(reflect.ValueOf(uint(i)))
		case reflect.Int:
			v.Set(reflect.ValueOf(i))
		}
		return
	}

	switch v.Kind() {
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		v.SetInt(int64(i))
	case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64:
		v.SetUint(uint64(i))
	}
}

func setFloat(v reflect.Value, k reflect.Kind, f float64) {

	if v.Kind() == reflect.Interface && v.IsNil() {
		switch k {
		case reflect.Float32:
			v.Set(reflect.ValueOf(float32(f)))
		case reflect.Float64:
			v.Set(reflect.ValueOf(float64(f)))
		}
		return
	}

	v.SetFloat(f)
}

func getStructTags(ptr reflect.Value) map[string]int {
	if ptr.Kind() != reflect.Struct {
		return nil
	}

	m := make(map[string]int)

	t := ptr.Type()

	l := t.NumField()
	for i := 0; i < l; i++ {
		field := t.Field(i).Tag.Get("sereal")
		m[field] = i
	}

	return m
}

func getValue(ptr reflect.Value, key string, structTags map[string]int) (reflect.Value, bool) {
	if ptr.Kind() == reflect.Map {
		return reflect.New(ptr.Type().Elem()).Elem(), true
	}

	if ptr.Kind() == reflect.Struct {

		if structTags != nil {
			i, ok := structTags[key]
			if !ok {
				var iface interface{}
				return reflect.ValueOf(&iface).Elem(), false
			}
			return ptr.Field(i), true
		}

		f := ptr.FieldByName(key)
		if f.IsValid() {
			return f, true
		}
		f = ptr.FieldByName(strings.Title(key))
		if f.IsValid() {
			return f, true
		}
	}

	var iface interface{}

	return reflect.ValueOf(&iface).Elem(), false
}

func setKeyValue(ptr reflect.Value, key string, val reflect.Value, structTags map[string]int) {

	if ptr.Kind() == reflect.Map {
		if ptr.IsNil() {
			ptr.Set(reflect.MakeMap(ptr.Type()))
		}
		ptr.SetMapIndex(reflect.ValueOf(key), val)
		return
	}

	if ptr.Kind() == reflect.Struct {

		if structTags != nil {
			i, ok := structTags[key]
			if !ok {
				return
			}
			f := ptr.Field(i)
			f.Set(val)
			return
		}

		f := ptr.FieldByName(key)
		if !f.IsValid() {
			f = ptr.FieldByName(strings.Title(key))
		}

		if !f.IsValid() {
			return
		}

		f.Set(val)
		return
	}

	if ptr.Kind() == reflect.Interface && ptr.Elem().Kind() == reflect.Map {
		ptr.Elem().SetMapIndex(reflect.ValueOf(key), val)
		return
	}

	panic("unknown type for setKeyValue: " + ptr.Kind().String())

}

func setString(slice reflect.Value, b []byte) {

	switch slice.Kind() {

	case reflect.Array, reflect.Slice:
		reflect.Copy(slice.Slice(0, slice.Len()), reflect.ValueOf(b))
	case reflect.String:
		slice.SetString(string(b))
	default:
		panic("bad type for setString: " + slice.Kind().String())
	}
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
