package sereal

import (
	"encoding/binary"
	"errors"
	"math"
	"strconv"
)

// A Decoder reads and decodes Sereal objects from an input buffer
type Merger struct {
	version     int
	numElements int
	numOffset   int
	bodyOffset  int
	finalized   bool
	strTable    map[string]uint
	buf         []byte
}

// NewDecoder returns a decoder with default flags
func NewMerger() *Merger {
	m := Merger{
		version:  2,
		strTable: make(map[string]uint),
		buf:      make([]byte, headerSize, 32),
	}

	binary.LittleEndian.PutUint32(m.buf[:4], magicHeaderBytes) // fill magic
	m.buf[4] = byte(m.version)                                 // fill version
	m.bodyOffset = len(m.buf)                                  // remember body offset

	m.buf = append(m.buf, 0) // no header

	// TODO make use select top level element
	m.buf = append(m.buf, typeREFN)
	m.buf = append(m.buf, typeARRAY)
	//m.buf = append(m.buf, typeARRAYREF_0 + 2)

	m.numOffset = len(m.buf)
	// padding bytes for length
	for i := 0; i < 8; i++ {
		m.buf = append(m.buf, typePAD)
	}

	return &m
}

func (m *Merger) Append(b []byte) (err error) {
	header, err := readHeader(b)
	if err != nil {
		return err
	}

	startOffset := len(m.buf)
	idx := headerSize + header.suffixSize

	// TODO check version
	// TODO handle compression
	// TODO parse header, why?

	for idx < len(b) && err == nil {
		idx, err = m.mergeItem(idx, b)
	}

	if err == nil {
		m.numElements++
	} else {
		m.buf = m.buf[0:startOffset]
	}

	return err
}

func (m *Merger) Finish() []byte {
	if !m.finalized {
		var numElementsVarInt []uint8
		numElementsVarInt = appendVarint(numElementsVarInt, uint(m.numElements))

		// TODO len(numElementsVarInt) <= 8
		copy(m.buf[m.numOffset:], numElementsVarInt)
		m.finalized = true
	}

	return m.buf
}

func (m *Merger) mergeItem(idx int, b []byte) (int, error) {
	if idx < 0 || idx > len(b) {
		return 0, errors.New("invalid index")
	}

	if m.finalized {
		return 0, errors.New("finalized document")
	}

	var err error
	tag := b[idx]
	//trackme := (tag & trackFlag) == trackFlag
	tag &^= trackFlag

	switch {
	case tag < typeVARINT,
		tag == typeTRUE, tag == typeFALSE,
		tag == typePAD, tag == typeREFN,
		tag == typeUNDEF, tag == typeCANONICAL_UNDEF:
		m.buf = append(m.buf, b[idx])
		idx++

	case tag == typeVARINT, tag == typeZIGZAG:
		_, sz := varintdecode(b[idx+1:])
		m.buf = append(m.buf, b[idx:idx+sz+1]...)
		idx += sz + 1

	case tag == typeFLOAT:
		m.buf = append(m.buf, b[idx:idx+5]...)
		idx += 5 // 4 bytes + tag

	case tag == typeDOUBLE:
		m.buf = append(m.buf, b[idx:idx+9]...)
		idx += 9 // 8 bytes + tag

	case tag == typeLONG_DOUBLE:
		m.buf = append(m.buf, b[idx:idx+17]...)
		idx += 17 // 16 bytes + tag

	case tag == typeBINARY, tag == typeSTR_UTF8:
		ln, sz := varintdecode(b[idx+1:])
		if ln < 0 || ln > math.MaxInt32 {
			return 0, errors.New("bad size for string")
		}

		endIdx := idx + sz + ln + 1
		if endIdx > len(b) {
			return 0, errors.New("truncated document")
		}

		val := string(b[idx+sz+1 : endIdx])
		savedOffset, ok := m.strTable[val]

		if !ok {
			m.strTable[val] = uint(len(m.buf) - m.bodyOffset)
			m.buf = append(m.buf, b[idx:endIdx]...)
		} else {
			m.buf = append(m.buf, typeCOPY)
			m.buf = appendVarint(m.buf, savedOffset)
		}

		idx = endIdx

	case tag == typeARRAY:
		ln, sz := varintdecode(b[idx+1:])
		m.buf = append(m.buf, b[idx:idx+sz+1]...)
		idx += sz + 1

		for i := 0; i < ln; i++ {
			idx, err = m.mergeItem(idx, b)
			if err != nil {
				return 0, err
			}
		}

	case tag == typeHASH:
		ln, sz := varintdecode(b[idx+1:])
		m.buf = append(m.buf, b[idx:idx+sz+1]...)
		idx += sz + 1

		// merge keys and values
		for i := 0; i < ln*2; i++ {
			idx, err = m.mergeItem(idx, b)
			if err != nil {
				return 0, err
			}
		}

		//    case tag == typeCOPY:
		//        offset, sz := varintdecode(b[idx+1:])
		//        m.buf = append(m.buf, b[idx:idx + sz + 1]...)
		//        idx += sz + 1
		//
		//        if !isStringish(b, offset) {
		//            return 0, errors.New("Expect stringish at typeCOPY offset")
		//        }
		//        //val := string(b[idx+sz+1 : endIdx])

	case tag >= typeSHORT_BINARY_0 && tag < typeSHORT_BINARY_0+32:
		ln := int(tag & 0x1F) // get length from tag
		endIdx := idx + ln + 1

		if endIdx > len(b) {
			return 0, errors.New("truncated document")
		}

		m.buf = append(m.buf, b[idx:endIdx]...)
		idx += endIdx

	default:
		return 0, errors.New("unknown tag byte: " + strconv.Itoa(int(tag)))
	}

	return idx, nil
}

func appendVarint(by []byte, n uint) []uint8 {
	for n >= 0x80 {
		b := byte(n) | 0x80
		by = append(by, b)
		n >>= 7
	}

	return append(by, byte(n))
}
