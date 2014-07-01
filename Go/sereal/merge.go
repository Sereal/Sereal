package sereal

import (
	"encoding/binary"
	"errors"
	"fmt"
	"sort"
	"strconv"
)

type topLevelElementType int

const (
	TopLevelArray topLevelElementType = iota
	TopLevelArrayRef
	// TopLevelHash
	// TopLevelHashRef
)

const reservedBytesForLength = 8

type Merger struct {
	version    int
	length     int
	lenOffset  int
	bodyOffset int // 1-based
	inited     bool
	finished   bool
	strTable   map[string]int
	buf        []byte

	// public arguments
	TopLevelElement topLevelElementType
	// ProtocolVersion
	// DedupeStrings
	// Compress
}

type mergerDoc struct {
	buf        []byte
	version    int
	startIdx   int // 0-based
	bodyOffset int // 1-based
	headerLen  int
	trackTable map[int]int
	trackIdxs  []int
}

// NewDecoder returns a decoder with default flags
func NewMerger() *Merger {
	m := Merger{
		version:         0,
		TopLevelElement: TopLevelArrayRef,
	}

	return &m
}

func (m *Merger) initMerger() error {
	if m.inited {
		return nil
	}

	m.strTable = make(map[string]int)
	m.buf = make([]byte, headerSize, 32)

	switch {
	case m.version == 0:
		m.version = ProtocolVersion
	case m.version > ProtocolVersion:
		return fmt.Errorf("protocol version '%v' not yet supported", m.version)
	case m.version < 3:
		binary.LittleEndian.PutUint32(m.buf[:4], magicHeaderBytes)
	default:
		binary.LittleEndian.PutUint32(m.buf[:4], magicHeaderBytesHighBit)
	}

	m.buf[4] = byte(m.version)    // fill version
	m.buf = append(m.buf, 0)      // no header
	m.bodyOffset = len(m.buf) - 1 // remember body offset

	switch m.TopLevelElement {
	case TopLevelArray:
		m.buf = append(m.buf, typeARRAY)
	case TopLevelArrayRef:
		m.buf = append(m.buf, typeREFN, typeARRAY)
	}

	// remember len offset + pad bytes for length
	m.lenOffset = len(m.buf)
	for i := 0; i < reservedBytesForLength; i++ {
		m.buf = append(m.buf, typePAD)
	}

	m.inited = true
	return nil
}

func (m *Merger) Append(b []byte) (err error) {
	if err := m.initMerger(); err != nil {
		return err
	}

	if m.finished {
		return errors.New("finished document")
	}

	// TODO check version
	// TODO handle compression
	// TODO parse header, why?

	header, err := readHeader(b)
	if err != nil {
		return err
	}

	doc := mergerDoc{
		buf:        b,
		version:    2,
		startIdx:   headerSize + header.suffixSize,
		bodyOffset: headerSize + header.suffixSize - 1,
	}

	lastElementOffset := len(m.buf)

	// first pass: build table of tracked tags
	if err := m.buildTrackTable(&doc); err != nil {
		return err
	}

	// second pass: do the work
	startIdx := doc.startIdx
	for startIdx < len(b) && err == nil {
		startIdx, err = m.mergeItem(startIdx, &doc)
	}

	if err != nil {
		// remove current element
		m.buf = m.buf[0:lastElementOffset]
	} else {
		m.length++
	}

	return err
}

func (m *Merger) Finish() ([]byte, error) {
	err := m.initMerger()
	if err != nil {
		return m.buf, err
	}

	if !m.finished {
		var lengthVarInt []uint8
		lengthVarInt = appendVarint(lengthVarInt, uint(m.length))
		copy(m.buf[m.lenOffset:], lengthVarInt)
		m.finished = true
	}

	return m.buf, err
}

func (m *Merger) buildTrackTable(doc *mergerDoc) error {
	buf := doc.buf
	idx := doc.startIdx
	if idx < 0 || idx > len(buf) {
		return errors.New("invalid index")
	}

	var err error
	doc.trackTable = make(map[int]int)
	doc.trackIdxs = make([]int, 0)

	for idx < len(buf) && err == nil {
		tag := buf[idx]

		if (tag & trackFlag) == trackFlag {
			doc.trackTable[idx-doc.bodyOffset] = -1
		}

		tag &^= trackFlag
		//fmt.Printf("%x (%x) at %d (%d)\n", tag, buf[idx], idx, idx - doc.bodyOffset)

		switch {
		case tag < typeVARINT,
			tag == typePAD, tag == typeREFN, tag == typeWEAKEN,
			tag == typeUNDEF, tag == typeCANONICAL_UNDEF,
			tag == typeTRUE, tag == typeFALSE,
			tag == typePACKET_START, tag == typeEXTEND:
			idx++

		case tag == typeVARINT, tag == typeZIGZAG:
			_, sz := varintdecode(buf[idx+1:])
			idx += sz + 1

		case tag == typeFLOAT:
			idx += 5 // 4 bytes + tag

		case tag == typeDOUBLE:
			idx += 9 // 8 bytes + tag

		case tag == typeLONG_DOUBLE:
			idx += 17 // 16 bytes + tag

		case tag == typeBINARY, tag == typeSTR_UTF8:
			ln, sz := varintdecode(buf[idx+1:])
			idx += sz + ln + 1

			if ln < 0 {
				err = errors.New("bad size for string or binary")
				break
			}

			if idx > len(buf) {
				err = errors.New("truncated document")
				break
			}

		case tag == typeARRAY, tag == typeHASH:
			_, sz := varintdecode(buf[idx+1:])
			idx += sz + 1

		case tag == typeCOPY, tag == typeALIAS, tag == typeREFP:
			offset, sz := varintdecode(buf[idx+1:])
			if offset < 0 || offset >= idx {
				err = errors.New("bad offset")
				break
			}

			doc.trackTable[offset] = -1
			idx += sz + 1

		//case tag == typeOBJECT:
		//case tag == typeOBJECTV:
		//case tag == typeREGEXP:
		//case tag == typeOBJECT_FREEZE:
		//case tag == typeOBJECTV_FREEZE:
		//case tag == typeMANY:

		case tag >= typeARRAYREF_0 && tag < typeARRAYREF_0+16:
			idx++

		case tag >= typeHASHREF_0 && tag < typeHASHREF_0+16:
			idx++

		case tag >= typeSHORT_BINARY_0 && tag < typeSHORT_BINARY_0+32:
			idx += 1 + int(tag&0x1F)

		default:
			err = errors.New("unknown tag byte: " + strconv.Itoa(int(tag)) + " at offset " + strconv.Itoa(idx))
		}
	}

	for idx, _ := range doc.trackTable {
		doc.trackIdxs = append(doc.trackIdxs, idx)
	}

	sort.Ints(doc.trackIdxs)
	return err
}

func (m *Merger) mergeItem(idx int, doc *mergerDoc) (int, error) {
	buf := doc.buf
	if idx < 0 || idx > len(buf) {
		return 0, errors.New("invalid index")
	}

	var err error
	tag := buf[idx]
	tag &^= trackFlag

	docRelativeIdx := idx - doc.bodyOffset
	mrgRelativeIdx := len(m.buf) - m.bodyOffset
	trackme := len(doc.trackIdxs) > 0 && doc.trackIdxs[0] == docRelativeIdx

	if trackme {
		doc.trackIdxs = doc.trackIdxs[1:]
	}

	switch {
	case tag < typeVARINT,
		tag == typePAD, tag == typeREFN, tag == typeWEAKEN,
		tag == typeUNDEF, tag == typeCANONICAL_UNDEF,
		tag == typeTRUE, tag == typeFALSE,
		tag == typePACKET_START, tag == typeEXTEND:
		m.buf = append(m.buf, buf[idx])
		idx++

	case tag == typeVARINT, tag == typeZIGZAG:
		_, sz := varintdecode(buf[idx+1:])
		m.buf = append(m.buf, buf[idx:idx+sz+1]...)
		idx += sz + 1

	case tag == typeFLOAT:
		m.buf = append(m.buf, buf[idx:idx+5]...)
		idx += 5 // 4 bytes + tag

	case tag == typeDOUBLE:
		m.buf = append(m.buf, buf[idx:idx+9]...)
		idx += 9 // 8 bytes + tag

	case tag == typeLONG_DOUBLE:
		m.buf = append(m.buf, buf[idx:idx+17]...)
		idx += 17 // 16 bytes + tag

	case tag == typeBINARY, tag == typeSTR_UTF8:
		ln, sz := varintdecode(buf[idx+1:])
		endIdx := idx + sz + ln + 1

		if ln < 0 {
			err = errors.New("bad size for string")
			break
		}

		if endIdx > len(buf) {
			err = errors.New("truncated document")
			break
		}

		val := string(buf[idx+sz+1 : endIdx])
		if savedOffset, ok := m.strTable[val]; ok {
			mrgRelativeIdx = savedOffset
			m.buf = append(m.buf, typeCOPY)
			m.buf = appendVarint(m.buf, uint(savedOffset))
		} else {
			m.strTable[val] = mrgRelativeIdx
			m.buf = append(m.buf, buf[idx:endIdx]...)
		}

		idx = endIdx

	case tag >= typeSHORT_BINARY_0 && tag < typeSHORT_BINARY_0+32:
		ln := int(tag & 0x1F) // get length from tag
		endIdx := idx + ln + 1

		if endIdx > len(buf) {
			err = errors.New("truncated document")
			break
		}

		val := string(buf[idx+1 : endIdx])
		if savedOffset, ok := m.strTable[val]; ok {
			mrgRelativeIdx = savedOffset
			m.buf = append(m.buf, typeCOPY)
			m.buf = appendVarint(m.buf, uint(savedOffset))
		} else {
			m.strTable[val] = mrgRelativeIdx
			m.buf = append(m.buf, buf[idx:endIdx]...)
		}

		idx = endIdx

	case tag == typeCOPY, tag == typeREFP:
		offset, sz := varintdecode(buf[idx+1:])
		targetOffset, ok := doc.trackTable[offset]

		if !ok || targetOffset < 0 {
			err = errors.New("bad target offset at COPY or REFP tag")
			break
		}

		m.buf = append(m.buf, buf[idx])
		m.buf = appendVarint(m.buf, uint(targetOffset))
		idx += sz + 1

	case tag == typeARRAY, tag == typeHASH:
		ln, sz := varintdecode(buf[idx+1:])
		if ln < 0 {
			err = errors.New("bad array or hash length")
			break
		}

		m.buf = append(m.buf, buf[idx:idx+sz+1]...)
		idx += sz + 1

		if tag == typeHASH {
			ln *= 2
		}

		for i := 0; i < ln && err == nil; i++ {
			idx, err = m.mergeItem(idx, doc)
		}

	case (tag >= typeARRAYREF_0 && tag < typeARRAYREF_0+16) || (tag >= typeHASHREF_0 && tag < typeHASHREF_0+16):
		m.buf = append(m.buf, buf[idx])
		idx++

		// for hash read 2*ln items
		ln := int(tag & 0xF)
		if tag >= typeHASHREF_0 {
			ln *= 2
		}

		for i := 0; i < ln && err == nil; i++ {
			idx, err = m.mergeItem(idx, doc)
		}

		//case tag == typeOBJECT:
		//case tag == typeOBJECTV:
		//case tag == typeREGEXP:
		//case tag == typeOBJECT_FREEZE:
		//case tag == typeOBJECTV_FREEZE:
		//case tag == typeMANY:
		//case tag == typeALIAS

	default:
		err = errors.New("unknown tag byte: " + strconv.Itoa(int(tag)))
	}

	if trackme {
		// if tag is tracked, remember its offset
		doc.trackTable[docRelativeIdx] = mrgRelativeIdx
	}

	return idx, err
}

func appendVarint(by []byte, n uint) []uint8 {
	for n >= 0x80 {
		b := byte(n) | 0x80
		by = append(by, b)
		n >>= 7
	}

	return append(by, byte(n))
}
