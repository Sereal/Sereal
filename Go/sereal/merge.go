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

// use latest version
func NewMerger() *Merger {
	return &Merger{
		TopLevelElement: TopLevelArrayRef,
	}
}

func NewMergerV2() *Merger {
	return &Merger{
		version:         2,
		TopLevelElement: TopLevelArrayRef,
	}
}

func NewMergerV3() *Merger {
	return &Merger{
		version:         3,
		TopLevelElement: TopLevelArrayRef,
	}
}

func (m *Merger) initMerger() error {
	if m.inited {
		return nil
	}

	m.strTable = make(map[string]int)
	m.buf = make([]byte, headerSize, 32)

	if m.version == 0 {
		m.version = ProtocolVersion
	}

	switch {
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

func (m *Merger) Append(b []byte) error {
	if err := m.initMerger(); err != nil {
		return err
	}

	if m.finished {
		return errors.New("finished document")
	}

	docHeader, err := readHeader(b)
	if err != nil {
		return err
	}

	doc := mergerDoc{
		buf:        b[headerSize+docHeader.suffixSize:],
		version:    int(docHeader.version),
		startIdx:   0,
		bodyOffset: -1, // 1-based offsets
	}

	var decomp decompressor
	switch docHeader.doctype {
	case serealRaw:
		// nothing

	case serealSnappy:
		if doc.version != 1 {
			return errors.New("snappy compression only valid for v1 documents")
		}

		decomp = SnappyCompressor{Incremental: false}

	case serealSnappyIncremental:
		decomp = SnappyCompressor{Incremental: true}

	case serealZlib:
		if doc.version < 3 {
			return errors.New("zlib compression only valid for v3 documents and up")
		}

		decomp = ZlibCompressor{}

	default:
		return fmt.Errorf("document type '%d' not yet supported", docHeader.doctype)
	}

	/* XXX instead of creating an uncompressed copy of the document,
	 *     it would be more flexible to use a sort of "Reader" interface */
	if decomp != nil {
		if doc.buf, err = decomp.decompress(doc.buf); err != nil {
			return err
		}
	}

	lastElementOffset := len(m.buf)

	// first pass: build table of tracked tags
	if err := m.buildTrackTable(&doc); err != nil {
		return err
	}

	// preallocate memory
	m.buf = append(m.buf, doc.buf...)
	//doc.buf = m.buf[lastElementOffset:]
	m.buf = m.buf[:lastElementOffset]

	// second pass: do the work
	if err := m.mergeItems(&doc); err != nil {
		m.buf = m.buf[0:lastElementOffset] // remove appended stuff
		return err
	}

	return nil
}

func (m *Merger) Finish() ([]byte, error) {
	if err := m.initMerger(); err != nil {
		return m.buf, err
	}

	if !m.finished {
		lengthVarInt := make([]byte, 8, 8)
		sz := copyVarint(lengthVarInt, 0, uint(m.length))
		copy(m.buf[m.lenOffset:], lengthVarInt[:sz])
		m.finished = true
	}

	return m.buf, nil
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
			tag &^= trackFlag
		}

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

func (m *Merger) mergeItems(doc *mergerDoc) error {
	if cap(m.buf)-len(m.buf) < len(doc.buf) {
		return errors.New("Buffer is not long enough, preallocation didn't work!")
	}

	var err error
	midx := len(m.buf)
	mbuf := m.buf[:cap(m.buf)-1]

	dbuf := doc.buf
	didx := doc.startIdx

	stack := make([]int, 1, 16) // 16 nested levels
	stack[0] = -1

	for didx < len(dbuf) && err == nil {
		tag := dbuf[didx]
		tag &^= trackFlag

		docRelativeIdx := didx - doc.bodyOffset
		mrgRelativeIdx := midx - m.bodyOffset
		trackme := len(doc.trackIdxs) > 0 && doc.trackIdxs[0] == docRelativeIdx

		level := len(stack) - 1
		for stack[level] == 0 {
			stack = stack[:level]
			level--
		}

		//fmt.Printf("%x (%x) at %d (%d)\n", tag, dbuf[didx], didx, didx-doc.bodyOffset)
		//fmt.Printf("level: %d, value: %d len: %d\n", level, stack[level], len(stack))
		//fmt.Println("------")

		switch {
		case tag < typeVARINT, tag == typeUNDEF, tag == typeCANONICAL_UNDEF, tag == typeTRUE, tag == typeFALSE:
			mbuf[midx] = dbuf[didx]
			didx++
			midx++
			stack[level]--

		case tag == typePAD, tag == typeREFN, tag == typeWEAKEN, tag == typePACKET_START, tag == typeEXTEND:
			mbuf[midx] = dbuf[didx]
			didx++
			midx++

		case tag == typeVARINT, tag == typeZIGZAG:
			_, sz := varintdecode(dbuf[didx+1:])
			copy(mbuf[midx:], dbuf[didx:didx+sz+1])
			didx += sz + 1
			midx += sz + 1
			stack[level]--

		case tag == typeFLOAT:
			copy(mbuf[midx:], dbuf[didx:didx+5])
			didx += 5
			midx += 5 // 4 bytes + tag
			stack[level]--

		case tag == typeDOUBLE:
			copy(mbuf[midx:], dbuf[didx:didx+9])
			didx += 9
			midx += 9 // 8 bytes + tag
			stack[level]--

		case tag == typeLONG_DOUBLE:
			copy(mbuf[midx:], dbuf[didx:didx+17])
			didx += 17
			midx += 17 // 16 bytes + tag
			stack[level]--

		case tag == typeBINARY, tag == typeSTR_UTF8:
			ln, sz := varintdecode(dbuf[didx+1:])
			length := sz + ln + 1

			if ln < 0 {
				err = errors.New("bad size for string")
				break
			}

			if didx+length > len(dbuf) {
				err = errors.New("truncated document")
				break
			}

			val := dbuf[didx+sz+1 : didx+length]
			if savedOffset, ok := m.strTable[string(val)]; ok {
				mbuf[midx] = typeCOPY
				midx += 1 + copyVarint(mbuf, midx+1, uint(savedOffset))
				mrgRelativeIdx = savedOffset
			} else {
				m.strTable[string(val)] = mrgRelativeIdx
				copy(mbuf[midx:], dbuf[didx:didx+length])
				midx += length
			}

			stack[level]--
			didx += length

		case tag >= typeSHORT_BINARY_0 && tag < typeSHORT_BINARY_0+32:
			ln := int(tag & 0x1F) // get length from tag
			length := ln + 1

			if didx+length > len(dbuf) {
				err = errors.New("truncated document")
				break
			}

			val := dbuf[didx+1 : didx+length]
			if savedOffset, ok := m.strTable[string(val)]; ok {
				mbuf[midx] = typeCOPY
				midx += 1 + copyVarint(mbuf, midx+1, uint(savedOffset))
				mrgRelativeIdx = savedOffset
			} else {
				m.strTable[string(val)] = mrgRelativeIdx
				copy(mbuf[midx:], dbuf[didx:didx+length])
				midx += length
			}

			stack[level]--
			didx += length

		case tag == typeCOPY, tag == typeREFP:
			offset, sz := varintdecode(dbuf[didx+1:])
			targetOffset, ok := doc.trackTable[offset]

			if !ok || targetOffset < 0 {
				err = errors.New("bad target offset at COPY or REFP tag")
				break
			}

			mbuf[midx] = dbuf[didx]
			midx += 1 + copyVarint(mbuf, midx+1, uint(targetOffset))
			didx += sz + 1
			stack[level]--

		case tag == typeARRAY, tag == typeHASH:
			ln, sz := varintdecode(dbuf[didx+1:])
			if ln < 0 {
				err = errors.New("bad array or hash length")
				break
			}

			copy(mbuf[midx:], dbuf[didx:didx+sz+1])
			didx += sz + 1
			midx += sz + 1

			if tag == typeHASH {
				ln *= 2
			}

			stack[level]--
			stack = append(stack, ln)

		case (tag >= typeARRAYREF_0 && tag < typeARRAYREF_0+16) || (tag >= typeHASHREF_0 && tag < typeHASHREF_0+16):
			mbuf[midx] = dbuf[didx]
			didx++
			midx++

			// for hash read 2*ln items
			ln := int(tag & 0xF)
			if tag >= typeHASHREF_0 {
				ln *= 2
			}

			stack[level]--
			stack = append(stack, ln)

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
			doc.trackIdxs = doc.trackIdxs[1:]
		}
	}

	level := len(stack) - 1
	for stack[level] == 0 {
		stack = stack[:level]
		level--
	}

	if len(stack) > 1 {
		return errors.New("Failed to append invalid Sereal document")
	}

	m.length += -(stack[0] + 1)
	m.buf = mbuf[:midx]
	return err
}

func copyVarint(b []byte, idx int, n uint) int {
	oidx := idx
	for n >= 0x80 {
		b[idx] = byte(n) | 0x80
		n >>= 7
		idx++
	}

	b[idx] = byte(n)
	return idx - oidx + 1
}

//func appendVarint(by []byte, n uint) []uint8 {
//	for n >= 0x80 {
//		b := byte(n) | 0x80
//		by = append(by, b)
//		n >>= 7
//	}
//
//	return append(by, byte(n))
//}
