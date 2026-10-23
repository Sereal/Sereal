package sereal

// ProtocolVersion is the protocol version a zero-value Encoder emits (and the
// version NewMerger produces). The decoder accepts every version up to
// MaxProtocolVersion.
const ProtocolVersion = 3

// MaxProtocolVersion is the highest protocol version the sereal package can
// decode and encode (see NewEncoderV6).
const MaxProtocolVersion = 6

// magicHeadrBytes is a magic string for header. Every packet in protocol
// version 1 and 2 starts with this.
const magicHeaderBytes = uint32(0x6c72733d) // "=srl"

// magicHeaderBytesHighBit is a new magic string for header used in protocol
// version 3 and up, with high bit set for UTF8 sanity check. It is an error to
// use a new magic header on a v1 or v2 packet, and it is an error to use the
// old magic header in v3 or later.
const magicHeaderBytesHighBit = uint32(0x6c72f33d) // "=\xF3rl"

// magicHeaderBytesHighBitUTF8 is a magic string for header v3, corrupted by
// accidental UTF8 encoding. It makes it easy to detect when a Sereal document
// has been accidentally UTF-8 encoded because the \xF3 is translated to
// \xC3\xB3.
const magicHeaderBytesHighBitUTF8 = uint32(0x72b3c33d) // "=\xC3\xB3r"

const headerSize = 5 // 4 magic + 1 version-type

// checksumFlag is bit 7 of the version-type byte: when set (protocol version 6
// and later) a keyed checksum envelope follows the header suffix.
const checksumFlag = byte(0x80)

// doctypeMask extracts the base document type from the high nibble of the
// version-type byte once it has been shifted down by 4.
const doctypeMask = byte(0x07)

type documentType int

const (
	serealRaw documentType = iota
	serealSnappy
	serealSnappyIncremental
	serealZlib
	serealZstd
)

const trackFlag = byte(0x80)

const (
	typeVARINT          = 0x20
	typeZIGZAG          = 0x21
	typeFLOAT           = 0x22
	typeDOUBLE          = 0x23
	typeLONG_DOUBLE     = 0x24
	typeUNDEF           = 0x25
	typeBINARY          = 0x26
	typeSTR_UTF8        = 0x27
	typeREFN            = 0x28
	typeREFP            = 0x29
	typeHASH            = 0x2a
	typeARRAY           = 0x2b
	typeOBJECT          = 0x2c
	typeOBJECTV         = 0x2d
	typeALIAS           = 0x2e
	typeCOPY            = 0x2f
	typeWEAKEN          = 0x30
	typeREGEXP          = 0x31
	typeOBJECT_FREEZE   = 0x32
	typeOBJECTV_FREEZE  = 0x33
	typeNO              = 0x34
	typeYES             = 0x35
	typeFLOAT128        = 0x38
	typeCANONICAL_UNDEF = 0x39
	typeFALSE           = 0x3a
	typeTRUE            = 0x3b
	typeNEG_VARINT      = 0x3c
	typePACKET_START    = 0x3d
	typeEXTEND          = 0x3e
	typePAD             = 0x3f
	typeARRAYREF_0      = 0x40
	typeHASHREF_0       = 0x50
	typeSHORT_BINARY_0  = 0x60
)

// ids following the EXTEND tag (protocol version 6)
const (
	extUNDEF_DEADLY   = 0xff // diagnostic undef; compliant encoders must not emit this tag
	extVSTRING_BINARY = 0x26 // v-string, no component above 127
	extVSTRING_UTF8   = 0x27 // v-string with a component above 127
)

// checksum types of the protocol version 6 keyed checksum envelope
const (
	checksumSipHash24     = 0 // SipHash-2-4, 64-bit tag
	checksumSipHash24_128 = 1 // SipHash-2-4-128, 128-bit tag
	checksumSipHash36_128 = 2 // SipHash-3-6-128, 128-bit tag
)
