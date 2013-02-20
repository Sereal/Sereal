package sereal

const Magic = uint32(0x6c72733d)

type VersionType byte

const (
	VersionRaw VersionType = iota
	VersionSnappy
	VersionSnappyLength
)

type TypeTag byte

const TrackFlag = byte(0x80)

const (
	TypeVARINT         = 0x20
	TypeZIGZAG         = 0x21
	TypeFLOAT          = 0x22
	TypeDOUBLE         = 0x23
	TypeLONG_DOUBLE    = 0x24
	TypeUNDEF          = 0x25
	TypeBINARY         = 0x26
	TypeSTR_UTF8       = 0x27
	TypeREFN           = 0x28
	TypeREFP           = 0x29
	TypeHASH           = 0x2a
	TypeARRAY          = 0x2b
	TypeOBJECT         = 0x2c
	TypeOBJECTV        = 0x2d
	TypeALIAS          = 0x2e
	TypeCOPY           = 0x2f
	TypeWEAKEN         = 0x30
	TypeREGEXP         = 0x31
	TypeFALSE          = 0x3a
	TypeTRUE           = 0x3b
	TypeMANY           = 0x3c
	TypePACKET_START   = 0x3d
	TypeEXTEND         = 0x3e
	TypePAD            = 0x3f
	TypeARRAYREF_0     = 0x40
	TypeHASHREF_0      = 0x50
	TypeSHORT_BINARY_0 = 0x60
)

const SnappyThreshold = 1024
