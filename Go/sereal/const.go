package sereal

const magicHeaderBytes = uint32(0x6c72733d)

type versionType byte

const (
	versionRaw versionType = iota
	versionSnappy
	versionSnappyLength
)

type typeTag byte

const trackFlag = byte(0x80)

const (
	typeVARINT         = 0x20
	typeZIGZAG         = 0x21
	typeFLOAT          = 0x22
	typeDOUBLE         = 0x23
	typeLONG_DOUBLE    = 0x24
	typeUNDEF          = 0x25
	typeBINARY         = 0x26
	typeSTR_UTF8       = 0x27
	typeREFN           = 0x28
	typeREFP           = 0x29
	typeHASH           = 0x2a
	typeARRAY          = 0x2b
	typeOBJECT         = 0x2c
	typeOBJECTV        = 0x2d
	typeALIAS          = 0x2e
	typeCOPY           = 0x2f
	typeWEAKEN         = 0x30
	typeREGEXP         = 0x31
	typeFALSE          = 0x3a
	typeTRUE           = 0x3b
	typeMANY           = 0x3c
	typePACKET_START   = 0x3d
	typeEXTEND         = 0x3e
	typePAD            = 0x3f
	typeARRAYREF_0     = 0x40
	typeHASHREF_0      = 0x50
	typeSHORT_BINARY_0 = 0x60
)

const snappyThreshold = 1024
