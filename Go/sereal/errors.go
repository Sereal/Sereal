package sereal

import "errors"

// Errors
var (
	ErrBadHeaderUTF8 = errors.New("bad header: it seems your document was accidentally UTF-8 encoded")
	ErrBadHeader     = errors.New("bad header: not a valid Sereal document")
	ErrBadSnappy     = errors.New("snappy compression only valid for v1 documents")
	ErrBadZlibV3     = errors.New("zlib compression only valid for v3 documents and up")
	ErrBadZstdV4     = errors.New("zstd compression only valid for v4 documents and up")

	ErrHeaderPointer = errors.New("expected pointer for header")
	ErrBodyPointer   = errors.New("expected pointer for body")

	ErrTruncated  = errors.New("truncated document")
	ErrUnknownTag = errors.New("unknown tag byte")

	ErrTooLarge = errors.New("sereal: document too large to be compressed with snappy")

	ErrMaxRecursionLimit = errors.New("sereal: recursion limit exceeded")

	ErrMaxUncompressedSize = errors.New("sereal: uncompressed document exceeds the maximum size")

	// ErrDecodePanic wraps a panic recovered while decoding, merging or
	// inspecting a document
	ErrDecodePanic = errors.New("sereal: panic during decode")

	// protocol version 6 checksum envelope
	ErrChecksumVersion       = errors.New("sereal: checksummed documents require protocol version 6 or later")
	ErrChecksumRequired      = errors.New("sereal: checksum policy require_key requires a checksummed document")
	ErrChecksumKeyRequired   = errors.New("sereal: checksum policy require_key requires at least one checksum key")
	ErrChecksumKeyMissing    = errors.New("sereal: checksummed document requires a key for its checksum type")
	ErrChecksumUnknownType   = errors.New("sereal: unsupported checksum type")
	ErrChecksumMismatch      = errors.New("sereal: checksum verification failed")
	ErrChecksumPayloadLength = errors.New("sereal: checksummed payload length mismatch")
	ErrChecksummedDocument   = errors.New("sereal: checksummed documents are not supported by this operation")
	ErrChecksumMultipleKeys  = errors.New("sereal: specify only one checksum key on the encoder")
	ErrChecksumKeyVersion    = errors.New("sereal: checksum keys require protocol version 6 or later")

	// protocol version 6 body tags
	ErrVStringVersion = errors.New("sereal: v-strings require protocol version 6 or later")
	ErrBadVString     = errors.New("sereal: invalid v-string spelling")
	ErrDeadlyTag      = errors.New("sereal: encountered DEADLY extended tag")
)

// ErrCorrupt is returned if the sereal document was corrupt
type ErrCorrupt struct{ Err string }

// internal constants used for corrupt
var (
	errBadSliceSize         = "bad size for slice"
	errBadStringSize        = "bad size for string"
	errBadOffset            = "bad offset"
	errUntrackedOffsetREFP  = "untracked offset for REFP"
	errBadHashSize          = "bad size for hash"
	errUntrackedOffsetAlias = "untracked offset for alias"
	errNestedCOPY           = "bad nested copy tag"
	errBadVarint            = "bad varint"
	errBadUncompressedSize  = "bad uncompressed size"
	errFreezeNotRefnArray   = "OBJECT_FREEZE value not REFN+ARRAY"
	errFreezeNotArray       = "OBJECT_FREEZE value not an array"
	errFreezeMultipleElts   = "OBJECT_FREEZE array contains multiple elements"
	errFreezeNotByteSlice   = "OBJECT_FREEZE array not []byte"
	errBadChecksumLength    = "bad checksum length"
	errBadPayloadLength     = "bad checksummed payload length"
	errBadNegVarint         = "bad NEG_VARINT magnitude"
	errNegVarintVersion     = "NEG_VARINT tag requires protocol version 6"
	errExtendVersion        = "EXTEND tag requires protocol version 6"
	errUnknownExtend        = "unknown extended tag id"
	errBadVString           = "bad v-string spelling"
	errVStringEncoding      = "VSTRING extension id does not match the spelling"
)

func (c ErrCorrupt) Error() string { return "sereal: corrupt document:" + c.Err }
