package sereal

import "errors"

// Errors
var (
	ErrBadHeaderUTF8 = errors.New("bad header: it seems your document was accidentally UTF-8 encoded")
	ErrBadHeader     = errors.New("bad header: not a valid Sereal document")
	ErrBadSnappy     = errors.New("snappy compression only valid for v1 documents")
	ErrBadZlibV3     = errors.New("zlib compression only valid for v3 documents and up")

	ErrHeaderPointer = errors.New("expected pointer for header")
	ErrBodyPointer   = errors.New("expected pointer for body")

	ErrTruncated  = errors.New("truncated document")
	ErrUnknownTag = errors.New("unknown tag byte")
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
	errStringish            = "expected stringish for classname"
	errUntrackedOffsetAlias = "untracked offset for alias"
	errNestedCOPY           = "bad nested copy tag"
)

func (c ErrCorrupt) Error() string { return "sereal: corrupt document" }
