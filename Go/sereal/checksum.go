package sereal

import (
	"crypto/subtle"
	"encoding/binary"
	"fmt"
)

// Protocol version 6 keyed checksum envelope.
//
// A document whose version-type byte has bit 7 set carries an envelope
// between the header suffix and the (possibly compressed) payload:
//
//	<CHECKSUM-TYPE u8> <CHECKSUM-LENGTH varint> <CHECKSUM bytes> <PAYLOAD-LENGTH varint> <PAYLOAD>
//
// The checksum is a keyed SipHash over every byte of the document from the
// magic up to the payload end, except the checksum bytes themselves. See the
// "Keyed checksum envelope" section of sereal_spec.pod.

// ChecksumPolicy selects how a Decoder treats checksummed documents
type ChecksumPolicy int

const (
	// ChecksumVerifyIfAvailable verifies a checksummed document when a key
	// for its checksum type is configured; otherwise it warns and decodes the
	// document unauthenticated. Unsigned documents are accepted. This is the
	// default.
	ChecksumVerifyIfAvailable ChecksumPolicy = iota
	// ChecksumDie verifies every checksummed document and fails when no key
	// for its checksum type is configured. Unsigned documents are accepted.
	ChecksumDie
	// ChecksumRequireKey behaves like ChecksumDie and also rejects unsigned
	// documents. At least one checksum key must be configured.
	ChecksumRequireKey
	// ChecksumIgnore skips checksum verification; the envelope framing is
	// still validated.
	ChecksumIgnore
	// ChecksumIgnoreWarn behaves like ChecksumIgnore and warns for every
	// checksummed document.
	ChecksumIgnoreWarn
)

// warnings passed to Decoder.ChecksumWarningHandler
const (
	checksumWarnNoKey   = "decoding checksummed document without authentication: no matching checksum key is configured"
	checksumWarnIgnored = "ignoring checksum authentication by request"
)

// checksumTagLength returns the tag length of a known checksum type, or 0
func checksumTagLength(typ byte) int {
	switch typ {
	case checksumSipHash24:
		return 8
	case checksumSipHash24_128, checksumSipHash36_128:
		return 16
	}
	return 0
}

// PrepareChecksumKey derives a 16-byte checksum key from an arbitrary key and
// an optional second key, exactly as perl's prepare_checksum_key does. Use it
// to normalise keys that are not already 16 random bytes. nil arguments are
// treated as empty strings.
func PrepareChecksumKey(key, optKey []byte) [16]byte {
	var seed [16]byte
	copy(seed[:], "Call me Ishmael.")

	k1 := sipHash24_128(&seed, []byte("Out damned spot!"), optKey)
	return sipHash24_128(&k1, []byte("Luke, I am your father."), key)
}

type checksumEnvelope struct {
	typ                      byte
	known                    bool // typ is 0, 1 or 2
	tagStart, tagEnd         int
	payloadStart, payloadEnd int
}

// readChecksumEnvelope parses the envelope starting at pos, the first byte
// after the header suffix. It validates the framing only: a known checksum
// type must declare its tag length, and the tag and payload must fit in b.
func readChecksumEnvelope(b []byte, pos int) (checksumEnvelope, error) {
	var env checksumEnvelope

	if pos < 0 || pos >= len(b) {
		return env, ErrTruncated
	}

	env.typ = b[pos]
	pos++

	tagLen := checksumTagLength(env.typ)
	env.known = tagLen != 0

	if pos >= len(b) {
		return env, ErrTruncated
	}

	ln, sz, err := varintdecode(b[pos:])
	if err != nil {
		return env, err
	}
	pos += sz

	if ln < 0 || ln > len(b)-pos || (env.known && ln != tagLen) {
		return env, ErrCorrupt{errBadChecksumLength}
	}

	env.tagStart = pos
	env.tagEnd = pos + ln
	pos = env.tagEnd

	if pos >= len(b) {
		return env, ErrTruncated
	}

	ln, sz, err = varintdecode(b[pos:])
	if err != nil {
		return env, err
	}
	pos += sz

	if ln < 0 || ln > len(b)-pos {
		return env, ErrCorrupt{errBadPayloadLength}
	}

	env.payloadStart = pos
	env.payloadEnd = pos + ln

	return env, nil
}

// checksumMatches recomputes the tag of a known checksum type and compares it
// with the one in the envelope in constant time
func checksumMatches(b []byte, env checksumEnvelope, key *[16]byte) bool {
	tag := checksumTag(env.typ, key, b[:env.tagStart], b[env.tagEnd:env.payloadStart], b[env.payloadStart:env.payloadEnd])
	return tag != nil && subtle.ConstantTimeCompare(tag, b[env.tagStart:env.tagEnd]) == 1
}

// protectPacket turns the unsigned document doc, whose header (magic,
// version-type and suffix) is headerLen bytes long, into a checksummed one:
// the checksum bit is set and the envelope is inserted before the payload.
func protectPacket(doc []byte, headerLen int, typ byte, key *[16]byte) []byte {
	payload := doc[headerLen:]
	tagLen := checksumTagLength(typ)

	out := make([]byte, 0, headerLen+1+1+tagLen+binary.MaxVarintLen64+len(payload))
	out = append(out, doc[:headerLen]...)
	out[4] |= checksumFlag
	out = append(out, typ)
	out = varint(out, uint(tagLen))
	tagStart := len(out)
	out = append(out, make([]byte, tagLen)...)
	tagEnd := len(out)
	out = varint(out, uint(len(payload)))
	out = append(out, payload...)

	copy(out[tagStart:tagEnd], checksumTag(typ, key, out[:tagStart], out[tagEnd:]))

	return out
}

// inspectChecksum validates the header of b and reads its checksum envelope.
// signed is false for a document without the checksum bit.
func inspectChecksum(b []byte) (env checksumEnvelope, signed bool, err error) {
	// readHeader validates the magic against the version
	header, err := readHeader(b)
	if err != nil {
		return env, false, err
	}

	if header.version > MaxProtocolVersion {
		return env, false, fmt.Errorf("document version '%d' not yet supported", header.version)
	}

	if b[4]&checksumFlag == 0 {
		return env, false, nil
	}

	if header.version < 6 {
		return env, false, ErrChecksumVersion
	}

	headerEnd := headerSize + header.suffixSize
	if header.suffixSize < 0 || headerEnd > len(b) {
		return env, false, ErrCorrupt{errBadOffset}
	}

	env, err = readChecksumEnvelope(b, headerEnd)
	return env, err == nil, err
}

// GetChecksum returns the checksum tag of the document in b, or nil when the
// document is not checksummed. It works for checksum types this package
// cannot verify and returns an error when the header or envelope framing is
// malformed.
func GetChecksum(b []byte) ([]byte, error) {
	env, signed, err := inspectChecksum(b)
	if err != nil || !signed {
		return nil, err
	}

	tag := make([]byte, env.tagEnd-env.tagStart)
	copy(tag, b[env.tagStart:env.tagEnd])
	return tag, nil
}

// ValidateChecksum reports whether the checksum of the document in b is valid
// for key. It returns false for a mismatch and for a document that is not
// checksummed, and an error for malformed framing, an unsupported checksum
// type or a nil key on a checksummed document. Only the envelope is checked;
// the payload is not decoded.
func ValidateChecksum(b []byte, key *[16]byte) (bool, error) {
	env, signed, err := inspectChecksum(b)
	if err != nil || !signed {
		return false, err
	}

	if !env.known {
		return false, ErrChecksumUnknownType
	}

	if key == nil {
		return false, ErrChecksumKeyMissing
	}

	return checksumMatches(b, env, key), nil
}

// compressedPayloadLength returns the number of bytes of compression framing
// plus compressed data at the start of b, so the decoder can check that a
// checksummed payload is consumed exactly
func compressedPayloadLength(doctype documentType, b []byte) (int, error) {
	// lenPrefixed returns the size of a <varint length> <data> block
	lenPrefixed := func(b []byte) (int, error) {
		ln, sz, err := varintdecode(b)
		if err != nil || ln < 0 || ln > len(b)-sz {
			return 0, ErrCorrupt{errBadOffset}
		}
		return sz + ln, nil
	}

	switch doctype {
	case serealSnappy:
		// legacy snappy has no length framing: it extends to the payload end
		return len(b), nil

	case serealSnappyIncremental, serealZstd:
		return lenPrefixed(b)

	case serealZlib:
		// <uncompressed length varint> <compressed length varint> <data>
		_, usz, err := varintdecode(b)
		if err != nil {
			return 0, ErrCorrupt{errBadOffset}
		}
		n, err := lenPrefixed(b[usz:])
		if err != nil {
			return 0, err
		}
		return usz + n, nil
	}

	return 0, fmt.Errorf("document type '%d' is not compressed", doctype)
}
