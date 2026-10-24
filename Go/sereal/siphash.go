package sereal

import (
	"encoding/binary"
	"math/bits"
)

// SipHash as used by the protocol version 6 keyed checksum envelope. The
// envelope needs SipHash-2-4 with 64- and 128-bit output and SipHash-3-6 with
// 128-bit output; see Perl/shared/srl_siphash.h for the reference
// implementation.

type sipState struct {
	v0, v1, v2, v3   uint64
	cRounds, dRounds int
	tail             [8]byte
	tailLen          int
	total            uint64
}

func newSipState(key *[16]byte, cRounds, dRounds int, is128 bool) sipState {
	k0 := binary.LittleEndian.Uint64(key[0:8])
	k1 := binary.LittleEndian.Uint64(key[8:16])

	s := sipState{
		v0:      0x736f6d6570736575 ^ k0,
		v1:      0x646f72616e646f6d ^ k1,
		v2:      0x6c7967656e657261 ^ k0,
		v3:      0x7465646279746573 ^ k1,
		cRounds: cRounds,
		dRounds: dRounds,
	}

	if is128 {
		s.v1 ^= 0xee
	}

	return s
}

func (s *sipState) round() {
	s.v0 += s.v1
	s.v1 = bits.RotateLeft64(s.v1, 13)
	s.v1 ^= s.v0
	s.v0 = bits.RotateLeft64(s.v0, 32)
	s.v2 += s.v3
	s.v3 = bits.RotateLeft64(s.v3, 16)
	s.v3 ^= s.v2
	s.v0 += s.v3
	s.v3 = bits.RotateLeft64(s.v3, 21)
	s.v3 ^= s.v0
	s.v2 += s.v1
	s.v1 = bits.RotateLeft64(s.v1, 17)
	s.v1 ^= s.v2
	s.v2 = bits.RotateLeft64(s.v2, 32)
}

func (s *sipState) block(m uint64) {
	s.v3 ^= m
	for i := 0; i < s.cRounds; i++ {
		s.round()
	}
	s.v0 ^= m
}

// write feeds p into the hash; it may be called any number of times
func (s *sipState) write(p []byte) {
	s.total += uint64(len(p))

	if s.tailLen > 0 {
		n := copy(s.tail[s.tailLen:], p)
		s.tailLen += n
		p = p[n:]
		if s.tailLen < 8 {
			return
		}
		s.block(binary.LittleEndian.Uint64(s.tail[:]))
		s.tailLen = 0
	}

	for len(p) >= 8 {
		s.block(binary.LittleEndian.Uint64(p))
		p = p[8:]
	}

	s.tailLen = copy(s.tail[:], p)
}

// finish processes the final block (length byte plus buffered tail)
func (s *sipState) finish() {
	var last [8]byte
	copy(last[:], s.tail[:s.tailLen])
	last[7] = byte(s.total)
	s.block(binary.LittleEndian.Uint64(last[:]))
}

func (s *sipState) finalRounds() {
	for i := 0; i < s.dRounds; i++ {
		s.round()
	}
}

func (s *sipState) sum64() uint64 {
	t := *s
	t.finish()
	t.v2 ^= 0xff
	t.finalRounds()
	return t.v0 ^ t.v1 ^ t.v2 ^ t.v3
}

// sum128 returns the 128-bit output as two little-endian 64-bit words
func (s *sipState) sum128() [16]byte {
	var out [16]byte

	t := *s
	t.finish()
	t.v2 ^= 0xee
	t.finalRounds()
	binary.LittleEndian.PutUint64(out[0:8], t.v0^t.v1^t.v2^t.v3)

	t.v1 ^= 0xdd
	t.finalRounds()
	binary.LittleEndian.PutUint64(out[8:16], t.v0^t.v1^t.v2^t.v3)

	return out
}

func sipHash24(key *[16]byte, parts ...[]byte) uint64 {
	s := newSipState(key, 2, 4, false)
	for _, p := range parts {
		s.write(p)
	}
	return s.sum64()
}

func sipHash24_128(key *[16]byte, parts ...[]byte) [16]byte {
	s := newSipState(key, 2, 4, true)
	for _, p := range parts {
		s.write(p)
	}
	return s.sum128()
}

func sipHash36_128(key *[16]byte, parts ...[]byte) [16]byte {
	s := newSipState(key, 3, 6, true)
	for _, p := range parts {
		s.write(p)
	}
	return s.sum128()
}

// checksumTag computes the envelope tag for checksum type typ over the
// concatenation of parts: 8 little-endian bytes for type 0, 16 for types 1
// and 2, nil for any other type.
func checksumTag(typ byte, key *[16]byte, parts ...[]byte) []byte {
	switch typ {
	case checksumSipHash24:
		tag := make([]byte, 8)
		binary.LittleEndian.PutUint64(tag, sipHash24(key, parts...))
		return tag
	case checksumSipHash24_128:
		tag := sipHash24_128(key, parts...)
		return tag[:]
	case checksumSipHash36_128:
		tag := sipHash36_128(key, parts...)
		return tag[:]
	}
	return nil
}
