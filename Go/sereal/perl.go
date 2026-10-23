package sereal

// types for emulating perl data structure

// PerlObject represents a perl blessed reference
type PerlObject struct {
	Class     string
	Reference interface{}
}

// PerlAlias represents an aliased value
type PerlAlias struct {
	Alias interface{}
}

// PerlWeakRef represents a weak reference
type PerlWeakRef struct {
	Reference interface{}
}

// PerlUndef represents perl's "undef" value
type PerlUndef struct {
	canonical bool
}

type PerlNo struct{}

type PerlYes struct{}

type Float128 struct {
	Bytes [16]byte
}

// perlCanonicalUndef is the value that represents the perl's PL_sv_undef and
// is encoded via the CANONICAL_UNDEF tag. It must be the only instance having
// the canonical field set to true.
var perlCanonicalUndef = &PerlUndef{canonical: true}

// PerlCanonicalUndef returns a value that represents perl's shared undef (PL_sv_undef).
//
// For more details see
// https://github.com/Sereal/Sereal/blob/master/sereal_spec.pod#user-content-dealing-with-undefined-values
func PerlCanonicalUndef() *PerlUndef {
	return perlCanonicalUndef
}

// PerlRegexp represents a perl regular expression
type PerlRegexp struct {
	Pattern   []byte
	Modifiers []byte
}

// PerlFreeze represents an object's custom Freeze implementation
type PerlFreeze struct {
	Class string
	Data  []byte
}

// PerlVString represents a perl v-string (such as v1.2.3), carried by the
// protocol version 6 EXTEND VSTRING tags. Spelling is the literal the string
// was written as; perl reconstructs the string value from it. Go has no
// native equivalent, so v-strings always decode to *PerlVString.
type PerlVString struct {
	Spelling string
}

// IsUTF8 reports whether perl decodes the v-string as a UTF-8 string, which
// is the case when any of its components is above 127. It is false for an
// invalid spelling.
func (v PerlVString) IsUTF8() bool {
	isUTF8, ok := parseVString([]byte(v.Spelling))
	return ok && isUTF8
}

// parseVString checks s against the v-string grammar perl's scan_vstring
// accepts, ^v?[0-9_]*(\.[0-9][0-9_]*)*$ (not empty), and reports whether any
// component exceeds 127. Components are parsed as unsigned 64-bit integers
// with wraparound, ignoring underscores, as perl does.
func parseVString(s []byte) (isUTF8 bool, ok bool) {
	if len(s) == 0 {
		return false, false
	}

	i := 0
	if s[0] == 'v' {
		i++
	}

	// component scans [0-9_]* starting at i
	component := func() {
		var n uint64
		for ; i < len(s) && (s[i] == '_' || ('0' <= s[i] && s[i] <= '9')); i++ {
			if s[i] != '_' {
				n = n*10 + uint64(s[i]-'0')
			}
		}
		if n > 127 {
			isUTF8 = true
		}
	}

	component()
	for i < len(s) {
		if s[i] != '.' {
			return false, false
		}
		i++

		// a component after a dot must start with a digit
		if i >= len(s) || s[i] < '0' || s[i] > '9' {
			return false, false
		}
		component()
	}

	return isUTF8, true
}
