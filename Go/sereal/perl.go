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
