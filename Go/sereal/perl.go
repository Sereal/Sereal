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
type PerlUndef struct{}

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
