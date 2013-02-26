package sereal

// types for emulating perl data structure

type PerlObject struct {
	Class     string
	Reference interface{}
}

type PerlAlias struct {
	Alias interface{}
}

type PerlWeakRef struct {
	Reference interface{}
}

type PerlUndef struct{}

type PerlRegexp struct {
	Pattern   string
	Modifiers string
}
