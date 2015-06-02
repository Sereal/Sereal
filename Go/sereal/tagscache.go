package sereal

import "reflect"

type tagsCache struct {
	cmap map[reflect.Type]map[string]int
}

func (tc *tagsCache) Get(ptr reflect.Value) map[string]int {
	if ptr.Kind() != reflect.Struct {
		return nil
	}

	if tc.cmap == nil {
		tc.cmap = make(map[reflect.Type]map[string]int)
	}

	ptrType := ptr.Type()
	if m, ok := tc.cmap[ptrType]; ok {
		return m
	}

	numTags := 0
	m := make(map[string]int)

	l := ptrType.NumField()
	for i := 0; i < l; i++ {
		field := ptrType.Field(i).Tag.Get("sereal")
		if field != "" {
			m[field] = i
			numTags++
		}
	}

	if numTags != 0 {
		tc.cmap[ptrType] = m
		return m
	}

	// build one from the public names
	for i := 0; i < l; i++ {
		pkgpath := ptrType.Field(i).PkgPath
		if pkgpath == "" { // exported
			field := ptrType.Field(i).Name
			m[field] = i
			numTags++
		}
	}

	if numTags != 0 {
		tc.cmap[ptrType] = m
		return m
	}

	tc.cmap[ptrType] = nil
	return nil
}
