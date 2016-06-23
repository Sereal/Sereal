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

	m := make(map[string]int)

	l := ptrType.NumField()
	for i := 0; i < l; i++ {
		field := ptrType.Field(i).Tag.Get("sereal")
		if field == "-" {
			// sereal tag is "-" -- skip
			continue
		}

		if field == "" {
			// no tag? make one from the field name
			if pkgpath := ptrType.Field(i).PkgPath; pkgpath != "" {
				// field not exported -- skip
				continue
			}
			field = ptrType.Field(i).Name
		}
		m[field] = i
	}

	// empty map -- may as well store a nil
	if len(m) == 0 {
		m = nil
	}

	tc.cmap[ptrType] = m
	return m
}
