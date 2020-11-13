package sereal

import "reflect"

type tagsCache struct {
	cmap map[reflect.Type]map[string]tag
}

type tag struct {
	id        int
	omitEmpty bool
}

func (tc *tagsCache) Get(ptr reflect.Value) map[string]tag {
	if ptr.Kind() != reflect.Struct {
		return nil
	}

	if tc.cmap == nil {
		tc.cmap = make(map[reflect.Type]map[string]tag)
	}

	ptrType := ptr.Type()
	if m, ok := tc.cmap[ptrType]; ok {
		return m
	}

	m := make(map[string]int)

	l := ptrType.NumField()
	for i := 0; i < l; i++ {
		name, opts := parseTag(ptrType.Field(i).Tag.Get("sereal"))
		if name == "-" {
			// sereal tag is "-" -- skip
			continue
		}

		if name == "" {
			// no tag? make one from the field name
			if pkgpath := ptrType.Field(i).PkgPath; pkgpath != "" {
				// field not exported -- skip
				continue
			}
			name = ptrType.Field(i).Name
		}
		m[name] = tag{i, opts.Contains("omitempty")}
	}

	// empty map -- may as well store a nil
	if len(m) == 0 {
		m = nil
	}

	tc.cmap[ptrType] = m
	return m
}
