package com.booking.sereal;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class DefaultTypeMapper implements TypeMapper {
    @Override
    public boolean useObjectArray() { return false; }

    @Override
    public List<Object> makeArray(int size) {
        return new ArrayList<Object>(size);
    }

    @Override
    public Map<String, Object> makeMap(int size) {
        return new HashMap<String, Object>(size);
    }

    @Override
    public Object makeObject(String className, Object data) {
        return new PerlObject(className, data);
    }
}
