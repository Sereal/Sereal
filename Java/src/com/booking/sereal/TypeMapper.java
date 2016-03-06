package com.booking.sereal;

import java.util.List;
import java.util.Map;

public interface TypeMapper {
    public boolean useObjectArray();

    public List<Object> makeArray(int size);

    public Map<String, Object> makeMap(int size);

    public Object makeObject(String className, Object data);
}
