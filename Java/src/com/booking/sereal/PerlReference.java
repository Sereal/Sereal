package com.booking.sereal;

/**
 * So we can encode references perl style
 * (otherwise there is no way to distinguish Strings from "Stringrefs"
 *
 */
public class PerlReference {

	private Object value;

	public PerlReference(Object value) {
		this.value = value;
	}

	public Object getValue() {
		return value;
	}

	void setValue(Object value) {
		this.value = value;
	}

    @Override
    public String toString() {
         return "Reference to: " + (value == null ? "null" : value.toString());
    }
}
