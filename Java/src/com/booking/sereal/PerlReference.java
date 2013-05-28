package com.booking.sereal;

/**
 * So we can encode references perl style
 * (otherwise there is no way to distinguish Strings from "Stringrefs"
 *
 */
public class PerlReference {

	private final Object value;

	public PerlReference(Object value) {
		this.value = value;
	}

	public Object getValue() {
		return value;
	}
}
