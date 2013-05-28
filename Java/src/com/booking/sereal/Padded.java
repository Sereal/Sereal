package com.booking.sereal;

/**
 *  Nn object that is preceded by a padding tag (for perfect roundtripping)
 *
 */
public final class Padded {

	private final Object value;

	public Padded(Object value) {
		this.value = value;
	}

	public Object getValue() {
		return value;
	}

}
