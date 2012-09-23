package com.booking.sereal;

import java.util.Map;

/*
 * Perl object which is defined by a name and has a map of properties.
 */
public class PerlObject {

	private String name;
	public final Map<String, Object> hashdata;
	public final Object[] arraydata;

	public PerlObject(String name, Map<String, Object> data) {
		this.name = name;
		this.hashdata = data;
		this.arraydata = null;
	}

	public PerlObject(String name, Object[] data) {
		this.name = name;
		this.arraydata = data;
		this.hashdata = null;
	}

	public String getName() {
		return name;
	}

	public boolean isHash() {
		return hashdata != null;
	}

	public boolean isArray() {
		return arraydata != null;
	}

}