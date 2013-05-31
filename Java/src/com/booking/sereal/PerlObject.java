package com.booking.sereal;

import java.util.Map;

/*
 * Perl object which is defined by a name and either an array, hash or ref
 * (in Perl all of these are basically refs)
 *
 * Also, this is very ugly
 */
public class PerlObject {

	private String name;
	private final Object data;

	public PerlObject(String className, Object obj) {
		this.name = className;
		this.data = obj;
	}

	public String getName() {
		return name;
	}

	public boolean isHash() {
		return getData() instanceof Map;
	}

	public boolean isArray() {
		return getData().getClass().isArray();
	}

	public boolean isReference() {
		return !isHash() && !isArray();
	}

	public Object getData() {
		return data;
	}
}