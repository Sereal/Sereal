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
	public final Object data;

	public PerlObject(String className, Object obj) {
		this.name = className;
		this.data = obj;
	}

	public String getName() {
		return name;
	}

	public boolean isHash() {
		return data.getClass() == Map.class;
	}

	public boolean isArray() {
		return data.getClass().isArray();
	}

	public boolean isReference() {
		return !isHash() && !isArray();
	}
}