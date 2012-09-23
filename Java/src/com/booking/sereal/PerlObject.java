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
	public final Map<String, Object> hashdata;
	public final Object[] arraydata;
	public final PerlReference refdata;

	public PerlObject(String name, Map<String, Object> data) {
		this.name = name;
		this.hashdata = data;
		this.arraydata = null;
		this.refdata = null;
	}

	public PerlObject(String name, Object[] data) {
		this.name = name;
		this.arraydata = data;
		this.hashdata = null;
		this.refdata = null;
	}

	public PerlObject(String name, PerlReference data) {
		this.name = name;
		this.arraydata = null;
		this.hashdata = null;
		this.refdata = data;
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

	public boolean isReference() {
		return refdata != null;
	}
}