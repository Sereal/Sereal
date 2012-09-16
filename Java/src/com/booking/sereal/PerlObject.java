package com.booking.sereal;

import java.util.Map;

/*
 * Perl object which is defined by a name and has a map of properties.
 */
public class PerlObject {

	private String name;
	public final Map<String, Object> data;

	public PerlObject(String name, Map<String, Object> data) {
		this.name = name;
		this.data = data;
	}

	public String getName() {
		return name;
	}
	
}