package com.booking.sereal;

/**
 * Since the Sereal format has the notion of both reference and alias we
 * use this class to wrap those values. That way we can always roundtrip
 * between Perl and Java. 
 *
 */
public class Alias {

	public final Object value;
	
	public Alias(Object value) {
		this.value = value;
	}

}


