package com.booking.sereal;

import java.nio.charset.Charset;

public class Latin1String implements CharSequence {

	private Charset charset_latin1 = Charset.forName( "ISO-8859-1" );
	private String s;

	public Latin1String(String s) {
		this.s = s;
	}

	@Override
	public char charAt(int index) {
		return s.charAt( index );
	}

	@Override
	public int length() {
		return s.length();
	}

	@Override
	public CharSequence subSequence(int start, int end) {
		return s.subSequence( start, end );
	}

	public byte[] getBytes() {
		return s.getBytes( charset_latin1 );
	}

	public String getString() {
		return s;
	}

}
