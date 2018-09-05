package com.booking.sereal;

import java.nio.charset.Charset;
import java.util.Arrays;

public class Latin1String implements CharSequence {
	private final static Charset charset_latin1 = Charset.forName( "ISO-8859-1" );
	private final byte[] bytes;
	private boolean hashCodeSet = false;
	private int hashcode;

	public Latin1String(String s) {
		this.bytes = s.getBytes(charset_latin1);
		this.hashCodeSet = false;
	}

	public Latin1String(byte[] bytes) {
		this.bytes = bytes;
		this.hashCodeSet = false;
	}

	@Override
	public String toString() {
		return new String(bytes, charset_latin1);
	}

	@Override
	public int hashCode() {
		if (!hashCodeSet) {
			hashcode = Arrays.hashCode(bytes);
		}
		return hashcode;
	}

	@Override
	public char charAt(int index) {
		return (char) bytes[index];
	}

	@Override
	public int length() {
		return bytes.length;
	}

	@Override
	public CharSequence subSequence(int start, int end) {
		return this.toString().subSequence(start, end);
	}

	public byte[] getBytes() {
		return this.bytes;
	}

	public String getString() {
		return this.toString();
	}

	@Override
	public boolean equals(Object o) {
		if (this == o) {
			return true;
		}
		if (o == null) {
			return false;
		}
		if (this.hashCode() != o.hashCode()) {
			return false;
		}
		if (!this.getClass().equals(o.getClass())) {
			return false;
		}

		return Arrays.equals(this.bytes, ((Latin1String) o).bytes);
	}
}
