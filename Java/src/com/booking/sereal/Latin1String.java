package com.booking.sereal;

import java.nio.ByteBuffer;
import java.nio.charset.Charset;
import java.util.Arrays;

public class Latin1String implements CharSequence {

	private final static Charset charset_latin1 = Charset.forName( "ISO-8859-1" );
    private final byte[] bytes;

	public Latin1String(String s) {
        this.bytes = s.getBytes(charset_latin1);
	}

    public Latin1String(byte[] bytes) {
        this.bytes = bytes;
    }

	@Override
	public String toString() {
		return Charset.forName( "ISO-8859-1" ).decode( ByteBuffer.wrap(bytes) ).toString();
	}

    @Override
    public int hashCode() {
        return this.toString().hashCode();
    }
	
	@Override
	public char charAt(int index) {
		return this.toString().charAt(index);
	}

	@Override
	public int length() {
		return this.toString().length();
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
        boolean result = false;

        if(o instanceof Latin1String) {
            Latin1String other = (Latin1String) o;
            result = Arrays.equals(this.getBytes(), other.getBytes());
        }

        return result;
    }
}
