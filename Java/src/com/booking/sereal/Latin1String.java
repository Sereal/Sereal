package com.booking.sereal;

import java.nio.ByteBuffer;
import java.nio.charset.Charset;

public class Latin1String implements CharSequence {

	private final Charset charset_latin1 = Charset.forName( "ISO-8859-1" );
	private final String s;

	public Latin1String(String s) {
		this.s = s;
	}

    public Latin1String(byte[] bytes) {
        this.s = Charset.forName( "ISO-8859-1" ).decode( ByteBuffer.wrap(bytes) ).toString();
    }

	@Override
	public String toString() {
		return  s;
	}

    @Override
    public int hashCode() {
        return s.hashCode();
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

    @Override
    public boolean equals(Object o) {
        boolean result = false;

        if(o instanceof Latin1String) {
            Latin1String other = (Latin1String) o;
            result = this.getString().equals(other.getString());
        }

        return result;
    }

}
