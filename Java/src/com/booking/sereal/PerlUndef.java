package com.booking.sereal;

/**
 * So we can preserve the identity of different undefs
 *
 */
public final class PerlUndef {
	public static final PerlUndef CANONICAL = new PerlUndef();

	@Override
	public String toString() {
	    return "Undef";
	}
}
