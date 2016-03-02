package com.booking.sereal;

public class EncoderOptions {
	private boolean perlRefs = false;
	private boolean perlAlias = false;

	public boolean perlReferences() {
		return perlRefs;
	}

	public boolean perlAliases() {
		return perlAlias;
	}

	public EncoderOptions perlReferences(boolean perlReferences) {
		this.perlRefs = perlReferences;

		return this;
	}

	public EncoderOptions perlAliases(boolean perlAliases) {
		this.perlAlias = perlAliases;

		return this;
	}
}
