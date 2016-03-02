package com.booking.sereal;

public class DecoderOptions {
	public enum ObjectType {
		PERL_OBJECT, // Perl style object (name + hash)
		POJO // Dynamically compile a Plain Old Java Object
	}

	private boolean perlRefs = false;
	private boolean perlAlias = false;
	private boolean preserveUndef = false;
	private boolean preferLatin1 = false;
	private boolean refuseSnappy = false;
	private ObjectType objectType = ObjectType.PERL_OBJECT;

	public boolean perlReferences() {
		return perlRefs;
	}

	public boolean perlAliases() {
		return perlAlias;
	}

	public boolean preserveUndef() {
		return preserveUndef;
	}

	public boolean preferLatin1() {
		return preferLatin1;
	}

	public boolean refuseSnappy() {
		return refuseSnappy;
	}

	public ObjectType objectType() {
		return objectType;
	}

	public DecoderOptions perlReferences(boolean perlReferences) {
		this.perlRefs = perlReferences;

		return this;
	}

	public DecoderOptions perlAliases(boolean perlAliases) {
		this.perlAlias = perlAliases;

		return this;
	}

	public DecoderOptions preserveUndef(boolean preserveUndef) {
		this.preserveUndef = preserveUndef;

		return this;
	}

	public DecoderOptions preferLatin1(boolean preferLatin1) {
		this.preferLatin1 = preferLatin1;

		return this;
	}

	public DecoderOptions refuseSnappy(boolean refuseSnappy) {
		this.refuseSnappy = refuseSnappy;

		return this;
	}

	public DecoderOptions objectType(ObjectType objectType) {
		this.objectType = objectType;

		return this;
	}
}
