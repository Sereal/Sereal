package com.booking.sereal;

public class DecoderOptions {
	private boolean perlRefs = false;
	private boolean perlAlias = false;
	private boolean preserveUndef = false;
	private boolean preferLatin1 = false;
	private boolean refuseSnappy = false;
	private boolean refuseObjects = false;
	private boolean stripObjects = false;
	private TypeMapper typeMapper = new DefaultTypeMapper();

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

	public boolean refuseObjects() {
		return refuseObjects;
	}

	public boolean stripObjects() {
		return stripObjects;
	}

	public TypeMapper typeMapper() {
		return typeMapper;
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

	public DecoderOptions refuseObjects(boolean refuseObjects) {
		this.refuseObjects = refuseObjects;

		return this;
	}

	public DecoderOptions stripObjects(boolean stripObjects) {
		this.stripObjects = stripObjects;

		return this;
	}

	public DecoderOptions typeMapper(TypeMapper typeMapper) {
		this.typeMapper = typeMapper;

		return this;
	}
}
