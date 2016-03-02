package com.booking.sereal;

public class EncoderOptions {
	public enum CompressionType {
		NONE,
		SNAPPY,
		ZLIB,
	}

	private boolean perlRefs = false;
	private boolean perlAlias = false;
	private int protocolVersion = 2;
	private CompressionType compressionType = CompressionType.NONE;

	public boolean perlReferences() {
		return perlRefs;
	}

	public boolean perlAliases() {
		return perlAlias;
	}

	public int protocolVersion() {
		return protocolVersion;
	}

	public CompressionType compressionType() {
		return compressionType;
	}

	public EncoderOptions perlReferences(boolean perlReferences) {
		this.perlRefs = perlReferences;

		return this;
	}

	public EncoderOptions perlAliases(boolean perlAliases) {
		this.perlAlias = perlAliases;

		return this;
	}

	public EncoderOptions protocolVersion(int protocolVersion) {
		if (protocolVersion < 0 || protocolVersion > 2)
			throw new IllegalArgumentException("Unknown Sereal version " + protocolVersion);
		this.protocolVersion = protocolVersion;

		return this;
	}

	public EncoderOptions compressionType(CompressionType compressionType) {
		this.compressionType = compressionType;

		return this;
	}
}
