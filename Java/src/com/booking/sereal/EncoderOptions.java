package com.booking.sereal;

public class EncoderOptions {
	public enum CompressionType {
		NONE,
		SNAPPY,
		ZLIB,
		ZSTD,
	}

	private boolean perlRefs = false;
	private boolean perlAlias = false;
	private int protocolVersion = 4;
	private CompressionType compressionType = CompressionType.NONE;
	private long compressionThreshold = 1024;
	private int zlibCompressionLevel = 6;
	private int zstdCompressionLevel = 3;

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

	public long compressionThreshold() {
		return compressionThreshold;
	}

	public int zlibCompressionLevel() {
		return zlibCompressionLevel;
	}

	public int zstdCompressionLevel() {
		return zstdCompressionLevel;
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
		if (protocolVersion < 0 || protocolVersion > 4)
			throw new IllegalArgumentException("Unknown Sereal version " + protocolVersion);
		this.protocolVersion = protocolVersion;

		return this;
	}

	public EncoderOptions compressionType(CompressionType compressionType) {
		this.compressionType = compressionType;

		return this;
	}

	public EncoderOptions compressionThreshold(long compressionThreshold) {
		this.compressionThreshold = compressionThreshold;

		return this;
	}

	public EncoderOptions zlibCompressionLevel(int zlibCompressionLevel) {
		this.zlibCompressionLevel = zlibCompressionLevel;


		return this;
	}

	public EncoderOptions zstdCompressionLevel(int zstdCompressionLevel) {
		this.zstdCompressionLevel = zstdCompressionLevel;
		return this;
	}
}
