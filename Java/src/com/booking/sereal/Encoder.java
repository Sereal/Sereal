package com.booking.sereal;

import static com.booking.sereal.EncoderOptions.CompressionType;

import com.booking.sereal.impl.BytearrayCopyMap;
import com.booking.sereal.impl.IdentityMap;
import com.booking.sereal.impl.StringCopyMap;

import java.io.IOException;
import java.lang.ref.WeakReference;
import java.lang.reflect.Array;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;
import java.util.zip.Deflater;

import com.github.luben.zstd.Zstd;
import org.xerial.snappy.Snappy;

/**
 * WIP
 * Functions for encoding various things.
 * TODO: Probably just call all methods write() and not have them return the encoded value
 */
public class Encoder {

	private static final EncoderOptions DEFAULT_OPTIONS = new EncoderOptions();
	private static final byte[] EMPTY_ARRAY = new byte[0];

	boolean debugTrace;

	void trace(String info) {
		if (!debugTrace)
			throw new RuntimeException("All calls to trace() must be guarded with 'if (debugTrace)'");
		System.out.println( info );
	}

	private final boolean perlRefs;
	private final boolean perlAlias;
	private final int protocolVersion, encoding;
	private final CompressionType compressionType;
	private final long compressionThreshold;
	private final Deflater deflater;
	private final int zstdCompressionLevel;

	// so we don't need to allocate this every time we encode a varint
	private byte[] varint_buf = new byte[12];

	private final byte[] HEADER = new byte[] {
		(byte) (SerealHeader.MAGIC >> 24),
		(byte) (SerealHeader.MAGIC >> 16),
		(byte) (SerealHeader.MAGIC >>  8),
		(byte) (SerealHeader.MAGIC >>  0),
	};
	private final byte[] HEADER_V3 = new byte[] {
		(byte) (SerealHeader.MAGIC_V3 >> 24),
		(byte) (SerealHeader.MAGIC_V3 >> 16),
		(byte) (SerealHeader.MAGIC_V3 >>  8),
		(byte) (SerealHeader.MAGIC_V3 >>  0),
	};

	// track things we've encoded so we can emit refs and copies
	private IdentityMap tracked = new IdentityMap();
	private IdentityMap aliases, maybeAliases;
	private BytearrayCopyMap trackedBytearrayCopy = new BytearrayCopyMap();
	private StringCopyMap trackedStringCopy = new StringCopyMap();
	private StringCopyMap trackedClassnames = new StringCopyMap();

	// where we store the various encoded things
	private byte[] bytes = new byte[1024];
	private byte[] compressedBytes = EMPTY_ARRAY;
	private long size = 0; // size of everything encoded so far
	private long compressedSize;
	private int headerSize, headerOffset;

	public Encoder() {
		this(DEFAULT_OPTIONS);
	}

	public Encoder(EncoderOptions options) {
		perlRefs = options.perlReferences();
		perlAlias = options.perlAliases();
		protocolVersion = options.protocolVersion();
		compressionType = options.compressionType();
		compressionThreshold = options.compressionThreshold();
		zstdCompressionLevel = options.zstdCompressionLevel();

		switch (protocolVersion) {
		case 4:
			encoding = compressionType.equals(CompressionType.ZSTD) ? 4 :
					compressionType.equals(CompressionType.ZLIB) ? 3 :
					compressionType.equals(CompressionType.SNAPPY) ? 2 : 0;
			break;
		case 3:
			encoding = compressionType.equals(CompressionType.ZLIB) ? 3 :
				   compressionType.equals(CompressionType.SNAPPY) ? 2 : 0;
			break;
		case 2:
			encoding = compressionType.equals(CompressionType.SNAPPY) ? 2 : 0;
			break;
		case 1:
			encoding = compressionType.equals(CompressionType.SNAPPY) ? 1 : 0;
			break;
		default:
			encoding = 0;
			break;
		}
		if (encoding == 3)
			deflater = new Deflater(options.zlibCompressionLevel());
		else
			deflater = null;

		if (perlAlias) {
			aliases = new IdentityMap();
			maybeAliases = new IdentityMap();
		}
	}

	// write header and version/encoding
	private void init(Object header, boolean hasHeader) throws SerealException {
		if (protocolVersion >= 3)
			appendBytesUnsafe(HEADER_V3);
		else
			appendBytesUnsafe(HEADER);
		appendByteUnsafe((byte) ((encoding << 4) | protocolVersion));

		if (hasHeader) {
			encodeUserHeader(header);
		} else {
			appendByteUnsafe((byte) 0x00);
		}

		headerSize = (int) size;
		if (protocolVersion > 1)
			// because offsets start at 1
			headerOffset = headerSize - 1;
		else
			headerOffset = 0;
	}

	private void encodeUserHeader(Object header) throws SerealException {
		long originalSize = size;

		// be optimistic about encoded header size
		size += 2; // one for the size, one for 8bit bitfield
		encode(header);

		int suffixSize = (int) (size - originalSize - 1);
		if (suffixSize < 128) {
			bytes[(int) originalSize] = (byte) suffixSize;
			bytes[(int) originalSize + 1] = 0x01;
		} else {
			// we were too optimistic
			int sizeLength = varintLength(suffixSize);

			// make space
			ensureAvailable(sizeLength - 1);
			System.arraycopy(bytes, (int) originalSize + 2, bytes, (int) originalSize + sizeLength + 1, suffixSize - 1);
			size += sizeLength - 1;

			// now write size and 8bit bitfield
			encodeVarint(suffixSize, bytes, (int) originalSize);
			bytes[(int) originalSize + sizeLength] = 0x01;
		}

		resetTracked();
	}

	public ByteArray getDataReference() {
		if (compressedSize != 0) {
			return new ByteArray(compressedBytes, (int) compressedSize);
		} else {
			return new ByteArray(bytes, (int) size);
		}
	}

	public byte[] getData() {
		if (compressedSize != 0) {
			return Arrays.copyOf(compressedBytes, (int) compressedSize);
		} else {
			return Arrays.copyOf(bytes, (int) size);
		}
	}

	private void markNotCompressed() {
		compressedSize = 0;
		bytes[4] &= (byte) 0xf;
	}

	private void compressSnappy() throws SerealException {
		int maxSize = Snappy.maxCompressedLength((int) size - headerSize);
		int sizeLength = encoding == 2 ? varintLength(maxSize) : 0;

		// I don't think there is any point in overallocating here
		if ((headerSize + sizeLength + maxSize) > compressedBytes.length)
			compressedBytes = new byte[headerSize + sizeLength + maxSize];

		prepareHeader(bytes, compressedBytes, headerSize, sizeLength);

		int compressed;
		try {
			compressed = Snappy.compress(bytes, headerSize, (int) size - headerSize, compressedBytes, headerSize + sizeLength);
		} catch (IOException e) {
			throw new SerealException(e);
		}
		compressedSize = headerSize + sizeLength + compressed;
		if (compressedSize > size) {
			markNotCompressed();
			return;
		}

		if (encoding == 2) {
			finishHeader(compressedBytes, compressed, headerSize, sizeLength);
		}
	}

	// from miniz.c
	private int zlibMaxSize(int sourceLen) {
		return Math.max(128 + (sourceLen * 110) / 100, 128 + sourceLen + ((sourceLen / (31 * 1024)) + 1) * 5);
	}

	private void compressZlib() {
		deflater.reset();

		int sourceSize = (int) size - headerSize;
		int maxSize = zlibMaxSize(sourceSize);
		int sizeLength = varintLength(sourceSize);
		int sizeLength2 = varintLength(maxSize);
		int pos = 0;

		// I don't think there is any point in overallocating here
		if ((headerSize + sizeLength + sizeLength2 + maxSize) > compressedBytes.length)
			compressedBytes = new byte[headerSize + sizeLength + sizeLength2 + maxSize];

		System.arraycopy(bytes, 0, compressedBytes, 0, headerSize);
		pos += headerSize;
		pos = encodeVarint(sourceSize, compressedBytes, pos);

		// varint-encoded 0, filling all space
		int encodedSizePos = pos;
		for (int max = pos + sizeLength2 - 1; pos < max; )
			compressedBytes[pos++] = (byte) 128;
		compressedBytes[pos++] = 0;

		deflater.setInput(bytes, headerSize, sourceSize);
		deflater.finish();

		int compressed = deflater.deflate(compressedBytes, pos, compressedBytes.length - pos);
		compressedSize = headerSize + sizeLength + sizeLength2 + compressed;
		if (compressedSize > size) {
			markNotCompressed();
			return;
		}

		int after = encodeVarint(compressed, compressedBytes, encodedSizePos);
		if (after != headerSize + sizeLength + sizeLength2)
			compressedBytes[after - 1] |= (byte) 0x80;
	}

	private void compressZstd() throws SerealException {
		long maxSize = Zstd.compressBound((int) size - headerSize);
		int sizeLength = varintLength(maxSize);

		if (headerSize + sizeLength + maxSize > Integer.MAX_VALUE)
			throw new SerealException("Compressed data size exceeds integer MAX_VALUE: " + (headerSize + maxSize));
		if (headerSize + sizeLength + maxSize > compressedBytes.length)
			compressedBytes = new byte[(int) (headerSize + sizeLength + maxSize)];

		prepareHeader(bytes, compressedBytes, headerSize, sizeLength);

		long compressed = Zstd.compressUsingDict(compressedBytes, headerSize + sizeLength, bytes, headerSize,
				(int) size - headerSize, new byte[0], zstdCompressionLevel);
		if (Zstd.isError(compressed))
			throw new SerealException(Zstd.getErrorName(compressed));
		compressedSize = headerSize + sizeLength + compressed;
		if (compressedSize > size) {
			markNotCompressed();
			return;
		}

		finishHeader(compressedBytes, compressed, headerSize, sizeLength);
	}

	private static void prepareHeader(byte[] originBytes, byte[] compressedBytes, int headerSize, int sizeLength) {
		System.arraycopy(originBytes, 0, compressedBytes, 0, headerSize);
		// varint-encoded 0, filling all space
		for (int i = headerSize; i < headerSize + sizeLength - 1; i++)
			compressedBytes[i] = (byte) 128;
		compressedBytes[headerSize + sizeLength - 1] = 0;
	}

	private static void finishHeader(byte[] compressedBytes, long compressedSize, int headerSize, int sizeLength) {
		int after = encodeVarint(compressedSize, compressedBytes, headerSize);
		if (after != headerSize + sizeLength)
			compressedBytes[after - 1] |= (byte) 0x80;
	}

	private static int varintLength(long n) {
		int length = 0;

		while (n > 127) {
			n >>= 7;
			length++;
		}

		return length + 1;
	}

	private static int encodeVarint(long n, byte[] buffer, int pos) {
		while (n > 127) {
			buffer[pos++] = (byte) ((n & 127) | 128);
			n >>= 7;
		}
		buffer[pos++] = (byte) n;

		return pos;
	}

	/**
	 * Write an integer as a varint
	 * 
	 * Note: sometimes the next thing while decoding is know to be a varint, sometimes there must be a tag
	 * that denotes the next item *is* a varint. So don't forget to write that tag.
	 * 
	 * @param n
	 *           positive integer
	 * @return
	 */
	private void appendVarint(long n) {
		int length = 0;

		while( n > 127 ) {
			varint_buf[length++] = (byte) ((n & 127) | 128);
			n >>= 7;
		}
		varint_buf[length++] = (byte) n;

		appendBytes(varint_buf, length);
	}

	private void setTrackBit(long offset) {
		bytes[(int) offset + headerOffset] |= (byte) 0x80;
	}

	/**
	 * Encode a number as zigzag
	 * 
	 * @param n
	 * @return
	 */
	private void appendZigZag(long n) {
		appendByte(SerealHeader.SRL_HDR_ZIGZAG);
		appendVarint( (n << 1) ^ (n >> 63) ); // note the signed right shift
	}

	/**
	 * Encode a short ascii string
	 * 
	 * @param latin1
	 *           String to encode as US-ASCII bytes
	 * @throws SerealException
	 *            if the string is not short enough
	 */
	private void appendShortBinary(byte[] latin1) throws SerealException {
		if (debugTrace) trace( "Writing short binary: " + latin1 );

		// maybe we can just COPY
		long copyOffset = getTrackedItemCopy(latin1);
		if (copyOffset != BytearrayCopyMap.NOT_FOUND) {
			appendCopy(copyOffset);
			return;
		}

		int length = latin1.length;
		long location = size;

		if( length > 31 ) {
			throw new SerealException( "Cannot create short binary for " + latin1 + ": too long" );
		}

		// length of string
		appendByte((byte) (length | SerealHeader.SRL_HDR_SHORT_BINARY));

		// save it
		appendBytes(latin1);

		trackForCopy(latin1, location);
	}

	/**
	 * Encode a long ascii string
	 * 
	 * @param latin1
	 *           String to encode as US-ASCII bytes
	 */
	private void appendBinary(byte[] latin1) {
		if (debugTrace) trace( "Writing binary: " + latin1 );

		// maybe we can just COPY
		long copyOffset = getTrackedItemCopy(latin1);
		if (copyOffset != BytearrayCopyMap.NOT_FOUND) {
			appendCopy(copyOffset);
			return;
		}

		int length = latin1.length;
		long location = size;

		// length of string
		appendByte(SerealHeader.SRL_HDR_BINARY);
		appendBytesWithLength(latin1);

		trackForCopy(latin1, location);
	}

	private void appendCopy(long location) {
		if (debugTrace) trace( "Emitting a COPY for location " + location );

		appendByte(SerealHeader.SRL_HDR_COPY);
		appendVarint(location);
	}

	/**
	 * Encode a regex
	 * 
	 * @param p
	 *           regex pattern. Only support flags "smix": DOTALL | MULTILINE | CASE_INSENSITIVE | COMMENTS
	 * @throws SerealException
	 *            if the pattern is longer that a short binary string
	 */
	private void appendRegex(Pattern p) throws SerealException {

		if (debugTrace) trace( "Writing a Pattern: " + Utils.dump( p ) );

		byte[] flags = new byte[4];
		int flags_size = 0;
		if ((p.flags() & Pattern.MULTILINE) != 0)
			flags[flags_size++] = 'm';
		if ((p.flags() & Pattern.DOTALL) != 0)
			flags[flags_size++] = 's';
		if ((p.flags() & Pattern.CASE_INSENSITIVE) != 0)
			flags[flags_size++] = 'i';
		if ((p.flags() & Pattern.COMMENTS) != 0)
			flags[flags_size++] = 'x';

		appendByte(SerealHeader.SRL_HDR_REGEXP);
		appendStringType(new Latin1String(p.pattern()));
		appendByte((byte) (flags_size | SerealHeader.SRL_HDR_SHORT_BINARY));
		appendBytes(flags, flags_size);
	}

	/**
	 * Encodes a byte array
	 * 
	 * @param in
	 * @return
	 */
	private void appendBytesWithLength(byte[] in) {
		appendVarint(in.length);
		appendBytes(in);
	}

	private void appendBoolean(boolean b) {
		appendByte(b ? SerealHeader.SRL_HDR_TRUE : SerealHeader.SRL_HDR_FALSE);
	}

	/**
	 * Write something to the encoder.
	 * 
	 * @param obj
	 * @return a buffer with the encoded data
	 * @throws SerealException
	 */
	public Encoder write(Object obj) throws SerealException {
		return write(obj, null, false);
	}

	public Encoder write(Object obj, Object header) throws SerealException {
		return write(obj, header, true);
	}

	private Encoder write(Object obj, Object header, boolean hasHeader) throws SerealException {
		if (hasHeader && protocolVersion == 1)
			throw new SerealException("Can't encode user header in Sereal protocol version 1");

		reset();
		init(header, hasHeader);
		encode( obj );

		if (size - headerSize > compressionThreshold) {
			if (compressionType.equals(CompressionType.SNAPPY))
				compressSnappy();
			else if (compressionType.equals(CompressionType.ZLIB))
				compressZlib();
			else if (compressionType.equals(CompressionType.ZSTD))
				compressZstd();
		} else {
			// we did not do compression after all
			markNotCompressed();
		}

		return this;
	}

	@SuppressWarnings("unchecked")
	private void encode(Object obj) throws SerealException {
		if (debugTrace) trace( "Currently tracked: " + Utils.dump( tracked ) );

		// track it (for ALIAS tags)
		long location = size;

		if (perlAlias) {
			long aliasOffset = aliases.get(obj);
			if(aliasOffset != IdentityMap.NOT_FOUND) {
				if (debugTrace) trace( "Track: We saw this before: " + Utils.dump( obj ) + " at location " + aliasOffset );
				appendAlias(aliasOffset);
				return;
			} else {
				maybeAliases.put(obj, location - headerOffset);
			}
		}

		Class<?> type = obj == null ? PerlUndef.class : obj.getClass();

		// this needs to be first for obvious reasons :)
		if (type == PerlUndef.class) {
			if (debugTrace) trace( "Emitting a NULL/undef" );
			if (protocolVersion == 3 && obj == PerlUndef.CANONICAL)
				appendByte(SerealHeader.SRL_HDR_CANONICAL_UNDEF);
			else
				appendByte(SerealHeader.SRL_HDR_UNDEF);
			return;
		}

		if (debugTrace) trace( "Encoding type: " + type );

		// this is ugly :)
		if (type == Long.class || type == Integer.class || type == Byte.class) {
			appendNumber( ((Number) obj).longValue() );
		} else if (type == Boolean.class) {
			appendBoolean((Boolean) obj);
		} else if (type == String.class) {
			appendStringType((String) obj);
		} else if (type == Latin1String.class) {
			appendStringType((Latin1String) obj);
		} else if (type == byte[].class) {
			appendStringType((byte[]) obj);
		} else if (type.isArray()) {
			if (perlRefs || !tryAppendRefp(obj)) {
				if (obj instanceof Object[])
					appendArray((Object[]) obj);
				else
					appendArray(obj);
			}
		} else if (type == HashMap.class) {
			if (perlRefs || !tryAppendRefp(obj))
				appendMap((Map<Object, Object>) obj);
		} else if (type == ArrayList.class) {
			if (perlRefs || !tryAppendRefp(obj))
				appendArray((List<Object>) obj);
		} else if (type == Pattern.class) {
			appendRegex((Pattern) obj);
		} else if (type == Double.class) {
			appendDouble((Double) obj);
		} else if (type == Float.class) {
			appendFloat((Float) obj);
		} else if (type == PerlReference.class) {
			PerlReference ref = (PerlReference) obj;
			long trackedRef = getTrackedItem(ref.getValue());

			if(trackedRef != IdentityMap.NOT_FOUND) {
				appendRefp(trackedRef);
			} else {
				appendRef(ref);
			}
		} else if (type == WeakReference.class) {
			appendByte(SerealHeader.SRL_HDR_WEAKEN);

			PerlReference wref = (PerlReference) ((WeakReference<PerlReference>) obj).get(); // pretend weakref is a marker
			encode( wref );
		} else if (type == PerlAlias.class) {
			Object value = ((PerlAlias) obj).getValue();

			if (perlAlias) {
				long maybeAlias = maybeAliases.get(value);
				long alias = aliases.get(value);

				if (alias != IdentityMap.NOT_FOUND) {
					appendAlias(alias);
				} else if (maybeAlias != IdentityMap.NOT_FOUND) {
					appendAlias(maybeAlias);
					aliases.put(value, maybeAlias);
				} else {
					encode(value);
					aliases.put(value, location - headerOffset);
				}
			} else {
				encode(value);
			}
		} else if (type == PerlObject.class) {
			PerlObject po = (PerlObject) obj;

			appendPerlObject(po.getName(), po.getData());
		} else if (obj instanceof Map) {
			if (perlRefs || !tryAppendRefp(obj))
				appendMap((Map<Object, Object>) obj);
		} else if (obj instanceof List) {
			if (perlRefs || !tryAppendRefp(obj))
				appendArray((List<Object>) obj);
		}

		if (size == location) { // didn't write anything
			throw new SerealException( "Don't know how to encode: " + type.getName() + " = " + obj.toString() );
		}
	}

	private void appendPerlObject(String className, Object data) throws SerealException {
		long nameOffset = trackedClassnames.get(className);
		if (nameOffset != StringCopyMap.NOT_FOUND) {
			if (debugTrace) trace( "Already emitted this classname, making objectv for " + className );

			appendByte(SerealHeader.SRL_HDR_OBJECTV);
			appendVarint(nameOffset);
		} else {
			appendByte(SerealHeader.SRL_HDR_OBJECT);
			trackedClassnames.put(className, size - headerOffset);
			appendStringType(className);
		}

		// write the data structure
		encode(data);
	}

	/**
	 * 
	 * @param obj
	 * @return location in bytestream of object
	 */
	private long getTrackedItem(Object obj) {
		return tracked.get(obj);
	}

	private long getTrackedItemCopy(byte[] bytes) {
		return trackedBytearrayCopy.get(bytes);
	}

	private long getTrackedItemCopy(String string) {
		return trackedStringCopy.get(string);
	}

	private void track(Object obj, long obj_location) {
		if (debugTrace) trace( "Tracking " + (obj == null ? "NULL/undef" : obj.getClass().getName()) + "@" + System.identityHashCode( obj ) + " at location " + obj_location );
		tracked.put(obj, obj_location - headerOffset);
	}

	private void trackForCopy(byte[] bytes, long location) {
		if (debugTrace) trace( "Tracking for copy " + (bytes == null ? "NULL/undef" : "bytes") + " at location " + location );
		trackedBytearrayCopy.put(bytes, location - headerOffset);
	}

	private void trackForCopy(String string, long location) {
		if (debugTrace) trace( "Tracking for copy " + (string == null ? "NULL/undef" : "string") + " at location " + location );
		trackedStringCopy.put(string, location - headerOffset);
	}

	private void appendDouble(Double d) {
		appendByte(SerealHeader.SRL_HDR_DOUBLE);

		long bits = Double.doubleToLongBits( d ); // very convienent, thanks Java guys! :)
		for (int i = 0; i < 8; i++) {
			varint_buf[i] = (byte) ((bits >> (i * 8)) & 0xff);
		}
		appendBytes(varint_buf, 8);
	}

	private void appendFloat(Float f) {
		appendByte(SerealHeader.SRL_HDR_FLOAT);

		int bits = Float.floatToIntBits( f ); // very convienent, thanks Java guys! :)
		for (int i = 0; i < 4; i++) {
			varint_buf[i] = (byte) ((bits >> (i * 8)) & 0xff);
		}
		appendBytes(varint_buf, 4);
	}

	private void appendRefp(long location) {
		if (debugTrace) trace( "Emitting a REFP for location " + location );

		setTrackBit(location);
		appendByte(SerealHeader.SRL_HDR_REFP);
		appendVarint(location);
	}

	private boolean tryAppendRefp(Object obj) {
		long location = getTrackedItem(obj);

		if (location != IdentityMap.NOT_FOUND) {
			appendRefp(location);

			return true;
		} else {
			return false;
		}
	}

	private void appendAlias(long location) {
		if (debugTrace) trace( "Emitting an ALIAS for location " + location );

		setTrackBit(location);
		appendByte(SerealHeader.SRL_HDR_ALIAS);
		appendVarint(location);
	}

	private void appendMap(Map<Object, Object> hash) throws SerealException {
		if (debugTrace) trace( "Writing hash: " + Utils.dump( hash ) );

		if (!perlRefs) {
			appendByte(SerealHeader.SRL_HDR_REFN);
			track(hash, size);
		}
		appendByte(SerealHeader.SRL_HDR_HASH);
		appendVarint(hash.size());

		for (Map.Entry<Object, Object> entry : hash.entrySet()) {
			encode(entry.getKey().toString());
			encode(entry.getValue());
		}
	}

	private void appendRef(PerlReference ref) throws SerealException {
		if (debugTrace) trace( "Emitting a REFN for @" + System.identityHashCode( ref ) + " -> @" + System.identityHashCode( ref.getValue() ) );

		Object refValue = ref.getValue();

		appendByte(SerealHeader.SRL_HDR_REFN);
		track(refValue, size);
		encode(refValue);
	}

	private void appendArray(Object obj) throws SerealException {
		// checking length without casting to Object[] since they might primitives
		int count = Array.getLength(obj);

		if (debugTrace) trace( "Emitting an array of length " + count );

		if (!perlRefs) {
			appendByte(SerealHeader.SRL_HDR_REFN);
			track(obj, size);
		}
		appendByte(SerealHeader.SRL_HDR_ARRAY);
		appendVarint(count);

		// write the objects (works for both Objects and primitives)
		for (int index = 0; index < count; index++) {
			if (debugTrace) trace( "Emitting array index " + index + " ("
					+ (Array.get( obj, index ) == null ? " NULL/undef" : Array.get( obj, index ).getClass().getSimpleName()) + ")" );
			encode(Array.get(obj, index));
		}
	}

	private void appendArray(Object[] array) throws SerealException {
		int count = array.length;

		if (debugTrace) trace( "Emitting an array of length " + count );

		if (!perlRefs) {
			appendByte(SerealHeader.SRL_HDR_REFN);
			track(array, size);
		}
		appendByte(SerealHeader.SRL_HDR_ARRAY);
		appendVarint(count);

		for (Object item : array) {
			if (debugTrace) trace( "Emitting array item: " + " ("
					+ (item == null ? " NULL/undef" : item.getClass().getSimpleName()) + ")" );
			encode(item);
		}
	}

	private void appendArray(List<Object> list) throws SerealException {
		int count = list.size();

		if (debugTrace) trace( "Emitting an array of length " + count );

		if (!perlRefs) {
			appendByte(SerealHeader.SRL_HDR_REFN);
			track(list, size);
		}
		appendByte(SerealHeader.SRL_HDR_ARRAY);
		appendVarint(count);

		for (Object item : list) {
			if (debugTrace) trace( "Emitting array item: " + " ("
					+ (item == null ? " NULL/undef" : item.getClass().getSimpleName()) + ")" );
			encode(item);
		}
	}

	private Charset charset_utf8 = Charset.forName( "UTF-8" );

	private void appendStringType(byte[] bytes) throws SerealException {
		if (debugTrace) trace( "Encoding byte array as latin1: " + bytes );
		if (bytes.length < SerealHeader.SRL_MASK_SHORT_BINARY_LEN) {
			appendShortBinary(bytes);
		} else {
			appendBinary(bytes);
		}
	}

	private void appendStringType(Latin1String str) throws SerealException {
		if (debugTrace) trace( "Encoding as latin1: " + str );
		byte[] latin1 = str.getBytes();
		if (str.length() < SerealHeader.SRL_MASK_SHORT_BINARY_LEN) {
			appendShortBinary(latin1);
		} else {
			appendBinary(latin1);
		}
	}

	private void appendStringType(String str) throws SerealException {
		if (debugTrace) trace( "Encoding as utf8: " + str );

		// maybe we can just COPY
		long copyOffset = getTrackedItemCopy(str);
		if (copyOffset != StringCopyMap.NOT_FOUND) {
			appendCopy(copyOffset);
			return;
		}

		long location = size;

		byte[] utf8 = ((String) str).getBytes(charset_utf8);
		appendByte(SerealHeader.SRL_HDR_STR_UTF8);
		appendVarint(utf8.length);
		appendBytes(utf8);

		trackForCopy(str, location);
	}

	private void appendNumber(long l) {
		if (l < 0) {
			if (l > -17) {
				appendByte((byte) (SerealHeader.SRL_HDR_NEG_LOW | (l + 32)));
			} else {
				appendZigZag(l);
			}
		} else {
			if (l < 16) {
				appendByte((byte) (SerealHeader.SRL_HDR_POS_LOW | l));
			} else {
				appendByte(SerealHeader.SRL_HDR_VARINT);
				appendVarint(l);
			}
		}

	}

	/**
	 * Discard all previous tracking clear the buffers etc
	 * Call this when you reuse the encoder
	 */
	private void reset() {
		size = compressedSize = headerSize = 0;
		resetTracked();
	}

	private void resetTracked() {
		tracked.clear();
		trackedBytearrayCopy.clear();
		trackedStringCopy.clear();
		if (perlAlias) {
			aliases.clear();
			maybeAliases.clear();
		}
		trackedClassnames.clear();
	}

	private void ensureAvailable(int required) {
		long total = required + size;

		if (total > bytes.length)
			bytes = Arrays.copyOf(bytes, (int) (total * 3 / 2));
	}

	private void appendBytes(byte[] data) {
		ensureAvailable(data.length);
		appendBytesUnsafe(data);
	}

	private void appendBytes(byte[] data, int length) {
		ensureAvailable(length);
		appendBytesUnsafe(data, length);
	}

	private void appendBytesUnsafe(byte[] data) {
		System.arraycopy(data, 0, bytes, (int) size, data.length);
		size += data.length;
	}

	private void appendBytesUnsafe(byte[] data, int length) {
		System.arraycopy(data, 0, bytes, (int) size, length);
		size += length;
	}

	private void appendByte(byte data) {
		ensureAvailable(1);
		appendByteUnsafe(data);
	}

	private void appendByteUnsafe(byte data) {
		bytes[(int) size] = data;
		size++;
	}
}
