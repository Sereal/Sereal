package com.booking.sereal;

import static com.booking.sereal.DecoderOptions.ObjectType;

import com.booking.sereal.impl.RefpMap;

import java.io.IOException;
import java.lang.ref.WeakReference;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.charset.Charset;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.regex.Pattern;
import java.util.zip.DataFormatException;
import java.util.zip.Inflater;

import org.xerial.snappy.Snappy;

/**
 * WIP Decoder for Sereal WIP
 */
public class Decoder implements SerealHeader {

	private static final DecoderOptions DEFAULT_OPTIONS = new DecoderOptions();

	boolean debugTrace;

	void trace(String info) {
		if (!debugTrace)
			throw new RuntimeException("All calls to trace() must be guarded with 'if (debugTrace)'");
		System.out.println( info );
	}

	private ByteBuffer data;

	// where we track items for REFP purposes
	private RefpMap tracked = new RefpMap();

	private int protocolVersion = -1;
	private int encoding = -1;
	private int baseOffset = Integer.MAX_VALUE;
	private long userHeaderPosition = -1;
	private long userHeaderSize = -1;

	private ObjectType objectType;

	private final boolean perlRefs;
	private final boolean perlAlias;
	private final boolean preserveUndef;
	private final boolean refuseSnappy;
	private final boolean preferLatin1;

	private ByteBuffer realData;
	private Inflater inflater;

	private Charset charset_utf8 = Charset.forName("UTF-8");

	/**
	 * Create a new Decoder
	 */
	public Decoder() {
		this(DEFAULT_OPTIONS);
	}

	public Decoder(DecoderOptions options) {
		objectType = options.objectType();
		perlRefs = options.perlReferences();
		perlAlias = options.perlAliases();
		preserveUndef = options.preserveUndef();
		refuseSnappy = options.refuseSnappy();
		preferLatin1 = options.preferLatin1();
	}

	private void checkHeader() throws SerealException {

		if( data.remaining() < 4 ) {
			throw new SerealException( "Invalid Sereal header: too few bytes" );
		}

		int magic = data.getInt();
		if (magic != MAGIC && magic != MAGIC_V3) {
			throw new SerealException( "Invalid Seareal header: doesn't match magic" );
		}

	}

	private void checkHeaderSuffix() {
		long suffix_size = read_varint();
		long basePosition = data.position();

		if (debugTrace) trace( "Header suffix size: " + suffix_size );

		userHeaderSize = 0;
		if (suffix_size > 0) {
			byte bitfield = data.get();

			if ((bitfield & 0x01) == 0x01) {
				userHeaderPosition = data.position();
				userHeaderSize = suffix_size - 1;
			}
		}

		// skip everything in the optional suffix part
		data.position( (int) (basePosition + suffix_size) );
	}

	private void checkNoEOD() throws SerealException {

		if( data.remaining() == 0 ) {
			throw new SerealException( "Unexpected end of data at byte " + data.limit() );
		}

	}

	private void checkProtoAndFlags() throws SerealException {

		if( data.remaining() < 1 ) {
			throw new SerealException( "Invalid Sereal header: no protocol/version byte" );
		}

		int protoAndFlags = data.get();
		protocolVersion = protoAndFlags & 15; // 4 bits for version

		if (debugTrace) trace( "Version: " + protocolVersion );
		if( protocolVersion < 0 || protocolVersion > 3 ) {
			throw new SerealException( String.format( "Invalid Sereal header: unsupported protocol version %d", protocolVersion ) );
		}

		encoding = (protoAndFlags & ~15) >> 4;
		if (debugTrace) trace( "Encoding: " + encoding );
		if((encoding == 1 || encoding == 2) && refuseSnappy) {
			throw new SerealException( "Unsupported encoding: Snappy" );
		} else if(encoding < 0 || encoding > 3) {
			throw new SerealException( "Unsupported encoding: unknown");
		}
	}

	public boolean hasHeader() throws SerealException {
		parseHeader();

		return userHeaderSize > 0;
	}

	public long headerSize() throws SerealException {
		parseHeader();

		return userHeaderSize > 0 ? userHeaderSize : 0;
	}

	public Object decodeHeader() throws SerealException {
		parseHeader();

		if (userHeaderSize <= 0)
			throw new SerealException("Sereal user header not present");
		ByteBuffer originalData = data;
		int originalPosition = data.position(), originalLimit = data.limit();
		try {
			data = realData;
			data.limit((int) (userHeaderPosition + userHeaderSize));
			data.position((int) userHeaderPosition);

			return readSingleValue();
		} finally {
			data = originalData;
			data.limit(originalLimit);
			data.position(originalPosition);
			resetTracked();
		}
	}

	private void parseHeader() throws SerealException {
		if (userHeaderSize >= 0)
			return;

		checkHeader();
		checkProtoAndFlags();
		checkHeaderSuffix();
	}

	/**
	 *
	 * @return deserealized object
	 * @throws SerealException
	 * @throws IOException 
	 */
	public Object decode() throws SerealException, IOException {

		if( data == null ) {
			throw new SerealException( "No data set" );
		}

		if (debugTrace) trace( "Decoding: " + data.toString() + " - " + new String( data.array() ) );

		parseHeader();

		if( encoding != 0 ) {
			if (encoding == 1 || encoding == 2)
				uncompressSnappy();
			else
				uncompressZlib();
			if (protocolVersion == 1)
				baseOffset = 0;
			else
				// because offsets start at 1
				baseOffset = -1;
		} else {
			if (protocolVersion == 1)
				baseOffset = 0;
			else
				// because offsets start at 1
				baseOffset = data.position() - 1;
		}
		Object out = readSingleValue();

		if (debugTrace) trace( "Read: " + out );
		if (debugTrace) trace( "Data left: " + (realData.limit() - realData.position()) );

		return out;
	}

	private void uncompressSnappy() throws IOException, SerealException {
		int len = realData.limit() - realData.position();
		int pos = protocolVersion == 1 ? realData.position() : 0;

		if(encoding == 2) {
			len = (int) read_varint();
		}
		byte[] compressed = new byte[len];
		realData.get( compressed, 0, len );
		byte[] uncompressed = new byte[pos + Snappy.uncompressedLength( compressed, 0, len ) ];
		if(!Snappy.isValidCompressedBuffer( compressed)) {
			throw new SerealException("Invalid snappy data");
		}
		Snappy.uncompress( compressed, 0, len, uncompressed, pos );
		this.data = ByteBuffer.wrap( uncompressed );
		this.data.position(pos);
	}

	private void uncompressZlib() throws SerealException {
		if (inflater == null)
			inflater = new Inflater();
		inflater.reset();

		long uncompressedLength = read_varint();
		long compressedLength = read_varint();
		byte[] compressed = new byte[(int) compressedLength];
		realData.get(compressed, 0, (int) compressedLength);
		inflater.setInput(compressed);
		byte[] uncompressed = new byte[(int) uncompressedLength];
		try {
			int inflatedSize = inflater.inflate(uncompressed);
		} catch (DataFormatException e) {
			throw new SerealException(e);
		}
		this.data = ByteBuffer.wrap(uncompressed);
		this.data.position(0);
	}

	/**
	 * if tag == 0, next is varint for number of elements,
	 * otherwise lower 4 bits are length
	 *
	 * @param tag
	 *           : lower 4 bits is length or 0 for next varint is length
	 * @param track we might need to track since array elements could refer to us
	 * @return
	 * @throws SerealException
	 */
	private Object[] read_array(byte tag, int track) throws SerealException {

		int length = 0;
		if( tag == 0 ) {
			length = (int) read_varint();
		} else {
			length = tag & 15;
		}

		if (debugTrace) trace( "Array length: " + length );

		Object[] out = new Object[length];
		if( track != 0 ) { // track ourself
			track_stuff( track, out );
		}

		for(int i = 0; i < length; i++) {
			out[i] = readSingleValue();
			if (debugTrace) trace( "Read array element " + i + ": " + Utils.dump( out[i] ) );
		}

		return out;
	}

	/**
	 * Reads a byte array, but was called read_binary in C, so for grepping purposes I kept the name
	 * 
	 * For some reason we call them Latin1Strings.
	 * @return
	 */
	byte[] read_binary() {

		int length = (int) read_varint();
		byte[] out = new byte[length];
		for(int i = 0; i < length; i++) {
			out[i] = data.get();
		}

		return out;
	}

	private Map<String, Object> read_hash(byte tag, int track) throws SerealException {
		long num_keys = 0;
		if( tag == 0 ) {
			num_keys = read_varint();
		} else {
			num_keys = tag & 15;
		}

		Map<String, Object> hash = new HashMap<String, Object>( (int) num_keys );
        if( track != 0 ) { // track ourself
            track_stuff( track, hash );
        }

		if (debugTrace) trace( "Reading " + num_keys + " hash elements" );

		for(int i = 0; i < num_keys; i++) {
            Object keyObject = readSingleValue();
            CharSequence key;
            if(keyObject instanceof CharSequence) {
                key = (CharSequence) keyObject;
            } else if(keyObject instanceof byte[]) {
                key = new Latin1String((byte[]) keyObject);
            } else {
                throw new SerealException("A key is expected to be a byte or character sequence, but got " + keyObject.toString());
            }
			Object val = readSingleValue();
			hash.put( key.toString(), val );
		}

		return hash;
	}

	private Object get_tracked_item() {
		long offset = read_varint();
		if (debugTrace) trace( "Creating ref to item previously read at offset: " + offset + " which is: " + tracked.get( offset ) );
		if (debugTrace) trace( "Currently tracked: " + Utils.dump( tracked ) );
		return tracked.get( offset );
	}

	// top bit set (0x80) means next byte is 7 bits more more varint
	long read_varint() {

		long uv = 0;
		int lshift = 0;

		byte b = data.get();
		while( data.hasRemaining() && (b < 0) ) {
			uv |= ((long) b & 127) << lshift; // add 7 bits
			lshift += 7;
			b = data.get();
		}
		uv |= (long) b << lshift; // add final (or first if there is only 1)

		return uv;

	}

	Object readSingleValue() throws SerealException {

		checkNoEOD();

		byte tag = data.get();

		int track = 0;
		if( (tag & SRL_HDR_TRACK_FLAG) != 0 ) {
			tag = (byte) (tag & ~SRL_HDR_TRACK_FLAG);
			track = data.position() - 1 - baseOffset;
			if (debugTrace) trace( "Tracking stuff at position: " + track );
		}

		if (debugTrace) trace( "Tag: " + (tag & 0xFF) + " = " + Utils.hexStringFromByteArray( new byte[] { tag } ) );
		Object out;

		if( tag <= SRL_HDR_POS_HIGH ) {
			if (debugTrace) trace( "Read small positive int:" + tag );
			out = (long) tag;
		} else if( tag <= SRL_HDR_NEG_HIGH ) {
			if (debugTrace) trace( "Read small negative int:" + (tag - 32) );
			out = (long) (tag - 32);
		} else if( (tag & SRL_HDR_SHORT_BINARY_LOW) == SRL_HDR_SHORT_BINARY_LOW ) {
			byte[] short_binary = read_short_binary( tag );
			if (debugTrace) trace( "Read short binary: " + short_binary + " length " + short_binary.length );
            out = preferLatin1 ? new Latin1String(short_binary) : short_binary;
		} else if( (tag & SRL_HDR_HASHREF) == SRL_HDR_HASHREF ) {
			Map<String, Object> hash = read_hash( tag, track );
			if (debugTrace) trace( "Read hash: " + hash );
			if (perlRefs) {
				out = new PerlReference(hash);
			} else {
				out = hash;
			}
		} else if( (tag & SRL_HDR_ARRAYREF) == SRL_HDR_ARRAYREF ) {
			if (debugTrace) trace( "Reading arrayref" );
			Object[] arr = read_array( tag, track );
			if (debugTrace) trace( "Read arrayref: " + arr );
			if (perlRefs) {
				out = new PerlReference(arr);
			} else {
				out = arr;
			}
		} else {
			switch (tag) {
			case SRL_HDR_VARINT:
				long l = read_varint();
				if (debugTrace) trace( "Read varint: " + l );
				out = l;
				break;
			case SRL_HDR_ZIGZAG:
				long zz = read_zigzag();
				if (debugTrace) trace( "Read zigzag: " + zz );
				out = zz;
				break;
			case SRL_HDR_FLOAT:
				// Java defaults to BE, maybe we can jsut do this generally, don't know yet (but think so)
				data.order( ByteOrder.LITTLE_ENDIAN );
				float f = data.getFloat();
				data.order( ByteOrder.BIG_ENDIAN );
				if (debugTrace) trace( "Read float: " + f );
				out = f;
				break;
			case SRL_HDR_DOUBLE:
				// Java defaults to BE, maybe we can jsut do this generally, don't know yet (but think so)
				data.order( ByteOrder.LITTLE_ENDIAN );
				double d = data.getDouble();
				data.order( ByteOrder.BIG_ENDIAN );
				if (debugTrace) trace( "Read double: " + d );
				out = d;
				break;
			case SRL_HDR_TRUE:
				if (debugTrace) trace( "Read: TRUE" );
				out = true;
				break;
			case SRL_HDR_FALSE:
				if (debugTrace) trace( "Read: FALSE" );
				out = false;
				break;
			case SRL_HDR_UNDEF:
				if (debugTrace) trace( "Read a null/undef" );
				if (preserveUndef)
					out = new PerlUndef();
				else
					out = null;
				break;
			case SRL_HDR_CANONICAL_UNDEF:
				if (debugTrace) trace( "Read a null/undef" );
				if (preserveUndef)
					out = PerlUndef.CANONICAL;
				else
					out = null;
				break;
			case SRL_HDR_BINARY:
				byte[] bytes = read_binary();
				if (debugTrace) trace( "Read binary: " + bytes );
				out = preferLatin1 ? new Latin1String(bytes) : bytes;
				break;
			case SRL_HDR_STR_UTF8:
				String utf8 = read_UTF8();
				if (debugTrace) trace( "Read UTF8: " + utf8 );
				out = utf8;
				break;
			case SRL_HDR_REFN:
				if (debugTrace) trace( "Reading ref to next" );
				if( perlRefs ) {
					PerlReference refn = new PerlReference(null);
					// track early for weak references
					if( track != 0 ) { // track ourself
						track_stuff( track, refn );
					}
					refn.setValue(readSingleValue());
					out = refn;
				} else {
					out = readSingleValue();
				}
				if (debugTrace) trace( "Read ref: " + Utils.dump( out ) );
				break;
			case SRL_HDR_REFP:
				if (debugTrace) trace( "Reading REFP (ref to prev)" );
				long offset_prev = read_varint();
				Object prv_value = tracked.get(offset_prev);
				if (prv_value == RefpMap.NOT_FOUND) {
					throw new SerealException( "REFP to offset " + offset_prev + ", which is not tracked" );
				}
				Object prev = perlRefs ? new PerlReference(prv_value) : prv_value;
				if (debugTrace) trace( "Read prev: " + Utils.dump( prev ) );
				out = prev;
				break;
			case SRL_HDR_OBJECT:
				if (debugTrace) trace( "Reading an object" );
				Object obj = read_object();
				if (debugTrace) trace( "Read object: " + obj );
				out = obj;
				break;
			case SRL_HDR_OBJECTV:
				if (debugTrace) trace( "Reading an objectv" );
				CharSequence className = (CharSequence) get_tracked_item();
				if (debugTrace) trace( "Read an objectv of class: " + className);
				out = new PerlObject( ((Latin1String)className).getString(), readSingleValue() );
				break;
			case SRL_HDR_COPY:
				if (debugTrace) trace( "Reading a copy" );
				Object copy = read_copy();
				if (debugTrace) trace( "Read copy: " + copy );
				out = copy;
				break;
			case SRL_HDR_ALIAS:
				if (debugTrace) trace("Reading an alias");
				Object value = get_tracked_item();

				if (perlAlias) {
					out = new PerlAlias(value);
				} else {
					out = value;
				}
				if (debugTrace) trace( "Read alias: " + Utils.dump( out ) );
				break;
			case SRL_HDR_WEAKEN:
				if (debugTrace) trace("Weakening the next thing");
				// so the next thing HAS to be a ref (afaict) which means we can track it
				PerlReference placeHolder = new PerlReference(null);
				// track early for weak references
				if( track != 0 ) { // track ourself
					track_stuff( track, placeHolder );
				}
				placeHolder.setValue(((PerlReference)readSingleValue()).getValue());
				WeakReference<PerlReference> wref = new WeakReference<PerlReference>( placeHolder );
				out = wref;
				break;
			case SRL_HDR_HASH:
				Object hash = read_hash( (byte) 0, track );
				if (debugTrace) trace( "Read hash: " + hash );
				out = hash;
				break;
			case SRL_HDR_ARRAY:
				if (debugTrace) trace( "Reading array" );
				Object[] arr = read_array( (byte) 0, track );
				if (debugTrace) trace( "Read array: " + Utils.dump( arr ) );
				out = arr;
				break;
			case SRL_HDR_REGEXP:
				if (debugTrace) trace( "Reading Regexp" );
				Pattern pattern = read_regex();
				if (debugTrace) trace( "Read regexp: " + pattern );
				out = pattern;
				break;
                       case SRL_HDR_PAD:
                               if (debugTrace) trace("Padding byte: skip");
                               return readSingleValue();
			default:
				throw new SerealException( "Tag not supported: " + tag );
			}
		}

		if( track != 0 ) { // we double-track arrays ATM (but they just overwrite)
			track_stuff( track, out );
		}
		if (debugTrace) trace( "returning: " + out );

		return out;

	}


	/**
	 * Read a short binary ISO-8859-1 (latin1) string, the lower bits of the tag hold the length
	 * @param tag
	 * @return
	 */
	byte[] read_short_binary(byte tag) {
		int length = tag & SRL_MASK_SHORT_BINARY_LEN;
		if (debugTrace) trace( "Short binary, length: " + length );
		byte[] buf = new byte[length];
		data.get( buf );
		return buf;
	}

	/**
	 * From the spec:
	 * Sometimes it is convenient to be able to reuse a previously emitted sequence in the packet to reduce duplication. For instance a data structure with many
	 * hashes with the same keys. The COPY tag is used for this. Its argument is a varint which is the offset of a previously emitted tag, and decoders are to
	 * behave as though the tag it references was inserted into the packet stream as a replacement for the COPY tag.
	 *
	 * Note, that in this case the track flag is not set. It is assumed the decoder can jump back to reread the tag from its location alone.
	 *
	 * Copy tags are forbidden from referring to another COPY tag, and are also forbidden from referring to anything containing a COPY tag, with the exception
	 * that a COPY tag used as a value may refer to an tag that uses a COPY tag for a classname or hash key.
	 *
	 * @return
	 * @throws SerealException
	 */
	Object read_copy() throws SerealException {

		int originalPosition = (int) read_varint();
		int currentPosition = data.position(); // remember where we parked

		// note: you might think you'd like to use mark() and reset(), but setting position(..) discards the mark
		data.position( originalPosition + baseOffset );
		Object copy = readSingleValue();
		data.position( currentPosition ); // go back to where we were

		return copy;
	}

	private String read_UTF8() {
		int length = (int) read_varint();

		if (data.hasArray()) {
			int position = data.position();

			data.position(position + length);

			return new String(data.array(), data.arrayOffset() + position, length, charset_utf8);
		} else {
			byte[] buf = new byte[length];
			data.get( buf );
			return new String(buf, charset_utf8);
		}
	}

	long read_zigzag() {

		long n = read_varint();

		return (n >>> 1) ^ (-(n & 1)); // note the unsigned right shift
	}

	Pattern read_regex() throws SerealException {

		int flags = 0;
		Object str = readSingleValue();
        String regex;
        if(str instanceof CharSequence) {
            regex = ((CharSequence) str).toString();
        } else if (str instanceof byte[]) {
            regex = (new Latin1String((byte[]) str)).toString();
        } else {
            throw new SerealException("Regex has to be built from a char or byte sequence");
        }
		if (debugTrace) trace( "Read pattern: " + regex );

		// now read modifiers
		byte tag = data.get();
		if( (tag & SRL_HDR_SHORT_BINARY_LOW) == SRL_HDR_SHORT_BINARY_LOW ) {
			int length = tag & SRL_MASK_SHORT_BINARY_LEN;
			while( length-- > 0 ) {
				byte value = data.get();
				switch (value) {
				case 'm':
					flags = flags | Pattern.MULTILINE;
					break;
				case 's':
					flags = flags | Pattern.DOTALL;
					break;
				case 'i':
					flags = flags | Pattern.CASE_INSENSITIVE;
					break;
				case 'x':
					flags = flags | Pattern.COMMENTS;
					break;
				case 'p':
					// ignored
					break;
				default:
					throw new SerealException( "Unknown regex modifier: " + value );
				}

			}
		} else {
			throw new SerealException( "Expecting SRL_HDR_SHORT_BINARY for modifiers of regexp, got: " + tag );
		}

		return Pattern.compile( regex, flags );
	}

	private Object read_object() throws SerealException {

		// first read the classname
		// Maybe we should have some kind of read_string() method?
		int position = data.position();
		byte tag = data.get();
		Latin1String className;
		if( (tag & SRL_HDR_SHORT_BINARY_LOW) == SRL_HDR_SHORT_BINARY_LOW ) {
			int length = tag & SRL_MASK_SHORT_BINARY_LEN;
			byte[] buf = new byte[length];
			data.get( buf );
			className = new Latin1String( new String( buf ) );
		} else {
			throw new SerealException( "Don't know how to read classname from tag" + tag );
		}
		// apparently class names do not need a track_bit set to be the target of objectv's. WTF
		track_stuff( position - baseOffset, className );

		if (debugTrace) trace( "Object Classname: " + className );

		// now read the struct (better be a hash!)
		Object structure = readSingleValue();
		if (debugTrace) trace( "Object Type: " + structure.getClass().getName() );
		if( structure instanceof Map ) {
			// now "bless" this into a class, perl style
			@SuppressWarnings("unchecked")
			Map<String, Object> classData = (Map<String, Object>) structure;
			try {
				// either an existing java class
				Class<?> c = Class.forName( className.getString() );
				return Utils.bless( c, classData );
			} catch (ClassNotFoundException e) {
				// or we make a new one
				if( objectType == ObjectType.POJO ) {
					return Utils.bless( className.getString(), classData );
				} else {
					// or we make a Perl-style one
					return new PerlObject( className.getString(), classData );
				}

			}
		} else if( structure.getClass().isArray() ) {
			// nothing we can really do here except make Perl objects..
			return new PerlObject( className.getString(), structure );
		} else if( structure instanceof PerlReference ) {
			return new PerlObject( className.getString(), structure);
		}

		// it's a regexp for example
		return structure;

	}

	/**
	 * Set the data to deserealize
	 * (for calling decode multiple times when there are concatenated packets)
	 * (never tested)
	 *
	 * @param blob
	 */
	public void setData(ByteBuffer blob) {
		reset();
		this.data = this.realData = blob;
		data.rewind();
	}

	private void track_stuff(int pos, Object ref) {
		if (debugTrace) trace( "Saving " + ref + " at offset " + pos );
		tracked.put( pos, ref );
	}

	private void reset() {
		data = realData = null;
		protocolVersion = encoding = -1;
		baseOffset = Integer.MAX_VALUE;
		userHeaderPosition = userHeaderSize = -1;
		resetTracked();
	}

	private void resetTracked() {
		tracked.clear();
	}
}
