package com.booking.sereal;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.lang.ref.WeakReference;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.charset.Charset;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.regex.Pattern;

/**
 * WIP Decoder for Sereal WIP
 */
public class Decoder implements SerealHeader {

	public enum ObjectType {
		PERL_OBJECT, // Perl style object (name + hash)
		POJO // Dynamically compile a Plain Old Java Object
	}

	// set up logging: be compatible with log4j etc but not suffer so much :)
	interface Log {
		void info(String info);

		void fine(String info);

		void setLevel(Level lvl);
	}

	Log log = new Log() {

		private Level level = Level.INFO;

		@Override
		public void info(String info) {
			System.out.println( "INFO: " + info );
		}

		@Override
		public void fine(String info) {
			if( level.intValue() <= Level.FINE.intValue() ) {
				System.out.println( info );
			}
		}

		@Override
		public void setLevel(Level lvl) {
			this.level = lvl;
		}

	};

	// end logging

	/**
	 * Decodes a sereal
	 *
	 * @param f
	 *           data to decode
	 * @param options
	 *           options like Snappy or not
	 * @return
	 * @throws SerealException
	 * @throws IOException
	 */
	public static Object decode_sereal(File f, Map<String, Object> options) throws SerealException, IOException {

		Decoder d = new Decoder( options );

		return d.decodeFile( f );
	}

	public Object decodeFile(File f) throws SerealException, IOException {

		log.fine( "Decoding: " + f.getName() );

		if( !f.exists() ) {
			throw new FileNotFoundException( "No such file: " + f.getCanonicalPath() );
		}

		// read everything
		int size = (int) f.length(); // yeah yeah truncate
		log.fine( "File size: " + size );
		ByteBuffer buf = ByteBuffer.allocate( size );
		new FileInputStream( f ).getChannel().read( buf );
		log.fine( "Raw: " + new String( buf.array() ) );

		setData( buf );
		Object structure = decode();
		log.fine( "Decoded: " + Utils.dump( structure ) );

		return structure;
	}

	private Map<String, Object> options; // options (currently do not accept any)

	private Map<String, Object> properties = new HashMap<String, Object>(); // where
																									// we
																									// save
																									// protocol
																									// version
																									// and
																									// encoding

	private ByteBuffer data;

	// where we track items for REFP purposes
	private Map<String, Object> tracked = new HashMap<String, Object>();

	private ObjectType objectType;

	private boolean perlRefs = false;

	private boolean preservePadding = false;

	/**
	 * Create a new Decoder
	 *
	 * @param options
	 *           object_type: ObjectType (defaults to PERL_OBJECT)
	 *           use_perl_refs: if true wraps things in References to we can "perfectly" roundtrip
	 */
	public Decoder(Map<String, Object> options) {
		this.options = options == null ? new HashMap<String, Object>() : options;

		objectType = this.options.containsKey( "object_type" ) ? ((ObjectType) this.options.get( "object_type" )) : ObjectType.PERL_OBJECT;
		perlRefs = this.options.containsKey( "use_perl_refs" ) ? ((Boolean) this.options.get( "use_perl_refs" )) : false;
		preservePadding = this.options.containsKey( "preserve_pad_tags" ) ? ((Boolean) this.options.get( "preserve_pad_tags" )) : false;
	}

	private void checkHeader() throws SerealException {

		if( data.limit() < 4 ) {
			throw new SerealException( "Invalid Sereal header: too few bytes" );
		}

		if( data.getInt() != MAGIC ) {
			throw new SerealException( "Invalid Seareal header: doesn't match magic" );
		}

	}

	private void checkHeaderSuffix() {

		long suffix_size = read_varint();

		log.fine( "Header suffix size: " + suffix_size );

		// skip everything in the optional suffix part
		data.position( (int) (data.position() + suffix_size) );

	}

	private void checkNoEOD() throws SerealException {

		if( data.remaining() == 0 ) {
			throw new SerealException( "Unexpected end of data at byte " + data.limit() );
		}

	}

	private void checkProtoAndFlags() throws SerealException {

		if( data.limit() < 1 ) {
			throw new SerealException( "Invalid Sereal header: no protocol/version byte" );
		}

		int protoAndFlags = data.get();
		int protocolVersion = protoAndFlags & 15; // 4 bits for version

		log.fine( "Version: " + protocolVersion );
		if( protocolVersion != 1 ) {
			throw new SerealException( String.format( "Invalid Sereal header: unsupported protocol version %d", protocolVersion ) );
		}
		properties.put( "protocol_version", protocolVersion );

		int encoding = protoAndFlags & ~15;
		log.fine( "Encoding: " + encoding );
		if( encoding == 1 && options.containsKey( "snappy_support" ) ) {
			throw new SerealException( "Unsupported encoding: Snappy" );
		}
		properties.put( "encoding", encoding );

	}

	/**
	 *
	 * @return deserealized object
	 * @throws SerealException
	 */
	public Object decode() throws SerealException {

		if( data == null ) {
			throw new SerealException( "No data set" );
		}

		log.fine( "Decoding: " + data.toString() + " - " + new String( data.array() ) );

		checkHeader();
		checkProtoAndFlags();
		checkHeaderSuffix();

		Object out = readSingleValue();

		log.fine( "Read: " + out );
		log.fine( "Data left: " + (data.limit() - data.position()) );

		return out;
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

		log.fine( "Array length: " + length );

		Object[] out = new Object[length];
		if( track != 0 ) { // track ourself
			track_stuff( track, out );
		}

		for(int i = 0; i < length; i++) {
			out[i] = readSingleValue();
			log.fine( "Read array element " + i + ": " + Utils.dump( out[i] ) );
		}

		return out;
	}

	/**
	 * Reads a byte array, but was called read_binary in C, so for grepping porposes I kept the name
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

	private Map<CharSequence, Object> read_hash(byte tag) throws SerealException {
		long num_keys = 0;
		if( tag == 0 ) {
			num_keys = read_varint();
		} else {
			num_keys = tag & 15;
		}

		Map<CharSequence, Object> hash = new LinkedHashMap<CharSequence, Object>( (int) num_keys );

		log.fine( "Reading " + num_keys + " hash elements" );

		for(int i = 0; i < num_keys; i++) {
			CharSequence key = (CharSequence) readSingleValue();
			Object val = readSingleValue();
			hash.put( key, val );
		}

		return hash;
	}

	private Object get_tracked_item() {
		long offset = read_varint();
		log.fine( "Creating ref to item previously read at offset: " + offset + " which is: " + tracked.get( "track_" + offset ) );
		log.fine( "keys: " + tracked.keySet() + " vals: " + tracked.values() );
		return tracked.get( "track_" + offset );
	}

	// top bit set (0x80) means next byte is 7 bits more more varint
	long read_varint() {

		long uv = 0;
		int lshift = 0;

		byte b = data.get();
		while( data.hasRemaining() && (b < 0) ) {
			uv |= (b & 127) << lshift; // add 7 bits
			lshift += 7;
			b = data.get();
		}
		uv |= b << lshift; // add final (or first if there is only 1)

		return uv;

	}

	Object readSingleValue() throws SerealException {

		checkNoEOD();

		byte tag = data.get();

		int track = 0;
		if( (tag & SRL_HDR_TRACK_FLAG) != 0 ) {
			tag = (byte) (tag & ~SRL_HDR_TRACK_FLAG);
			track = data.position() - 1;
			log.fine( "Tracking stuff at position: " + track );
		}

		log.fine( "Tag: " + (tag & 0xFF) + " = " + Utils.hexStringFromByteArray( new byte[] { tag } ) );
		Object out;

		if( tag <= SRL_HDR_POS_HIGH ) {
			log.fine( "Read small positive int:" + tag );
			out = tag;
		} else if( tag <= SRL_HDR_NEG_HIGH ) {
			log.fine( "Read small negative int:" + (tag - 32) );
			out = tag - 32;
		} else if( (tag & SRL_HDR_SHORT_BINARY_LOW) == SRL_HDR_SHORT_BINARY_LOW ) {
			CharSequence short_binary = read_short_binary( tag );
			log.fine( "Read short binary: " + short_binary + " length " + short_binary.length() );
			out = short_binary;
		} else if( (tag & SRL_HDR_HASHREF) == SRL_HDR_HASHREF ) {
			Map<CharSequence, Object> hash = read_hash( tag );
			log.fine( "Read hash: " + hash );
			out = hash;
		} else if( (tag & SRL_HDR_ARRAYREF) == SRL_HDR_ARRAYREF ) {
			log.fine( "Reading arrayref" );
			Object[] arr = read_array( tag, track );
			log.fine( "Read arrayref: " + arr );
			out = arr;
		} else {
			switch (tag) {
			case SRL_HDR_VARINT:
				long l = read_varint();
				log.fine( "Read varint: " + l );
				out = l;
				break;
			case SRL_HDR_ZIGZAG:
				long zz = read_zigzag();
				log.fine( "Read zigzag: " + zz );
				out = zz;
				break;
			case SRL_HDR_DOUBLE:
				// Java defaults to BE, maybe we can jsut do this generally, don't know yet (but think so)
				data.order( ByteOrder.LITTLE_ENDIAN );
				double d = data.getDouble();
				data.order( ByteOrder.BIG_ENDIAN );
				log.fine( "Read double: " + d );
				out = d;
				break;
			case SRL_HDR_TRUE:
				log.fine( "Read: TRUE" );
				out = true;
				break;
			case SRL_HDR_FALSE:
				log.fine( "Read: FALSE" );
				out = false;
				break;
			case SRL_HDR_UNDEF:
				log.fine( "Read a null/undef" );
				out = null;
				break;
			case SRL_HDR_BINARY:
				byte[] bytes = read_binary();
				log.fine( "Read binary: " + Utils.hexStringFromByteArray( bytes ) );
				out = bytes;
				break;
			case SRL_HDR_STR_UTF8:
				String utf8 = read_UTF8();
				log.fine( "Read UTF8: " + utf8 );
				out = utf8;
				break;
			case SRL_HDR_REFN:
				log.fine( "Reading ref to next" );
				if( perlRefs ) {
					PerlReference refn = new PerlReference();
					if( track != 0 ) {
						track_stuff( track, refn );
					}
					refn.value = readSingleValue();
					out = refn;
				} else {
					out = readSingleValue();
				}
				log.fine( "Read ref: " + Utils.dump( out ) );
				break;
			case SRL_HDR_REFP:
				log.fine( "Reading REFP (ref to prev)" );
				long offset_prev = read_varint();
				if( !tracked.containsKey( "track_" + offset_prev ) ) {
					throw new SerealException( "REFP to offset " + offset_prev + ", which is not tracked" );
				}
				Object prv_value = tracked.get( "track_" + offset_prev );
				Object prev = perlRefs ? new PerlReference(prv_value) : prv_value;
				log.fine( "Read prev: " + Utils.dump( prev ) );
				out = prev;
				break;
			case SRL_HDR_OBJECT:
				log.fine( "Reading an object" );
				Object obj = read_object();
				log.fine( "Read object: " + obj );
				out = obj;
				break;
			case SRL_HDR_OBJECTV:
				log.fine( "Reading an objectv" );
				CharSequence className = (CharSequence) get_tracked_item();
				log.fine( "Read an objectv of class: " + className);
				out = new PerlObject( ((Latin1String)className).getString(), readSingleValue() );
				break;
			case SRL_HDR_COPY:
				log.fine( "Reading a copy" );
				Object copy = read_copy();
				log.fine( "Read copy: " + copy );
				out = copy;
				break;
			case SRL_HDR_ALIAS:
				log.fine("Reading an alias");
				Object alias = new Alias(get_tracked_item());
				log.fine( "Read alias: " + Utils.dump( alias ) );
				out = alias;
				break;
			case SRL_HDR_WEAKEN:
				log.fine("Weakening the next thing");
				// so the next thing HAS to be a ref (afaict) which means we can track it
				PerlReference placeHolder = new PerlReference();
				WeakReference wref = new WeakReference( placeHolder );
				if( track != 0 ) {
					track_stuff( track, wref );
				}
				placeHolder.value = ((PerlReference)readSingleValue()).value;
				out = wref;
				break;
			case SRL_HDR_HASH:
				Object hash = read_hash( (byte) 0 );
				log.fine( "Read hash: " + hash );
				out = hash;
				break;
			case SRL_HDR_ARRAY:
				log.fine( "Reading array" );
				Object[] arr = read_array( (byte) 0, track );
				log.fine( "Read array: " + Utils.dump( arr ) );
				out = arr;
				break;
			case SRL_HDR_REGEXP:
				log.fine( "Reading Regexp" );
				Pattern pattern = read_regex();
				log.fine( "Read regexp: " + pattern );
				out = pattern;
				break;
			case SRL_HDR_PAD:
				log.fine("Padding byte: skip");
				return preservePadding ? new Padded(readSingleValue()) : readSingleValue();
			default:
				throw new SerealException( "Tag not supported: " + tag );
			}
		}

		if( track != 0 ) { // we double-track arrays ATM (but they just overwrite)
			track_stuff( track, out );
		}
		log.fine( "returning: " + out );

		return out;

	}


	/**
	 * Read a short binary ISO-8859-1 (latin1) string, the lower bits of the tag hold the length
	 * @param tag
	 * @return
	 */
	CharSequence read_short_binary(byte tag) {
		int length = tag & SRL_MASK_SHORT_BINARY_LEN;
		log.fine( "Short binary, length: " + length );
		byte[] buf = new byte[length];
		data.get( buf );
		return new Latin1String( Charset.forName( "ISO-8859-1" ).decode( ByteBuffer.wrap( buf ) ).toString() );
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
		data.position( originalPosition );
		Object copy = readSingleValue();
		data.position( currentPosition ); // go back to where we were

		return copy;
	}

	private String read_UTF8() {
		int length = (int) read_varint();
		byte[] buf = new byte[length];
		data.get( buf );
		return Charset.forName( "UTF-8" ).decode( ByteBuffer.wrap( buf ) ).toString();
	}

	long read_zigzag() {

		long n = read_varint();

		return (n >>> 1) ^ (-(n & 1)); // note the unsigned right shift
	}

	Pattern read_regex() throws SerealException {

		int flags = 0;
		Object str = readSingleValue();
		String regex = str instanceof Latin1String ? ((Latin1String)str).getString() : (String) str;
		log.fine( "Read pattern: " + regex );

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

		Pattern out = Pattern.compile( regex, flags );

		return out;
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
		track_stuff( position, className );

		log.fine( "Object Classname: " + className );

		// now read the struct (better be a hash!)
		Object structure = readSingleValue();
		log.fine( "Object Type: " + structure.getClass().getName() );
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
		this.data = blob;
		data.rewind();
	}

	private void track_stuff(int pos, Object thing) {
		log.fine( "Saving " + thing + " at offset " + pos );
		Object ref = thing; // autoboxing ftw
		tracked.put( "track_" + pos, ref );
	}

}
