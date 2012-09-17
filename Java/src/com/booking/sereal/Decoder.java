package com.booking.sereal;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.charset.Charset;
import java.util.Arrays;
import java.util.HashMap;
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

		public void info(String info) {
			System.out.println( "INFO: " + info );
		}

		public void fine(String info) {
			if( level.intValue() <= Level.FINE.intValue() ) {
				System.out.println( "FINE: " + info );
			}
		}

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

	/**
	 * Create a new Decoder
	 * 
	 * @param options
	 * 		object_type: ObjectType (defaults to PERL_OBJECT)
	 */
	public Decoder(Map<String, Object> options) {
		this.options = options == null ? new HashMap<String, Object>() : options;
		
		objectType = this.options.containsKey("object_type") ? ((ObjectType)this.options.get("object_type")) : ObjectType.PERL_OBJECT;
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
	 * @return
	 * @throws SerealException
	 */
	private Object[] read_array(byte tag) throws SerealException {

		int length = 0;
		if( tag == 0 ) {
			length = (int) read_varint();
		} else {
			length = tag & 15;
		}

		log.fine( "Array length: " + length );

		Object[] out = new Object[length];

		for(int i = 0; i < length; i++) {
			// could be an alias or a single value
			if( data.get( data.position() ) == SRL_HDR_ALIAS ) {
				out[i] = new Alias(read_previous());
			} else {
				out[i] = readSingleValue();
			}
			log.fine( "Read array element " + i + ": " + out[i] );
		}

		return out;
	}

	byte[] read_binary() {

		int length = (int) read_varint();
		byte[] out = new byte[length];
		for(int i = 0; i < length; i++) {
			out[i] = data.get();
		}

		return out;
	}

	private Map<String, Object> read_hash(byte tag) throws SerealException {
		long num_keys = 0;
		if( tag == 0 ) {
			num_keys = read_varint();
		} else {
			num_keys = tag & 15;
		}

		Map<String, Object> hash = new HashMap<String, Object>( (int) num_keys );

		log.fine( "Reading " + num_keys + " hash elements" );

		for(int i = 0; i < num_keys; i++) {
			String key = (String) readSingleValue();
			Object val = readSingleValue();
			hash.put( key, val );
		}

		return hash;
	}

	private Object read_previous() {
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

	private Object readSingleValue() throws SerealException {

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
			int length = tag & SRL_MASK_SHORT_BINARY_LEN;
			log.fine( "Short binary, length: " + length );
			byte[] buf = new byte[length];
			data.get( buf );
			String str = Charset.forName("US-ASCII").decode(ByteBuffer.wrap(buf)).toString();
			log.fine( "Read short binary: " + str + " length " + buf.length );
			out = str;
		} else if( (tag & SRL_HDR_HASHREF) == SRL_HDR_HASHREF ) {
			Map<String, Object> hash = read_hash( tag );
			log.fine( "Read hash: " + hash );
			out = hash;
		} else if( (tag & SRL_HDR_ARRAYREF) == SRL_HDR_ARRAYREF ) {
			log.fine( "Reading arrayref" );
			Object[] arr = read_array( tag );
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
				log.fine("Read zigzag: " + zz);
				out = zz;
				break;
			case SRL_HDR_DOUBLE:
				double d = data.getDouble();
				log.fine( "Read double: " + d );
				out = d;
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
				log.fine("Read UTF8: " + utf8);
				out = utf8;
				break;
			case SRL_HDR_REFN:
				log.fine( "Reading ref to next" );
				Object o = readSingleValue();
				log.fine( "Read ref: " + o );
				out = o;
				break;
			case SRL_HDR_REFP:
				log.fine( "Reading REFP (ref to prev)" );
				Object prev = read_previous();
				log.fine( "Read prev: " + prev );
				out = prev;
				break;
			case SRL_HDR_OBJECT:
				log.fine( "Reading an object" );
				Object obj = read_object();
				log.fine( "Read object: " + obj );
				out = obj;
				break;
			case SRL_HDR_HASH:
				Object hash = read_hash( (byte) 0 );
				log.fine( "Read hash: " + hash );
				out = hash;
				break;
			case SRL_HDR_ARRAY:
				Object[] arr = read_array( (byte) 0 );
				log.fine( "Read array: " + Arrays.asList( arr ).toString() );
				out = arr;
				break;
			case SRL_HDR_REGEXP:
				log.fine( "Reading Regexp" );
				Pattern pattern = read_regex();
				log.fine( "Read regexp: " + pattern );
				out = pattern;
				break;
			default:
				throw new SerealException( "Tag not supported: " + tag );
			}
		}

		if( track != 0 ) {
			track_stuff( track, out );
		}

		log.fine( "readSingleValue: " + out );

		return out;

	}

	private String read_UTF8() {
		int length = (int) read_varint();
		byte[] buf = new byte[length];
		data.get(buf);
		return Charset.forName("UTF-8").decode(ByteBuffer.wrap(buf)).toString();
	}

	private long read_zigzag() {
		
		long n = read_varint();
		
		return (n >>> 1) ^ (-(n & 1)); // note the unsigned right shift
	}

	Pattern read_regex() throws SerealException {

		int flags = 0;
		String regex = (String) readSingleValue();
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
		byte tag = data.get();
		String className;
		if( (tag & SRL_HDR_SHORT_BINARY_LOW) == SRL_HDR_SHORT_BINARY_LOW ) {
			int length = tag & SRL_MASK_SHORT_BINARY_LEN;
			byte[] buf = new byte[length];
			data.get( buf );
			className = new String( buf );
		} else {
			throw new SerealException( "Don't know how to read classname from tag" + tag );
		}
		log.fine( "Classname: " + className );

		// now read the struct (better be a hash!)
		Object structure = readSingleValue();
		if( structure instanceof Map) {
			// now "bless" this into a class, perl style
			@SuppressWarnings("unchecked")
			Map<String, Object> classData = (Map<String, Object>)structure;
			try {
				// either an existing java class
				Class<?> c = Class.forName( className );
				return Utils.bless( c, classData );
			} catch (ClassNotFoundException e) {
				// or we make a new one
				if( objectType == ObjectType.POJO) {
					return Utils.bless( className, classData );					
				} else {
					// or we make a Perl-style one
					return new PerlObject(className, classData);
				}
				
			}
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
