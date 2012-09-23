package com.booking.sereal;

import java.lang.ref.WeakReference;
import java.lang.reflect.Array;
import java.nio.ByteBuffer;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.logging.Level;
import java.util.regex.Pattern;

/**
 * WIP
 * Functions for encoding various things.
 * TODO: Probably just call all methods write() and not have them return the encoded value
 */
public class Encoder {

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

	// so we don't need to allocate this every time we encode a varint
	private byte[] varint_buf = new byte[12];

	private final Map<String, Object> options;

	private final byte[] HEADER = ByteBuffer.allocate( 4 ).putInt( SerealHeader.MAGIC ).array();

	// track things we've encoded so we can emit refs and copies
	private Map<Object, Integer> tracked = new HashMap<Object, Integer>();

	// where we store the various encoded things
	private final ArrayList<byte[]> data;
	private int size = 0; // size of everything encoded so far

	private List<Integer> tracked_and_used = new ArrayList<Integer>();

	private Map<Object, Integer> refcounts = new HashMap<Object, Integer>();

	private Map<Object, Integer> arrayrefs = new HashMap<Object, Integer>();

	public Encoder(Map<String, Object> options) {

		this.options = options == null ? new HashMap<String, Object>() : options;
		data = new ArrayList<byte[]>();

		init();
	}

	// write header and version/encoding
	private void init() {

		data.add( HEADER );
		size += HEADER.length;

		data.add( new byte[] { 0x01 } ); // protocol 1, no encoding
		size++;

		data.add( new byte[] { 0x00 } ); // no header suffix
		size++;

	}

	/**
	 * Returns the encoded data as a ByteBuffer
	 *
	 * @return
	 */
	public ByteBuffer getData() {


		// now that everything is encoded, we have correct refcounts of arrays,
		// so we should convert any that are SRL_HDR_ARRAYREF to SRL_HDR_ARRAY+count if their refcount != 1
		log.fine("Refcounts: " + Utils.dump( refcounts ));
		for(Entry<Object, Integer> entry : arrayrefs.entrySet()){
			int segment = entry.getValue();
			log.fine("Arrayref fixups: segment="+segment + " refcount:" + refcounts.get( entry.getKey() ) + " array= " + Utils.dump( entry.getKey() ));
			if( refcounts.containsKey( entry.getKey() ) && refcounts.get( entry.getKey() ) > 1) {
				byte[] tag_data = data.get( segment );
				// we already have the REFN set
				data.set( segment, new byte[]{
						SerealHeader.SRL_HDR_ARRAY,
						(byte) (tag_data[0] & ~SerealHeader.SRL_HDR_ARRAYREF_LOW)} // varints < 16 are always a byte :)
				);
				size += 1;// we added 1 byte
				log.fine( "Converted to: " + Utils.hexStringFromByteArray( data.get( segment ) ) );
			}
		}

		// concat all the segments
		ByteBuffer buf = concatSegments();

		// set tracking bits for everything we tracked
		for(int offset : tracked_and_used) {
			log.fine( "Setting a tracking bit for " +  Utils.hexStringFromByteArray( new byte[]{ buf.get( offset )} ) + " at offset " + offset );
			buf.position( offset );
			buf.put( (byte) (buf.get( offset ) | SerealHeader.SRL_HDR_TRACK_FLAG) );
		}


		buf.rewind();

		return buf;
	}

	private ByteBuffer concatSegments() {
		ByteBuffer buf = ByteBuffer.allocate( size );
		for(byte[] segment : data) {
			log.fine("getData(): Segment: " + Utils.hexStringFromByteArray( segment ));
			buf.put( segment );
		}
		return buf;
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
	byte[] write_varint(long n) {

		int length = 0;

		while( n > 127 ) {
			varint_buf[length++] = (byte) ((n & 127) | 128);
			n >>= 7;
		}
		varint_buf[length++] = (byte) n;

		byte[] copy = Arrays.copyOf( varint_buf, length );
		data.add( copy );
		size += copy.length;
		return copy;
	}

	/**
	 * Encode a number as zigzag
	 *
	 * @param n
	 * @return
	 */
	byte[] write_zigzag(long n) {

		return write_varint( (n << 1) ^ (n >> 63) ); // note the unsigned right shift
	}

	/**
	 * Encode a short ascii string
	 *
	 * @param s
	 *           String to encode as US-ASCII bytes
	 * @throws SerealException
	 *            if the string is not short enough
	 */
	void write_short_binary(String s) throws SerealException {

		log.fine( "Writing short binary: " + s );

		// maybe we can just COPY (but obviously not emit a copy tag for ourselves)
		if( isTracked( s ) && getTrackedItem( s ) != size ) {
			write_copy( s );
			return;
		}

		int length = s.length();

		if( length > 31 ) {
			throw new SerealException( "Cannot create short binary for " + s + ": too long" );
		}

		// 0 reserves space for the length byte
		byte[] out = Charset.forName( "US-ASCII" ).encode( 0 + s ).array();
		// length of string
		out[0] = (byte) (length | SerealHeader.SRL_HDR_SHORT_BINARY);

		// save it
		data.add( out );
		size += out.length;

	}

	protected void write_copy(String s) {

		log.fine("Emitting a copy for: '" + s + "'");

		data.add( new byte[]{ SerealHeader.SRL_HDR_COPY} );
		size++;

		write_varint( getTrackedItem( s ) );

		// do not track since spec says no
	}

	/**
	 * Encode a regex
	 *
	 * @param p
	 *           regex pattern. Only support flags "smix": DOTALL | MULTILINE | CASE_INSENSITIVE | COMMENTS
	 * @throws SerealException
	 *            if the pattern is longer that a short binary string
	 */
	void write_regex(Pattern p) throws SerealException {

		log.fine( "Writing a Pattern: " + Utils.dump( p ) );

		String flags = "";
		flags += (p.flags() & Pattern.MULTILINE) != 0 ? "m" : "";
		flags += (p.flags() & Pattern.DOTALL) != 0 ? "s" : "";
		flags += (p.flags() & Pattern.CASE_INSENSITIVE) != 0 ? "i" : "";
		flags += (p.flags() & Pattern.COMMENTS) != 0 ? "x" : "";

		String pattern = p.pattern();

		int length = pattern.length();
		if( length < 32 ) {

			data.add( new byte[] { SerealHeader.SRL_HDR_REGEXP } );
			size++;

			// make array with bytes for (pattern + pattern length tag) + space for flags length tag + flags
			write_short_binary( pattern );
			data.add( new byte[]{ (byte) (flags.length() | SerealHeader.SRL_HDR_SHORT_BINARY) } );
			size++;
			data.add( flags.getBytes( Charset.forName( "US-ASCII" ) ) );
			size += flags.length();

		} else {
			throw new SerealException( "Don't know how to write a pattern of length " + length );
		}
	}

	/**
	 * Encodes a byte array
	 *
	 * @param in
	 * @return
	 */
	byte[] write_bytearray(byte[] in) {

		write_varint( in.length );

		data.add( in );
		size += in.length;

		return in;
	}

	public void write_boolean(boolean b) {

		data.add( new byte[] { b ? SerealHeader.SRL_HDR_TRUE : SerealHeader.SRL_HDR_FALSE } );
		size++;

	}

	/**
	 * Write something to the encoder.
	 *
	 * @param obj
	 * @return a buffer with the encoded data
	 * @throws SerealException
	 */
	public ByteBuffer write(Object obj) throws SerealException {

		encode( obj );

		return getData();
	}

	private void encode(Object obj) throws SerealException {

		int obj_location = size; // location where we start putting this item

		log.fine("Currently tracked: " + Utils.dump( tracked ));

		if( isTracked( obj )) {
			log.fine("Track: We saw this before: " + Utils.dump( obj ) + " at location " + getTrackedItem( obj ));
			write_ref_previous( obj );
			return;
		}

		// this needs to be first for obvious reasons :)
		if( obj == null ) {
			data.add( new byte[] { SerealHeader.SRL_HDR_UNDEF } );
			size++;
			return;
		}

		Class<? extends Object> type = obj.getClass();
		log.fine( "Encoding type: " + type );


		// track it (for COPY and REFP tags)
		track( obj, obj_location );

		// this is ugly :)
		if( type == Long.class || type == Integer.class || type == Byte.class ) {
			write_integer_type( ((Number) obj).longValue() );
		} else if( type == HashMap.class ) {
			write_hash( (HashMap<String, Object>)obj ); // we only allow string keys afaict
		} else if( type == String.class ) {
			write_string_type( (String) obj );
		} else if( type.isArray() ) {
			write_array( obj );
		} else if( type == Pattern.class ) {
			write_regex( (Pattern)obj );
		} else if( type == Double.class ) {
			write_double( (Double)obj );
		} else if( type == Padded.class ) {
			// emit pad bytes until we hit a real object
			while( obj instanceof Padded ) {
				data.add( new byte[]{ SerealHeader.SRL_HDR_PAD } );
				size++;
				obj = ((Padded)obj).value;
			}
			encode( obj );
		} else if( type == PerlReference.class ) {

			PerlReference ref = (PerlReference)obj;
			if( isTracked( ref.value )) {
				write_ref_previous( ref.value );
			} else {
				write_ref( ref );
			}

		} else if( type == WeakReference.class ) {

			data.add( new byte[]{ SerealHeader.SRL_HDR_WEAKEN } );
			size++;

			PerlReference wref = (PerlReference) ((WeakReference)obj).get(); // pretend weakref is a marker
			refcount( wref.value, -1 ); // since the following ref will increment it (weakref is more of a marker in perl)
			encode( wref );

		} else if( type == Alias.class ) {

			write_alias( ((Alias)obj).value );

		}



		if( size == obj_location ) {  // didn't write anything
			throw new SerealException( "Don't know how to encode: " + type.getName() + " = " + obj.toString() );
		}

	}

	/**
	 *
	 * @param obj
	 * @return location in bytestream of object
	 */
	private Integer getTrackedItem(Object obj) {
		return tracked.get( System.identityHashCode( obj ) );
	}

	private boolean isTracked(Object obj) {
		return tracked.containsKey( System.identityHashCode( obj ) );
	}

	private void track(Object obj, int obj_location) {
		log.fine("Tracking " + obj.getClass().getName() + "@" + System.identityHashCode( obj ) + " at location " + obj_location);
		tracked.put( System.identityHashCode( obj ), obj_location );
	}

	private void write_double(Double d) {

		data.add( new byte[]{ SerealHeader.SRL_HDR_DOUBLE} );
		size++;

		long bits = Double.doubleToLongBits(d); // very convienent, thanks Java guys! :)
		byte[] db = new byte[8];
		for(int i=0; i<8; i++) {
			db[i] = (byte) ((bits >> (i*8)) & 0xff);
		}
		data.add( db );
		size += 8;
	}

	private void write_ref_previous(Object obj) {

		int prev_location = getTrackedItem( obj );
		log.fine( "Emitting a REFP for location " + prev_location );

		data.add( new byte[]{ SerealHeader.SRL_HDR_REFP } );
		size++;

		write_varint( prev_location );

		tracked_and_used.add( prev_location );
	}

	private void write_alias(Object obj) {

		int prev_location = getTrackedItem( obj );
		log.fine( "Emitting a REFP for location " + prev_location );

		data.add( new byte[]{ SerealHeader.SRL_HDR_ALIAS } );
		size++;

		write_varint( prev_location );

		tracked_and_used.add( prev_location );
	}



	private void write_hash(HashMap<String, Object> hash) throws SerealException {

		log.fine("Writing hash: " + Utils.dump( hash ));


		int count = hash.size();
		if( count < 16 ) { // store size in lower 4 bits
			data.add( new byte[]{ (byte) (SerealHeader.SRL_HDR_HASHREF_LOW | count) });
			size++;
		} else { // store size in varint
			data.add( new byte[]{ (SerealHeader.SRL_HDR_HASH) });
			size++;
			write_varint( count );
		}

		for(Entry<String, Object> entry : hash.entrySet()) {
			encode( entry.getKey() );
			encode( entry.getValue() );
		}

	}

	private void write_ref(PerlReference ref) throws SerealException {

		log.fine( "Emitting a REFN for @" + System.identityHashCode( ref ) + " -> @" + System.identityHashCode(ref.value) );

		refcount( ref.value, 1);

		data.add( new byte[]{ (SerealHeader.SRL_HDR_REFN) });
		size++;

		encode(ref.value);

	}

	private void refcount(Object value, int change) {

		if( !value.getClass().isArray() ) {
			log.fine("Not bothering with refcounts for: " + value.getClass().getSimpleName());
			return;
		}

		log.fine( "Refcount "+(change>0?"+":"")+change+" for: @" + System.identityHashCode( value ) );

		int count = refcounts.containsKey( value ) ? refcounts.get( value ) : 0;
		refcounts.put( value, count+change );

	}

	private void write_array(Object obj) throws SerealException {

		log.fine( "Emitting an array of type " + obj.getClass().getComponentType() );


		// checking length without casting to Object[] since they might primitives
		int count = Array.getLength( obj );

		// exception: byte[] should be output as SRL_HDR_BINARY
		if( obj.getClass().getComponentType() == byte.class ) {
			data.add( new byte[]{ (SerealHeader.SRL_HDR_BINARY) });
			size++;
			write_bytearray( (byte[]) obj );
			return;
		}

		// double tracking (but not for byte arrays!)
		track(obj, size);

		refcount(obj, 1); // in Perl arrays are always implicitly refs

		/*
		 * So in Perl country, you always have arrayrefs.
		 * This means either outputting REFN ARRAY
		 * or as an optimization ARRAY_REF (if it's smaller than 16 elements)
		 * The problem is: if the refcount of the array is > 1, we need to always
		 * output REFN ARRAY (with the tracking bit set on the array) so the Perl
		 * decoder can keep track of refcounts.
		 *
		 * Needless to say, things get hairy.
		 * What I'm doing for now: we default to emitting ARRAYREF for small arrays,
		 * and keep track of every one we do, then whenever we see another ref to that item
		 * replace the ARRAYREF with a REFN ARRAY (with tracking bit)
		 * This uses more memory but avoids having to either
		 * a) iterate over the entire data structure manually counting refs, or
		 * b) Having the Decoder in "perl-compat-mode" emit a {data: object, refs: refcounts}
		 * so we can pass that around. (which would be a nightmare to deal with as any change means
		 * you manually need to keep the refcount data up to date and if we wanted to do that we'd be doing
		 * COM programming :)
		 */
		if( count < 16 ) {
			log.fine("Tracking ARRAYREF: segment=" +  (data.size()-1) + " byte=" + Utils.hexStringFromByteArray( data.get( data.size()-1 )) );
			arrayrefs.put(obj, data.size());

			data.add( new byte[]{ (byte) (SerealHeader.SRL_HDR_ARRAYREF + count) });
			size++;
		} else {
			data.add( new byte[]{ SerealHeader.SRL_HDR_ARRAY });
			size++;
			write_varint( count );
		}

		// write the objects (works for both Objects and primitives)
		for(int index=0; index<count; index++) {
			log.fine("Emitting array index " + index);
			encode( Array.get( obj, index ));
		}

	}

	private void write_string_type(String str) throws SerealException {
		// find out if the bytes of the string encoded as ASCII are the same as the bytes of the string as UTF8
		// there must be a simpler way but this will do for now :)
		Charset charset_ascii = Charset.forName( "US-ASCII" );
		Charset charset_utf8 = Charset.forName( "UTF-8" );
		byte[] utf8 = str.getBytes( charset_utf8 );
		boolean encodableAsASCII = charset_ascii.encode( str ).equals( ByteBuffer.wrap( utf8 ) );

		if( encodableAsASCII ) {

			if( str.length() < SerealHeader.SRL_MASK_SHORT_BINARY_LEN ) {
				write_short_binary( str );
			} else {
				write_bytearray( str.getBytes( charset_ascii ) );
			}

		} else {

			data.add( new byte[]{ SerealHeader.SRL_HDR_STR_UTF8 } );
			size++;

			write_varint( utf8.length );

			data.add( utf8 );
			size += utf8.length;
		}

	}

	private void write_integer_type(long l) {
		if( l < 0 ) {
			if( l > -17 ) {
				data.add( new byte[] { (byte) (SerealHeader.SRL_HDR_NEG_LOW | (l + 32)) } );
				size++;
			} else {
				write_zigzag( l );
			}
		} else {
			if( l < 16 ) {
				data.add( new byte[]{ (byte) (SerealHeader.SRL_HDR_POS_LOW | l) } );
				size++;
			} else {
				// write varint tag
				data.add( new byte[]{ SerealHeader.SRL_HDR_VARINT } );
				size++;
				write_varint( l );
			}
		}

	}

	/**
	 * Discard all previous tracking clear the buffers etc
	 * Call this when you reuse the encoder
	 */
	public void reset() {
		size = 0;
		data.clear();
		tracked.clear();
		tracked_and_used.clear();
		refcounts.clear();
		init();
	}

}
