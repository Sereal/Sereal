//package com.booking.sereal;

//import java.io.File;
//import java.io.FileInputStream;
//import java.io.FileNotFoundException;
//import java.io.IOException;
//import java.lang.ref.WeakReference;
//import java.nio.ByteBuffer;
//import java.nio.ByteOrder;
//import java.nio.charset.Charset;
//import java.util.HashMap;
//import java.util.LinkedHashMap;
//import java.util.Map;
//import java.util.logging.Level;
//import java.util.regex.Pattern;

//import org.xerial.snappy.Snappy;

/**
 * WIP Decoder for Sereal WIP
 */
function Decoder(options) { // implements SerealHeader {
    var _this = this;
    // 0x6c72733d but little endian for some reason
    var MAGIC = 1039364716;//(0x6c) + (0x72 << 8) + (0x73 << 16) + (0x3d << 24);

    var SRL_MASK_SHORT_BINARY_LEN = 31; // lower 5 bits


    /*
	Note: Despite this interface already being named SerealHeader we still use SRL_HDR_
			as a prefix so grepping will show both these and the C ones.
	
=for autoupdater start
* NOTE this section is autoupdated by author_tools/update_from_header.pl */
    var SRL_HDR_POS = 0; /*   0 0x00 0b00000000 small positive integer - value in low 4 bits (identity) */
    var SRL_HDR_POS_LOW = 0; /*   0 0x00 0b00000000 small positive integer - value in low 4 bits (identity) */
    var SRL_HDR_POS_HIGH = 15; /*  15 0x0f 0b00001111 small positive integer - value in low 4 bits (identity) */
    var SRL_HDR_NEG = 16; /*  16 0x10 0b00010000 small negative integer - value in low 4 bits (k+32) */
    var SRL_HDR_NEG_LOW = 16; /*  16 0x10 0b00010000 small negative integer - value in low 4 bits (k+32) */
    var SRL_HDR_NEG_HIGH = 31; /*  31 0x1f 0b00011111 small negative integer - value in low 4 bits (k+32) */
    var SRL_HDR_VARINT = 32; /*  32 0x20 0b00100000 <VARINT> - Varint variable length integer */
    var SRL_HDR_ZIGZAG = 33; /*  33 0x21 0b00100001 <ZIGZAG-VARINT> - Zigzag variable length integer */
    var SRL_HDR_FLOAT = 34; /*  34 0x22 0b00100010 <IEEE-FLOAT> */
    var SRL_HDR_DOUBLE = 35; /*  35 0x23 0b00100011 <IEEE-DOUBLE> */
    var SRL_HDR_LONG_DOUBLE = 36; /*  36 0x24 0b00100100 <IEEE-LONG-DOUBLE> */
    var SRL_HDR_UNDEF = 37; /*  37 0x25 0b00100101 None - Perl undef var; eg my $var= undef; */
    var SRL_HDR_BINARY = 38; /*  38 0x26 0b00100110 <LEN-VARINT> <BYTES> - binary/(latin1) string */
    var SRL_HDR_STR_UTF8 = 39; /*  39 0x27 0b00100111 <LEN-VARINT> <UTF8> - utf8 string */
    var SRL_HDR_REFN = 40; /*  40 0x28 0b00101000 <ITEM-TAG>    - ref to next item */
    var SRL_HDR_REFP = 41; /*  41 0x29 0b00101001 <OFFSET-VARINT> - ref to previous item stored at offset */
    var SRL_HDR_HASH = 42; /*  42 0x2a 0b00101010 <COUNT-VARINT> [<KEY-TAG> <ITEM-TAG> ...] - count followed by key/value pairs */
    var SRL_HDR_ARRAY = 43; /*  43 0x2b 0b00101011 <COUNT-VARINT> [<ITEM-TAG> ...] - count followed by items */
    var SRL_HDR_OBJECT = 44; /*  44 0x2c 0b00101100 <STR-TAG> <ITEM-TAG> - class, object-item */
    var SRL_HDR_OBJECTV = 45; /*  45 0x2d 0b00101101 <OFFSET-VARINT> <ITEM-TAG> - offset of previously used classname tag - object-item */
    var SRL_HDR_ALIAS = 46; /*  46 0x2e 0b00101110 <OFFSET-VARINT> - alias to item defined at offset */
    var SRL_HDR_COPY = 47; /*  47 0x2f 0b00101111 <OFFSET-VARINT> - copy of item defined at offset */
    var SRL_HDR_WEAKEN = 48; /*  48 0x30 0b00110000 <REF-TAG> - Weaken the following reference */
    var SRL_HDR_REGEXP = 49; /*  49 0x31 0b00110001 <PATTERN-STR-TAG> <MODIFIERS-STR-TAG> */
    var SRL_HDR_OBJECT_FREEZE = 50; /*  50 0x32 0b00110010 <STR-TAG> <ITEM-TAG> - class, object-item. Need to call "THAW" method on class after decoding */
    var SRL_HDR_OBJECTV_FREEZE = 51; /*  51 0x33 0b00110011 <OFFSET-VARINT> <ITEM-TAG> - (OBJECTV_FREEZE is to OBJECT_FREEZE as OBJECTV is to OBJECT) */
    var SRL_HDR_RESERVED = 52; /*  52 0x34 0b00110100 reserved */
    var SRL_HDR_RESERVED_LOW = 52; /*  52 0x34 0b00110100 reserved */
    var SRL_HDR_RESERVED_HIGH = 56; /*  56 0x38 0b00111000 reserved */
    var SRL_HDR_CANONICAL_UNDEF = 57; /*  57 0x39 0b00111001 undef (PL_sv_undef) - "the" Perl undef (see notes) */
    var SRL_HDR_FALSE = 58; /*  58 0x3a 0b00111010 false (PL_sv_no) */
    var SRL_HDR_TRUE = 59; /*  59 0x3b 0b00111011 true  (PL_sv_yes) */
    var SRL_HDR_MANY = 60; /*  60 0x3c 0b00111100 <LEN-VARINT> <TYPE-BYTE> <TAG-DATA> - repeated tag (not done yet, will be implemented in version 3) */
    var SRL_HDR_PACKET_START = 61; /*  61 0x3d 0b00111101 (first byte of magic string in header) */
    var SRL_HDR_EXTEND = 62; /*  62 0x3e 0b00111110 <BYTE> - for additional tags */
    var SRL_HDR_PAD = 63; /*  63 0x3f 0b00111111 (ignored tag, skip to next byte) */
    var SRL_HDR_ARRAYREF = 64; /*  64 0x40 0b01000000 [<ITEM-TAG> ...] - count of items in low 4 bits (ARRAY must be refcnt=1) */
    var SRL_HDR_ARRAYREF_LOW = 64; /*  64 0x40 0b01000000 [<ITEM-TAG> ...] - count of items in low 4 bits (ARRAY must be refcnt=1) */
    var SRL_HDR_ARRAYREF_HIGH = 79; /*  79 0x4f 0b01001111 [<ITEM-TAG> ...] - count of items in low 4 bits (ARRAY must be refcnt=1) */
    var SRL_HDR_HASHREF = 80; /*  80 0x50 0b01010000 [<KEY-TAG> <ITEM-TAG> ...] - count in low 4 bits, key/value pairs (HASH must be refcnt=1) */
    var SRL_HDR_HASHREF_LOW = 80; /*  80 0x50 0b01010000 [<KEY-TAG> <ITEM-TAG> ...] - count in low 4 bits, key/value pairs (HASH must be refcnt=1) */
    var SRL_HDR_HASHREF_HIGH = 95; /*  95 0x5f 0b01011111 [<KEY-TAG> <ITEM-TAG> ...] - count in low 4 bits, key/value pairs (HASH must be refcnt=1) */
    var SRL_HDR_SHORT_BINARY = 96; /*  96 0x60 0b01100000 <BYTES> - binary/latin1 string, length encoded in low 5 bits of tag */
    var SRL_HDR_SHORT_BINARY_LOW = 96; /*  96 0x60 0b01100000 <BYTES> - binary/latin1 string, length encoded in low 5 bits of tag */
    var SRL_HDR_SHORT_BINARY_HIGH = 127; /* 127 0x7f 0b01111111 <BYTES> - binary/latin1 string, length encoded in low 5 bits of tag */
    var SRL_HDR_TRACK_FLAG = 128; /* 128 0x80 0b10000000 if this bit is set track the item */
    /*
    * NOTE the above section is auto-updated by author_tools/update_from_header.pl
    =for autoupdater stop
        */

    //private final boolean 
    var prefer_latin1;

    var ObjectType = {
        PERL_OBJECT: "PERL_OBJECT", // Perl style object (name + hash)
        POJO: "POJO", // Dynamically compile a Plain Old Java Object
    }

    //// set up logging: be compatible with log4j etc but not suffer so much :)
    //interface Log {
    //	void info(String info);

    //	void fine(String info);

    //	void setLevel(Level lvl);
    //}

    var log = new Log();// {
    //	private Level level = Level.INFO;
    //	@Override
    //	public void info(String info) {
    //		System.out.println( "INFO: " + info );
    //	}
    //	@Override
    //	public void fine(String info) {
    //		if( level.intValue() <= Level.FINE.intValue() ) {
    //			System.out.println( info );
    //		}
    //	}
    //	@Override
    //	public void setLevel(Level lvl) {
    //		this.level = lvl;
    //	}
    //};

    //var options; // options (currently do not accept any)

    var properties = {};//new HashMap<String, Object>(); // where
    // we
    // save
    // protocol
    // version
    // and
    // encoding

    var /*ByteBuffer*/ data;

    // where we track items for REFP purposes
    var tracked = {};//new HashMap<String, Object>();

    var /*private ObjectType*/ objectType;
    var perlRefs = false;
    var preservePadding = false;

    var /*private ByteBuffer*/ realData;

    Function.addTo(_this, [decode, decodeFile, decode_sereal, getState, setData,readSingleValue]);
    ctor();
    /**
	 * Create a new Decoder
	 *
	 * @param options
	 *           object_type: ObjectType (defaults to PERL_OBJECT)
	 *           use_perl_refs: if true wraps things in References to we can "perfectly" roundtrip
	 */
    function ctor() {
        if (options == null)
            options = {};
        //this.options = options == null ? new HashMap<String, Object>() : options;

        objectType = options.object_type || ObjectType.PERL_OBJECT;
        perlRefs = options.use_perl_refs || false;//( "use_perl_refs" ) ? ((Boolean) options.get( "use_perl_refs" )) : false;
        preservePadding = options.preserve_pad_tags || false;//" ) ? ((Boolean) options.get( "preserve_pad_tags" )) : false;
        prefer_latin1 = options.prefer_latin1 || false;//") ? ((Boolean) options.get("prefer_latin1")) : false;
    }
    function getState() {
        return { properties, tracked, perlRefs, preservePadding, realData };
    }


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
    function decode_sereal(/*File*/ f, /*Map<String, Object>*/ options) {//throws SerealException, IOException {
        var d = new Decoder(options);
        return d.decodeFile(f);
    }

    function decodeFile(/*File*/ f) {//throws SerealException, IOException {

        log.fine("Decoding: " + f.getName());

        if (!f.exists()) {
            throw new FileNotFoundException("No such file: " + f.getCanonicalPath());
        }

        // read everything
        var size = f.length(); // yeah yeah truncate
        log.fine("File size: " + size);
        var buf = new ArrayBuffer(size);
        //FileInputStream fi = new FileInputStream( f );
        //fi.getChannel().read( buf );
        //fi.close();
        //log.fine( "Raw: " + new String( buf.array() ) );

        setData(buf);
        var structure = decode();
        console.info("Decoded: ", structure);

        return structure;
    }

    function checkHeader() {//throws SerealException {

        if (data.limit() < 4) {
            throw new SerealException("Invalid Sereal header: too few bytes");
        }

        if (data.getInt() != MAGIC) {
            throw new SerealException("Invalid Seareal header: doesn't match magic");
        }

    }

    function checkHeaderSuffix() {

        var /*long*/ suffix_size = read_varint();
        properties.suffix_size = suffix_size;

        log.fine("Header suffix size: " + suffix_size);

        // skip everything in the optional suffix part
        //HACK:
        //data.position(data.position() + suffix_size);
        data.get(); //TODO: parse eightBitFlags

    }

    function checkNoEOD() {// throws SerealException {

        if (data.remaining() == 0) {
            throw new SerealException("Unexpected end of data at byte " + data.limit());
        }

    }

    function checkProtoAndFlags() {// throws SerealException {

        if (data.limit() < 1) {
            throw new SerealException("Invalid Sereal header: no protocol/version byte");
        }

        var /*int*/ protoAndFlags = data.get();
        var /*int*/protocolVersion = protoAndFlags & 15; // 4 bits for version

        log.fine("Version: " + protocolVersion);
        //HACK
        //if (protocolVersion != 1) {
        //    throw new SerealException(String.format("Invalid Sereal header: unsupported protocol version %d", protocolVersion));
        //}
        properties.protocol_version = protocolVersion;

        var /*int*/ encoding = (protoAndFlags & ~15) >> 4;
        log.fine("Encoding: " + encoding);
        //HACK:
        //if ((encoding == 1 || encoding == 2) && !options.containsKey("snappy_support")) {
        //    throw new SerealException("Unsupported encoding: Snappy");
        //} else if (encoding < 0 || encoding > 2) {
        //    throw new SerealException("Unsupported encoding: unknown");
        //}

        properties.encoding = encoding;

    }

    /**
	 *
	 * @return deserealized object
	 * @throws SerealException
	 * @throws IOException 
	 */
    function decode() {// throws SerealException, IOException {

        if (data == null) {
            throw new SerealException("No data set");
        }

        log.fine("Decoding: " + data.toString());// + " - " + new String(data.array()));

        checkHeader();
        checkProtoAndFlags();
        checkHeaderSuffix();

        realData = data;
        var /*int*/encoding = properties.encoding;
        if (encoding == 1 || encoding == 2) {
            uncompressSnappy();
        }
        var out = readSingleValue();

        log.fine("Read: " + out);
        log.fine("Data left: " + (realData.limit() - realData.position()));

        return out;
    }

    function uncompressSnappy() {// throws IOException, SerealException {
        var /*int*/ len = realData.limit() - realData.position() - 1;

        if (properties.encoding == 2) {
            len = read_varint();
        }
        var pos = realData.position();
        var /*byte[]*/ compressed = new byte[len];
        realData.get(compressed, 0, len);
        var /*byte[]*/ uncompressed = new byte[pos + Snappy.uncompressedLength(compressed, 0, len)];
        if (!Snappy.isValidCompressedBuffer(compressed)) {
            throw new SerealException("Invalid snappy data");
        }
        Snappy.uncompress(compressed, 0, len, uncompressed, pos);
        data = ByteBuffer.wrap(uncompressed);
        data.position(pos);
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
    function /*Object[]*/ read_array(/*byte*/ tag, /*int*/ track) {// throws SerealException {

        var /*int*/ length = 0;
        if (tag == 0) {
            length = read_varint();
        } else {
            length = tag & 15;
        }

        log.fine("Array length: " + length);

        var /*Object[]*/ out = new Array(length);//new Object[length];
        if (track != 0) { // track ourself
            track_stuff(track, out);
        }

        for (var i = 0; i < length; i++) {
            out[i] = readSingleValue();
            log.fine("Read array element " + i + ": " + Utils.dump(out[i]));
        }

        return out;
    }

    /**
	 * Reads a byte array, but was called read_binary in C, so for grepping purposes I kept the name
	 * 
	 * For some reason we call them Latin1Strings.
	 * @return
	 */
    function /*byte[]*/ read_binary() {

        var /*int*/ length = read_varint();
        var /*byte[]*/ out = new Uint8Array(length);//byte[length];
        for (var i = 0; i < length; i++) {
            arr[i] = data.get();
        }

        return out.buffer;
    }

    function /*Map<String, Object>*/ read_hash(/*byte*/ tag, /*int */ track) {// throws SerealException {
        var num_keys = 0;
        if (tag == 0) {
            num_keys = read_varint();
        } else {
            num_keys = tag & 15;
        }

        var hash = {};//new HashMap<String, Object>( num_keys );
        if (track != 0) { // track ourself
            track_stuff(track, hash);
        }

        log.fine("Reading " + num_keys + " hash elements");

        for (var i = 0; i < num_keys; i++) {
            var keyObject = readSingleValue();
            var /*CharSequence*/ key;
            if (typeof (keyObject) == "string") {
                key = /*(CharSequence)*/ keyObject;
            } else if (keyObject instanceof ArrayBuffer) {
                key = new Latin1String(keyObject).toString();
            } else {
                throw new SerealException("A key is expected to be a byte or character sequence, but got " + keyObject.toString());
            }
            var val = readSingleValue();
            hash[key] = val;
        }

        return hash;
    }

    function get_tracked_item() {
        var offset = read_varint();
        log.fine("Creating ref to item previously read at offset: " + offset + " which is: " + tracked.get("track_" + offset));
        log.fine("keys: " + tracked.keySet() + " vals: " + tracked.values());
        return tracked.get("track_" + offset);
    }

    // top bit set (0x80) means next byte is 7 bits more more varint
    function read_varint() {

        var uv = 0;
        var lshift = 0;

        var /*byte*/ b = data.get();
        while (data.hasRemaining() && (b < 0)) {
            uv |= (b & 127) << lshift; // add 7 bits
            lshift += 7;
            b = data.get();
        }
        uv |= b << lshift; // add final (or first if there is only 1)

        return uv;

    }

    function readSingleValue() {//  throws SerealException {

        checkNoEOD();

        var tag = data.get();

        var track = 0;
        if ((tag & SRL_HDR_TRACK_FLAG) != 0) {
            tag = tag & ~SRL_HDR_TRACK_FLAG;
            track = data.position() - 1;
            log.fine("Tracking stuff at position: " + track);
        }

        log.fine("Tag: " + (tag & 0xFF));// + " = " + tag.toHex());
        var out;

        if (tag <= SRL_HDR_POS_HIGH) {
            log.fine("Read small positive int:" + tag);
            out = tag;
        } else if (tag <= SRL_HDR_NEG_HIGH) {
            log.fine("Read small negative int:" + (tag - 32));
            out = tag - 32;
        } else if ((tag & SRL_HDR_SHORT_BINARY_LOW) == SRL_HDR_SHORT_BINARY_LOW) {
            var /*byte[]*/ short_binary = read_short_binary(tag);
            log.fine("Read short binary: " + short_binary + " length " + short_binary.length);
            out = prefer_latin1 ? new Latin1String(short_binary).toString() : short_binary;
        } else if ((tag & SRL_HDR_HASHREF) == SRL_HDR_HASHREF) {
            var hash = read_hash(tag, track);
            log.fine("Read hash: " + hash);
            out = hash;
        } else if ((tag & SRL_HDR_ARRAYREF) == SRL_HDR_ARRAYREF) {
            log.fine("Reading arrayref");
            var /*Object[]*/ arr = read_array(tag, track);
            log.fine("Read arrayref: " + arr);
            out = arr;
        } else {
            switch (tag) {
                case SRL_HDR_VARINT:
                    var l = read_varint();
                    log.fine("Read varint: " + l);
                    out = l;
                    break;
                case SRL_HDR_ZIGZAG:
                    var zz = read_zigzag();
                    log.fine("Read zigzag: " + zz);
                    out = zz;
                    break;
                case SRL_HDR_DOUBLE:
                    // Java defaults to BE, maybe we can jsut do this generally, don't know yet (but think so)
                    data.order(ByteOrder.LITTLE_ENDIAN);
                    var d = data.getDouble();
                    data.order(ByteOrder.BIG_ENDIAN);
                    log.fine("Read double: " + d);
                    out = d;
                    break;
                case SRL_HDR_TRUE:
                    log.fine("Read: TRUE");
                    out = true;
                    break;
                case SRL_HDR_FALSE:
                    log.fine("Read: FALSE");
                    out = false;
                    break;
                case SRL_HDR_UNDEF:
                    log.fine("Read a null/undef");
                    out = null;
                    break;
                case SRL_HDR_BINARY:
                    var /*byte[]*/ bytes = read_binary();
                    log.fine("Read binary: " + bytes);
                    out = prefer_latin1 ? new Latin1String(bytes).toString() : bytes;
                    break;
                case SRL_HDR_STR_UTF8:
                    var utf8 = read_UTF8();
                    log.fine("Read UTF8: " + utf8);
                    out = utf8;
                    break;
                case SRL_HDR_REFN:
                    log.fine("Reading ref to next");
                    var /*PerlReference*/ refn = new PerlReference(readSingleValue());
                    if (perlRefs) {
                        out = refn;
                    } else {
                        out = refn.getValue();
                    }
                    log.fine("Read ref: " + Utils.dump(out));
                    break;
                case SRL_HDR_REFP:
                    log.fine("Reading REFP (ref to prev)");
                    var offset_prev = read_varint();
                    if (!tracked.containsKey("track_" + offset_prev)) {
                        throw new SerealException("REFP to offset " + offset_prev + ", which is not tracked");
                    }
                    var prv_value = tracked.get("track_" + offset_prev);
                    var prev = perlRefs ? new PerlReference(prv_value) : prv_value;
                    log.fine("Read prev: " + Utils.dump(prev));
                    out = prev;
                    break;
                case SRL_HDR_OBJECT:
                    log.fine("Reading an object");
                    var obj = read_object();
                    log.fine("Read object: " + obj);
                    out = obj;
                    break;
                case SRL_HDR_OBJECTV:
                    log.fine("Reading an objectv");
                    var className = /*(CharSequence)*/ get_tracked_item();
                    log.fine("Read an objectv of class: " + className);
                    out = new PerlObject(className.getString(), readSingleValue());
                    break;
                case SRL_HDR_COPY:
                    log.fine("Reading a copy");
                    var copy = read_copy();
                    log.fine("Read copy: " + copy);
                    out = copy;
                    break;
                case SRL_HDR_ALIAS:
                    log.fine("Reading an alias");
                    var alias = new Alias(get_tracked_item());
                    log.fine("Read alias: " + Utils.dump(alias));
                    out = alias;
                    break;
                case SRL_HDR_WEAKEN:
                    log.fine("Weakening the next thing");
                    // so the next thing HAS to be a ref (afaict) which means we can track it
                    var placeHolder = new PerlReference(readSingleValue().getValue());
                    var /*WeakReference<PerlReference>*/ wref = new WeakReference(placeHolder);
                    out = wref;
                    break;
                case SRL_HDR_HASH:
                    var hash = read_hash(0, track);
                    log.fine("Read hash: " + hash);
                    out = hash;
                    break;
                case SRL_HDR_ARRAY:
                    log.fine("Reading array");
                    var arr = read_array(0, track);
                    log.fine("Read array: " + Utils.dump(arr));
                    out = arr;
                    break;
                case SRL_HDR_REGEXP:
                    log.fine("Reading Regexp");
                    var pattern = read_regex();
                    log.fine("Read regexp: " + pattern);
                    out = pattern;
                    break;
                case SRL_HDR_PAD:
                    log.fine("Padding byte: skip");
                    return preservePadding ? new Padded(readSingleValue()) : readSingleValue();
                default:
                    throw new SerealException("Tag not supported: " + tag);
            }
        }

        if (track != 0) { // we double-track arrays ATM (but they just overwrite)
            track_stuff(track, out);
        }
        log.fine("returning: " + out);

        return out;

    }


    /**
	 * Read a short binary ISO-8859-1 (latin1) string, the lower bits of the tag hold the length
	 * @param tag
	 * @return
	 */
    function /*byte[]*/ read_short_binary(/*byte*/ tag) {
        var /*int*/length = tag & SRL_MASK_SHORT_BINARY_LEN;
        log.fine("Short binary, length: " + length);
        var /*byte[]*/ buf = new ArrayBuffer(length);
        data.get(buf);
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
    function read_copy() {//  throws SerealException {

        var /*int*/originalPosition = read_varint();
        var /*int*/currentPosition = data.position(); // remember where we parked

        // note: you might think you'd like to use mark() and reset(), but setting position(..) discards the mark
        data.position(originalPosition);
        var copy = readSingleValue();
        data.position(currentPosition); // go back to where we were

        return copy;
    }

    function /*string*/ read_UTF8() {
        var /*int*/ length = read_varint();
        var /*byte[]*/ buf = new byte[length];
        data.get(buf);
        return Charset.forName("UTF-8").decode(ByteBuffer.wrap(buf)).toString();
    }

    function /*long*/ read_zigzag() {

        var n = read_varint();

        return (n >>> 1) ^ (-(n & 1)); // note the unsigned right shift
    }

    function /*Pattern */  read_regex() {//throws SerealException {

        var flags = 0;
        var str = readSingleValue();
        var /*String*/ regex;
        if (typeof (str) == "string") {
            regex = str.toString();
        } else if (str instanceof ArrayBuffer) {
            regex = new Latin1String(str).toString();
        } else {
            throw new SerealException("Regex has to be built from a char or byte sequence");
        }
        log.fine("Read pattern: " + regex);

        // now read modifiers
        var /*byte*/ tag = data.get();
        if ((tag & SRL_HDR_SHORT_BINARY_LOW) == SRL_HDR_SHORT_BINARY_LOW) {
            var length = tag & SRL_MASK_SHORT_BINARY_LEN;
            while (length-- > 0) {
                var /*byte*/ value = data.get();
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
                        throw new SerealException("Unknown regex modifier: " + value);
                }

            }
        } else {
            throw new SerealException("Expecting SRL_HDR_SHORT_BINARY for modifiers of regexp, got: " + tag);
        }

        return Pattern.compile(regex, flags);
    }

    function read_object() {// throws SerealException {

        // first read the classname
        // Maybe we should have some kind of read_string() method?
        var position = data.position();
        var tag = data.get();
        var className;
        if ((tag & SRL_HDR_SHORT_BINARY_LOW) == SRL_HDR_SHORT_BINARY_LOW) {
            var length = tag & SRL_MASK_SHORT_BINARY_LEN;
            var /*byte[]*/ buf = new byte[length];
            data.get(buf);
            className = new Latin1String(/*new String(*/buf/*)*/).toString();
        } else {
            throw new SerealException("Don't know how to read classname from tag" + tag);
        }
        // apparently class names do not need a track_bit set to be the target of objectv's. WTF
        track_stuff(position, className);

        log.fine("Object Classname: " + className);

        // now read the struct (better be a hash!)
        var structure = readSingleValue();
        log.fine("Object Type: " + structure.getClass().getName());
        if (structure instanceof Map) {
            // now "bless" this into a class, perl style
            //@SuppressWarnings("unchecked")
            var classData = structure;
            try {
                // either an existing java class
                var c = Class.forName(className.getString());
                return Utils.bless(c, classData);
            } catch (e) {
                // or we make a new one
                if (objectType == ObjectType.POJO) {
                    return Utils.bless(className.getString(), classData);
                } else {
                    // or we make a Perl-style one
                    return new PerlObject(className.getString(), classData);
                }

            }
        } else if (structure.getClass().isArray()) {
            // nothing we can really do here except make Perl objects..
            return new PerlObject(className.getString(), structure);
        } else if (structure instanceof PerlReference) {
            return new PerlObject(className.getString(), structure);
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
    function setData(/*ByteBuffer*/ blob) {
        data = new DataReader(blob);
        data.rewind();
    }

    function track_stuff(pos, /*Object*/ thing) {
        log.fine("Saving " + thing + " at offset " + pos);
        var ref = thing; // autoboxing ftw
        tracked["track_" + pos] = ref;
    }

    function reset() {
        data = null;
        realData = null;
        tracked.clear();
    }

}

function DataReader(_buffer) {
    var _this = this;
    Function.addTo(_this, [rewind, limit, getInt, get, hasRemaining, position, remaining, toString]);
    var _view = new DataView(_buffer);
    var _pos;

    _this._view = _view;

    function toString() {
        return "DataReader pos=" + _pos;
    }

    function position(value) {
        if (arguments.length == 0)
            return _pos;
        _pos = value;
    }
    function rewind() {
        _pos = _view.byteOffset;
    }
    function hasRemaining() {
        return remaining() > 0;
    }
    function remaining() {
        return _view.byteLength - _pos;
    }
    function limit() {
        return _view.byteLength - _pos;
    }
    function getInt() {
        var value = _view.getInt32(_pos);
        _pos += 4;
        return value;
    }
    function get(buf) {
        if (arguments.length == 1) {
            var arr = new Uint8Array(buf);
            for (var i = 0; i < arr.length; i++)
                arr[i] = get();
            return buf;
        }
        var value = _view.getInt8(_pos);
        _pos++;
        return value;
    }
}

Number.prototype.toHex = function () { return this.toString(16); }
//DataView.prototype.rewind = function () {
//    this.pos = this.byteOffset;
//}

//DataView.prototype.limit = function () {
//    return this.byteLength - this.pos;
//}

function Log() {
    var _this = this;
    Function.addTo(_this, [fine]);
    function fine(arg) {
        console.info(arg);
    }

}

function PerlReference(_value) {
    var _this = this;
    Function.addTo(_this, [getValue]);
    function getValue(){
        return _value;
    }
}
function Latin1String(_bytes) {
    var _this = this;
    _this._string = ab2str2(_bytes);
    //console.log("decoded Latin1String", _bytes, _this._string);
    Function.addTo(_this, [valueOf, toString]);

    function valueOf() {
        return _this._string;
    }
    function toString() {
        return _this._string;
    }
    _this.valueOf = function () { return _this._string; }
}

function Utils(){

}
Utils.dump = function(obj){
    console.debug(obj);
}