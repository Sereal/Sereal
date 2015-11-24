interface DecoderOptions {
    object_type?: string;
    use_perl_refs?: boolean;
    preserve_pad_tags?: boolean;
    prefer_latin1?: boolean;
}
/**
 * WIP Decoder for Sereal WIP
 */
class Decoder {
    static PERL_OBJECT = "PERL_OBJECT"; // Perl style object (name + hash)
    static POJO = "POJO"; // Dynamically compile a Plain Old Java Object
    
    // 0x6c72733d but little endian for some reason
    static MAGIC = 1039364716;//(0x6c) + (0x72 << 8) + (0x73 << 16) + (0x3d << 24);
    static SRL_MASK_SHORT_BINARY_LEN: number = 31; // lower 5 bits






    /*
	Note: Despite this interface already being named SerealHeader we still use SRL_HDR_
			as a prefix so grepping will show both these and the C ones.
	
=for autoupdater start
* NOTE this section is autoupdated by author_tools/update_from_header.pl */
    static SRL_HDR_POS = 0; /*   0 0x00 0b00000000 small positive integer - value in low 4 bits (identity) */
    static SRL_HDR_POS_LOW = 0; /*   0 0x00 0b00000000 small positive integer - value in low 4 bits (identity) */
    static SRL_HDR_POS_HIGH = 15; /*  15 0x0f 0b00001111 small positive integer - value in low 4 bits (identity) */
    static SRL_HDR_NEG = 16; /*  16 0x10 0b00010000 small negative integer - value in low 4 bits (k+32) */
    static SRL_HDR_NEG_LOW = 16; /*  16 0x10 0b00010000 small negative integer - value in low 4 bits (k+32) */
    static SRL_HDR_NEG_HIGH = 31; /*  31 0x1f 0b00011111 small negative integer - value in low 4 bits (k+32) */
    static SRL_HDR_VARINT = 32; /*  32 0x20 0b00100000 <VARINT> - Varint variable length integer */
    static SRL_HDR_ZIGZAG = 33; /*  33 0x21 0b00100001 <ZIGZAG-VARINT> - Zigzag variable length integer */
    static SRL_HDR_FLOAT = 34; /*  34 0x22 0b00100010 <IEEE-FLOAT> */
    static SRL_HDR_DOUBLE = 35; /*  35 0x23 0b00100011 <IEEE-DOUBLE> */
    static SRL_HDR_LONG_DOUBLE = 36; /*  36 0x24 0b00100100 <IEEE-LONG-DOUBLE> */
    static SRL_HDR_UNDEF = 37; /*  37 0x25 0b00100101 None - Perl undef var; eg my $var= undef; */
    static SRL_HDR_BINARY = 38; /*  38 0x26 0b00100110 <LEN-VARINT> <BYTES> - binary/(latin1) string */
    static SRL_HDR_STR_UTF8 = 39; /*  39 0x27 0b00100111 <LEN-VARINT> <UTF8> - utf8 string */
    static SRL_HDR_REFN = 40; /*  40 0x28 0b00101000 <ITEM-TAG>    - ref to next item */
    static SRL_HDR_REFP = 41; /*  41 0x29 0b00101001 <OFFSET-VARINT> - ref to previous item stored at offset */
    static SRL_HDR_HASH = 42; /*  42 0x2a 0b00101010 <COUNT-VARINT> [<KEY-TAG> <ITEM-TAG> ...] - count followed by key/value pairs */
    static SRL_HDR_ARRAY = 43; /*  43 0x2b 0b00101011 <COUNT-VARINT> [<ITEM-TAG> ...] - count followed by items */
    static SRL_HDR_OBJECT = 44; /*  44 0x2c 0b00101100 <STR-TAG> <ITEM-TAG> - class, object-item */
    static SRL_HDR_OBJECTV = 45; /*  45 0x2d 0b00101101 <OFFSET-VARINT> <ITEM-TAG> - offset of previously used classname tag - object-item */
    static SRL_HDR_ALIAS = 46; /*  46 0x2e 0b00101110 <OFFSET-VARINT> - alias to item defined at offset */
    static SRL_HDR_COPY = 47; /*  47 0x2f 0b00101111 <OFFSET-VARINT> - copy of item defined at offset */
    static SRL_HDR_WEAKEN = 48; /*  48 0x30 0b00110000 <REF-TAG> - Weaken the following reference */
    static SRL_HDR_REGEXP = 49; /*  49 0x31 0b00110001 <PATTERN-STR-TAG> <MODIFIERS-STR-TAG> */
    static SRL_HDR_OBJECT_FREEZE = 50; /*  50 0x32 0b00110010 <STR-TAG> <ITEM-TAG> - class, object-item. Need to call "THAW" method on class after decoding */
    static SRL_HDR_OBJECTV_FREEZE = 51; /*  51 0x33 0b00110011 <OFFSET-VARINT> <ITEM-TAG> - (OBJECTV_FREEZE is to OBJECT_FREEZE as OBJECTV is to OBJECT) */
    static SRL_HDR_RESERVED = 52; /*  52 0x34 0b00110100 reserved */
    static SRL_HDR_RESERVED_LOW = 52; /*  52 0x34 0b00110100 reserved */
    static SRL_HDR_RESERVED_HIGH = 56; /*  56 0x38 0b00111000 reserved */
    static SRL_HDR_CANONICAL_UNDEF = 57; /*  57 0x39 0b00111001 undef (PL_sv_undef) - "the" Perl undef (see notes) */
    static SRL_HDR_FALSE = 58; /*  58 0x3a 0b00111010 false (PL_sv_no) */
    static SRL_HDR_TRUE = 59; /*  59 0x3b 0b00111011 true  (PL_sv_yes) */
    static SRL_HDR_MANY = 60; /*  60 0x3c 0b00111100 <LEN-VARINT> <TYPE-BYTE> <TAG-DATA> - repeated tag (not done yet, will be implemented in version 3) */
    static SRL_HDR_PACKET_START = 61; /*  61 0x3d 0b00111101 (first byte of magic string in header) */
    static SRL_HDR_EXTEND = 62; /*  62 0x3e 0b00111110 <BYTE> - for additional tags */
    static SRL_HDR_PAD = 63; /*  63 0x3f 0b00111111 (ignored tag, skip to next byte) */
    static SRL_HDR_ARRAYREF = 64; /*  64 0x40 0b01000000 [<ITEM-TAG> ...] - count of items in low 4 bits (ARRAY must be refcnt=1) */
    static SRL_HDR_ARRAYREF_LOW = 64; /*  64 0x40 0b01000000 [<ITEM-TAG> ...] - count of items in low 4 bits (ARRAY must be refcnt=1) */
    static SRL_HDR_ARRAYREF_HIGH = 79; /*  79 0x4f 0b01001111 [<ITEM-TAG> ...] - count of items in low 4 bits (ARRAY must be refcnt=1) */
    static SRL_HDR_HASHREF = 80; /*  80 0x50 0b01010000 [<KEY-TAG> <ITEM-TAG> ...] - count in low 4 bits, key/value pairs (HASH must be refcnt=1) */
    static SRL_HDR_HASHREF_LOW = 80; /*  80 0x50 0b01010000 [<KEY-TAG> <ITEM-TAG> ...] - count in low 4 bits, key/value pairs (HASH must be refcnt=1) */
    static SRL_HDR_HASHREF_HIGH = 95; /*  95 0x5f 0b01011111 [<KEY-TAG> <ITEM-TAG> ...] - count in low 4 bits, key/value pairs (HASH must be refcnt=1) */
    static SRL_HDR_SHORT_BINARY = 96; /*  96 0x60 0b01100000 <BYTES> - binary/latin1 string, length encoded in low 5 bits of tag */
    static SRL_HDR_SHORT_BINARY_LOW = 96; /*  96 0x60 0b01100000 <BYTES> - binary/latin1 string, length encoded in low 5 bits of tag */
    static SRL_HDR_SHORT_BINARY_HIGH = 127; /* 127 0x7f 0b01111111 <BYTES> - binary/latin1 string, length encoded in low 5 bits of tag */
    static SRL_HDR_TRACK_FLAG = 128; /* 128 0x80 0b10000000 if this bit is set track the item */
    /*
    * NOTE the above section is auto-updated by author_tools/update_from_header.pl
    =for autoupdater stop
        */

    //private final boolean 
    prefer_latin1: boolean;


    //// set up logging: be compatible with log4j etc but not suffer so much :)
    //interface Log {
    //	void info(String info);

    //	void fine(String info);

    //	void setLevel(Level lvl);
    //}

    log = new Log();// {
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

    properties = {};//new HashMap<String, Object>(); // where
    // we
    // save
    // protocol
    // version
    // and
    // encoding

    data: DataReader;

    // where we track items for REFP purposes
    tracked: Object = {};//new HashMap<String, Object>();

    objectType: string;
    perlRefs = false;
    preservePadding = false;

    realData: ArrayBuffer;

    //Function.addTo(_this, [decode, decodeFile, decode_sereal, getState, setData, readSingleValue]);
    /**
     * Create a new Decoder
     *
     * @param options
     *           object_type: ObjectType (defaults to PERL_OBJECT)
     *           use_perl_refs: if true wraps things in References to we can "perfectly" roundtrip
     */                      
    //{ object_type: string, use_perl_refs: boolean, preserve_pad_tags: boolean, prefer_latin1:boolean}
    constructor(options: DecoderOptions = null) {
        if (options == null)
            options = {};
        //this.options = options == null ? new HashMap<String, Object>() : options;

        this.objectType = options.object_type || Decoder.PERL_OBJECT;
        this.perlRefs = options.use_perl_refs || false;//( "use_perl_refs" ) ? ((Boolean) options.get( "use_perl_refs" )) : false;
        this.preservePadding = options.preserve_pad_tags || false;//" ) ? ((Boolean) options.get( "preserve_pad_tags" )) : false;
        this.prefer_latin1 = options.prefer_latin1 || false;//") ? ((Boolean) options.get("prefer_latin1")) : false;
    }
    getState() {
        return { properties: this.properties, tracked: this.tracked, perlRefs: this.perlRefs, preservePadding: this.preservePadding, realData: this.realData };
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
    //static decode_sereal(f: File, options: DecoderOptions) {//throws SerealException, IOException {
    //    var d = new Decoder(options);
    //    return d.decodeFile(f);
    //}

    //decodeFile( f: File) {//throws SerealException, IOException {

    //    this.log.fine("Decoding: " + f.getName());

    //    if (!f.exists()) {
    //        throw new FileNotFoundException("No such file: " + f.getCanonicalPath());
    //    }

    //    // read everything
    //    var size = f.length(); // yeah yeah truncate
    //    log.fine("File size: " + size);
    //    var buf = new ArrayBuffer(size);
    //    //FileInputStream fi = new FileInputStream( f );
    //    //fi.getChannel().read( buf );
    //    //fi.close();
    //    //log.fine( "Raw: " + new String( buf.array() ) );

    //    setData(buf);
    //    var structure = decode();
    //    console.info("Decoded: ", structure);

    //    return structure;
    //}

    //function checkHeader() {//throws SerealException {

    //    if (data.limit() < 4) {
    //        throw new SerealException("Invalid Sereal header: too few bytes");
    //    }

    //    if (data.getInt() != MAGIC) {
    //        throw new SerealException("Invalid Seareal header: doesn't match magic");
    //    }

    //}

    //function checkHeaderSuffix() {

    //    var /*long*/ suffix_size = read_varint();
    //    properties.suffix_size = suffix_size;

    //    log.fine("Header suffix size: " + suffix_size);

    //    // skip everything in the optional suffix part
    //    //HACK:
    //    //data.position(data.position() + suffix_size);
    //    data.get(); //TODO: parse eightBitFlags

    //}

    checkNoEOD() {// throws SerealException {

        if (this.data.remaining() == 0) {
            throw new SerealException("Unexpected end of data at byte " + this.data.limit());
        }

    }

    //function checkProtoAndFlags() {// throws SerealException {

    //    if (data.limit() < 1) {
    //        throw new SerealException("Invalid Sereal header: no protocol/version byte");
    //    }

    //    var /*int*/ protoAndFlags = data.get();
    //    var /*int*/protocolVersion = protoAndFlags & 15; // 4 bits for version

    //    log.fine("Version: " + protocolVersion);
    //    //HACK
    //    //if (protocolVersion != 1) {
    //    //    throw new SerealException(String.format("Invalid Sereal header: unsupported protocol version %d", protocolVersion));
    //    //}
    //    properties.protocol_version = protocolVersion;

    //    var /*int*/ encoding = (protoAndFlags & ~15) >> 4;
    //    log.fine("Encoding: " + encoding);
    //    //HACK:
    //    //if ((encoding == 1 || encoding == 2) && !options.containsKey("snappy_support")) {
    //    //    throw new SerealException("Unsupported encoding: Snappy");
    //    //} else if (encoding < 0 || encoding > 2) {
    //    //    throw new SerealException("Unsupported encoding: unknown");
    //    //}

    //    properties.encoding = encoding;

    //}

    ///**
    // *
    // * @return deserealized object
    // * @throws SerealException
    // * @throws IOException 
    // */
    //function decode() {// throws SerealException, IOException {

    //    if (data == null) {
    //        throw new SerealException("No data set");
    //    }

    //    log.fine("Decoding: " + data.toString());// + " - " + new String(data.array()));

    //    checkHeader();
    //    checkProtoAndFlags();
    //    checkHeaderSuffix();

    //    realData = data;
    //    var /*int*/encoding = properties.encoding;
    //    if (encoding == 1 || encoding == 2) {
    //        uncompressSnappy();
    //    }
    //    var out = readSingleValue();

    //    log.fine("Read: " + out);
    //    log.fine("Data left: " + (realData.limit() - realData.position()));

    //    return out;
    //}

    //function uncompressSnappy() {// throws IOException, SerealException {
    //    var /*int*/ len = realData.limit() - realData.position() - 1;

    //    if (properties.encoding == 2) {
    //        len = read_varint();
    //    }
    //    var pos = realData.position();
    //    var /*byte[]*/ compressed = new byte[len];
    //    realData.get(compressed, 0, len);
    //    var /*byte[]*/ uncompressed = new byte[pos + Snappy.uncompressedLength(compressed, 0, len)];
    //    if (!Snappy.isValidCompressedBuffer(compressed)) {
    //        throw new SerealException("Invalid snappy data");
    //    }
    //    Snappy.uncompress(compressed, 0, len, uncompressed, pos);
    //    data = ByteBuffer.wrap(uncompressed);
    //    data.position(pos);
    //}

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
    read_array(tag: number, track: number): Object[] {// throws SerealException {

        var length: number = 0;
        if (tag == 0) {
            length = this.read_varint();
        } else {
            length = tag & 15;
        }

        this.log.fine("Array length: " + length);

        var /*Object[]*/ out = new Array(length);//new Object[length];
        if (track != 0) { // track ourself
            this.track_stuff(track, out);
        }

        for (var i = 0; i < length; i++) {
            out[i] = this.readSingleValue();
            this.log.fine("Read array element " + i + ": " + Utils.dump(out[i]));
        }

        return out;
    }

    /**
     * Reads a byte array, but was called read_binary in C, so for grepping purposes I kept the name
     * 
     * For some reason we call them Latin1Strings.
     * @return
     */
    read_binary(): ArrayBuffer {

        var /*int*/ length = this.read_varint();
        var /*byte[]*/ out = new Uint8Array(length);//byte[length];
        for (var i = 0; i < length; i++) {
            out[i] = this.data.get();
        }

        return out.buffer;
    }

    read_hash(/*byte*/ tag: number, /*int */ track: number): Object {// throws SerealException {
        var num_keys = 0;
        if (tag == 0) {
            num_keys = this.read_varint();
        } else {
            num_keys = tag & 15;
        }

        var hash = {};//new HashMap<String, Object>( num_keys );
        if (track != 0) { // track ourself
            this.track_stuff(track, hash);
        }

        this.log.fine("Reading " + num_keys + " hash elements");

        for (var i = 0; i < num_keys; i++) {
            var keyObject = this.readSingleValue();
            var /*CharSequence*/ key;
            if (typeof (keyObject) == "string") {
                key = /*(CharSequence)*/ keyObject;
            } else if (keyObject instanceof ArrayBuffer) {
                key = new Latin1String(keyObject).toString();
            } else {
                throw new Error("A key is expected to be a byte or character sequence, but got " + keyObject.toString());
            }
            var val = this.readSingleValue();
            hash[key] = val;
        }

        return hash;
    }

    get_tracked_item(): Object {
        var offset = this.read_varint();
        this.log.fine("Creating ref to item previously read at offset: " + offset + " which is: " + this.tracked["track_" + offset]);
        this.log.fine("keys: " + Object.keys(this.tracked) + " vals: " + Object.values(this.tracked));
        return this.tracked["track_" + offset];
    }

    // top bit set (0x80) means next byte is 7 bits more more varint
    read_varint(): number {

        var uv = 0;
        var lshift = 0;

        var /*byte*/ b = this.data.get();
        while (this.data.hasRemaining() && (b < 0)) {
            uv |= (b & 127) << lshift; // add 7 bits
            lshift += 7;
            b = this.data.get();
        }
        uv |= b << lshift; // add final (or first if there is only 1)

        return uv;

    }

    readSingleValue(): any {//  throws SerealException {

        this.checkNoEOD();

        var tag = this.data.get();

        var track = 0;
        if ((tag & Decoder.SRL_HDR_TRACK_FLAG) != 0) {
            tag = tag & ~Decoder.SRL_HDR_TRACK_FLAG;
            track = this.data.position() - 1;
            this.log.fine("Tracking stuff at position: " + track);
        }

        this.log.fine("Tag: " + (tag & 0xFF));// + " = " + tag.toHex());
        var out;

        if (tag <= Decoder.SRL_HDR_POS_HIGH) {
            this.log.fine("Read small positive int:" + tag);
            out = tag;
        } else if (tag <= Decoder.SRL_HDR_NEG_HIGH) {
            this.log.fine("Read small negative int:" + (tag - 32));
            out = tag - 32;
        } else if ((tag & Decoder.SRL_HDR_SHORT_BINARY_LOW) == Decoder.SRL_HDR_SHORT_BINARY_LOW) {
            var /*byte[]*/ short_binary = this.read_short_binary(tag);
            this.log.fine("Read short binary: " + short_binary + " length " + short_binary.byteLength);
            out = this.prefer_latin1 ? new Latin1String(short_binary).toString() : short_binary;
        } else if ((tag & Decoder.SRL_HDR_HASHREF) == Decoder.SRL_HDR_HASHREF) {
            var hash = this.read_hash(tag, track);
            this.log.fine("Read hash: " + hash);
            out = hash;
        } else if ((tag & Decoder.SRL_HDR_ARRAYREF) == Decoder.SRL_HDR_ARRAYREF) {
            this.log.fine("Reading arrayref");
            var /*Object[]*/ arr = this.read_array(tag, track);
            this.log.fine("Read arrayref: " + arr);
            out = arr;
        } else {
            switch (tag) {
                case Decoder.SRL_HDR_VARINT:
                    var l = this.read_varint();
                    this.log.fine("Read varint: " + l);
                    out = l;
                    break;
                case Decoder.SRL_HDR_ZIGZAG:
                    var zz = this.read_zigzag();
                    this.log.fine("Read zigzag: " + zz);
                    out = zz;
                    break;
                case Decoder.SRL_HDR_DOUBLE:
                    // Java defaults to BE, maybe we can jsut do this generally, don't know yet (but think so)
                    this.data.order(ByteOrder.LITTLE_ENDIAN);
                    var d = this.data.getDouble();
                    this.data.order(ByteOrder.BIG_ENDIAN);
                    this.log.fine("Read double: " + d);
                    out = d;
                    break;
                case Decoder.SRL_HDR_TRUE:
                    this.log.fine("Read: TRUE");
                    out = true;
                    break;
                case Decoder.SRL_HDR_FALSE:
                    this.log.fine("Read: FALSE");
                    out = false;
                    break;
                case Decoder.SRL_HDR_UNDEF:
                    this.log.fine("Read a null/undef");
                    out = null;
                    break;
                case Decoder.SRL_HDR_BINARY:
                    var /*byte[]*/ bytes = this.read_binary();
                    this.log.fine("Read binary: " + bytes);
                    out = this.prefer_latin1 ? new Latin1String(bytes).toString() : bytes;
                    break;
                case Decoder.SRL_HDR_STR_UTF8:
                    var utf8 = this.read_UTF8();
                    this.log.fine("Read UTF8: " + utf8);
                    out = utf8;
                    break;
                case Decoder.SRL_HDR_REFN:
                    this.log.fine("Reading ref to next");
                    var /*PerlReference*/ refn = new PerlReference(this.readSingleValue());
                    if (this.perlRefs) {
                        out = refn;
                    } else {
                        out = refn.getValue();
                    }
                    this.log.fine("Read ref: " + Utils.dump(out));
                    break;
                case Decoder.SRL_HDR_REFP:
                    this.log.fine("Reading REFP (ref to prev)");
                    var offset_prev = this.read_varint();
                    if (!this.tracked.hasOwnProperty("track_" + offset_prev)) {
                        throw new SerealException("REFP to offset " + offset_prev + ", which is not tracked");
                    }
                    var prv_value = this.tracked.hasOwnProperty("track_" + offset_prev);
                    var prev = this.perlRefs ? new PerlReference(prv_value) : prv_value;
                    this.log.fine("Read prev: " + Utils.dump(prev));
                    out = prev;
                    break;
                case Decoder.SRL_HDR_OBJECT:
                    this.log.fine("Reading an object");
                    var obj = this.read_object();
                    this.log.fine("Read object: " + obj);
                    out = obj;
                    break;
                case Decoder.SRL_HDR_OBJECTV:
                    this.log.fine("Reading an objectv");
                    var className: string = <string>this.get_tracked_item();
                    this.log.fine("Read an objectv of class: " + className);
                    out = new PerlObject(className, this.readSingleValue());
                    break;
                case Decoder.SRL_HDR_COPY:
                    this.log.fine("Reading a copy");
                    var copy = this.read_copy();
                    this.log.fine("Read copy: " + copy);
                    out = copy;
                    break;
                case Decoder.SRL_HDR_ALIAS:
                    this.log.fine("Reading an alias");
                    var alias = new Alias(this.get_tracked_item());
                    this.log.fine("Read alias: " + Utils.dump(alias));
                    out = alias;
                    break;
                case Decoder.SRL_HDR_WEAKEN:
                    this.log.fine("Weakening the next thing");
                    // so the next thing HAS to be a ref (afaict) which means we can track it
                    var placeHolder = new PerlReference(this.readSingleValue().getValue());
                    var /*WeakReference<PerlReference>*/ wref = new WeakReference(placeHolder);
                    out = wref;
                    break;
                case Decoder.SRL_HDR_HASH:
                    var hash = this.read_hash(0, track);
                    this.log.fine("Read hash: " + hash);
                    out = hash;
                    break;
                case Decoder.SRL_HDR_ARRAY:
                    this.log.fine("Reading array");
                    var arr = this.read_array(0, track);
                    this.log.fine("Read array: " + Utils.dump(arr));
                    out = arr;
                    break;
                case Decoder.SRL_HDR_REGEXP:
                    this.log.fine("Reading Regexp");
                    var pattern = this.read_regex();
                    this.log.fine("Read regexp: " + pattern);
                    out = pattern;
                    break;
                case Decoder.SRL_HDR_PAD:
                    this.log.fine("Padding byte: skip");
                    return this.preservePadding ? new Padded(this.readSingleValue()) : this.readSingleValue();
                default:
                    throw new SerealException("Tag not supported: " + tag);
            }
        }

        if (track != 0) { // we double-track arrays ATM (but they just overwrite)
            this.track_stuff(track, out);
        }
        this.log.fine("returning: " + out);

        return out;

    }


    /**
     * Read a short binary ISO-8859-1 (latin1) string, the lower bits of the tag hold the length
     * @param tag
     * @return
     */
    read_short_binary(tag: number): ArrayBuffer {
        var length: number = tag & Decoder.SRL_MASK_SHORT_BINARY_LEN;
        this.log.fine("Short binary, length: " + length);
        var buf = new ArrayBuffer(length);
        this.data.getBytes(buf);
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
    read_copy(): any {//  throws SerealException {

        var /*int*/originalPosition = this.read_varint();
        var /*int*/currentPosition = this.data.position(); // remember where we parked

        // note: you might think you'd like to use mark() and reset(), but setting position(..) discards the mark
        this.data.position(originalPosition);
        var copy = this.readSingleValue();
        this.data.position(currentPosition); // go back to where we were

        return copy;
    }

    read_UTF8(): string {
        throw new Error("notimplemented");
        //TODO:
        //var /*int*/ length = this.read_varint();
        //var /*byte[]*/ buf = new ArrayBuffer(length);
        //this.data.get(buf);
        //return Charset.forName("UTF-8").decode(ByteBuffer.wrap(buf)).toString();
    }

    read_zigzag(): number {

        var n = this.read_varint();

        return (n >>> 1) ^ (-(n & 1)); // note the unsigned right shift
    }

    read_regex(): RegExp {//throws SerealException {

        var flags = "";
        var str = this.readSingleValue();
        var regex: string;
        if (typeof (str) == "string") {
            regex = str.toString();
        } else if (str instanceof ArrayBuffer) {
            regex = new Latin1String(str).toString();
        } else {
            throw new SerealException("Regex has to be built from a char or byte sequence");
        }
        this.log.fine("Read pattern: " + regex);

        // now read modifiers
        var /*byte*/ tag = this.data.get();
        if ((tag & Decoder.SRL_HDR_SHORT_BINARY_LOW) == Decoder.SRL_HDR_SHORT_BINARY_LOW) {
            var length: number = tag & Decoder.SRL_MASK_SHORT_BINARY_LEN;
            while (length-- > 0) {
                var /*byte*/ value = String.fromCharCode(this.data.get());
                switch (value) {
                    case 'm':
                        flags += "m";
                        break;
                    case 's':
                        flags += "s";
                        break;
                    case 'i':
                        flags += "i";
                        break;
                    case 'x':
                        flags += "x";//flags | Pattern.COMMENTS;
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

        return new RegExp(regex, flags);
    }

    read_object(): Object {// throws SerealException {

        // first read the classname
        // Maybe we should have some kind of read_string() method?
        var position = this.data.position();
        var tag = this.data.get();
        var className;
        if ((tag & Decoder.SRL_HDR_SHORT_BINARY_LOW) == Decoder.SRL_HDR_SHORT_BINARY_LOW) {
            var length = tag & Decoder.SRL_MASK_SHORT_BINARY_LEN;
            var /*byte[]*/ buf = new ArrayBuffer(length);
            this.data.getBytes(buf);
            className = new Latin1String(/*new String(*/buf/*)*/).toString();
        } else {
            throw new SerealException("Don't know how to read classname from tag" + tag);
        }
        // apparently class names do not need a track_bit set to be the target of objectv's. WTF
        this.track_stuff(position, className);

        this.log.fine("Object Classname: " + className);

        // now read the struct (better be a hash!)
        var structure = this.readSingleValue();
        this.log.fine("Object Type: " + structure.getClass().getName());
        if (structure instanceof Object) {
            // now "bless" this into a class, perl style
            //@SuppressWarnings("unchecked")
            var classData = structure;
            try {
                // either an existing java class
                var c = Class.forName(className.getString());
                return Utils.bless(c, classData);
            } catch (e) {
                // or we make a new one
                if (this.objectType == Decoder.POJO) {
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
    setData(/*ByteBuffer*/ blob) {
        this.data = new DataReader(blob);
        this.data.rewind();
    }

    track_stuff(pos, /*Object*/ thing) {
        this.log.fine("Saving " + thing + " at offset " + pos);
        var ref = thing; // autoboxing ftw
        this.tracked["track_" + pos] = ref;
    }

    reset() {
        this.data = null;
        this.realData = null;
        this.tracked = {};//.clear();
    }

}


