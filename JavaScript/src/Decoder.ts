interface DecoderOptions {
    /** (defaults to PERL_OBJECT) */
    object_type?: string;
    /** if true wraps things in References to we can "perfectly" roundtrip */
    use_perl_refs?: boolean;
    preserve_pad_tags?: boolean;
    prefer_latin1?: boolean;
}

/** Decoder for Sereal */
class Decoder {

    static PERL_OBJECT = "PERL_OBJECT"; // Perl style object (name + hash)
    static POJO = "POJO";               // Dynamically compile a Plain Old Java Object

    prefer_latin1: boolean;
    log = new Log();
    properties = {};                    // where we save protocol, version and encoding
    data: DataReader;
    tracked: Object = {};//new HashMap<String, Object>(); // where we track items for REFP purposes
    objectType: string;
    perlRefs = false;
    preservePadding = false;
    realData: ArrayBuffer;
    tagReaders: Array<() => any> = new Array(128);

    constructor(options: DecoderOptions = null) {
        if (options == null)
            options = {};
        //this.options = options == null ? new HashMap<String, Object>() : options;

        this.objectType = options.object_type || Decoder.PERL_OBJECT;
        this.perlRefs = options.use_perl_refs || false;//( "use_perl_refs" ) ? ((Boolean) options.get( "use_perl_refs" )) : false;
        this.preservePadding = options.preserve_pad_tags || false;//" ) ? ((Boolean) options.get( "preserve_pad_tags" )) : false;
        this.prefer_latin1 = options.prefer_latin1 || false;//") ? ((Boolean) options.get("prefer_latin1")) : false;

        this.tagReaders[Tags.VARINT] = this.read_varint;
        this.tagReaders[Tags.ZIGZAG] = this.read_zigzag;
        this.tagReaders[Tags.DOUBLE] = this.read_double;

        this.tagReaders[Tags.DOUBLE] = this.read_double;
        this.tagReaders[Tags.TRUE] = () => true;
        this.tagReaders[Tags.FALSE] = () => false;
        this.tagReaders[Tags.UNDEF] = () => null;
        this.tagReaders[Tags.BINARY] = this.read_binary;
        this.tagReaders[Tags.STR_UTF8] = this.read_UTF8;

        this.tagReaders[Tags.REFN] = this.read_refn;
        this.tagReaders[Tags.REFP] = this.read_refp;

        this.tagReaders[Tags.OBJECT] = this.read_object;
        this.tagReaders[Tags.OBJECTV] = this.read_object_v;

        this.tagReaders[Tags.COPY] = this.read_copy;
        this.tagReaders[Tags.ALIAS] = this.read_alias;
        this.tagReaders[Tags.WEAKEN] = this.read_weaken;


        this.tagReaders[Tags.REGEXP] = this.read_regex;
    }

    read_weaken() {
        this.log.fine("Weakening the next thing");
        // so the next thing HAS to be a ref (afaict) which means we can track it
        var placeHolder = new PerlReference(this.readSingleValue().getValue());
        var /*WeakReference<PerlReference>*/ wref = new WeakReference(placeHolder);
        return wref;
    }
    read_alias() {
        this.log.fine("Reading an alias");
        var alias = new Alias(this.get_tracked_item());
        this.log.fine("Read alias: " + Utils.dump(alias));
        return alias;
    }
    read_object_v() {
        this.log.fine("Reading an objectv");
        var className: string = <string>this.get_tracked_item();
        this.log.fine("Read an objectv of class: " + className);
        var out = new PerlObject(className, this.readSingleValue());
        return out;

    }
    read_refp() {
        this.log.fine("Reading REFP (ref to prev)");
        var offset_prev = this.read_varint();
        if (!this.tracked.hasOwnProperty("track_" + offset_prev)) {
            throw new SerealException("REFP to offset " + offset_prev + ", which is not tracked");
        }
        var prv_value = this.tracked.hasOwnProperty("track_" + offset_prev);
        var prev = this.perlRefs ? new PerlReference(prv_value) : prv_value;
        this.log.fine("Read prev: " + Utils.dump(prev));
        return prev;
    }
    read_refn() {
        this.log.fine("Reading ref to next");
        var out;
        var /*PerlReference*/ refn = new PerlReference(this.readSingleValue());
        if (this.perlRefs) {
            out = refn;
        }
        else {
            out = refn.getValue();
        }
        this.log.fine("Read ref: " + Utils.dump(out));
        return out;
    }
    read_double() {
        // Java defaults to BE, maybe we can jsut do this generally, don't know yet (but think so)
        this.data.order(ByteOrder.LITTLE_ENDIAN);
        var d = this.data.getDouble();
        this.data.order(ByteOrder.BIG_ENDIAN);
        this.log.fine("Read double: " + d);
        return d;
    }
    /**
     * if tag == 0, next is varint for number of elements, otherwise lower 4 bits are length
     *
     * @param tag lower 4 bits is length or 0 for next varint is length
     * @param track we might need to track since array elements could refer to us
     */
    read_array(tag: number, track: number): Object[] {

        var length: number = 0;
        if (tag == 0) {
            length = this.read_varint();
        }
        else {
            length = tag & 15;
        }

        this.log.fine("Array length: " + length);

        var out = new Array(length);
        if (track != 0)  // track ourself
            this.track_stuff(track, out);

        for (var i = 0; i < length; i++) {
            out[i] = this.readSingleValue();
            this.log.fine("Read array element " + i + ": " + Utils.dump(out[i]));
        }

        return out;
    }
    /** Reads a byte array, but was called read_binary in C, so for grepping purposes I kept the name. */
    read_binary(): any {

        var /*int*/ length = this.read_varint();
        var /*byte[]*/ out = new Uint8Array(length);//byte[length];
        for (var i = 0; i < length; i++) {
            out[i] = this.data.getByte();
        }

        var buf = out.buffer;
        if (this.prefer_latin1) {
            var res2 = new Latin1String(buf).toString();
            return res2;
        }
        return buf;
    }
    read_hash(tag: number, track: number): Object {
        var num_keys = 0;
        if (tag == 0) {
            num_keys = this.read_varint();
        }
        else {
            num_keys = tag & 15;
        }

        var hash = {};//new HashMap<String, Object>( num_keys );
        if (track != 0) { // track ourself
            this.track_stuff(track, hash);
        }

        this.log.fine("Reading " + num_keys + " hash elements");

        for (var i = 0; i < num_keys; i++) {
            var keyObject = this.readSingleValue();
            var key: string;
            if (typeof (keyObject) == "string") {
                key = <string>keyObject;
            }
            else if (keyObject instanceof ArrayBuffer) {
                key = new Latin1String(keyObject).toString();
            }
            else {
                throw new Error("A key is expected to be a byte or character sequence, but got " + keyObject.toString());
            }
            var val = this.readSingleValue();
            hash[key] = val;
        }

        return hash;
    }
    /** top bit set (0x80) means next byte is 7 bits more more varint */
    read_varint(): number {

        var uv = 0;
        var lshift = 0;

        var b: number = this.data.getByte();
        while (this.data.hasRemaining() && (b < 0)) {
            uv |= (b & 127) << lshift; // add 7 bits
            lshift += 7;
            b = this.data.getByte();
        }
        uv |= b << lshift; // add final (or first if there is only 1)

        return uv;

    }
    readSingleValue(): any {//  throws SerealException {

        this.checkNoEOD();

        var tag = this.data.getByte();

        var track = 0;
        if ((tag & Consts.TRACK_FLAG) != 0) {
            tag = tag & ~Consts.TRACK_FLAG;
            track = this.data.pos - 1;
            this.log.fine("Tracking stuff at position: " + track);
        }

        this.log.fine("Tag: " + (tag & 0xFF));// + " = " + tag.toHex());
        var out;

        if (tag <= Consts.POS_HIGH) {
            this.log.fine("Read small positive int:" + tag);
            out = tag;
        }
        else if (tag <= Consts.NEG_HIGH) {
            this.log.fine("Read small negative int:" + (tag - 32));
            out = tag - 32;
        }
        else if ((tag & Consts.SHORT_BINARY_LOW) == Consts.SHORT_BINARY_LOW) {
            var short_binary: ArrayBuffer = this.read_short_binary(tag);
            this.log.fine("Read short binary: " + short_binary + " length " + short_binary.byteLength);
            out = this.prefer_latin1 ? new Latin1String(short_binary).toString() : short_binary;
        }
        else if ((tag & Tags.HASHREF_0) == Tags.HASHREF_0) {
            var hash = this.read_hash(tag, track);
            this.log.fine("Read hash: " + hash);
            out = hash;
        }
        else if ((tag & Tags.ARRAYREF_0) == Tags.ARRAYREF_0) {
            this.log.fine("Reading arrayref");
            var arr: Object[] = this.read_array(tag, track);
            this.log.fine("Read arrayref: " + arr);
            out = arr;
        }
        else if (this.tagReaders[tag] != null) {
            var func = this.tagReaders[tag];
            out = func.call(this);
        }
        else {
            switch (tag) {
                case Tags.HASH:
                    var hash = this.read_hash(0, track);
                    this.log.fine("Read hash: " + hash);
                    out = hash;
                    break;
                case Tags.ARRAY:
                    this.log.fine("Reading array");
                    var arr = this.read_array(0, track);
                    this.log.fine("Read array: " + Utils.dump(arr));
                    out = arr;
                    break;
                case Tags.PAD:
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
    /** Read a short binary ISO-8859-1 (latin1) string, the lower bits of the tag hold the length */
    read_short_binary(tag: number): ArrayBuffer {
        var length: number = tag & Consts.MASK_SHORT_BINARY_LEN;
        this.log.fine("Short binary, length: " + length);
        var buf = new ArrayBuffer(length);
        this.data.getBytesTo(buf);
        return buf;
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
    read_regex(): RegExp {
        var flags = "";
        var str = this.readSingleValue();
        var regex: string;
        if (typeof (str) == "string") {
            regex = str.toString();
        }
        else if (str instanceof ArrayBuffer) {
            regex = new Latin1String(str).toString();
        }
        else {
            throw new SerealException("Regex has to be built from a char or byte sequence");
        }
        this.log.fine("Read pattern: " + regex);

        // now read modifiers
        var /*byte*/ tag = this.data.getByte();
        if ((tag & Consts.SHORT_BINARY_LOW) == Consts.SHORT_BINARY_LOW) {
            var length: number = tag & Consts.MASK_SHORT_BINARY_LEN;
            while (length-- > 0) {
                var /*byte*/ value = String.fromCharCode(this.data.getByte());
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
        }
        else {
            throw new SerealException("Expecting SHORT_BINARY for modifiers of regexp, got: " + tag);
        }

        return new RegExp(regex, flags);
    }
    read_object(): Object {
        // first read the classname
        // Maybe we should have some kind of read_string() method?
        var position = this.data.pos;
        var tag = this.data.getByte();
        var className;
        if ((tag & Consts.SHORT_BINARY_LOW) == Consts.SHORT_BINARY_LOW) {
            var length = tag & Consts.MASK_SHORT_BINARY_LEN;
            var /*byte[]*/ buf = new ArrayBuffer(length);
            this.data.getBytesTo(buf);
            className = new Latin1String(/*new String(*/buf/*)*/).toString();
        }
        else {
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
                }
                else {
                    // or we make a Perl-style one
                    return new PerlObject(className.getString(), classData);
                }

            }
        }
        else if (structure.getClass().isArray()) {
            // nothing we can really do here except make Perl objects..
            return new PerlObject(className.getString(), structure);
        }
        else if (structure instanceof PerlReference) {
            return new PerlObject(className.getString(), structure);
        }

        // it's a regexp for example
        return structure;

    }
    get_tracked_item(): Object {
        var offset = this.read_varint();
        this.log.fine("Creating ref to item previously read at offset: " + offset + " which is: " + this.tracked["track_" + offset]);
        this.log.fine("keys: " + Object.keys(this.tracked) + " vals: " + Object.values(this.tracked));
        return this.tracked["track_" + offset];
    }
    /** Set the data to deserealize(for calling decode multiple times when there are concatenated packets)(never tested)     */
    setData(data: Uint8Array): void {
        this.data = new DataReader(data);
        this.data.rewind();
    }

    track_stuff(pos: number, thing: Object) {
        this.log.fine("Saving " + thing + " at offset " + pos);
        var ref = thing; // autoboxing ftw
        this.tracked["track_" + pos] = ref;
    }

    reset() {
        this.data = null;
        this.realData = null;
        this.tracked = {};//.clear();
    }
    checkNoEOD(): void {
        if (this.data.remaining() > 0)
            return;
        throw new SerealException("Unexpected end of data at byte " + this.data.limit());
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
    read_copy(): any {

        var originalPosition: number = this.read_varint();
        var currentPosition: number = this.data.pos; // remember where we parked

        // note: you might think you'd like to use mark() and reset(), but setting position(..) discards the mark
        this.data.pos = originalPosition;
        var copy = this.readSingleValue();
        this.data.pos = currentPosition; // go back to where we were

        return copy;
    }

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
