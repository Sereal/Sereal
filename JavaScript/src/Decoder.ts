"use strict";

module Sereal {
    export interface DecoderOptions {
        /** (defaults to PERL_OBJECT) */
        object_type?: string;
        /** if true wraps things in References to we can "perfectly" roundtrip */
        use_perl_refs?: boolean;
        preserve_pad_tags?: boolean;
        prefer_latin1?: boolean;
    }

    /** Decoder for Sereal */
    export class Decoder {

        static PERL_OBJECT = "PERL_OBJECT"; // Perl style object (name + hash)
        static POJO = "POJO";               // Dynamically compile a Plain Old Java Object

        prefer_latin1: boolean;
        log = console;
        reader: DataReader;
        tracked: Object = {};
        objectType: string;
        perlRefs = false;
        preservePadding = false;
        _data: Uint8Array;
        doc: SerealDocument;

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



        decodeBinaryText(binaryText: string): SerealDocument {
            this.doc = new SerealDocument();

            var doc = this.doc;
            var _buf = Utils.str2ab2(binaryText);
            this.reader = new DataReader(new DataView(_buf));
            console.log({ pos: this.reader.pos });
            doc.magic = this.reader.readInt32();
            if (doc.magic != Consts.MAGIC)
                throw new Error();

            console.log({ pos: this.reader.pos });
            var s = this.reader.readByte().toString(2);
            doc.version = parseInt(s.substr(0, 4), 2);
            doc.type = parseInt(s.substr(4, 4), 2);
            console.log(doc.version, doc.type);
            doc.header_suffix_size = this.reader.readVarInt();
            if (doc.header_suffix_size > 0) {
                doc.eight_bit_field = { value: this.reader.readByte(), };
                doc.eight_bit_field.has_user_metadata = doc.eight_bit_field.value.to8BitString().last() == "1";
                if (doc.eight_bit_field.has_user_metadata) {
                    var mdByteArray = this.reader.readBytes(doc.header_suffix_size - 1);
                    doc.user_metadata = this.decodeDocumentBody(mdByteArray);
                }
            }

            if (doc.type != 3)
                throw new Error("only doc.type==3 is implemented");

            doc.body_uncompressed_length = this.reader.readVarInt();
            doc.body_compressed_length = this.reader.readVarInt();

            console.log(doc);

            var deflated = this.deflate();
            doc.body = this.decodeDocumentBody(deflated);
            console.log("DONE!!!!!!!!!", doc);
            return doc;
        }

        decodeDocumentBody(byteArray: Uint8Array): any {
            var dec = new Decoder({ prefer_latin1: true });
            dec.data = byteArray;
            var x = dec.readSingleValue();
            console.log(x);
            return x;
        }


        deflate(): Uint8Array {
            var pos = this.reader.pos;
            var arr = this.reader.readBytes();
            if (arr.length != this.doc.body_compressed_length)
                throw new Error();
            var zip = new Zlib.Inflate(arr);
            var deflated = zip.decompress();
            if (deflated.length != this.doc.body_uncompressed_length)
                throw new Error();
            return deflated;
        }



        read_weaken() {
            //this.log.debug("Weakening the next thing");
            // so the next thing HAS to be a ref (afaict) which means we can track it
            var placeHolder = new PerlReference(this.readSingleValue().getValue());
            var /*WeakReference<PerlReference>*/ wref = new WeakReference(placeHolder);
            return wref;
        }
        read_alias() {
            //this.log.debug("Reading an alias");
            var alias = new Alias(this.get_tracked_item());
            //this.log.debug("Read alias: ", alias);
            return alias;
        }
        read_object_v() {
            //this.log.debug("Reading an objectv");
            var className: string = <string>this.get_tracked_item();
            this.log.debug("Read an objectv of class: ", className);
            var out = new PerlObject(className, this.readSingleValue());
            return out;
        }
        read_refp() {
            var offset_prev = this.read_varint();
            var prv_value = this.getTracked(offset_prev);
            var prev = this.perlRefs ? new PerlReference(prv_value) : prv_value;
            this.log.debug("Read prev: ", prev);
            return prev;
        }
        read_refn() {
            //this.log.debug("read_refn", "Reading ref to next");
            var out;
            var /*PerlReference*/ refn = new PerlReference(this.readSingleValue());
            if (this.perlRefs) {
                out = refn;
            }
            else {
                out = refn.getValue();
            }
            this.log.debug("read_refn", "Read ref: ", out);
            return out;
        }
        read_double() {
            var d = this.reader.readDouble();
            return d;
        }
        /**
         * if tag == 0, next is varint for number of elements, otherwise lower 4 bits are length
         *
         * @param tag lower 4 bits is length or 0 for next varint is length
         * @param track we might need to track since array elements could refer to us
         */
        read_array(tag: number, trackPos: number): Object[] {

            var length: number = 0;
            if (tag == 0) {
                length = this.read_varint();
            }
            else {
                length = tag & 15;
            }

            this.log.debug("Array length: ", length);

            var out = new Array(length);
            if (trackPos != null)  // track ourself
                this.track(trackPos, out);

            for (var i = 0; i < length; i++) {
                out[i] = this.readSingleValue();
                this.log.debug("Read array element ", i, out[i]);
            }

            return out;
        }
        /** Reads a byte array, but was called read_binary in C, so for grepping purposes I kept the name. */
        read_binary(): any {
            this.log.debug("read_binary");

            var /*int*/ length = this.read_varint();
            var /*byte[]*/ out = new Uint8Array(length);//byte[length];
            for (var i = 0; i < length; i++) {
                out[i] = this.reader.readByte();
            }

            var buf = out.buffer;
            if (this.prefer_latin1) {
                var res2 = new Latin1String(buf).toString();
                this.log.debug("returning Latin1String", JSON.stringify(res2));
                return res2;
            }
            return buf;
        }
        read_hash(tag: number, trackPos: number): Object {
            var num_keys = 0;
            if (tag == 0) {
                num_keys = this.read_varint();
            }
            else {
                num_keys = tag & 15; //parseInt(tag.to8BitString().substr(4), 2);
            }

            var hash = {};//new HashMap<String, Object>( num_keys );
            if (trackPos != null) { // track ourself
                this.track(trackPos, hash);
            }

            this.log.debug("Reading ", num_keys, " hash elements");

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
                    key = keyObject.toString();
                    console.warn("A key is expected to be a byte or character sequence, but got ", keyObject);
                }
                var val = this.readSingleValue();
                hash[key] = val;
            }

            return hash;
        }
        read_varint(): number {
            var x = this.reader.readVarInt();
            return x;
        }
        readSingleValue(): any {

            this.checkNoEOD();

            var byte = this.reader.readByte();
            var tag = byte;

            var trackPos = null;
            if ((byte & Consts.TRACK_FLAG) != 0) {
                tag = byte & ~Consts.TRACK_FLAG;
                trackPos = this.reader.pos;// - 1;
                this.log.debug("Tracking stuff at position: ", trackPos);
            }
            this.log.debug("reading", { tag: tag, tagName: Tags[tag], absPos: this.reader.absPos, trackPos });

            //this.log.debug("Tag: " + (tag & 0xFF));// + " = " + tag.toHex());
            var out;

            if (tag <= Consts.POS_HIGH) {
                this.log.debug("Read small positive int:", tag);
                out = tag;
            }
            else if (tag <= Consts.NEG_HIGH) {
                this.log.debug("Read small negative int:", (tag - 32));
                out = tag - 32;
            }
            else if ((tag & Consts.SHORT_BINARY_LOW) == Consts.SHORT_BINARY_LOW) {
                var short_binary: ArrayBuffer = this.read_short_binary(tag);
                out = this.prefer_latin1 ? new Latin1String(short_binary).toString() : short_binary;
                this.log.debug("Read short binary: ", { short_binary, length: short_binary.byteLength, value: out });
            }
            else if ((tag & Tags.HASHREF_0) == Tags.HASHREF_0) {
                var hash = this.read_hash(tag, trackPos);
                this.log.debug("Read hash: ", hash);
                out = hash;
            }
            else if ((tag & Tags.ARRAYREF_0) == Tags.ARRAYREF_0) {
                //this.log.debug("Reading arrayref");
                var arr: Object[] = this.read_array(tag, trackPos);
                this.log.debug("Read arrayref: ", arr);
                out = arr;
            }
            else if (this.tagReaders[tag] != null) {
                var func = this.tagReaders[tag];
                out = func.call(this);
            }
            else {
                switch (tag) {
                    case Tags.HASH:
                        var hash = this.read_hash(0, trackPos);
                        this.log.debug("Read hash: ", hash);
                        out = hash;
                        break;
                    case Tags.ARRAY:
                        //this.log.debug("Reading array");
                        var arr = this.read_array(0, trackPos);
                        this.log.debug("Read array: ", arr);
                        out = arr;
                        break;
                    case Tags.PAD:
                        this.log.debug("Padding byte: skip");
                        return this.preservePadding ? new Padded(this.readSingleValue()) : this.readSingleValue();
                    default:
                        throw new SerealException("Tag not supported: " + tag);
                }
            }

            if (trackPos != null) { // we double-track arrays ATM (but they just overwrite)
                this.track(trackPos, out);
            }
            this.log.debug("read", { tag: Tags[tag], value: out, pos: this.reader.pos, trackPos, });

            return out;

        }

        assertEqual(x: any, y: any) {
            if (x == y)
                return true;
            console.error("not equal", x, y);
            return false;
        }
        /** Read a short binary ISO-8859-1 (latin1) string, the lower bits of the tag hold the length */
        read_short_binary(tag: number): ArrayBuffer {
            var length: number = tag & Consts.MASK_SHORT_BINARY_LEN;
            var length2 = parseInt(tag.to8BitString().substr(3), 2);
            this.assertEqual(length, length2);
            this.log.debug("Short binary, length: " + length);
            var buf = new ArrayBuffer(length);
            this.reader.readBytesTo(buf);
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
            this.log.debug("Read pattern: " + regex);

            // now read modifiers
            var /*byte*/ tag = this.reader.readByte();
            if ((tag & Consts.SHORT_BINARY_LOW) == Consts.SHORT_BINARY_LOW) {
                var length: number = tag & Consts.MASK_SHORT_BINARY_LEN;
                while (length-- > 0) {
                    var value = String.fromCharCode(this.reader.readByte());
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
            var position = this.reader.pos;
            var tag = this.reader.readByte();
            var className;
            if ((tag & Consts.SHORT_BINARY_LOW) == Consts.SHORT_BINARY_LOW) {
                var length = tag & Consts.MASK_SHORT_BINARY_LEN;
                var /*byte[]*/ buf = new ArrayBuffer(length);
                this.reader.readBytesTo(buf);
                className = new Latin1String(/*new String(*/buf/*)*/).toString();
            }
            else {
                throw new SerealException("Don't know how to read classname from tag" + tag);
            }
            // apparently class names do not need a track_bit set to be the target of objectv's. WTF
            this.track(position, className);

            this.log.debug("Object Classname: " + className);

            // now read the struct (better be a hash!)
            var structure = this.readSingleValue();
            this.log.debug("Object Type: " + structure.getClass().getName());
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
            this.log.debug("Creating ref to item previously read at offset: ", offset, this.tracked["track_" + offset]);
            this.log.debug("keys: ", Object.keys(this.tracked), " vals: ", Object.values(this.tracked));
            return this.getTracked(offset);
        }
        /** Set the data to deserealize(for calling decode multiple times when there are concatenated packets)(never tested)     */
        set data(data: Uint8Array) {
            this._data = data;
            this.reader = new DataReader(data);
        }

        track(pos: number, val: Object) {
            this.log.debug("track_stuff", "Saving ", val, " at offset ", pos);
            var ref = val; // autoboxing ftw
            this.tracked["track_" + pos] = ref;
        }

        getTracked(pos: number) {
            var key = "track_" + pos;
            if (!this.tracked.hasOwnProperty(key))
                throw new Error("tracked object not found");
            var val = this.tracked["track_" + pos];
            return val;
        }

        reset() {
            this.reader = null;
            this.tracked = {};//.clear();
        }
        checkNoEOD(): void {
            if (this.reader.remaining() > 0)
                return;
            throw new SerealException("Unexpected end of data at byte "+ this.reader.absPos);
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
            var currentPosition: number = this.reader.pos; // remember where we parked

            // note: you might think you'd like to use mark() and reset(), but setting position(..) discards the mark
            this.reader.pos = originalPosition;
            var copy = this.readSingleValue();
            this.reader.pos = currentPosition; // go back to where we were

            return copy;
        }

    }


}