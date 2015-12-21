"use strict";

module Sereal {

    export class Decoder {

        private perlStyleObjects: boolean = false;
        private prefer_latin1: boolean = true;
        private log = console;
        private reader: DataReader;
        private tracked: Object;
        private objectType: string;
        private perlRefs = false;
        private preservePadding = false;
        private doc: SerealDocument;
        private debug: boolean = false;
        private tagReaders: Array<(tag?: number, trackPos?: number) => any> = new Array(128);

        constructor() {
            this.mapTagReaders();
        }

        init(data: any) {
            this.tracked = {};
            if (data instanceof DataReader)
                this.reader = <DataReader>data;
            else
                this.reader = new DataReader(data);
        }

        decodeDocument(data?: any) {
            if (data != null) {
                this.init(data);
            }

            this.doc = new SerealDocument();
            var doc = this.doc;
            doc.header = new SerealDocumentHeader();


            doc.header.magic = this.reader.readInt32();
            if (doc.header.magic != Consts.MAGIC)
                throw new Error();

            var versionAndType = this.reader.readByte();

            doc.header.version= versionAndType & 15;
            doc.header.type = (versionAndType & ~15) >> 4;

            doc.header.header_suffix_size = this.reader.readVarInt();
            if (doc.header.header_suffix_size > 0) {
                doc.header.eight_bit_field = { value: this.reader.readByte() };
                doc.header.eight_bit_field.has_user_metadata = doc.header.eight_bit_field.value.to8BitString().last() == "1";
                if (doc.header.eight_bit_field.has_user_metadata) {
                    var md = this.reader.readBytes(doc.header.header_suffix_size - 1);
                    doc.user_metadata = this.decodeDocumentBody(md);
                }
            }


            if (doc.header.type == 3) {
                doc.header.body_uncompressed_length = this.reader.readVarInt();
                doc.header.body_compressed_length = this.reader.readVarInt();
                var deflated = this.deflate();
                doc.body = this.decodeDocumentBody(deflated);
            }
            else if (doc.header.type == 0) {
                doc.body = this.decodeDocumentBody();
            }
            else {
                throw new Error("only doc.type==3 or 0 are implemented");
            }
            return doc;
        }

        decodeDocumentBody(data?: Uint8Array): any {
            var dec = new Decoder();
            if (data == null)
                dec.init(this.reader.toDataReader());
            else
                dec.init(new DataReader(data));
            var x = dec.read();
            return x;
        }

        read(): any {
            var byte = this.reader.readByte();
            var tag = byte;

            var trackPos = null;
            if ((byte & Consts.TRACK_FLAG) != 0) {
                tag = byte & ~Consts.TRACK_FLAG;
                trackPos = this.reader.pos; // actually it's pos-1+1;   -1 to get back to the tag pos, +1 because indexes are 1 based
            }
            if (this.debug) this.log.debug("reading", { tag: tag, tagName: Tags[tag], absPos: this.reader.absPos, trackPos });

            var func = this.tagReaders[tag];
            if (func == null)
                throw new SerealException("Tag not supported: " + tag);
            var out = func.call(this, tag, trackPos);

            if (trackPos != null)  // we double-track arrays ATM (but they just overwrite)
                this.track(trackPos, out);

            if (this.debug) this.log.debug("read", { tag: Tags[tag], value: out, pos: this.reader.pos, trackPos, });
            return out;
        }

        deflate(): Uint8Array {
            var pos = this.reader.pos;
            var arr = this.reader.readBytes(this.doc.header.body_compressed_length);
            var zip = new Zlib.Inflate(arr);
            var deflated = zip.decompress();
            if (deflated.length != this.doc.header.body_uncompressed_length)
                throw new Error("decompressed length doesn't match");
            return deflated;
        }

        read_weaken(): any {
            //this.log.debug("Weakening the next thing");
            // so the next thing HAS to be a ref (afaict) which means we can track it
            var placeHolder = new PerlReference(this.read().getValue());
            var /*WeakReference<PerlReference>*/ wref = new WeakReference(placeHolder);
            return wref;
        }
        read_alias(): any {
            var alias = new Alias(this.read_tracked_item());
            return alias;
        }
        read_object_v(): PerlObject {
            //this.log.debug("Reading an objectv");
            var className: string = <string>this.read_tracked_item();
            var out = new PerlObject(className, this.read());
            return out;
        }
        read_refp(): any {
            var offset_prev = this.read_varint();
            var prv_value = this.getTracked(offset_prev);
            var prev = this.perlRefs ? new PerlReference(prv_value) : prv_value;
            return prev;
        }
        read_refn(): any {
            var out = this.read();
            if (this.perlRefs)
                out = new PerlReference(out);
            return out;
        }
        read_double(): number {
            var d = this.reader.readDouble();
            return d;
        }
        read_array(tag: number, trackPos: number): Object[] {
            var length = this.read_varint();
            return this._read_array(length, trackPos);
        }
        /**
         * if tag == 0, next is varint for number of elements, otherwise lower 4 bits are length
         *
         * @param tag lower 4 bits is length or 0 for next varint is length
         * @param track we might need to track since array elements could refer to us
         */
        read_array_ref(tag: number, trackPos: number): Object[] {
            var length = tag & 15;
            return this._read_array(length, trackPos);
        }

        _read_array(length: number, trackPos: number): Object[] {
            var arr = new Array(length);
            if (trackPos != null)
                this.track(trackPos, arr);

            for (var i = 0; i < length; i++) {
                arr[i] = this.read();
                if (this.debug) this.log.debug("Read array element ", i, arr[i]);
            }
            return arr;
        }

        read_binary(): any {
            var length = this.read_varint();
            var s = this.reader.readString(length);
            return s;
        }
        read_hash(tag: number, trackPos: number): Object {
            var num_keys = this.read_varint();
            return this._read_hash(num_keys, trackPos);
        }

        read_hash_ref(tag: number, trackPos: number): Object {
            var num_keys = tag & 15;
            return this._read_hash(num_keys, trackPos);
        }

        _read_hash(num_keys: number, trackPos: number): Object {
            var hash = {};
            if (trackPos != null)
                this.track(trackPos, hash);

            if (this.debug) this.log.debug("Reading ", num_keys, " hash elements");

            for (var i = 0; i < num_keys; i++) {
                var keyObject = this.read();
                var key: string = keyObject;
                var val = this.read();
                hash[key] = val;
            }

            return hash;
        }
        read_varint(): number {
            var x = this.reader.readVarInt();
            return x;
        }


        read_pos(tag: number): number {
            return tag;
        }

        read_neg(tag: number): number {
            return tag - 32;
        }

        read_pad(): any {
            var res = this.read();
            if (this.preservePadding)
                return new Padded(res);
            return res;
        }

        /** Read a short binary ISO-8859-1 (latin1) string, the lower bits of the tag hold the length */
        read_short_binary(tag: number): string {
            var length: number = tag & Consts.MASK_SHORT_BINARY_LEN;
            var s = this.reader.readString(length);
            return s;
        }
        read_UTF8(): string {
            var length = this.read_varint();
            //var buf = new ArrayBuffer(length);
            //this.reader.readBytesTo(buf);
            var s = this.reader.readString(length);
            return s;
            //return Charset.forName("UTF-8").decode(ByteBuffer.wrap(buf)).toString();
        }
        read_zigzag(): number {
            var n = this.read_varint();
            return (n >>> 1) ^ (-(n & 1)); // note the unsigned right shift
        }
        read_regex(): RegExp {
            var flags;
            var regex: string = this.read();

            // now read modifiers
            var tag = this.reader.readByte();
            if ((tag & Consts.SHORT_BINARY_LOW) == Consts.SHORT_BINARY_LOW) {
                var length: number = tag & Consts.MASK_SHORT_BINARY_LEN;
                flags = this.reader.readString(length);
            }
            else {
                throw new SerealException("Expecting SHORT_BINARY for modifiers of regexp, got: " + tag);
            }

            return new RegExp(regex, flags);
        }

        read_object(): Object {
            // first read the classname
            var position = this.reader.pos;
            var tag = this.reader.readByte();
            var className;
            if ((tag & Consts.SHORT_BINARY_LOW) == Consts.SHORT_BINARY_LOW) {
                var length = tag & Consts.MASK_SHORT_BINARY_LEN;
                className = this.reader.readString(length);
            }
            else {
                throw new SerealException("Don't know how to read classname from tag" + tag);
            }
            // apparently class names do not need a track_bit set to be the target of objectv's
            this.track(position, className);

            if (this.debug) this.log.debug("Object Classname: " + className);

            // now read the struct (better be a hash!)
            var structure = this.read();

            if (typeof (structure) == "object") {
                // now "bless" this into a class, perl style
                var classData = structure;
                try {
                    // either an existing class
                    var c = this.getClassByName(className.getString());
                    return this.bless(c, classData);
                } catch (e) {
                    // or we make a new one
                    if (!this.perlStyleObjects) {
                        return this.bless(className.getString(), classData);
                    }
                    else {
                        // or we make a Perl-style one
                        return new PerlObject(className.getString(), classData);
                    }

                }
            }
            else if (structure instanceof Array) {
                // nothing we can really do here except make Perl objects..
                return new PerlObject(className.getString(), structure);
            }
            else if (structure instanceof PerlReference) {
                return new PerlObject(className.getString(), structure);
            }
            // it's a regexp for example
            return structure;
        }

        read_tracked_item(): Object {
            var offset = this.read_varint();
            return this.getTracked(offset);
        }

        track(pos: number, val: Object) {
            if (this.debug) this.log.debug("track_stuff", "Saving ", val, " at offset ", pos);
            var ref = val; // autoboxing ftw
            var key = pos.toString();
            this.tracked[key] = ref;
        }

        getTracked(pos: number) {
            var key = pos.toString();
            if (!this.tracked.hasOwnProperty(key))
                throw new Error("tracked object not found");
            var val = this.tracked[pos];
            return val;
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

            var copyPos = this.read_varint();
            copyPos--;   //indexes are 1 based in sereal, 0 based in the reader.
            var currentPos = this.reader.pos;
            if (this.debug) console.log("copy_tag - jumping from " + currentPos + " to "+copyPos);

            this.reader.pos = copyPos;
            var copy = this.read();
            this.reader.pos = currentPos; // go back to where we were
            if (this.debug) console.log("copy_tag - after reading from " +copyPos + " jumping back from " + this.reader.pos + " to " + currentPos);

            return copy;
        }


        getClassByName(name: string): Function {
            throw new Error();
        }

        bless(ctor: Function, obj: Object): Object {
            throw new Error();
        }

        mapTagReaders() {
            this.tagReaders[Tags.POS_0] = this.read_pos;
            this.tagReaders[Tags.POS_1] = this.read_pos;
            this.tagReaders[Tags.POS_2] = this.read_pos;
            this.tagReaders[Tags.POS_3] = this.read_pos;
            this.tagReaders[Tags.POS_4] = this.read_pos;
            this.tagReaders[Tags.POS_5] = this.read_pos;
            this.tagReaders[Tags.POS_6] = this.read_pos;
            this.tagReaders[Tags.POS_7] = this.read_pos;
            this.tagReaders[Tags.POS_8] = this.read_pos;
            this.tagReaders[Tags.POS_9] = this.read_pos;
            this.tagReaders[Tags.POS_10] = this.read_pos;
            this.tagReaders[Tags.POS_11] = this.read_pos;
            this.tagReaders[Tags.POS_12] = this.read_pos;
            this.tagReaders[Tags.POS_13] = this.read_pos;
            this.tagReaders[Tags.POS_14] = this.read_pos;
            this.tagReaders[Tags.POS_15] = this.read_pos;
            this.tagReaders[Tags.NEG_16] = this.read_neg;
            this.tagReaders[Tags.NEG_15] = this.read_neg;
            this.tagReaders[Tags.NEG_14] = this.read_neg;
            this.tagReaders[Tags.NEG_13] = this.read_neg;
            this.tagReaders[Tags.NEG_12] = this.read_neg;
            this.tagReaders[Tags.NEG_11] = this.read_neg;
            this.tagReaders[Tags.NEG_10] = this.read_neg;
            this.tagReaders[Tags.NEG_9] = this.read_neg;
            this.tagReaders[Tags.NEG_8] = this.read_neg;
            this.tagReaders[Tags.NEG_7] = this.read_neg;
            this.tagReaders[Tags.NEG_6] = this.read_neg;
            this.tagReaders[Tags.NEG_5] = this.read_neg;
            this.tagReaders[Tags.NEG_4] = this.read_neg;
            this.tagReaders[Tags.NEG_3] = this.read_neg;
            this.tagReaders[Tags.NEG_2] = this.read_neg;
            this.tagReaders[Tags.NEG_1] = this.read_neg;

            this.tagReaders[Tags.VARINT] = this.read_varint;
            this.tagReaders[Tags.ZIGZAG] = this.read_zigzag;
            this.tagReaders[Tags.DOUBLE] = this.read_double;
            this.tagReaders[Tags.FLOAT] = () => this.reader.readFloat();

            this.tagReaders[Tags.TRUE] = () => true;
            this.tagReaders[Tags.FALSE] = () => false;
            this.tagReaders[Tags.UNDEF] = () => null;
            this.tagReaders[Tags.CANONICAL_UNDEF] = () => undefined;
            this.tagReaders[Tags.BINARY] = this.read_binary;
            this.tagReaders[Tags.STR_UTF8] = this.read_UTF8;

            this.tagReaders[Tags.REFN] = this.read_refn;
            this.tagReaders[Tags.REFP] = this.read_refp;

            this.tagReaders[Tags.OBJECT] = this.read_object;
            this.tagReaders[Tags.OBJECTV] = this.read_object_v;

            this.tagReaders[Tags.OBJECT_FREEZE] = this.read_object;  //TODO: properly support freeze objects - invoke 'thaw'
            this.tagReaders[Tags.OBJECTV_FREEZE] = this.read_object_v; //TODO: properly support freeze objects - invoke 'thaw'

            this.tagReaders[Tags.COPY] = this.read_copy;
            this.tagReaders[Tags.ALIAS] = this.read_alias;
            this.tagReaders[Tags.WEAKEN] = this.read_weaken;


            this.tagReaders[Tags.REGEXP] = this.read_regex;
            this.tagReaders[Tags.PAD] = this.read_pad;

            this.tagReaders[Tags.HASH] = this.read_hash;
            this.tagReaders[Tags.ARRAY] = this.read_array;

            this.tagReaders[Tags.ARRAYREF_0] = this.read_array_ref;
            this.tagReaders[Tags.ARRAYREF_1] = this.read_array_ref;
            this.tagReaders[Tags.ARRAYREF_2] = this.read_array_ref;
            this.tagReaders[Tags.ARRAYREF_3] = this.read_array_ref;
            this.tagReaders[Tags.ARRAYREF_4] = this.read_array_ref;
            this.tagReaders[Tags.ARRAYREF_5] = this.read_array_ref;
            this.tagReaders[Tags.ARRAYREF_6] = this.read_array_ref;
            this.tagReaders[Tags.ARRAYREF_7] = this.read_array_ref;
            this.tagReaders[Tags.ARRAYREF_8] = this.read_array_ref;
            this.tagReaders[Tags.ARRAYREF_9] = this.read_array_ref;
            this.tagReaders[Tags.ARRAYREF_10] = this.read_array_ref;
            this.tagReaders[Tags.ARRAYREF_11] = this.read_array_ref;
            this.tagReaders[Tags.ARRAYREF_12] = this.read_array_ref;
            this.tagReaders[Tags.ARRAYREF_13] = this.read_array_ref;
            this.tagReaders[Tags.ARRAYREF_14] = this.read_array_ref;
            this.tagReaders[Tags.ARRAYREF_15] = this.read_array_ref;

            this.tagReaders[Tags.HASHREF_0] = this.read_hash_ref;
            this.tagReaders[Tags.HASHREF_1] = this.read_hash_ref;
            this.tagReaders[Tags.HASHREF_2] = this.read_hash_ref;
            this.tagReaders[Tags.HASHREF_3] = this.read_hash_ref;
            this.tagReaders[Tags.HASHREF_4] = this.read_hash_ref;
            this.tagReaders[Tags.HASHREF_5] = this.read_hash_ref;
            this.tagReaders[Tags.HASHREF_6] = this.read_hash_ref;
            this.tagReaders[Tags.HASHREF_7] = this.read_hash_ref;
            this.tagReaders[Tags.HASHREF_8] = this.read_hash_ref;
            this.tagReaders[Tags.HASHREF_9] = this.read_hash_ref;
            this.tagReaders[Tags.HASHREF_10] = this.read_hash_ref;
            this.tagReaders[Tags.HASHREF_11] = this.read_hash_ref;
            this.tagReaders[Tags.HASHREF_12] = this.read_hash_ref;
            this.tagReaders[Tags.HASHREF_13] = this.read_hash_ref;
            this.tagReaders[Tags.HASHREF_14] = this.read_hash_ref;
            this.tagReaders[Tags.HASHREF_15] = this.read_hash_ref;

            this.tagReaders[Tags.SHORT_BINARY_0] = this.read_short_binary;
            this.tagReaders[Tags.SHORT_BINARY_1] = this.read_short_binary;
            this.tagReaders[Tags.SHORT_BINARY_2] = this.read_short_binary;
            this.tagReaders[Tags.SHORT_BINARY_3] = this.read_short_binary;
            this.tagReaders[Tags.SHORT_BINARY_4] = this.read_short_binary;
            this.tagReaders[Tags.SHORT_BINARY_5] = this.read_short_binary;
            this.tagReaders[Tags.SHORT_BINARY_6] = this.read_short_binary;
            this.tagReaders[Tags.SHORT_BINARY_7] = this.read_short_binary;
            this.tagReaders[Tags.SHORT_BINARY_8] = this.read_short_binary;
            this.tagReaders[Tags.SHORT_BINARY_9] = this.read_short_binary;
            this.tagReaders[Tags.SHORT_BINARY_10] = this.read_short_binary;
            this.tagReaders[Tags.SHORT_BINARY_11] = this.read_short_binary;
            this.tagReaders[Tags.SHORT_BINARY_12] = this.read_short_binary;
            this.tagReaders[Tags.SHORT_BINARY_13] = this.read_short_binary;
            this.tagReaders[Tags.SHORT_BINARY_14] = this.read_short_binary;
            this.tagReaders[Tags.SHORT_BINARY_15] = this.read_short_binary;
            this.tagReaders[Tags.SHORT_BINARY_16] = this.read_short_binary;
            this.tagReaders[Tags.SHORT_BINARY_17] = this.read_short_binary;
            this.tagReaders[Tags.SHORT_BINARY_18] = this.read_short_binary;
            this.tagReaders[Tags.SHORT_BINARY_19] = this.read_short_binary;
            this.tagReaders[Tags.SHORT_BINARY_20] = this.read_short_binary;
            this.tagReaders[Tags.SHORT_BINARY_21] = this.read_short_binary;
            this.tagReaders[Tags.SHORT_BINARY_22] = this.read_short_binary;
            this.tagReaders[Tags.SHORT_BINARY_23] = this.read_short_binary;
            this.tagReaders[Tags.SHORT_BINARY_24] = this.read_short_binary;
            this.tagReaders[Tags.SHORT_BINARY_25] = this.read_short_binary;
            this.tagReaders[Tags.SHORT_BINARY_26] = this.read_short_binary;
            this.tagReaders[Tags.SHORT_BINARY_27] = this.read_short_binary;
            this.tagReaders[Tags.SHORT_BINARY_28] = this.read_short_binary;
            this.tagReaders[Tags.SHORT_BINARY_29] = this.read_short_binary;
            this.tagReaders[Tags.SHORT_BINARY_30] = this.read_short_binary;
            this.tagReaders[Tags.SHORT_BINARY_31] = this.read_short_binary;


        }
    }


}