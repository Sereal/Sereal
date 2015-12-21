"use strict";
var __extends = (this && this.__extends) || function (d, b) {
    for (var p in b) if (b.hasOwnProperty(p)) d[p] = b[p];
    function __() { this.constructor = d; }
    d.prototype = b === null ? Object.create(b) : (__.prototype = b.prototype, new __());
};
var Sereal;
(function (Sereal) {
    var SerealDocument = (function () {
        function SerealDocument() {
        }
        return SerealDocument;
    })();
    Sereal.SerealDocument = SerealDocument;
    var SerealDocumentHeader = (function () {
        function SerealDocumentHeader() {
        }
        return SerealDocumentHeader;
    })();
    Sereal.SerealDocumentHeader = SerealDocumentHeader;
    var PerlReference = (function () {
        function PerlReference(_value) {
            this._value = _value;
        }
        PerlReference.prototype.getValue = function () {
            return this._value;
        };
        return PerlReference;
    })();
    Sereal.PerlReference = PerlReference;
    var PerlObject = (function () {
        function PerlObject(name, obj) {
            this.name = name;
            this.obj = obj;
        }
        return PerlObject;
    })();
    Sereal.PerlObject = PerlObject;
    var Alias = (function () {
        function Alias(obj) {
            this.obj = obj;
        }
        return Alias;
    })();
    Sereal.Alias = Alias;
    var SerealException = (function (_super) {
        __extends(SerealException, _super);
        function SerealException(msg) {
            _super.call(this, msg);
            this.msg = msg;
        }
        return SerealException;
    })(Error);
    Sereal.SerealException = SerealException;
    var WeakReference = (function () {
        function WeakReference(obj) {
            this.obj = obj;
        }
        return WeakReference;
    })();
    Sereal.WeakReference = WeakReference;
    var Padded = (function () {
        function Padded(value) {
            this.value = value;
        }
        return Padded;
    })();
    Sereal.Padded = Padded;
    var varint = (function () {
        function varint() {
        }
        varint.read = function (buf, offset, out) {
            var res = 0, offset = offset || 0, shift = 0, counter = offset, b, l = buf.length;
            do {
                if (counter >= l) {
                    if (out != null)
                        out.bytesRead = 0;
                    return undefined;
                }
                b = buf[counter++];
                res += shift < 28
                    ? (b & varint.REST) << shift
                    : (b & varint.REST) * Math.pow(2, shift);
                shift += 7;
            } while (b >= varint.MSB);
            if (out != null)
                out.bytesRead = counter - offset;
            return res;
        };
        varint.MSB = 0x80;
        varint.REST = 0x7F;
        return varint;
    })();
    Sereal.varint = varint;
    var Utils = (function () {
        function Utils() {
        }
        Utils.bytesToString = function (arr) {
            return String.fromCharCode.apply(null, arr);
        };
        Utils.stringToBytes = function (str) {
            var arr = new Uint8Array(str.length);
            for (var i = 0, strLen = str.length; i < strLen; i++) {
                arr[i] = str.charCodeAt(i);
            }
            return arr;
        };
        return Utils;
    })();
    Sereal.Utils = Utils;
    (function (Consts) {
        /** (0x6c) + (0x72 << 8) + (0x73 << 16) + (0x3d << 24) == 0x6c72733d */
        Consts[Consts["MAGIC"] = 1039364716] = "MAGIC";
        /** (0x6c) + (0x72 << 8) + (0x73 << 16) + (0x3d << 24) == 0x6c72733d */
        Consts[Consts["MAGIC_OLD"] = 1039364716] = "MAGIC_OLD";
        /** lower 5 bits */
        Consts[Consts["MASK_SHORT_BINARY_LEN"] = 31] = "MASK_SHORT_BINARY_LEN";
        /** 0 0x00 0b00000000 small positive integer - value in low 4 bits (identity) */
        Consts[Consts["POS_LOW"] = 0] = "POS_LOW";
        /** 15 0x0f 0b00001111 small positive integer - value in low 4 bits (identity) */
        Consts[Consts["POS_HIGH"] = 15] = "POS_HIGH";
        /** 16 0x10 0b00010000 small negative integer - value in low 4 bits (k+32) */
        Consts[Consts["NEG_LOW"] = 16] = "NEG_LOW";
        /** 31 0x1f 0b00011111 small negative integer - value in low 4 bits (k+32) */
        Consts[Consts["NEG_HIGH"] = 31] = "NEG_HIGH";
        /** 64 0x40 0b01000000 [<ITEM-TAG> ...] - count of items in low 4 bits (ARRAY must be refcnt=1) */
        Consts[Consts["ARRAYREF_LOW"] = 64] = "ARRAYREF_LOW";
        /** 79 0x4f 0b01001111 [<ITEM-TAG> ...] - count of items in low 4 bits (ARRAY must be refcnt=1) */
        Consts[Consts["ARRAYREF_HIGH"] = 79] = "ARRAYREF_HIGH";
        /** 80 0x50 0b01010000 [<KEY-TAG> <ITEM-TAG> ...] - count in low 4 bits, key/value pairs (HASH must be refcnt=1) */
        Consts[Consts["HASHREF_LOW"] = 80] = "HASHREF_LOW";
        /** 95 0x5f 0b01011111 [<KEY-TAG> <ITEM-TAG> ...] - count in low 4 bits, key/value pairs (HASH must be refcnt=1) */
        Consts[Consts["HASHREF_HIGH"] = 95] = "HASHREF_HIGH";
        /** 96 0x60 0b01100000 <BYTES> - binary/latin1 string, length encoded in low 5 bits of tag */
        Consts[Consts["SHORT_BINARY_LOW"] = 96] = "SHORT_BINARY_LOW";
        /** 127 0x7f 0b01111111 <BYTES> - binary/latin1 string, length encoded in low 5 bits of tag */
        Consts[Consts["SHORT_BINARY_HIGH"] = 127] = "SHORT_BINARY_HIGH";
        /** 128 0x80 0b10000000 if this bit is set track the item */
        Consts[Consts["TRACK_FLAG"] = 128] = "TRACK_FLAG";
    })(Sereal.Consts || (Sereal.Consts = {}));
    var Consts = Sereal.Consts;
    (function (Tags) {
        Tags[Tags["POS_0"] = 0] = "POS_0";
        Tags[Tags["POS_1"] = 1] = "POS_1";
        Tags[Tags["POS_2"] = 2] = "POS_2";
        Tags[Tags["POS_3"] = 3] = "POS_3";
        Tags[Tags["POS_4"] = 4] = "POS_4";
        Tags[Tags["POS_5"] = 5] = "POS_5";
        Tags[Tags["POS_6"] = 6] = "POS_6";
        Tags[Tags["POS_7"] = 7] = "POS_7";
        Tags[Tags["POS_8"] = 8] = "POS_8";
        Tags[Tags["POS_9"] = 9] = "POS_9";
        Tags[Tags["POS_10"] = 10] = "POS_10";
        Tags[Tags["POS_11"] = 11] = "POS_11";
        Tags[Tags["POS_12"] = 12] = "POS_12";
        Tags[Tags["POS_13"] = 13] = "POS_13";
        Tags[Tags["POS_14"] = 14] = "POS_14";
        Tags[Tags["POS_15"] = 15] = "POS_15";
        Tags[Tags["NEG_16"] = 16] = "NEG_16";
        Tags[Tags["NEG_15"] = 17] = "NEG_15";
        Tags[Tags["NEG_14"] = 18] = "NEG_14";
        Tags[Tags["NEG_13"] = 19] = "NEG_13";
        Tags[Tags["NEG_12"] = 20] = "NEG_12";
        Tags[Tags["NEG_11"] = 21] = "NEG_11";
        Tags[Tags["NEG_10"] = 22] = "NEG_10";
        Tags[Tags["NEG_9"] = 23] = "NEG_9";
        Tags[Tags["NEG_8"] = 24] = "NEG_8";
        Tags[Tags["NEG_7"] = 25] = "NEG_7";
        Tags[Tags["NEG_6"] = 26] = "NEG_6";
        Tags[Tags["NEG_5"] = 27] = "NEG_5";
        Tags[Tags["NEG_4"] = 28] = "NEG_4";
        Tags[Tags["NEG_3"] = 29] = "NEG_3";
        Tags[Tags["NEG_2"] = 30] = "NEG_2";
        Tags[Tags["NEG_1"] = 31] = "NEG_1";
        Tags[Tags["VARINT"] = 32] = "VARINT";
        Tags[Tags["ZIGZAG"] = 33] = "ZIGZAG";
        Tags[Tags["FLOAT"] = 34] = "FLOAT";
        Tags[Tags["DOUBLE"] = 35] = "DOUBLE";
        Tags[Tags["LONG_DOUBLE"] = 36] = "LONG_DOUBLE";
        Tags[Tags["UNDEF"] = 37] = "UNDEF";
        Tags[Tags["BINARY"] = 38] = "BINARY";
        Tags[Tags["STR_UTF8"] = 39] = "STR_UTF8";
        Tags[Tags["REFN"] = 40] = "REFN";
        Tags[Tags["REFP"] = 41] = "REFP";
        Tags[Tags["HASH"] = 42] = "HASH";
        Tags[Tags["ARRAY"] = 43] = "ARRAY";
        Tags[Tags["OBJECT"] = 44] = "OBJECT";
        Tags[Tags["OBJECTV"] = 45] = "OBJECTV";
        Tags[Tags["ALIAS"] = 46] = "ALIAS";
        Tags[Tags["COPY"] = 47] = "COPY";
        Tags[Tags["WEAKEN"] = 48] = "WEAKEN";
        Tags[Tags["REGEXP"] = 49] = "REGEXP";
        Tags[Tags["OBJECT_FREEZE"] = 50] = "OBJECT_FREEZE";
        Tags[Tags["OBJECTV_FREEZE"] = 51] = "OBJECTV_FREEZE";
        Tags[Tags["RESERVED_0"] = 52] = "RESERVED_0";
        Tags[Tags["RESERVED_1"] = 53] = "RESERVED_1";
        Tags[Tags["RESERVED_2"] = 54] = "RESERVED_2";
        Tags[Tags["RESERVED_3"] = 55] = "RESERVED_3";
        Tags[Tags["RESERVED_4"] = 56] = "RESERVED_4";
        Tags[Tags["CANONICAL_UNDEF"] = 57] = "CANONICAL_UNDEF";
        Tags[Tags["FALSE"] = 58] = "FALSE";
        Tags[Tags["TRUE"] = 59] = "TRUE";
        Tags[Tags["MANY"] = 60] = "MANY";
        Tags[Tags["PACKET_START"] = 61] = "PACKET_START";
        Tags[Tags["EXTEND"] = 62] = "EXTEND";
        Tags[Tags["PAD"] = 63] = "PAD";
        Tags[Tags["ARRAYREF_0"] = 64] = "ARRAYREF_0";
        Tags[Tags["ARRAYREF_1"] = 65] = "ARRAYREF_1";
        Tags[Tags["ARRAYREF_2"] = 66] = "ARRAYREF_2";
        Tags[Tags["ARRAYREF_3"] = 67] = "ARRAYREF_3";
        Tags[Tags["ARRAYREF_4"] = 68] = "ARRAYREF_4";
        Tags[Tags["ARRAYREF_5"] = 69] = "ARRAYREF_5";
        Tags[Tags["ARRAYREF_6"] = 70] = "ARRAYREF_6";
        Tags[Tags["ARRAYREF_7"] = 71] = "ARRAYREF_7";
        Tags[Tags["ARRAYREF_8"] = 72] = "ARRAYREF_8";
        Tags[Tags["ARRAYREF_9"] = 73] = "ARRAYREF_9";
        Tags[Tags["ARRAYREF_10"] = 74] = "ARRAYREF_10";
        Tags[Tags["ARRAYREF_11"] = 75] = "ARRAYREF_11";
        Tags[Tags["ARRAYREF_12"] = 76] = "ARRAYREF_12";
        Tags[Tags["ARRAYREF_13"] = 77] = "ARRAYREF_13";
        Tags[Tags["ARRAYREF_14"] = 78] = "ARRAYREF_14";
        Tags[Tags["ARRAYREF_15"] = 79] = "ARRAYREF_15";
        Tags[Tags["HASHREF_0"] = 80] = "HASHREF_0";
        Tags[Tags["HASHREF_1"] = 81] = "HASHREF_1";
        Tags[Tags["HASHREF_2"] = 82] = "HASHREF_2";
        Tags[Tags["HASHREF_3"] = 83] = "HASHREF_3";
        Tags[Tags["HASHREF_4"] = 84] = "HASHREF_4";
        Tags[Tags["HASHREF_5"] = 85] = "HASHREF_5";
        Tags[Tags["HASHREF_6"] = 86] = "HASHREF_6";
        Tags[Tags["HASHREF_7"] = 87] = "HASHREF_7";
        Tags[Tags["HASHREF_8"] = 88] = "HASHREF_8";
        Tags[Tags["HASHREF_9"] = 89] = "HASHREF_9";
        Tags[Tags["HASHREF_10"] = 90] = "HASHREF_10";
        Tags[Tags["HASHREF_11"] = 91] = "HASHREF_11";
        Tags[Tags["HASHREF_12"] = 92] = "HASHREF_12";
        Tags[Tags["HASHREF_13"] = 93] = "HASHREF_13";
        Tags[Tags["HASHREF_14"] = 94] = "HASHREF_14";
        Tags[Tags["HASHREF_15"] = 95] = "HASHREF_15";
        Tags[Tags["SHORT_BINARY_0"] = 96] = "SHORT_BINARY_0";
        Tags[Tags["SHORT_BINARY_1"] = 97] = "SHORT_BINARY_1";
        Tags[Tags["SHORT_BINARY_2"] = 98] = "SHORT_BINARY_2";
        Tags[Tags["SHORT_BINARY_3"] = 99] = "SHORT_BINARY_3";
        Tags[Tags["SHORT_BINARY_4"] = 100] = "SHORT_BINARY_4";
        Tags[Tags["SHORT_BINARY_5"] = 101] = "SHORT_BINARY_5";
        Tags[Tags["SHORT_BINARY_6"] = 102] = "SHORT_BINARY_6";
        Tags[Tags["SHORT_BINARY_7"] = 103] = "SHORT_BINARY_7";
        Tags[Tags["SHORT_BINARY_8"] = 104] = "SHORT_BINARY_8";
        Tags[Tags["SHORT_BINARY_9"] = 105] = "SHORT_BINARY_9";
        Tags[Tags["SHORT_BINARY_10"] = 106] = "SHORT_BINARY_10";
        Tags[Tags["SHORT_BINARY_11"] = 107] = "SHORT_BINARY_11";
        Tags[Tags["SHORT_BINARY_12"] = 108] = "SHORT_BINARY_12";
        Tags[Tags["SHORT_BINARY_13"] = 109] = "SHORT_BINARY_13";
        Tags[Tags["SHORT_BINARY_14"] = 110] = "SHORT_BINARY_14";
        Tags[Tags["SHORT_BINARY_15"] = 111] = "SHORT_BINARY_15";
        Tags[Tags["SHORT_BINARY_16"] = 112] = "SHORT_BINARY_16";
        Tags[Tags["SHORT_BINARY_17"] = 113] = "SHORT_BINARY_17";
        Tags[Tags["SHORT_BINARY_18"] = 114] = "SHORT_BINARY_18";
        Tags[Tags["SHORT_BINARY_19"] = 115] = "SHORT_BINARY_19";
        Tags[Tags["SHORT_BINARY_20"] = 116] = "SHORT_BINARY_20";
        Tags[Tags["SHORT_BINARY_21"] = 117] = "SHORT_BINARY_21";
        Tags[Tags["SHORT_BINARY_22"] = 118] = "SHORT_BINARY_22";
        Tags[Tags["SHORT_BINARY_23"] = 119] = "SHORT_BINARY_23";
        Tags[Tags["SHORT_BINARY_24"] = 120] = "SHORT_BINARY_24";
        Tags[Tags["SHORT_BINARY_25"] = 121] = "SHORT_BINARY_25";
        Tags[Tags["SHORT_BINARY_26"] = 122] = "SHORT_BINARY_26";
        Tags[Tags["SHORT_BINARY_27"] = 123] = "SHORT_BINARY_27";
        Tags[Tags["SHORT_BINARY_28"] = 124] = "SHORT_BINARY_28";
        Tags[Tags["SHORT_BINARY_29"] = 125] = "SHORT_BINARY_29";
        Tags[Tags["SHORT_BINARY_30"] = 126] = "SHORT_BINARY_30";
        Tags[Tags["SHORT_BINARY_31"] = 127] = "SHORT_BINARY_31";
    })(Sereal.Tags || (Sereal.Tags = {}));
    var Tags = Sereal.Tags;
})(Sereal || (Sereal = {}));
"use strict";
var Sereal;
(function (Sereal) {
    /**
     * A wrapper for DataView that also handles its own position index
     */
    var DataReader = (function () {
        function DataReader(data) {
            this.view = DataReader.toDataView(data);
            this.pos = 0;
        }
        DataReader.prototype.readString = function (length) {
            var arr = this.readBytes(length);
            var s = Sereal.Utils.bytesToString(arr);
            return s;
        };
        DataReader.prototype.readFloat = function () {
            var val = this.view.getFloat32(this.pos, true); //little endian
            this.pos += 4;
            return val;
        };
        DataReader.prototype.readDouble = function () {
            var val = this.view.getFloat64(this.pos, true); //little endian
            this.pos += 8;
            return val;
        };
        DataReader.prototype.readInt32 = function () { return this.readInt(); };
        DataReader.prototype.asUint8Array = function () { return new Uint8Array(this.view.buffer, this.view.byteOffset, this.view.byteLength); };
        /**
         *  returns a new datareader from the current position until the rest of the data
         */
        DataReader.prototype.toDataReader = function () {
            var view = new DataView(this.view.buffer, this.view.byteOffset + this.pos, this.view.byteLength - this.pos);
            return new DataReader(view);
        };
        DataReader.prototype.readInt = function () {
            var value = this.view.getInt32(this.pos);
            this.pos += 4;
            return value;
        };
        DataReader.prototype.readBytesTo = function (buf) {
            var arr = new Uint8Array(buf);
            for (var i = 0; i < arr.length; i++)
                arr[i] = this.readByte();
            return buf;
        };
        DataReader.prototype.readByte = function () {
            var value = this.view.getUint8(this.pos);
            this.pos++;
            return value;
        };
        DataReader.prototype.readVarInt = function () {
            var out = { bytesRead: null };
            var value = Sereal.varint.read(this.asUint8Array(), this.pos, out);
            if (out.bytesRead == null)
                throw new Error();
            this.pos += out.bytesRead;
            return value;
        };
        DataReader.prototype.readBytes = function (length) {
            if (length == null)
                length = this.view.byteLength - this.pos;
            var arr = new Uint8Array(this.view.buffer, this.view.byteOffset + this.pos, length);
            this.pos += length;
            return arr;
        };
        Object.defineProperty(DataReader.prototype, "absPos", {
            get: function () {
                return this.view.byteOffset + this.pos;
            },
            enumerable: true,
            configurable: true
        });
        DataReader.prototype.toHex = function () {
            var list = [];
            for (var i = 0; i < this.view.byteLength; i++) {
                list.push(this.view.getUint8(i).toHex().padLeft(2, "0"));
            }
            return list.join(" ");
        };
        DataReader.prototype.hasRemaining = function () { return this.remaining() > 0; };
        DataReader.prototype.remaining = function () { return this.view.byteLength - this.pos; };
        DataReader.toDataView = function (data) {
            if (data instanceof DataView)
                return data;
            if (typeof (data) == "string")
                data = Sereal.Utils.stringToBytes(data);
            if (ArrayBuffer.isView(data))
                return new DataView(data.buffer, data.byteOffset, data.byteLength);
            if (data instanceof ArrayBuffer) {
                var buf = data;
                return new DataView(buf); //, 0, buf.byteLength
            }
            console.warn("unknown data", data);
            return null;
        };
        return DataReader;
    })();
    Sereal.DataReader = DataReader;
})(Sereal || (Sereal = {}));
"use strict";
var Sereal;
(function (Sereal) {
    var Decoder = (function () {
        function Decoder() {
            this.perlStyleObjects = false;
            this.prefer_latin1 = true;
            this.log = console;
            this.perlRefs = false;
            this.preservePadding = false;
            this.debug = false;
            this.tagReaders = new Array(128);
            this.mapTagReaders();
        }
        Decoder.prototype.init = function (data) {
            this.tracked = {};
            if (data instanceof Sereal.DataReader)
                this.reader = data;
            else
                this.reader = new Sereal.DataReader(data);
        };
        Decoder.prototype.decodeDocument = function (data) {
            if (data != null) {
                this.init(data);
            }
            this.doc = new Sereal.SerealDocument();
            var doc = this.doc;
            doc.header = new Sereal.SerealDocumentHeader();
            doc.header.magic = this.reader.readInt32();
            if (doc.header.magic != Sereal.Consts.MAGIC)
                throw new Error();
            var versionAndType = this.reader.readByte();
            doc.header.version = versionAndType & 15;
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
        };
        Decoder.prototype.decodeDocumentBody = function (data) {
            var dec = new Decoder();
            if (data == null)
                dec.init(this.reader.toDataReader());
            else
                dec.init(new Sereal.DataReader(data));
            var x = dec.read();
            return x;
        };
        Decoder.prototype.read = function () {
            var byte = this.reader.readByte();
            var tag = byte;
            var trackPos = null;
            if ((byte & Sereal.Consts.TRACK_FLAG) != 0) {
                tag = byte & ~Sereal.Consts.TRACK_FLAG;
                trackPos = this.reader.pos; // actually it's pos-1+1;   -1 to get back to the tag pos, +1 because indexes are 1 based
            }
            if (this.debug)
                this.log.debug("reading", { tag: tag, tagName: Sereal.Tags[tag], absPos: this.reader.absPos, trackPos: trackPos });
            var func = this.tagReaders[tag];
            if (func == null)
                throw new Sereal.SerealException("Tag not supported: " + tag);
            var out = func.call(this, tag, trackPos);
            if (trackPos != null)
                this.track(trackPos, out);
            if (this.debug)
                this.log.debug("read", { tag: Sereal.Tags[tag], value: out, pos: this.reader.pos, trackPos: trackPos, });
            return out;
        };
        Decoder.prototype.deflate = function () {
            var pos = this.reader.pos;
            var arr = this.reader.readBytes(this.doc.header.body_compressed_length);
            var zip = new Zlib.Inflate(arr);
            var deflated = zip.decompress();
            if (deflated.length != this.doc.header.body_uncompressed_length)
                throw new Error("decompressed length doesn't match");
            return deflated;
        };
        Decoder.prototype.read_weaken = function () {
            //this.log.debug("Weakening the next thing");
            // so the next thing HAS to be a ref (afaict) which means we can track it
            var placeHolder = new Sereal.PerlReference(this.read().getValue());
            var /*WeakReference<PerlReference>*/ wref = new Sereal.WeakReference(placeHolder);
            return wref;
        };
        Decoder.prototype.read_alias = function () {
            var alias = new Sereal.Alias(this.read_tracked_item());
            return alias;
        };
        Decoder.prototype.read_object_v = function () {
            //this.log.debug("Reading an objectv");
            var className = this.read_tracked_item();
            var out = new Sereal.PerlObject(className, this.read());
            return out;
        };
        Decoder.prototype.read_refp = function () {
            var offset_prev = this.read_varint();
            var prv_value = this.getTracked(offset_prev);
            var prev = this.perlRefs ? new Sereal.PerlReference(prv_value) : prv_value;
            return prev;
        };
        Decoder.prototype.read_refn = function () {
            var out = this.read();
            if (this.perlRefs)
                out = new Sereal.PerlReference(out);
            return out;
        };
        Decoder.prototype.read_double = function () {
            var d = this.reader.readDouble();
            return d;
        };
        Decoder.prototype.read_array = function (tag, trackPos) {
            var length = this.read_varint();
            return this._read_array(length, trackPos);
        };
        /**
         * if tag == 0, next is varint for number of elements, otherwise lower 4 bits are length
         *
         * @param tag lower 4 bits is length or 0 for next varint is length
         * @param track we might need to track since array elements could refer to us
         */
        Decoder.prototype.read_array_ref = function (tag, trackPos) {
            var length = tag & 15;
            return this._read_array(length, trackPos);
        };
        Decoder.prototype._read_array = function (length, trackPos) {
            var arr = new Array(length);
            if (trackPos != null)
                this.track(trackPos, arr);
            for (var i = 0; i < length; i++) {
                arr[i] = this.read();
                if (this.debug)
                    this.log.debug("Read array element ", i, arr[i]);
            }
            return arr;
        };
        Decoder.prototype.read_binary = function () {
            var length = this.read_varint();
            var s = this.reader.readString(length);
            return s;
        };
        Decoder.prototype.read_hash = function (tag, trackPos) {
            var num_keys = this.read_varint();
            return this._read_hash(num_keys, trackPos);
        };
        Decoder.prototype.read_hash_ref = function (tag, trackPos) {
            var num_keys = tag & 15;
            return this._read_hash(num_keys, trackPos);
        };
        Decoder.prototype._read_hash = function (num_keys, trackPos) {
            var hash = {};
            if (trackPos != null)
                this.track(trackPos, hash);
            if (this.debug)
                this.log.debug("Reading ", num_keys, " hash elements");
            for (var i = 0; i < num_keys; i++) {
                var keyObject = this.read();
                var key = keyObject;
                var val = this.read();
                hash[key] = val;
            }
            return hash;
        };
        Decoder.prototype.read_varint = function () {
            var x = this.reader.readVarInt();
            return x;
        };
        Decoder.prototype.read_pos = function (tag) {
            return tag;
        };
        Decoder.prototype.read_neg = function (tag) {
            return tag - 32;
        };
        Decoder.prototype.read_pad = function () {
            var res = this.read();
            if (this.preservePadding)
                return new Sereal.Padded(res);
            return res;
        };
        /** Read a short binary ISO-8859-1 (latin1) string, the lower bits of the tag hold the length */
        Decoder.prototype.read_short_binary = function (tag) {
            var length = tag & Sereal.Consts.MASK_SHORT_BINARY_LEN;
            var s = this.reader.readString(length);
            return s;
        };
        Decoder.prototype.read_UTF8 = function () {
            var length = this.read_varint();
            //var buf = new ArrayBuffer(length);
            //this.reader.readBytesTo(buf);
            var s = this.reader.readString(length);
            return s;
            //return Charset.forName("UTF-8").decode(ByteBuffer.wrap(buf)).toString();
        };
        Decoder.prototype.read_zigzag = function () {
            var n = this.read_varint();
            return (n >>> 1) ^ (-(n & 1)); // note the unsigned right shift
        };
        Decoder.prototype.read_regex = function () {
            var flags;
            var regex = this.read();
            // now read modifiers
            var tag = this.reader.readByte();
            if ((tag & Sereal.Consts.SHORT_BINARY_LOW) == Sereal.Consts.SHORT_BINARY_LOW) {
                var length = tag & Sereal.Consts.MASK_SHORT_BINARY_LEN;
                flags = this.reader.readString(length);
            }
            else {
                throw new Sereal.SerealException("Expecting SHORT_BINARY for modifiers of regexp, got: " + tag);
            }
            return new RegExp(regex, flags);
        };
        Decoder.prototype.read_object = function () {
            // first read the classname
            var position = this.reader.pos;
            var tag = this.reader.readByte();
            var className;
            if ((tag & Sereal.Consts.SHORT_BINARY_LOW) == Sereal.Consts.SHORT_BINARY_LOW) {
                var length = tag & Sereal.Consts.MASK_SHORT_BINARY_LEN;
                className = this.reader.readString(length);
            }
            else {
                throw new Sereal.SerealException("Don't know how to read classname from tag" + tag);
            }
            // apparently class names do not need a track_bit set to be the target of objectv's
            this.track(position, className);
            if (this.debug)
                this.log.debug("Object Classname: " + className);
            // now read the struct (better be a hash!)
            var structure = this.read();
            if (typeof (structure) == "object") {
                // now "bless" this into a class, perl style
                var classData = structure;
                try {
                    // either an existing class
                    var c = this.getClassByName(className.getString());
                    return this.bless(c, classData);
                }
                catch (e) {
                    // or we make a new one
                    if (!this.perlStyleObjects) {
                        return this.bless(className.getString(), classData);
                    }
                    else {
                        // or we make a Perl-style one
                        return new Sereal.PerlObject(className.getString(), classData);
                    }
                }
            }
            else if (structure instanceof Array) {
                // nothing we can really do here except make Perl objects..
                return new Sereal.PerlObject(className.getString(), structure);
            }
            else if (structure instanceof Sereal.PerlReference) {
                return new Sereal.PerlObject(className.getString(), structure);
            }
            // it's a regexp for example
            return structure;
        };
        Decoder.prototype.read_tracked_item = function () {
            var offset = this.read_varint();
            return this.getTracked(offset);
        };
        Decoder.prototype.track = function (pos, val) {
            if (this.debug)
                this.log.debug("track_stuff", "Saving ", val, " at offset ", pos);
            var ref = val; // autoboxing ftw
            var key = pos.toString();
            this.tracked[key] = ref;
        };
        Decoder.prototype.getTracked = function (pos) {
            var key = pos.toString();
            if (!this.tracked.hasOwnProperty(key))
                throw new Error("tracked object not found");
            var val = this.tracked[pos];
            return val;
        };
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
        Decoder.prototype.read_copy = function () {
            var copyPos = this.read_varint();
            copyPos--; //indexes are 1 based in sereal, 0 based in the reader.
            var currentPos = this.reader.pos;
            if (this.debug)
                console.log("copy_tag - jumping from " + currentPos + " to " + copyPos);
            this.reader.pos = copyPos;
            var copy = this.read();
            this.reader.pos = currentPos; // go back to where we were
            if (this.debug)
                console.log("copy_tag - after reading from " + copyPos + " jumping back from " + this.reader.pos + " to " + currentPos);
            return copy;
        };
        Decoder.prototype.getClassByName = function (name) {
            throw new Error();
        };
        Decoder.prototype.bless = function (ctor, obj) {
            throw new Error();
        };
        Decoder.prototype.mapTagReaders = function () {
            var _this = this;
            this.tagReaders[Sereal.Tags.POS_0] = this.read_pos;
            this.tagReaders[Sereal.Tags.POS_1] = this.read_pos;
            this.tagReaders[Sereal.Tags.POS_2] = this.read_pos;
            this.tagReaders[Sereal.Tags.POS_3] = this.read_pos;
            this.tagReaders[Sereal.Tags.POS_4] = this.read_pos;
            this.tagReaders[Sereal.Tags.POS_5] = this.read_pos;
            this.tagReaders[Sereal.Tags.POS_6] = this.read_pos;
            this.tagReaders[Sereal.Tags.POS_7] = this.read_pos;
            this.tagReaders[Sereal.Tags.POS_8] = this.read_pos;
            this.tagReaders[Sereal.Tags.POS_9] = this.read_pos;
            this.tagReaders[Sereal.Tags.POS_10] = this.read_pos;
            this.tagReaders[Sereal.Tags.POS_11] = this.read_pos;
            this.tagReaders[Sereal.Tags.POS_12] = this.read_pos;
            this.tagReaders[Sereal.Tags.POS_13] = this.read_pos;
            this.tagReaders[Sereal.Tags.POS_14] = this.read_pos;
            this.tagReaders[Sereal.Tags.POS_15] = this.read_pos;
            this.tagReaders[Sereal.Tags.NEG_16] = this.read_neg;
            this.tagReaders[Sereal.Tags.NEG_15] = this.read_neg;
            this.tagReaders[Sereal.Tags.NEG_14] = this.read_neg;
            this.tagReaders[Sereal.Tags.NEG_13] = this.read_neg;
            this.tagReaders[Sereal.Tags.NEG_12] = this.read_neg;
            this.tagReaders[Sereal.Tags.NEG_11] = this.read_neg;
            this.tagReaders[Sereal.Tags.NEG_10] = this.read_neg;
            this.tagReaders[Sereal.Tags.NEG_9] = this.read_neg;
            this.tagReaders[Sereal.Tags.NEG_8] = this.read_neg;
            this.tagReaders[Sereal.Tags.NEG_7] = this.read_neg;
            this.tagReaders[Sereal.Tags.NEG_6] = this.read_neg;
            this.tagReaders[Sereal.Tags.NEG_5] = this.read_neg;
            this.tagReaders[Sereal.Tags.NEG_4] = this.read_neg;
            this.tagReaders[Sereal.Tags.NEG_3] = this.read_neg;
            this.tagReaders[Sereal.Tags.NEG_2] = this.read_neg;
            this.tagReaders[Sereal.Tags.NEG_1] = this.read_neg;
            this.tagReaders[Sereal.Tags.VARINT] = this.read_varint;
            this.tagReaders[Sereal.Tags.ZIGZAG] = this.read_zigzag;
            this.tagReaders[Sereal.Tags.DOUBLE] = this.read_double;
            this.tagReaders[Sereal.Tags.FLOAT] = function () { return _this.reader.readFloat(); };
            this.tagReaders[Sereal.Tags.TRUE] = function () { return true; };
            this.tagReaders[Sereal.Tags.FALSE] = function () { return false; };
            this.tagReaders[Sereal.Tags.UNDEF] = function () { return null; };
            this.tagReaders[Sereal.Tags.CANONICAL_UNDEF] = function () { return undefined; };
            this.tagReaders[Sereal.Tags.BINARY] = this.read_binary;
            this.tagReaders[Sereal.Tags.STR_UTF8] = this.read_UTF8;
            this.tagReaders[Sereal.Tags.REFN] = this.read_refn;
            this.tagReaders[Sereal.Tags.REFP] = this.read_refp;
            this.tagReaders[Sereal.Tags.OBJECT] = this.read_object;
            this.tagReaders[Sereal.Tags.OBJECTV] = this.read_object_v;
            this.tagReaders[Sereal.Tags.OBJECT_FREEZE] = this.read_object; //TODO: properly support freeze objects - invoke 'thaw'
            this.tagReaders[Sereal.Tags.OBJECTV_FREEZE] = this.read_object_v; //TODO: properly support freeze objects - invoke 'thaw'
            this.tagReaders[Sereal.Tags.COPY] = this.read_copy;
            this.tagReaders[Sereal.Tags.ALIAS] = this.read_alias;
            this.tagReaders[Sereal.Tags.WEAKEN] = this.read_weaken;
            this.tagReaders[Sereal.Tags.REGEXP] = this.read_regex;
            this.tagReaders[Sereal.Tags.PAD] = this.read_pad;
            this.tagReaders[Sereal.Tags.HASH] = this.read_hash;
            this.tagReaders[Sereal.Tags.ARRAY] = this.read_array;
            this.tagReaders[Sereal.Tags.ARRAYREF_0] = this.read_array_ref;
            this.tagReaders[Sereal.Tags.ARRAYREF_1] = this.read_array_ref;
            this.tagReaders[Sereal.Tags.ARRAYREF_2] = this.read_array_ref;
            this.tagReaders[Sereal.Tags.ARRAYREF_3] = this.read_array_ref;
            this.tagReaders[Sereal.Tags.ARRAYREF_4] = this.read_array_ref;
            this.tagReaders[Sereal.Tags.ARRAYREF_5] = this.read_array_ref;
            this.tagReaders[Sereal.Tags.ARRAYREF_6] = this.read_array_ref;
            this.tagReaders[Sereal.Tags.ARRAYREF_7] = this.read_array_ref;
            this.tagReaders[Sereal.Tags.ARRAYREF_8] = this.read_array_ref;
            this.tagReaders[Sereal.Tags.ARRAYREF_9] = this.read_array_ref;
            this.tagReaders[Sereal.Tags.ARRAYREF_10] = this.read_array_ref;
            this.tagReaders[Sereal.Tags.ARRAYREF_11] = this.read_array_ref;
            this.tagReaders[Sereal.Tags.ARRAYREF_12] = this.read_array_ref;
            this.tagReaders[Sereal.Tags.ARRAYREF_13] = this.read_array_ref;
            this.tagReaders[Sereal.Tags.ARRAYREF_14] = this.read_array_ref;
            this.tagReaders[Sereal.Tags.ARRAYREF_15] = this.read_array_ref;
            this.tagReaders[Sereal.Tags.HASHREF_0] = this.read_hash_ref;
            this.tagReaders[Sereal.Tags.HASHREF_1] = this.read_hash_ref;
            this.tagReaders[Sereal.Tags.HASHREF_2] = this.read_hash_ref;
            this.tagReaders[Sereal.Tags.HASHREF_3] = this.read_hash_ref;
            this.tagReaders[Sereal.Tags.HASHREF_4] = this.read_hash_ref;
            this.tagReaders[Sereal.Tags.HASHREF_5] = this.read_hash_ref;
            this.tagReaders[Sereal.Tags.HASHREF_6] = this.read_hash_ref;
            this.tagReaders[Sereal.Tags.HASHREF_7] = this.read_hash_ref;
            this.tagReaders[Sereal.Tags.HASHREF_8] = this.read_hash_ref;
            this.tagReaders[Sereal.Tags.HASHREF_9] = this.read_hash_ref;
            this.tagReaders[Sereal.Tags.HASHREF_10] = this.read_hash_ref;
            this.tagReaders[Sereal.Tags.HASHREF_11] = this.read_hash_ref;
            this.tagReaders[Sereal.Tags.HASHREF_12] = this.read_hash_ref;
            this.tagReaders[Sereal.Tags.HASHREF_13] = this.read_hash_ref;
            this.tagReaders[Sereal.Tags.HASHREF_14] = this.read_hash_ref;
            this.tagReaders[Sereal.Tags.HASHREF_15] = this.read_hash_ref;
            this.tagReaders[Sereal.Tags.SHORT_BINARY_0] = this.read_short_binary;
            this.tagReaders[Sereal.Tags.SHORT_BINARY_1] = this.read_short_binary;
            this.tagReaders[Sereal.Tags.SHORT_BINARY_2] = this.read_short_binary;
            this.tagReaders[Sereal.Tags.SHORT_BINARY_3] = this.read_short_binary;
            this.tagReaders[Sereal.Tags.SHORT_BINARY_4] = this.read_short_binary;
            this.tagReaders[Sereal.Tags.SHORT_BINARY_5] = this.read_short_binary;
            this.tagReaders[Sereal.Tags.SHORT_BINARY_6] = this.read_short_binary;
            this.tagReaders[Sereal.Tags.SHORT_BINARY_7] = this.read_short_binary;
            this.tagReaders[Sereal.Tags.SHORT_BINARY_8] = this.read_short_binary;
            this.tagReaders[Sereal.Tags.SHORT_BINARY_9] = this.read_short_binary;
            this.tagReaders[Sereal.Tags.SHORT_BINARY_10] = this.read_short_binary;
            this.tagReaders[Sereal.Tags.SHORT_BINARY_11] = this.read_short_binary;
            this.tagReaders[Sereal.Tags.SHORT_BINARY_12] = this.read_short_binary;
            this.tagReaders[Sereal.Tags.SHORT_BINARY_13] = this.read_short_binary;
            this.tagReaders[Sereal.Tags.SHORT_BINARY_14] = this.read_short_binary;
            this.tagReaders[Sereal.Tags.SHORT_BINARY_15] = this.read_short_binary;
            this.tagReaders[Sereal.Tags.SHORT_BINARY_16] = this.read_short_binary;
            this.tagReaders[Sereal.Tags.SHORT_BINARY_17] = this.read_short_binary;
            this.tagReaders[Sereal.Tags.SHORT_BINARY_18] = this.read_short_binary;
            this.tagReaders[Sereal.Tags.SHORT_BINARY_19] = this.read_short_binary;
            this.tagReaders[Sereal.Tags.SHORT_BINARY_20] = this.read_short_binary;
            this.tagReaders[Sereal.Tags.SHORT_BINARY_21] = this.read_short_binary;
            this.tagReaders[Sereal.Tags.SHORT_BINARY_22] = this.read_short_binary;
            this.tagReaders[Sereal.Tags.SHORT_BINARY_23] = this.read_short_binary;
            this.tagReaders[Sereal.Tags.SHORT_BINARY_24] = this.read_short_binary;
            this.tagReaders[Sereal.Tags.SHORT_BINARY_25] = this.read_short_binary;
            this.tagReaders[Sereal.Tags.SHORT_BINARY_26] = this.read_short_binary;
            this.tagReaders[Sereal.Tags.SHORT_BINARY_27] = this.read_short_binary;
            this.tagReaders[Sereal.Tags.SHORT_BINARY_28] = this.read_short_binary;
            this.tagReaders[Sereal.Tags.SHORT_BINARY_29] = this.read_short_binary;
            this.tagReaders[Sereal.Tags.SHORT_BINARY_30] = this.read_short_binary;
            this.tagReaders[Sereal.Tags.SHORT_BINARY_31] = this.read_short_binary;
        };
        return Decoder;
    })();
    Sereal.Decoder = Decoder;
})(Sereal || (Sereal = {}));
"use strict";
Number.prototype.toHex = function () { return this.toString(16); };
Number.prototype.to8BitString = function () { return this.toString(2).padLeft(8, "0"); };
String.prototype.padLeft = function (totalWidth, paddingChar) {
    if (paddingChar == null || paddingChar == "")
        paddingChar = " ";
    var s = this;
    while (s.length < totalWidth)
        s = paddingChar + s;
    return s;
};
String.prototype.last = function (predicate) {
    if (this.length == 0)
        return null;
    if (predicate == null)
        return this[this.length - 1];
    for (var i = this.length; i >= 0; i--) {
        if (predicate(this[i]))
            return this[i];
    }
    return null;
};
