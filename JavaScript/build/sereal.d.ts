declare module Sereal {
    class SerealDocument {
        user_metadata: any;
        body: any;
        header: SerealDocumentHeader;
    }
    class SerealDocumentHeader {
        magic: number;
        version: number;
        type: number;
        header_suffix_size: number;
        eight_bit_field: {
            value: number;
            has_user_metadata?: boolean;
        };
        body_compressed_length: number;
        body_uncompressed_length: number;
    }
    class PerlReference {
        _value: any;
        constructor(_value: any);
        getValue(): any;
    }
    class Latin1String {
        _bytes: ArrayBuffer;
        constructor(_bytes: ArrayBuffer);
        _string: string;
        valueOf(): string;
        toString(): string;
    }
    class PerlObject {
        name: string;
        obj: Object;
        constructor(name: string, obj: Object);
    }
    class Alias {
        obj: Object;
        constructor(obj: Object);
    }
    class SerealException extends Error {
    }
    class WeakReference {
        obj: Object;
        constructor(obj: Object);
    }
    class Padded {
        value: any;
        constructor(value: any);
    }
    class varint {
        static MSB: number;
        static REST: number;
        static read(buf: Int8Array, offset: number, out?: {
            bytesRead: number;
        }): number;
    }
    class Utils {
        static ab2str(buf: ArrayBuffer): string;
        static str2ab(str: string): ArrayBuffer;
        static str2ab2(str: string): ArrayBuffer;
        static ab2str2(buf: ArrayBuffer): string;
        static bytesToString(arr: Uint8Array): string;
        static stringToBytes(str: string): Uint8Array;
    }
    enum Consts {
        /** (0x6c) + (0x72 << 8) + (0x73 << 16) + (0x3d << 24) == 0x6c72733d */
        MAGIC = 1039364716,
        /** lower 5 bits */
        MASK_SHORT_BINARY_LEN = 31,
        /** 0 0x00 0b00000000 small positive integer - value in low 4 bits (identity) */
        POS_LOW = 0,
        /** 15 0x0f 0b00001111 small positive integer - value in low 4 bits (identity) */
        POS_HIGH = 15,
        /** 16 0x10 0b00010000 small negative integer - value in low 4 bits (k+32) */
        NEG_LOW = 16,
        /** 31 0x1f 0b00011111 small negative integer - value in low 4 bits (k+32) */
        NEG_HIGH = 31,
        /** 64 0x40 0b01000000 [<ITEM-TAG> ...] - count of items in low 4 bits (ARRAY must be refcnt=1) */
        ARRAYREF_LOW = 64,
        /** 79 0x4f 0b01001111 [<ITEM-TAG> ...] - count of items in low 4 bits (ARRAY must be refcnt=1) */
        ARRAYREF_HIGH = 79,
        /** 80 0x50 0b01010000 [<KEY-TAG> <ITEM-TAG> ...] - count in low 4 bits, key/value pairs (HASH must be refcnt=1) */
        HASHREF_LOW = 80,
        /** 95 0x5f 0b01011111 [<KEY-TAG> <ITEM-TAG> ...] - count in low 4 bits, key/value pairs (HASH must be refcnt=1) */
        HASHREF_HIGH = 95,
        /** 96 0x60 0b01100000 <BYTES> - binary/latin1 string, length encoded in low 5 bits of tag */
        SHORT_BINARY_LOW = 96,
        /** 127 0x7f 0b01111111 <BYTES> - binary/latin1 string, length encoded in low 5 bits of tag */
        SHORT_BINARY_HIGH = 127,
        /** 128 0x80 0b10000000 if this bit is set track the item */
        TRACK_FLAG = 128,
    }
    enum Tags {
        POS_0 = 0,
        POS_1 = 1,
        POS_2 = 2,
        POS_3 = 3,
        POS_4 = 4,
        POS_5 = 5,
        POS_6 = 6,
        POS_7 = 7,
        POS_8 = 8,
        POS_9 = 9,
        POS_10 = 10,
        POS_11 = 11,
        POS_12 = 12,
        POS_13 = 13,
        POS_14 = 14,
        POS_15 = 15,
        NEG_16 = 16,
        NEG_15 = 17,
        NEG_14 = 18,
        NEG_13 = 19,
        NEG_12 = 20,
        NEG_11 = 21,
        NEG_10 = 22,
        NEG_9 = 23,
        NEG_8 = 24,
        NEG_7 = 25,
        NEG_6 = 26,
        NEG_5 = 27,
        NEG_4 = 28,
        NEG_3 = 29,
        NEG_2 = 30,
        NEG_1 = 31,
        VARINT = 32,
        ZIGZAG = 33,
        FLOAT = 34,
        DOUBLE = 35,
        LONG_DOUBLE = 36,
        UNDEF = 37,
        BINARY = 38,
        STR_UTF8 = 39,
        REFN = 40,
        REFP = 41,
        HASH = 42,
        ARRAY = 43,
        OBJECT = 44,
        OBJECTV = 45,
        ALIAS = 46,
        COPY = 47,
        WEAKEN = 48,
        REGEXP = 49,
        OBJECT_FREEZE = 50,
        OBJECTV_FREEZE = 51,
        RESERVED_0 = 52,
        RESERVED_1 = 53,
        RESERVED_2 = 54,
        RESERVED_3 = 55,
        RESERVED_4 = 56,
        CANONICAL_UNDEF = 57,
        FALSE = 58,
        TRUE = 59,
        MANY = 60,
        PACKET_START = 61,
        EXTEND = 62,
        PAD = 63,
        ARRAYREF_0 = 64,
        ARRAYREF_1 = 65,
        ARRAYREF_2 = 66,
        ARRAYREF_3 = 67,
        ARRAYREF_4 = 68,
        ARRAYREF_5 = 69,
        ARRAYREF_6 = 70,
        ARRAYREF_7 = 71,
        ARRAYREF_8 = 72,
        ARRAYREF_9 = 73,
        ARRAYREF_10 = 74,
        ARRAYREF_11 = 75,
        ARRAYREF_12 = 76,
        ARRAYREF_13 = 77,
        ARRAYREF_14 = 78,
        ARRAYREF_15 = 79,
        HASHREF_0 = 80,
        HASHREF_1 = 81,
        HASHREF_2 = 82,
        HASHREF_3 = 83,
        HASHREF_4 = 84,
        HASHREF_5 = 85,
        HASHREF_6 = 86,
        HASHREF_7 = 87,
        HASHREF_8 = 88,
        HASHREF_9 = 89,
        HASHREF_10 = 90,
        HASHREF_11 = 91,
        HASHREF_12 = 92,
        HASHREF_13 = 93,
        HASHREF_14 = 94,
        HASHREF_15 = 95,
        SHORT_BINARY_0 = 96,
        SHORT_BINARY_1 = 97,
        SHORT_BINARY_2 = 98,
        SHORT_BINARY_3 = 99,
        SHORT_BINARY_4 = 100,
        SHORT_BINARY_5 = 101,
        SHORT_BINARY_6 = 102,
        SHORT_BINARY_7 = 103,
        SHORT_BINARY_8 = 104,
        SHORT_BINARY_9 = 105,
        SHORT_BINARY_10 = 106,
        SHORT_BINARY_11 = 107,
        SHORT_BINARY_12 = 108,
        SHORT_BINARY_13 = 109,
        SHORT_BINARY_14 = 110,
        SHORT_BINARY_15 = 111,
        SHORT_BINARY_16 = 112,
        SHORT_BINARY_17 = 113,
        SHORT_BINARY_18 = 114,
        SHORT_BINARY_19 = 115,
        SHORT_BINARY_20 = 116,
        SHORT_BINARY_21 = 117,
        SHORT_BINARY_22 = 118,
        SHORT_BINARY_23 = 119,
        SHORT_BINARY_24 = 120,
        SHORT_BINARY_25 = 121,
        SHORT_BINARY_26 = 122,
        SHORT_BINARY_27 = 123,
        SHORT_BINARY_28 = 124,
        SHORT_BINARY_29 = 125,
        SHORT_BINARY_30 = 126,
        SHORT_BINARY_31 = 127,
    }
}
interface Number {
    toHex(): string;
    to8BitString(): string;
}
declare module Sereal {
    class DataReader {
        constructor(data: any);
        view: DataView;
        pos: number;
        readString(length: number): string;
        readFloat(): number;
        readDouble(): number;
        readInt32(): number;
        asUint8Array(): Uint8Array;
        readInt(): number;
        readBytesTo(buf: ArrayBuffer): ArrayBuffer;
        readByte(): number;
        readVarInt(): number;
        readBytes(length?: number): Uint8Array;
        absPos: number;
        toHex(): string;
        hasRemaining(): boolean;
        remaining(): number;
        static toDataView(data: any): DataView;
    }
}
declare module Sereal {
    class Decoder {
        private perlStyleObjects;
        private prefer_latin1;
        private log;
        private reader;
        private tracked;
        private objectType;
        private perlRefs;
        private preservePadding;
        private doc;
        private debug;
        private tagReaders;
        constructor();
        init(data: any): void;
        decodeDocument(data?: any): SerealDocument;
        decodeDocumentBody(data: Uint8Array): any;
        read(): any;
        deflate(): Uint8Array;
        read_weaken(): any;
        read_alias(): any;
        read_object_v(): PerlObject;
        read_refp(): any;
        read_refn(): any;
        read_double(): number;
        read_array(tag: number, trackPos: number): Object[];
        /**
         * if tag == 0, next is varint for number of elements, otherwise lower 4 bits are length
         *
         * @param tag lower 4 bits is length or 0 for next varint is length
         * @param track we might need to track since array elements could refer to us
         */
        read_array_ref(tag: number, trackPos: number): Object[];
        _read_array(length: number, trackPos: number): Object[];
        read_binary(): any;
        read_hash(tag: number, trackPos: number): Object;
        read_hash_ref(tag: number, trackPos: number): Object;
        _read_hash(num_keys: number, trackPos: number): Object;
        read_varint(): number;
        read_pos(tag: number): number;
        read_neg(tag: number): number;
        read_pad(): any;
        /** Read a short binary ISO-8859-1 (latin1) string, the lower bits of the tag hold the length */
        read_short_binary(tag: number): string;
        read_UTF8(): string;
        read_zigzag(): number;
        read_regex(): RegExp;
        read_object(): Object;
        read_tracked_item(): Object;
        track(pos: number, val: Object): void;
        getTracked(pos: number): any;
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
        read_copy(): any;
        getClassByName(name: string): Function;
        bless(ctor: Function, obj: Object): Object;
        mapTagReaders(): void;
    }
}
declare module Zlib {
    class Deflate {
        constructor(arr: Uint8Array);
        compress(): Uint8Array;
    }
    class Inflate {
        constructor(arr: Uint8Array);
        decompress(): Uint8Array;
    }
    class RawInflate {
        constructor(arr: Uint8Array);
        decompress(): Uint8Array;
    }
    class InflateStream {
        constructor(arr: Uint8Array);
        decompress(): Uint8Array;
    }
}
declare class Q {
    static stringifyFormatted(value: any): string;
}
interface ObjectConstructor {
    values(obj: Object): string[];
}
interface Array<T> {
    select<R>(selector: (item: T) => R): Array<R>;
    forEachAsyncProgressive(action: (item: T, callback: () => void) => void, finalCallback: () => void): any;
}
interface Number {
    toHex(): string;
}
interface JQuery {
    makeGraph(opts: any): JQuery;
}
interface ArrayConstructor {
    generateNumbers(from: number, until: number): number[];
}
interface JQueryXHR extends JQueryPromise {
}
interface String {
    padLeft(totalLength: number, paddingChar: string): string;
    last(): string;
}
