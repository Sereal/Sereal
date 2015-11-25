declare module Sereal {
    class SerealDocument {
        magic: number;
        version: number;
        type: number;
        header_suffix_size: number;
        eight_bit_field: {
            value: number;
            has_user_metadata?: boolean;
        };
        user_metadata: any;
        body: any;
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
    class Class {
        static forName(name: string): Function;
    }
    enum ByteOrder {
        LITTLE_ENDIAN = 1,
        BIG_ENDIAN = 2,
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
        static dump(obj: any): void;
        static bless(ctor: Function, obj: Object): Object;
        static ab2str(buf: ArrayBuffer): string;
        static str2ab(str: string): ArrayBuffer;
        static str2ab2(str: string): ArrayBuffer;
        static ab2str2(buf: ArrayBuffer): string;
    }
    enum Consts {
        MAGIC = 1039364716,
        MASK_SHORT_BINARY_LEN = 31,
        POS_LOW = 0,
        POS_HIGH = 15,
        NEG_LOW = 16,
        NEG_HIGH = 31,
        ARRAYREF_LOW = 64,
        ARRAYREF_HIGH = 79,
        HASHREF_LOW = 80,
        HASHREF_HIGH = 95,
        SHORT_BINARY_LOW = 96,
        SHORT_BINARY_HIGH = 127,
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
        static PERL_OBJECT: string;
        static POJO: string;
        prefer_latin1: boolean;
        log: Console;
        reader: DataReader;
        tracked: Object;
        objectType: string;
        perlRefs: boolean;
        preservePadding: boolean;
        _data: Uint8Array;
        doc: SerealDocument;
        tagReaders: Array<() => any>;
        constructor(options?: DecoderOptions);
        decodeBinaryText(binaryText: string): SerealDocument;
        decodeDocumentBody(byteArray: Uint8Array): any;
        deflate(): Uint8Array;
        read_weaken(): WeakReference;
        read_alias(): Alias;
        read_object_v(): PerlObject;
        read_refp(): any;
        read_refn(): any;
        read_double(): number;
        /**
         * if tag == 0, next is varint for number of elements, otherwise lower 4 bits are length
         *
         * @param tag lower 4 bits is length or 0 for next varint is length
         * @param track we might need to track since array elements could refer to us
         */
        read_array(tag: number, trackPos: number): Object[];
        /** Reads a byte array, but was called read_binary in C, so for grepping purposes I kept the name. */
        read_binary(): any;
        read_hash(tag: number, trackPos: number): Object;
        read_varint(): number;
        readSingleValue(): any;
        assertEqual(x: any, y: any): boolean;
        /** Read a short binary ISO-8859-1 (latin1) string, the lower bits of the tag hold the length */
        read_short_binary(tag: number): ArrayBuffer;
        read_UTF8(): string;
        read_zigzag(): number;
        read_regex(): RegExp;
        read_object(): Object;
        get_tracked_item(): Object;
        /** Set the data to deserealize(for calling decode multiple times when there are concatenated packets)(never tested)     */
        data: Uint8Array;
        track(pos: number, val: Object): void;
        getTracked(pos: number): any;
        reset(): void;
        checkNoEOD(): void;
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
