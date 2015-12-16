"use strict";
module Sereal {

    export class SerealDocument {
        user_metadata: any;
        body: any;
        header: SerealDocumentHeader;
    }
    export class SerealDocumentHeader {
        magic: number;
        version: number;
        type: number;
        header_suffix_size: number;
        eight_bit_field: { value: number, has_user_metadata?: boolean };
        body_compressed_length: number;
        body_uncompressed_length: number;
    }

    export class PerlReference {
        constructor(public _value: any) {
        }
        getValue() {
            return this._value;
        }
    }

    export class PerlObject {
        constructor(public name: string, public obj: Object) {
        }
    }

    export class Alias {
        constructor(public obj: Object) { }
    }

    export class SerealException extends Error {
        constructor(public msg?: string) {
            super(msg);
        }
    }

    export class WeakReference {
        constructor(public obj: Object) { }
    }

    export class Padded {
        constructor(public value: any) { }
    }

    export class varint {
        static MSB = 0x80;
        static REST = 0x7F;

        static read(buf: Int8Array, offset: number, out?: { bytesRead: number }): number {
            var res = 0
                , offset = offset || 0
                , shift = 0
                , counter = offset
                , b
                , l = buf.length;

            do {
                if (counter >= l) {
                    if (out != null)
                        out.bytesRead = 0;
                    return undefined;
                }
                b = buf[counter++]
                res += shift < 28
                    ? (b & varint.REST) << shift
                    : (b & varint.REST) * Math.pow(2, shift);
                shift += 7;
            } while (b >= varint.MSB);

            if (out != null)
                out.bytesRead = counter - offset;

            return res;
        }

    }

    export class Utils {
        static bytesToString(arr: Uint8Array): string {
            return String.fromCharCode.apply(null, arr);
        }
        static stringToBytes(str: string): Uint8Array {
            var arr = new Uint8Array(str.length);
            for (var i = 0, strLen = str.length; i < strLen; i++) {
                arr[i] = str.charCodeAt(i);
            }
            return arr;
        }

    }




    export enum Consts {
        /** (0x6c) + (0x72 << 8) + (0x73 << 16) + (0x3d << 24) == 0x6c72733d */
        MAGIC = 1039364716,
        /** (0x6c) + (0x72 << 8) + (0x73 << 16) + (0x3d << 24) == 0x6c72733d */
        MAGIC_OLD = 1039364716,
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

    export enum Tags {
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
        SHORT_BINARY_31 = 127

    }

}

