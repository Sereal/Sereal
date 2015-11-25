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
    var Latin1String = (function () {
        function Latin1String(_bytes) {
            this._bytes = _bytes;
            this._string = Utils.ab2str2(_bytes);
        }
        Latin1String.prototype.valueOf = function () {
            return this._string;
        };
        Latin1String.prototype.toString = function () {
            return this._string;
        };
        return Latin1String;
    })();
    Sereal.Latin1String = Latin1String;
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
        function SerealException() {
            _super.apply(this, arguments);
        }
        return SerealException;
    })(Error);
    Sereal.SerealException = SerealException;
    var Class = (function () {
        function Class() {
        }
        Class.forName = function (name) {
            throw new Error();
        };
        return Class;
    })();
    Sereal.Class = Class;
    (function (ByteOrder) {
        ByteOrder[ByteOrder["LITTLE_ENDIAN"] = 1] = "LITTLE_ENDIAN";
        ByteOrder[ByteOrder["BIG_ENDIAN"] = 2] = "BIG_ENDIAN";
    })(Sereal.ByteOrder || (Sereal.ByteOrder = {}));
    var ByteOrder = Sereal.ByteOrder;
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
        Utils.dump = function (obj) {
            console.debug(obj);
        };
        Utils.bless = function (ctor, obj) {
            throw new Error();
        };
        Utils.ab2str = function (buf) {
            return String.fromCharCode.apply(null, new Uint16Array(buf));
        };
        Utils.str2ab = function (str) {
            var buf = new ArrayBuffer(str.length * 2); // 2 bytes for each char
            var bufView = new Uint16Array(buf);
            for (var i = 0, strLen = str.length; i < strLen; i++) {
                bufView[i] = str.charCodeAt(i);
            }
            return buf;
        };
        Utils.str2ab2 = function (str) {
            var buf = new ArrayBuffer(str.length);
            var bufView = new Uint8Array(buf);
            for (var i = 0, strLen = str.length; i < strLen; i++) {
                var byte = str.charCodeAt(i);
                bufView[i] = byte;
            }
            return buf;
        };
        Utils.ab2str2 = function (buf) {
            return String.fromCharCode.apply(null, new Uint8Array(buf));
        };
        return Utils;
    })();
    Sereal.Utils = Utils;
    function getConsts() {
        return [
            { Tag: "POS_0", Char: "", Dec: "0", Hex: "0x00", Binary: "0b00000000", Follow: "small" },
            { Tag: "POS_1", Char: "", Dec: "1", Hex: "0x01", Binary: "0b00000001", Follow: "" },
            { Tag: "POS_2", Char: "", Dec: "2", Hex: "0x02", Binary: "0b00000010", Follow: "" },
            { Tag: "POS_3", Char: "", Dec: "3", Hex: "0x03", Binary: "0b00000011", Follow: "" },
            { Tag: "POS_4", Char: "", Dec: "4", Hex: "0x04", Binary: "0b00000100", Follow: "" },
            { Tag: "POS_5", Char: "", Dec: "5", Hex: "0x05", Binary: "0b00000101", Follow: "" },
            { Tag: "POS_6", Char: "", Dec: "6", Hex: "0x06", Binary: "0b00000110", Follow: "" },
            { Tag: "POS_7", Char: "\"\\a\"", Dec: "7", Hex: "0x07", Binary: "0b00000111", Follow: "" },
            { Tag: "POS_8", Char: "\"\\b\"", Dec: "8", Hex: "0x08", Binary: "0b00001000", Follow: "" },
            { Tag: "POS_9", Char: "\"\\t\"", Dec: "9", Hex: "0x09", Binary: "0b00001001", Follow: "" },
            { Tag: "POS_10", Char: "\"\\n\"", Dec: "10", Hex: "0x0a", Binary: "0b00001010", Follow: "" },
            { Tag: "POS_11", Char: "", Dec: "11", Hex: "0x0b", Binary: "0b00001011", Follow: "" },
            { Tag: "POS_12", Char: "\"\\f\"", Dec: "12", Hex: "0x0c", Binary: "0b00001100", Follow: "" },
            { Tag: "POS_13", Char: "\"\\r\"", Dec: "13", Hex: "0x0d", Binary: "0b00001101", Follow: "" },
            { Tag: "POS_14", Char: "", Dec: "14", Hex: "0x0e", Binary: "0b00001110", Follow: "" },
            { Tag: "POS_15", Char: "", Dec: "15", Hex: "0x0f", Binary: "0b00001111", Follow: "small" },
            { Tag: "NEG_16", Char: "", Dec: "16", Hex: "0x10", Binary: "0b00010000", Follow: "small" },
            { Tag: "NEG_15", Char: "", Dec: "17", Hex: "0x11", Binary: "0b00010001", Follow: "" },
            { Tag: "NEG_14", Char: "", Dec: "18", Hex: "0x12", Binary: "0b00010010", Follow: "" },
            { Tag: "NEG_13", Char: "", Dec: "19", Hex: "0x13", Binary: "0b00010011", Follow: "" },
            { Tag: "NEG_12", Char: "", Dec: "20", Hex: "0x14", Binary: "0b00010100", Follow: "" },
            { Tag: "NEG_11", Char: "", Dec: "21", Hex: "0x15", Binary: "0b00010101", Follow: "" },
            { Tag: "NEG_10", Char: "", Dec: "22", Hex: "0x16", Binary: "0b00010110", Follow: "" },
            { Tag: "NEG_9", Char: "", Dec: "23", Hex: "0x17", Binary: "0b00010111", Follow: "" },
            { Tag: "NEG_8", Char: "", Dec: "24", Hex: "0x18", Binary: "0b00011000", Follow: "" },
            { Tag: "NEG_7", Char: "", Dec: "25", Hex: "0x19", Binary: "0b00011001", Follow: "" },
            { Tag: "NEG_6", Char: "", Dec: "26", Hex: "0x1a", Binary: "0b00011010", Follow: "" },
            { Tag: "NEG_5", Char: "\"\\e\"", Dec: "27", Hex: "0x1b", Binary: "0b00011011", Follow: "" },
            { Tag: "NEG_4", Char: "", Dec: "28", Hex: "0x1c", Binary: "0b00011100", Follow: "" },
            { Tag: "NEG_3", Char: "", Dec: "29", Hex: "0x1d", Binary: "0b00011101", Follow: "" },
            { Tag: "NEG_2", Char: "", Dec: "30", Hex: "0x1e", Binary: "0b00011110", Follow: "" },
            { Tag: "NEG_1", Char: "", Dec: "31", Hex: "0x1f", Binary: "0b00011111", Follow: "small" },
            { Tag: "VARINT", Char: "\" \"", Dec: "32", Hex: "0x20", Binary: "0b00100000", Follow: "<VARIN" },
            { Tag: "ZIGZAG", Char: "\"!\"", Dec: "33", Hex: "0x21", Binary: "0b00100001", Follow: "<ZIGZA" },
            { Tag: "FLOAT", Char: "\"\\\"\"", Dec: "34", Hex: "0x22", Binary: "0b00100010", Follow: "<IEEE-" },
            { Tag: "DOUBLE", Char: "\"#\"", Dec: "35", Hex: "0x23", Binary: "0b00100011", Follow: "<IEEE-" },
            { Tag: "LONG_DOUBLE", Char: "\"\\$\"", Dec: "36", Hex: "0x24", Binary: "0b00100100", Follow: "<IEEE-" },
            { Tag: "UNDEF", Char: "\"%\"", Dec: "37", Hex: "0x25", Binary: "0b00100101", Follow: "None -" },
            { Tag: "BINARY", Char: "\"&\"", Dec: "38", Hex: "0x26", Binary: "0b00100110", Follow: "<LEN-V" },
            { Tag: "STR_UTF8", Char: "\"'\"", Dec: "39", Hex: "0x27", Binary: "0b00100111", Follow: "<LEN-V" },
            { Tag: "REFN", Char: "\"(\"", Dec: "40", Hex: "0x28", Binary: "0b00101000", Follow: "<ITEM-" },
            { Tag: "REFP", Char: "\")\"", Dec: "41", Hex: "0x29", Binary: "0b00101001", Follow: "<OFFSE" },
            { Tag: "HASH", Char: "\"*\"", Dec: "42", Hex: "0x2a", Binary: "0b00101010", Follow: "<COUNT" },
            { Tag: "ARRAY", Char: "\"+\"", Dec: "43", Hex: "0x2b", Binary: "0b00101011", Follow: "<COUNT" },
            { Tag: "OBJECT", Char: "\",\"", Dec: "44", Hex: "0x2c", Binary: "0b00101100", Follow: "<STR-T" },
            { Tag: "OBJECTV", Char: "\"-\"", Dec: "45", Hex: "0x2d", Binary: "0b00101101", Follow: "<OFFSE" },
            { Tag: "ALIAS", Char: "\".\"", Dec: "46", Hex: "0x2e", Binary: "0b00101110", Follow: "<OFFSE" },
            { Tag: "COPY", Char: "\"/\"", Dec: "47", Hex: "0x2f", Binary: "0b00101111", Follow: "<OFFSE" },
            { Tag: "WEAKEN", Char: "\"0\"", Dec: "48", Hex: "0x30", Binary: "0b00110000", Follow: "<REF-T" },
            { Tag: "REGEXP", Char: "\"1\"", Dec: "49", Hex: "0x31", Binary: "0b00110001", Follow: "<PATTE" },
            { Tag: "OBJECT_FREEZE", Char: "\"2\"", Dec: "50", Hex: "0x32", Binary: "0b00110010", Follow: "<STR-T" },
            { Tag: "OBJECTV_FREEZE", Char: "\"3\"", Dec: "51", Hex: "0x33", Binary: "0b00110011", Follow: "<OFFSE" },
            { Tag: "RESERVED_0", Char: "\"4\"", Dec: "52", Hex: "0x34", Binary: "0b00110100", Follow: "reserv" },
            { Tag: "RESERVED_1", Char: "\"5\"", Dec: "53", Hex: "0x35", Binary: "0b00110101", Follow: "" },
            { Tag: "RESERVED_2", Char: "\"6\"", Dec: "54", Hex: "0x36", Binary: "0b00110110", Follow: "" },
            { Tag: "RESERVED_3", Char: "\"7\"", Dec: "55", Hex: "0x37", Binary: "0b00110111", Follow: "" },
            { Tag: "RESERVED_4", Char: "\"8\"", Dec: "56", Hex: "0x38", Binary: "0b00111000", Follow: "reserv" },
            { Tag: "CANONICAL_UNDEF", Char: "\"9\"", Dec: "57", Hex: "0x39", Binary: "0b00111001", Follow: "undef" },
            { Tag: "FALSE", Char: "\":\"", Dec: "58", Hex: "0x3a", Binary: "0b00111010", Follow: "false" },
            { Tag: "TRUE", Char: "\";\"", Dec: "59", Hex: "0x3b", Binary: "0b00111011", Follow: "true" },
            { Tag: "MANY", Char: "\"<\"", Dec: "60", Hex: "0x3c", Binary: "0b00111100", Follow: "<LEN-V" },
            { Tag: "PACKET_START", Char: "\"=\"", Dec: "61", Hex: "0x3d", Binary: "0b00111101", Follow: "(first" },
            { Tag: "EXTEND", Char: "\">\"", Dec: "62", Hex: "0x3e", Binary: "0b00111110", Follow: "<BYTE>" },
            { Tag: "PAD", Char: "\"?\"", Dec: "63", Hex: "0x3f", Binary: "0b00111111", Follow: "(ignor" },
            { Tag: "ARRAYREF_0", Char: "\"\\@\"", Dec: "64", Hex: "0x40", Binary: "0b01000000", Follow: "[<ITEM" },
            { Tag: "ARRAYREF_1", Char: "\"A\"", Dec: "65", Hex: "0x41", Binary: "0b01000001", Follow: "" },
            { Tag: "ARRAYREF_2", Char: "\"B\"", Dec: "66", Hex: "0x42", Binary: "0b01000010", Follow: "" },
            { Tag: "ARRAYREF_3", Char: "\"C\"", Dec: "67", Hex: "0x43", Binary: "0b01000011", Follow: "" },
            { Tag: "ARRAYREF_4", Char: "\"D\"", Dec: "68", Hex: "0x44", Binary: "0b01000100", Follow: "" },
            { Tag: "ARRAYREF_5", Char: "\"E\"", Dec: "69", Hex: "0x45", Binary: "0b01000101", Follow: "" },
            { Tag: "ARRAYREF_6", Char: "\"F\"", Dec: "70", Hex: "0x46", Binary: "0b01000110", Follow: "" },
            { Tag: "ARRAYREF_7", Char: "\"G\"", Dec: "71", Hex: "0x47", Binary: "0b01000111", Follow: "" },
            { Tag: "ARRAYREF_8", Char: "\"H\"", Dec: "72", Hex: "0x48", Binary: "0b01001000", Follow: "" },
            { Tag: "ARRAYREF_9", Char: "\"I\"", Dec: "73", Hex: "0x49", Binary: "0b01001001", Follow: "" },
            { Tag: "ARRAYREF_10", Char: "\"J\"", Dec: "74", Hex: "0x4a", Binary: "0b01001010", Follow: "" },
            { Tag: "ARRAYREF_11", Char: "\"K\"", Dec: "75", Hex: "0x4b", Binary: "0b01001011", Follow: "" },
            { Tag: "ARRAYREF_12", Char: "\"L\"", Dec: "76", Hex: "0x4c", Binary: "0b01001100", Follow: "" },
            { Tag: "ARRAYREF_13", Char: "\"M\"", Dec: "77", Hex: "0x4d", Binary: "0b01001101", Follow: "" },
            { Tag: "ARRAYREF_14", Char: "\"N\"", Dec: "78", Hex: "0x4e", Binary: "0b01001110", Follow: "" },
            { Tag: "ARRAYREF_15", Char: "\"O\"", Dec: "79", Hex: "0x4f", Binary: "0b01001111", Follow: "[<ITEM" },
            { Tag: "HASHREF_0", Char: "\"P\"", Dec: "80", Hex: "0x50", Binary: "0b01010000", Follow: "[<KEY-" },
            { Tag: "HASHREF_1", Char: "\"Q\"", Dec: "81", Hex: "0x51", Binary: "0b01010001", Follow: "" },
            { Tag: "HASHREF_2", Char: "\"R\"", Dec: "82", Hex: "0x52", Binary: "0b01010010", Follow: "" },
            { Tag: "HASHREF_3", Char: "\"S\"", Dec: "83", Hex: "0x53", Binary: "0b01010011", Follow: "" },
            { Tag: "HASHREF_4", Char: "\"T\"", Dec: "84", Hex: "0x54", Binary: "0b01010100", Follow: "" },
            { Tag: "HASHREF_5", Char: "\"U\"", Dec: "85", Hex: "0x55", Binary: "0b01010101", Follow: "" },
            { Tag: "HASHREF_6", Char: "\"V\"", Dec: "86", Hex: "0x56", Binary: "0b01010110", Follow: "" },
            { Tag: "HASHREF_7", Char: "\"W\"", Dec: "87", Hex: "0x57", Binary: "0b01010111", Follow: "" },
            { Tag: "HASHREF_8", Char: "\"X\"", Dec: "88", Hex: "0x58", Binary: "0b01011000", Follow: "" },
            { Tag: "HASHREF_9", Char: "\"Y\"", Dec: "89", Hex: "0x59", Binary: "0b01011001", Follow: "" },
            { Tag: "HASHREF_10", Char: "\"Z\"", Dec: "90", Hex: "0x5a", Binary: "0b01011010", Follow: "" },
            { Tag: "HASHREF_11", Char: "\"[\"", Dec: "91", Hex: "0x5b", Binary: "0b01011011", Follow: "" },
            { Tag: "HASHREF_12", Char: "\"\\\\\"", Dec: "92", Hex: "0x5c", Binary: "0b01011100", Follow: "" },
            { Tag: "HASHREF_13", Char: "\"]\"", Dec: "93", Hex: "0x5d", Binary: "0b01011101", Follow: "" },
            { Tag: "HASHREF_14", Char: "\"^\"", Dec: "94", Hex: "0x5e", Binary: "0b01011110", Follow: "" },
            { Tag: "HASHREF_15", Char: "\"_\"", Dec: "95", Hex: "0x5f", Binary: "0b01011111", Follow: "[<KEY-" },
            { Tag: "SHORT_BINARY_0", Char: "\"`\"", Dec: "96", Hex: "0x60", Binary: "0b01100000", Follow: "<BYTES" },
            { Tag: "SHORT_BINARY_1", Char: "\"a\"", Dec: "97", Hex: "0x61", Binary: "0b01100001", Follow: "" },
            { Tag: "SHORT_BINARY_2", Char: "\"b\"", Dec: "98", Hex: "0x62", Binary: "0b01100010", Follow: "" },
            { Tag: "SHORT_BINARY_3", Char: "\"c\"", Dec: "99", Hex: "0x63", Binary: "0b01100011", Follow: "" },
            { Tag: "SHORT_BINARY_4", Char: "\"d\"", Dec: "100", Hex: "0x64", Binary: "0b01100100", Follow: "" },
            { Tag: "SHORT_BINARY_5", Char: "\"e\"", Dec: "101", Hex: "0x65", Binary: "0b01100101", Follow: "" },
            { Tag: "SHORT_BINARY_6", Char: "\"f\"", Dec: "102", Hex: "0x66", Binary: "0b01100110", Follow: "" },
            { Tag: "SHORT_BINARY_7", Char: "\"g\"", Dec: "103", Hex: "0x67", Binary: "0b01100111", Follow: "" },
            { Tag: "SHORT_BINARY_8", Char: "\"h\"", Dec: "104", Hex: "0x68", Binary: "0b01101000", Follow: "" },
            { Tag: "SHORT_BINARY_9", Char: "\"i\"", Dec: "105", Hex: "0x69", Binary: "0b01101001", Follow: "" },
            { Tag: "SHORT_BINARY_10", Char: "\"j\"", Dec: "106", Hex: "0x6a", Binary: "0b01101010", Follow: "" },
            { Tag: "SHORT_BINARY_11", Char: "\"k\"", Dec: "107", Hex: "0x6b", Binary: "0b01101011", Follow: "" },
            { Tag: "SHORT_BINARY_12", Char: "\"l\"", Dec: "108", Hex: "0x6c", Binary: "0b01101100", Follow: "" },
            { Tag: "SHORT_BINARY_13", Char: "\"m\"", Dec: "109", Hex: "0x6d", Binary: "0b01101101", Follow: "" },
            { Tag: "SHORT_BINARY_14", Char: "\"n\"", Dec: "110", Hex: "0x6e", Binary: "0b01101110", Follow: "" },
            { Tag: "SHORT_BINARY_15", Char: "\"o\"", Dec: "111", Hex: "0x6f", Binary: "0b01101111", Follow: "" },
            { Tag: "SHORT_BINARY_16", Char: "\"p\"", Dec: "112", Hex: "0x70", Binary: "0b01110000", Follow: "" },
            { Tag: "SHORT_BINARY_17", Char: "\"q\"", Dec: "113", Hex: "0x71", Binary: "0b01110001", Follow: "" },
            { Tag: "SHORT_BINARY_18", Char: "\"r\"", Dec: "114", Hex: "0x72", Binary: "0b01110010", Follow: "" },
            { Tag: "SHORT_BINARY_19", Char: "\"s\"", Dec: "115", Hex: "0x73", Binary: "0b01110011", Follow: "" },
            { Tag: "SHORT_BINARY_20", Char: "\"t\"", Dec: "116", Hex: "0x74", Binary: "0b01110100", Follow: "" },
            { Tag: "SHORT_BINARY_21", Char: "\"u\"", Dec: "117", Hex: "0x75", Binary: "0b01110101", Follow: "" },
            { Tag: "SHORT_BINARY_22", Char: "\"v\"", Dec: "118", Hex: "0x76", Binary: "0b01110110", Follow: "" },
            { Tag: "SHORT_BINARY_23", Char: "\"w\"", Dec: "119", Hex: "0x77", Binary: "0b01110111", Follow: "" },
            { Tag: "SHORT_BINARY_24", Char: "\"x\"", Dec: "120", Hex: "0x78", Binary: "0b01111000", Follow: "" },
            { Tag: "SHORT_BINARY_25", Char: "\"y\"", Dec: "121", Hex: "0x79", Binary: "0b01111001", Follow: "" },
            { Tag: "SHORT_BINARY_26", Char: "\"z\"", Dec: "122", Hex: "0x7a", Binary: "0b01111010", Follow: "" },
            { Tag: "SHORT_BINARY_27", Char: "\"{\"", Dec: "123", Hex: "0x7b", Binary: "0b01111011", Follow: "" },
            { Tag: "SHORT_BINARY_28", Char: "\"|\"", Dec: "124", Hex: "0x7c", Binary: "0b01111100", Follow: "" },
            { Tag: "SHORT_BINARY_29", Char: "\"}\"", Dec: "125", Hex: "0x7d", Binary: "0b01111101", Follow: "" },
            { Tag: "SHORT_BINARY_30", Char: "\"~\"", Dec: "126", Hex: "0x7e", Binary: "0b01111110", Follow: "" },
            { Tag: "SHORT_BINARY_31", Char: "", Dec: "127", Hex: "0x7f", Binary: "0b01111111", Follow: "<BYTES" }
        ];
    }
    function getConsts2() {
        var obj = {
            POS_0: 0,
            POS_1: 1,
            POS_2: 2,
            POS_3: 3,
            POS_4: 4,
            POS_5: 5,
            POS_6: 6,
            POS_7: 7,
            POS_8: 8,
            POS_9: 9,
            POS_10: 10,
            POS_11: 11,
            POS_12: 12,
            POS_13: 13,
            POS_14: 14,
            POS_15: 15,
            NEG_16: 16,
            NEG_15: 17,
            NEG_14: 18,
            NEG_13: 19,
            NEG_12: 20,
            NEG_11: 21,
            NEG_10: 22,
            NEG_9: 23,
            NEG_8: 24,
            NEG_7: 25,
            NEG_6: 26,
            NEG_5: 27,
            NEG_4: 28,
            NEG_3: 29,
            NEG_2: 30,
            NEG_1: 31,
            VARINT: 32,
            ZIGZAG: 33,
            FLOAT: 34,
            DOUBLE: 35,
            LONG_DOUBLE: 36,
            UNDEF: 37,
            BINARY: 38,
            STR_UTF8: 39,
            REFN: 40,
            REFP: 41,
            HASH: 42,
            ARRAY: 43,
            OBJECT: 44,
            OBJECTV: 45,
            ALIAS: 46,
            COPY: 47,
            WEAKEN: 48,
            REGEXP: 49,
            OBJECT_FREEZE: 50,
            OBJECTV_FREEZE: 51,
            RESERVED_0: 52,
            RESERVED_1: 53,
            RESERVED_2: 54,
            RESERVED_3: 55,
            RESERVED_4: 56,
            CANONICAL_UNDEF: 57,
            FALSE: 58,
            TRUE: 59,
            MANY: 60,
            PACKET_START: 61,
            EXTEND: 62,
            PAD: 63,
            ARRAYREF_0: 64,
            ARRAYREF_1: 65,
            ARRAYREF_2: 66,
            ARRAYREF_3: 67,
            ARRAYREF_4: 68,
            ARRAYREF_5: 69,
            ARRAYREF_6: 70,
            ARRAYREF_7: 71,
            ARRAYREF_8: 72,
            ARRAYREF_9: 73,
            ARRAYREF_10: 74,
            ARRAYREF_11: 75,
            ARRAYREF_12: 76,
            ARRAYREF_13: 77,
            ARRAYREF_14: 78,
            ARRAYREF_15: 79,
            HASHREF_0: 80,
            HASHREF_1: 81,
            HASHREF_2: 82,
            HASHREF_3: 83,
            HASHREF_4: 84,
            HASHREF_5: 85,
            HASHREF_6: 86,
            HASHREF_7: 87,
            HASHREF_8: 88,
            HASHREF_9: 89,
            HASHREF_10: 90,
            HASHREF_11: 91,
            HASHREF_12: 92,
            HASHREF_13: 93,
            HASHREF_14: 94,
            HASHREF_15: 95,
            SHORT_BINARY_0: 96,
            SHORT_BINARY_1: 97,
            SHORT_BINARY_2: 98,
            SHORT_BINARY_3: 99,
            SHORT_BINARY_4: 100,
            SHORT_BINARY_5: 101,
            SHORT_BINARY_6: 102,
            SHORT_BINARY_7: 103,
            SHORT_BINARY_8: 104,
            SHORT_BINARY_9: 105,
            SHORT_BINARY_10: 106,
            SHORT_BINARY_11: 107,
            SHORT_BINARY_12: 108,
            SHORT_BINARY_13: 109,
            SHORT_BINARY_14: 110,
            SHORT_BINARY_15: 111,
            SHORT_BINARY_16: 112,
            SHORT_BINARY_17: 113,
            SHORT_BINARY_18: 114,
            SHORT_BINARY_19: 115,
            SHORT_BINARY_20: 116,
            SHORT_BINARY_21: 117,
            SHORT_BINARY_22: 118,
            SHORT_BINARY_23: 119,
            SHORT_BINARY_24: 120,
            SHORT_BINARY_25: 121,
            SHORT_BINARY_26: 122,
            SHORT_BINARY_27: 123,
            SHORT_BINARY_28: 124,
            SHORT_BINARY_29: 125,
            SHORT_BINARY_30: 126,
            SHORT_BINARY_31: 127
        };
        return obj;
    }
    (function (Consts) {
        Consts[Consts["MAGIC"] = 1039364716] = "MAGIC";
        Consts[Consts["MASK_SHORT_BINARY_LEN"] = 31] = "MASK_SHORT_BINARY_LEN";
        Consts[Consts["POS_LOW"] = 0] = "POS_LOW";
        Consts[Consts["POS_HIGH"] = 15] = "POS_HIGH";
        Consts[Consts["NEG_LOW"] = 16] = "NEG_LOW";
        Consts[Consts["NEG_HIGH"] = 31] = "NEG_HIGH";
        Consts[Consts["ARRAYREF_LOW"] = 64] = "ARRAYREF_LOW";
        Consts[Consts["ARRAYREF_HIGH"] = 79] = "ARRAYREF_HIGH";
        Consts[Consts["HASHREF_LOW"] = 80] = "HASHREF_LOW";
        Consts[Consts["HASHREF_HIGH"] = 95] = "HASHREF_HIGH";
        Consts[Consts["SHORT_BINARY_LOW"] = 96] = "SHORT_BINARY_LOW";
        Consts[Consts["SHORT_BINARY_HIGH"] = 127] = "SHORT_BINARY_HIGH";
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
Number.prototype.toHex = function () { return this.toString(16); };
Number.prototype.to8BitString = function () { return this.toString(2).padLeft(8, "0"); };
"use strict";
var Sereal;
(function (Sereal) {
    var DataReader = (function () {
        function DataReader(data) {
            this.view = DataReader.toDataView(data);
            this.pos = 0;
        }
        DataReader.prototype.readDouble = function () { throw new Error(); };
        DataReader.prototype.readInt32 = function () { return this.readInt(); };
        DataReader.prototype.asUint8Array = function () { return new Uint8Array(this.view.buffer, this.view.byteOffset, this.view.byteLength); };
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
            if (ArrayBuffer.isView(data)) {
                return new DataView(data.buffer, data.byteOffset, data.byteLength);
            }
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
    /** Decoder for Sereal */
    var Decoder = (function () {
        function Decoder(options) {
            if (options === void 0) { options = null; }
            this.log = console;
            this.tracked = {};
            this.perlRefs = false;
            this.preservePadding = false;
            this.tagReaders = new Array(128);
            if (options == null)
                options = {};
            //this.options = options == null ? new HashMap<String, Object>() : options;
            this.objectType = options.object_type || Decoder.PERL_OBJECT;
            this.perlRefs = options.use_perl_refs || false; //( "use_perl_refs" ) ? ((Boolean) options.get( "use_perl_refs" )) : false;
            this.preservePadding = options.preserve_pad_tags || false; //" ) ? ((Boolean) options.get( "preserve_pad_tags" )) : false;
            this.prefer_latin1 = options.prefer_latin1 || false; //") ? ((Boolean) options.get("prefer_latin1")) : false;
            this.tagReaders[Sereal.Tags.VARINT] = this.read_varint;
            this.tagReaders[Sereal.Tags.ZIGZAG] = this.read_zigzag;
            this.tagReaders[Sereal.Tags.DOUBLE] = this.read_double;
            this.tagReaders[Sereal.Tags.DOUBLE] = this.read_double;
            this.tagReaders[Sereal.Tags.TRUE] = function () { return true; };
            this.tagReaders[Sereal.Tags.FALSE] = function () { return false; };
            this.tagReaders[Sereal.Tags.UNDEF] = function () { return null; };
            this.tagReaders[Sereal.Tags.BINARY] = this.read_binary;
            this.tagReaders[Sereal.Tags.STR_UTF8] = this.read_UTF8;
            this.tagReaders[Sereal.Tags.REFN] = this.read_refn;
            this.tagReaders[Sereal.Tags.REFP] = this.read_refp;
            this.tagReaders[Sereal.Tags.OBJECT] = this.read_object;
            this.tagReaders[Sereal.Tags.OBJECTV] = this.read_object_v;
            this.tagReaders[Sereal.Tags.COPY] = this.read_copy;
            this.tagReaders[Sereal.Tags.ALIAS] = this.read_alias;
            this.tagReaders[Sereal.Tags.WEAKEN] = this.read_weaken;
            this.tagReaders[Sereal.Tags.REGEXP] = this.read_regex;
        }
        Decoder.prototype.decodeBinaryText = function (binaryText) {
            this.doc = new Sereal.SerealDocument();
            var doc = this.doc;
            var _buf = Sereal.Utils.str2ab2(binaryText);
            this.reader = new Sereal.DataReader(new DataView(_buf));
            console.log({ pos: this.reader.pos });
            doc.magic = this.reader.readInt32();
            if (doc.magic != Sereal.Consts.MAGIC)
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
        };
        Decoder.prototype.decodeDocumentBody = function (byteArray) {
            var dec = new Decoder({ prefer_latin1: true });
            dec.data = byteArray;
            var x = dec.readSingleValue();
            console.log(x);
            return x;
        };
        Decoder.prototype.deflate = function () {
            var pos = this.reader.pos;
            var arr = this.reader.readBytes();
            if (arr.length != this.doc.body_compressed_length)
                throw new Error();
            var zip = new Zlib.Inflate(arr);
            var deflated = zip.decompress();
            if (deflated.length != this.doc.body_uncompressed_length)
                throw new Error();
            return deflated;
        };
        Decoder.prototype.read_weaken = function () {
            //this.log.debug("Weakening the next thing");
            // so the next thing HAS to be a ref (afaict) which means we can track it
            var placeHolder = new Sereal.PerlReference(this.readSingleValue().getValue());
            var /*WeakReference<PerlReference>*/ wref = new Sereal.WeakReference(placeHolder);
            return wref;
        };
        Decoder.prototype.read_alias = function () {
            //this.log.debug("Reading an alias");
            var alias = new Sereal.Alias(this.get_tracked_item());
            //this.log.debug("Read alias: ", alias);
            return alias;
        };
        Decoder.prototype.read_object_v = function () {
            //this.log.debug("Reading an objectv");
            var className = this.get_tracked_item();
            this.log.debug("Read an objectv of class: ", className);
            var out = new Sereal.PerlObject(className, this.readSingleValue());
            return out;
        };
        Decoder.prototype.read_refp = function () {
            var offset_prev = this.read_varint();
            var prv_value = this.getTracked(offset_prev);
            var prev = this.perlRefs ? new Sereal.PerlReference(prv_value) : prv_value;
            this.log.debug("Read prev: ", prev);
            return prev;
        };
        Decoder.prototype.read_refn = function () {
            //this.log.debug("read_refn", "Reading ref to next");
            var out;
            var /*PerlReference*/ refn = new Sereal.PerlReference(this.readSingleValue());
            if (this.perlRefs) {
                out = refn;
            }
            else {
                out = refn.getValue();
            }
            this.log.debug("read_refn", "Read ref: ", out);
            return out;
        };
        Decoder.prototype.read_double = function () {
            var d = this.reader.readDouble();
            return d;
        };
        /**
         * if tag == 0, next is varint for number of elements, otherwise lower 4 bits are length
         *
         * @param tag lower 4 bits is length or 0 for next varint is length
         * @param track we might need to track since array elements could refer to us
         */
        Decoder.prototype.read_array = function (tag, trackPos) {
            var length = 0;
            if (tag == 0) {
                length = this.read_varint();
            }
            else {
                length = tag & 15;
            }
            this.log.debug("Array length: ", length);
            var out = new Array(length);
            if (trackPos != null)
                this.track(trackPos, out);
            for (var i = 0; i < length; i++) {
                out[i] = this.readSingleValue();
                this.log.debug("Read array element ", i, out[i]);
            }
            return out;
        };
        /** Reads a byte array, but was called read_binary in C, so for grepping purposes I kept the name. */
        Decoder.prototype.read_binary = function () {
            this.log.debug("read_binary");
            var /*int*/ length = this.read_varint();
            var /*byte[]*/ out = new Uint8Array(length); //byte[length];
            for (var i = 0; i < length; i++) {
                out[i] = this.reader.readByte();
            }
            var buf = out.buffer;
            if (this.prefer_latin1) {
                var res2 = new Sereal.Latin1String(buf).toString();
                this.log.debug("returning Latin1String", JSON.stringify(res2));
                return res2;
            }
            return buf;
        };
        Decoder.prototype.read_hash = function (tag, trackPos) {
            var num_keys = 0;
            if (tag == 0) {
                num_keys = this.read_varint();
            }
            else {
                num_keys = tag & 15; //parseInt(tag.to8BitString().substr(4), 2);
            }
            var hash = {}; //new HashMap<String, Object>( num_keys );
            if (trackPos != null) {
                this.track(trackPos, hash);
            }
            this.log.debug("Reading ", num_keys, " hash elements");
            for (var i = 0; i < num_keys; i++) {
                var keyObject = this.readSingleValue();
                var key;
                if (typeof (keyObject) == "string") {
                    key = keyObject;
                }
                else if (keyObject instanceof ArrayBuffer) {
                    key = new Sereal.Latin1String(keyObject).toString();
                }
                else {
                    key = keyObject.toString();
                    console.warn("A key is expected to be a byte or character sequence, but got ", keyObject);
                }
                var val = this.readSingleValue();
                hash[key] = val;
            }
            return hash;
        };
        Decoder.prototype.read_varint = function () {
            var x = this.reader.readVarInt();
            return x;
        };
        Decoder.prototype.readSingleValue = function () {
            this.checkNoEOD();
            var byte = this.reader.readByte();
            var tag = byte;
            var trackPos = null;
            if ((byte & Sereal.Consts.TRACK_FLAG) != 0) {
                tag = byte & ~Sereal.Consts.TRACK_FLAG;
                trackPos = this.reader.pos; // - 1;
                this.log.debug("Tracking stuff at position: ", trackPos);
            }
            this.log.debug("reading", { tag: tag, tagName: Sereal.Tags[tag], absPos: this.reader.absPos, trackPos: trackPos });
            //this.log.debug("Tag: " + (tag & 0xFF));// + " = " + tag.toHex());
            var out;
            if (tag <= Sereal.Consts.POS_HIGH) {
                this.log.debug("Read small positive int:", tag);
                out = tag;
            }
            else if (tag <= Sereal.Consts.NEG_HIGH) {
                this.log.debug("Read small negative int:", (tag - 32));
                out = tag - 32;
            }
            else if ((tag & Sereal.Consts.SHORT_BINARY_LOW) == Sereal.Consts.SHORT_BINARY_LOW) {
                var short_binary = this.read_short_binary(tag);
                out = this.prefer_latin1 ? new Sereal.Latin1String(short_binary).toString() : short_binary;
                this.log.debug("Read short binary: ", { short_binary: short_binary, length: short_binary.byteLength, value: out });
            }
            else if ((tag & Sereal.Tags.HASHREF_0) == Sereal.Tags.HASHREF_0) {
                var hash = this.read_hash(tag, trackPos);
                this.log.debug("Read hash: ", hash);
                out = hash;
            }
            else if ((tag & Sereal.Tags.ARRAYREF_0) == Sereal.Tags.ARRAYREF_0) {
                //this.log.debug("Reading arrayref");
                var arr = this.read_array(tag, trackPos);
                this.log.debug("Read arrayref: ", arr);
                out = arr;
            }
            else if (this.tagReaders[tag] != null) {
                var func = this.tagReaders[tag];
                out = func.call(this);
            }
            else {
                switch (tag) {
                    case Sereal.Tags.HASH:
                        var hash = this.read_hash(0, trackPos);
                        this.log.debug("Read hash: ", hash);
                        out = hash;
                        break;
                    case Sereal.Tags.ARRAY:
                        //this.log.debug("Reading array");
                        var arr = this.read_array(0, trackPos);
                        this.log.debug("Read array: ", arr);
                        out = arr;
                        break;
                    case Sereal.Tags.PAD:
                        this.log.debug("Padding byte: skip");
                        return this.preservePadding ? new Sereal.Padded(this.readSingleValue()) : this.readSingleValue();
                    default:
                        throw new Sereal.SerealException("Tag not supported: " + tag);
                }
            }
            if (trackPos != null) {
                this.track(trackPos, out);
            }
            this.log.debug("read", { tag: Sereal.Tags[tag], value: out, pos: this.reader.pos, trackPos: trackPos, });
            return out;
        };
        Decoder.prototype.assertEqual = function (x, y) {
            if (x == y)
                return true;
            console.error("not equal", x, y);
            return false;
        };
        /** Read a short binary ISO-8859-1 (latin1) string, the lower bits of the tag hold the length */
        Decoder.prototype.read_short_binary = function (tag) {
            var length = tag & Sereal.Consts.MASK_SHORT_BINARY_LEN;
            var length2 = parseInt(tag.to8BitString().substr(3), 2);
            this.assertEqual(length, length2);
            this.log.debug("Short binary, length: " + length);
            var buf = new ArrayBuffer(length);
            this.reader.readBytesTo(buf);
            return buf;
        };
        Decoder.prototype.read_UTF8 = function () {
            throw new Error("notimplemented");
            //TODO:
            //var /*int*/ length = this.read_varint();
            //var /*byte[]*/ buf = new ArrayBuffer(length);
            //this.data.get(buf);
            //return Charset.forName("UTF-8").decode(ByteBuffer.wrap(buf)).toString();
        };
        Decoder.prototype.read_zigzag = function () {
            var n = this.read_varint();
            return (n >>> 1) ^ (-(n & 1)); // note the unsigned right shift
        };
        Decoder.prototype.read_regex = function () {
            var flags = "";
            var str = this.readSingleValue();
            var regex;
            if (typeof (str) == "string") {
                regex = str.toString();
            }
            else if (str instanceof ArrayBuffer) {
                regex = new Sereal.Latin1String(str).toString();
            }
            else {
                throw new Sereal.SerealException("Regex has to be built from a char or byte sequence");
            }
            this.log.debug("Read pattern: " + regex);
            // now read modifiers
            var /*byte*/ tag = this.reader.readByte();
            if ((tag & Sereal.Consts.SHORT_BINARY_LOW) == Sereal.Consts.SHORT_BINARY_LOW) {
                var length = tag & Sereal.Consts.MASK_SHORT_BINARY_LEN;
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
                            flags += "x"; //flags | Pattern.COMMENTS;
                            break;
                        case 'p':
                            // ignored
                            break;
                        default:
                            throw new Sereal.SerealException("Unknown regex modifier: " + value);
                    }
                }
            }
            else {
                throw new Sereal.SerealException("Expecting SHORT_BINARY for modifiers of regexp, got: " + tag);
            }
            return new RegExp(regex, flags);
        };
        Decoder.prototype.read_object = function () {
            // first read the classname
            // Maybe we should have some kind of read_string() method?
            var position = this.reader.pos;
            var tag = this.reader.readByte();
            var className;
            if ((tag & Sereal.Consts.SHORT_BINARY_LOW) == Sereal.Consts.SHORT_BINARY_LOW) {
                var length = tag & Sereal.Consts.MASK_SHORT_BINARY_LEN;
                var /*byte[]*/ buf = new ArrayBuffer(length);
                this.reader.readBytesTo(buf);
                className = new Sereal.Latin1String(/*new String(*/ buf /*)*/).toString();
            }
            else {
                throw new Sereal.SerealException("Don't know how to read classname from tag" + tag);
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
                    var c = Sereal.Class.forName(className.getString());
                    return Sereal.Utils.bless(c, classData);
                }
                catch (e) {
                    // or we make a new one
                    if (this.objectType == Decoder.POJO) {
                        return Sereal.Utils.bless(className.getString(), classData);
                    }
                    else {
                        // or we make a Perl-style one
                        return new Sereal.PerlObject(className.getString(), classData);
                    }
                }
            }
            else if (structure.getClass().isArray()) {
                // nothing we can really do here except make Perl objects..
                return new Sereal.PerlObject(className.getString(), structure);
            }
            else if (structure instanceof Sereal.PerlReference) {
                return new Sereal.PerlObject(className.getString(), structure);
            }
            // it's a regexp for example
            return structure;
        };
        Decoder.prototype.get_tracked_item = function () {
            var offset = this.read_varint();
            this.log.debug("Creating ref to item previously read at offset: ", offset, this.tracked["track_" + offset]);
            this.log.debug("keys: ", Object.keys(this.tracked), " vals: ", Object.values(this.tracked));
            return this.getTracked(offset);
        };
        Object.defineProperty(Decoder.prototype, "data", {
            /** Set the data to deserealize(for calling decode multiple times when there are concatenated packets)(never tested)     */
            set: function (data) {
                this._data = data;
                this.reader = new Sereal.DataReader(data);
            },
            enumerable: true,
            configurable: true
        });
        Decoder.prototype.track = function (pos, val) {
            this.log.debug("track_stuff", "Saving ", val, " at offset ", pos);
            var ref = val; // autoboxing ftw
            this.tracked["track_" + pos] = ref;
        };
        Decoder.prototype.getTracked = function (pos) {
            var key = "track_" + pos;
            if (!this.tracked.hasOwnProperty(key))
                throw new Error("tracked object not found");
            var val = this.tracked["track_" + pos];
            return val;
        };
        Decoder.prototype.reset = function () {
            this.reader = null;
            this.tracked = {}; //.clear();
        };
        Decoder.prototype.checkNoEOD = function () {
            if (this.reader.remaining() > 0)
                return;
            throw new Sereal.SerealException("Unexpected end of data at byte " + this.reader.absPos);
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
            var originalPosition = this.read_varint();
            var currentPosition = this.reader.pos; // remember where we parked
            // note: you might think you'd like to use mark() and reset(), but setting position(..) discards the mark
            this.reader.pos = originalPosition;
            var copy = this.readSingleValue();
            this.reader.pos = currentPosition; // go back to where we were
            return copy;
        };
        Decoder.PERL_OBJECT = "PERL_OBJECT"; // Perl style object (name + hash)
        Decoder.POJO = "POJO"; // Dynamically compile a Plain Old Java Object
        return Decoder;
    })();
    Sereal.Decoder = Decoder;
})(Sereal || (Sereal = {}));
"use strict";
Number.prototype.toHex = function () { return this.toString(16); };
//String.prototype.toHex = function () { return this.toString(16); }
