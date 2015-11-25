var __extends = (this && this.__extends) || function (d, b) {
    for (var p in b) if (b.hasOwnProperty(p)) d[p] = b[p];
    function __() { this.constructor = d; }
    d.prototype = b === null ? Object.create(b) : (__.prototype = b.prototype, new __());
};
var Log = (function () {
    function Log() {
    }
    Log.prototype.fine = function (arg) {
        console.info(arg);
    };
    return Log;
})();
var PerlReference = (function () {
    function PerlReference(_value) {
        this._value = _value;
    }
    PerlReference.prototype.getValue = function () {
        return this._value;
    };
    return PerlReference;
})();
var Latin1String = (function () {
    function Latin1String(_bytes) {
        this._bytes = _bytes;
        this._string = ab2str2(_bytes);
    }
    Latin1String.prototype.valueOf = function () {
        return this._string;
    };
    Latin1String.prototype.toString = function () {
        return this._string;
    };
    return Latin1String;
})();
var Utils = (function () {
    function Utils() {
    }
    Utils.dump = function (obj) {
        console.debug(obj);
    };
    Utils.bless = function (ctor, obj) {
        throw new Error();
    };
    return Utils;
})();
var PerlObject = (function () {
    function PerlObject(name, obj) {
        this.name = name;
        this.obj = obj;
    }
    return PerlObject;
})();
var Alias = (function () {
    function Alias(obj) {
        this.obj = obj;
    }
    return Alias;
})();
var SerealException = (function (_super) {
    __extends(SerealException, _super);
    function SerealException() {
        _super.apply(this, arguments);
    }
    return SerealException;
})(Error);
var Class = (function () {
    function Class() {
    }
    Class.forName = function (name) {
        throw new Error();
    };
    return Class;
})();
var ByteOrder;
(function (ByteOrder) {
    ByteOrder[ByteOrder["LITTLE_ENDIAN"] = 1] = "LITTLE_ENDIAN";
    ByteOrder[ByteOrder["BIG_ENDIAN"] = 2] = "BIG_ENDIAN";
})(ByteOrder || (ByteOrder = {}));
var WeakReference = (function () {
    function WeakReference(obj) {
        this.obj = obj;
    }
    return WeakReference;
})();
var Padded = (function () {
    function Padded(value) {
        this.value = value;
    }
    return Padded;
})();
Number.prototype.toHex = function () { return this.toString(16); };
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
function ab2str(buf) {
    return String.fromCharCode.apply(null, new Uint16Array(buf));
}
function str2ab(str) {
    var buf = new ArrayBuffer(str.length * 2); // 2 bytes for each char
    var bufView = new Uint16Array(buf);
    for (var i = 0, strLen = str.length; i < strLen; i++) {
        bufView[i] = str.charCodeAt(i);
    }
    return buf;
}
function str2ab2(str) {
    var buf = new ArrayBuffer(str.length);
    var bufView = new Uint8Array(buf);
    for (var i = 0, strLen = str.length; i < strLen; i++) {
        var byte = str.charCodeAt(i);
        bufView[i] = byte;
    }
    return buf;
}
function ab2str2(buf) {
    return String.fromCharCode.apply(null, new Uint8Array(buf));
}
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
//    Tag | Char | Dec |  Hex |     Binary | Follow
//  ------------------+------+-----+------+----------- |-----------------------------------------
//var X = [
//    "POS_0             ",      ,   0 , 0x00 , 0b00000000 ," small positive integer - value in low 4 bits (identity)
//    "POS_1             ",      ,   1 , 0x01 , 0b00000001 ,"
//    "POS_2             ",      ,   2 , 0x02 , 0b00000010 ,"
//    "POS_3             ",      ,   3 , 0x03 , 0b00000011 ,"
//    "POS_4             ",      ,   4 , 0x04 , 0b00000100 ,"
//    "POS_5             ",      ,   5 , 0x05 , 0b00000101 ,"
//    "POS_6             ",      ,   6 , 0x06 , 0b00000110 ,"
//    "POS_7             ", "\a" ,   7 , 0x07 , 0b00000111 ,"
//    "POS_8             ", "\b" ,   8 , 0x08 , 0b00001000 ,"
//    "POS_9             ", "\t" ,   9 , 0x09 , 0b00001001 ,"
//    "POS_10            ", "\n" ,  10 , 0x0a , 0b00001010 ,"
//    "POS_11            ",      ,  11 , 0x0b , 0b00001011 ,"
//    "POS_12            ", "\f" ,  12 , 0x0c , 0b00001100 ,"
//    "POS_13            ", "\r" ,  13 , 0x0d , 0b00001101 ,"
//    "POS_14            ",      ,  14 , 0x0e , 0b00001110 ,"
//    "POS_15            ",      ,  15 , 0x0f , 0b00001111 ," small positive integer - value in low 4 bits (identity)
//    "NEG_16            ",      ,  16 , 0x10 , 0b00010000 ," small negative integer - value in low 4 bits (k+32)
//    "NEG_15            ",      ,  17 , 0x11 , 0b00010001 ,"
//    "NEG_14            ",      ,  18 , 0x12 , 0b00010010 ,"
//    "NEG_13            ",      ,  19 , 0x13 , 0b00010011 ,"
//    "NEG_12            ",      ,  20 , 0x14 , 0b00010100 ,"
//    "NEG_11            ",      ,  21 , 0x15 , 0b00010101 ,"
//    "NEG_10            ",      ,  22 , 0x16 , 0b00010110 ,"
//    "NEG_9             ",      ,  23 , 0x17 , 0b00010111 ,"
//    "NEG_8             ",      ,  24 , 0x18 , 0b00011000 ,"
//    "NEG_7             ",      ,  25 , 0x19 , 0b00011001 ,"
//    "NEG_6             ",      ,  26 , 0x1a , 0b00011010 ,"
//    "NEG_5             ", "\e" ,  27 , 0x1b , 0b00011011 ,"
//    "NEG_4             ",      ,  28 , 0x1c , 0b00011100 ,"
//    "NEG_3             ",      ,  29 , 0x1d , 0b00011101 ,"
//    "NEG_2             ",      ,  30 , 0x1e , 0b00011110 ,"
//    "NEG_1             ",      ,  31 , 0x1f , 0b00011111 ," small negative integer - value in low 4 bits (k+32)
//    "VARINT            ", " "  ,  32 , 0x20 , 0b00100000 ," <VARINT> - Varint variable length integer
//    "ZIGZAG            ", "!"  ,  33 , 0x21 , 0b00100001 ," <ZIGZAG-VARINT> - Zigzag variable length integer
//    "FLOAT             ", "\"" ,  34 , 0x22 , 0b00100010 ," <IEEE-FLOAT>
//    "DOUBLE            ", "#"  ,  35 , 0x23 , 0b00100011 ," <IEEE-DOUBLE>
//    "LONG_DOUBLE       ", "\$" ,  36 , 0x24 , 0b00100100 ," <IEEE-LONG-DOUBLE>
//    "UNDEF             ", "%"  ,  37 , 0x25 , 0b00100101 ," None - Perl undef var; eg my $var= undef;
//    "BINARY            ", "&"  ,  38 , 0x26 , 0b00100110 ," <LEN-VARINT> <BYTES> - binary/(latin1) string
//    "STR_UTF8          ", "'"  ,  39 , 0x27 , 0b00100111 ," <LEN-VARINT> <UTF8> - utf8 string
//    "REFN              ", "("  ,  40 , 0x28 , 0b00101000 ," <ITEM-TAG>    - ref to next item
//    "REFP              ", ")"  ,  41 , 0x29 , 0b00101001 ," <OFFSET-VARINT> - ref to previous item stored at offset
//    "HASH              ", "*"  ,  42 , 0x2a , 0b00101010 ," <COUNT-VARINT> [<KEY-TAG> <ITEM-TAG> ...] - count followed by key/value pairs
//    "ARRAY             ", "+"  ,  43 , 0x2b , 0b00101011 ," <COUNT-VARINT> [<ITEM-TAG> ...] - count followed by items
//    "OBJECT            ", ","  ,  44 , 0x2c , 0b00101100 ," <STR-TAG> <ITEM-TAG> - class, object-item
//    "OBJECTV           ", "-"  ,  45 , 0x2d , 0b00101101 ," <OFFSET-VARINT> <ITEM-TAG> - offset of previously used classname tag - object-item
//    "ALIAS             ", "."  ,  46 , 0x2e , 0b00101110 ," <OFFSET-VARINT> - alias to item defined at offset
//    "COPY              ", "/"  ,  47 , 0x2f , 0b00101111 ," <OFFSET-VARINT> - copy of item defined at offset
//    "WEAKEN            ", "0"  ,  48 , 0x30 , 0b00110000 ," <REF-TAG> - Weaken the following reference
//    "REGEXP            ", "1"  ,  49 , 0x31 , 0b00110001 ," <PATTERN-STR-TAG> <MODIFIERS-STR-TAG>
//    "OBJECT_FREEZE     ", "2"  ,  50 , 0x32 , 0b00110010 ," <STR-TAG> <ITEM-TAG> - class, object-item. Need to call "THAW" method on class after decoding
//    "OBJECTV_FREEZE    ", "3"  ,  51 , 0x33 , 0b00110011 ," <OFFSET-VARINT> <ITEM-TAG> - (OBJECTV_FREEZE is to OBJECT_FREEZE as OBJECTV is to OBJECT)
//    "RESERVED_0        ", "4"  ,  52 , 0x34 , 0b00110100 ," reserved
//    "RESERVED_1        ", "5"  ,  53 , 0x35 , 0b00110101 ,"
//    "RESERVED_2        ", "6"  ,  54 , 0x36 , 0b00110110 ,"
//    "RESERVED_3        ", "7"  ,  55 , 0x37 , 0b00110111 ,"
//    "RESERVED_4        ", "8"  ,  56 , 0x38 , 0b00111000 ," reserved
//    "CANONICAL_UNDEF   ", "9"  ,  57 , 0x39 , 0b00111001 ," undef (PL_sv_undef) - "the" Perl undef (see notes)
//    "FALSE             ", ":"  ,  58 , 0x3a , 0b00111010 ," false (PL_sv_no)
//    "TRUE              ", ";"  ,  59 , 0x3b , 0b00111011 ," true  (PL_sv_yes)
//    "MANY              ", "<"  ,  60 , 0x3c , 0b00111100 ," <LEN-VARINT> <TYPE-BYTE> <TAG-DATA> - repeated tag (not done yet, will be implemented in version 3)
//    "PACKET_START      ", "="  ,  61 , 0x3d , 0b00111101 ," (first byte of magic string in header)
//    "EXTEND            ", ">"  ,  62 , 0x3e , 0b00111110 ," <BYTE> - for additional tags
//    "PAD               ", "?"  ,  63 , 0x3f , 0b00111111 ," (ignored tag, skip to next byte)
//    "ARRAYREF_0        ", "\@" ,  64 , 0x40 , 0b01000000 ," [<ITEM-TAG> ...] - count of items in low 4 bits (ARRAY must be refcnt=1)
//    "ARRAYREF_1        ", "A"  ,  65 , 0x41 , 0b01000001 ,"
//    "ARRAYREF_2        ", "B"  ,  66 , 0x42 , 0b01000010 ,"
//    "ARRAYREF_3        ", "C"  ,  67 , 0x43 , 0b01000011 ,"
//    "ARRAYREF_4        ", "D"  ,  68 , 0x44 , 0b01000100 ,"
//    "ARRAYREF_5        ", "E"  ,  69 , 0x45 , 0b01000101 ,"
//    "ARRAYREF_6        ", "F"  ,  70 , 0x46 , 0b01000110 ,"
//    "ARRAYREF_7        ", "G"  ,  71 , 0x47 , 0b01000111 ,"
//    "ARRAYREF_8        ", "H"  ,  72 , 0x48 , 0b01001000 ,"
//    "ARRAYREF_9        ", "I"  ,  73 , 0x49 , 0b01001001 ,"
//    "ARRAYREF_10       ", "J"  ,  74 , 0x4a , 0b01001010 ,"
//    "ARRAYREF_11       ", "K"  ,  75 , 0x4b , 0b01001011 ,"
//    "ARRAYREF_12       ", "L"  ,  76 , 0x4c , 0b01001100 ,"
//    "ARRAYREF_13       ", "M"  ,  77 , 0x4d , 0b01001101 ,"
//    "ARRAYREF_14       ", "N"  ,  78 , 0x4e , 0b01001110 ,"
//    "ARRAYREF_15       ", "O"  ,  79 , 0x4f , 0b01001111 ," [<ITEM-TAG> ...] - count of items in low 4 bits (ARRAY must be refcnt=1)
//    "HASHREF_0         ", "P"  ,  80 , 0x50 , 0b01010000 ," [<KEY-TAG> <ITEM-TAG> ...] - count in low 4 bits, key/value pairs (HASH must be refcnt=1)
//    "HASHREF_1         ", "Q"  ,  81 , 0x51 , 0b01010001 ,"
//    "HASHREF_2         ", "R"  ,  82 , 0x52 , 0b01010010 ,"
//    "HASHREF_3         ", "S"  ,  83 , 0x53 , 0b01010011 ,"
//    "HASHREF_4         ", "T"  ,  84 , 0x54 , 0b01010100 ,"
//    "HASHREF_5         ", "U"  ,  85 , 0x55 , 0b01010101 ,"
//    "HASHREF_6         ", "V"  ,  86 , 0x56 , 0b01010110 ,"
//    "HASHREF_7         ", "W"  ,  87 , 0x57 , 0b01010111 ,"
//    "HASHREF_8         ", "X"  ,  88 , 0x58 , 0b01011000 ,"
//    "HASHREF_9         ", "Y"  ,  89 , 0x59 , 0b01011001 ,"
//    "HASHREF_10        ", "Z"  ,  90 , 0x5a , 0b01011010 ,"
//    "HASHREF_11        ", "["  ,  91 , 0x5b , 0b01011011 ,"
//    "HASHREF_12        ", "\\" ,  92 , 0x5c , 0b01011100 ,"
//    "HASHREF_13        ", "]"  ,  93 , 0x5d , 0b01011101 ,"
//    "HASHREF_14        ", "^"  ,  94 , 0x5e , 0b01011110 ,"
//    "HASHREF_15        ", "_"  ,  95 , 0x5f , 0b01011111 ," [<KEY-TAG> <ITEM-TAG> ...] - count in low 4 bits, key/value pairs (HASH must be refcnt=1)
//    "SHORT_BINARY_0    ", "`"  ,  96 , 0x60 , 0b01100000 ," <BYTES> - binary/latin1 string, length encoded in low 5 bits of tag
//    "SHORT_BINARY_1    ", "a"  ,  97 , 0x61 , 0b01100001 ,"
//    "SHORT_BINARY_2    ", "b"  ,  98 , 0x62 , 0b01100010 ,"
//    "SHORT_BINARY_3    ", "c"  ,  99 , 0x63 , 0b01100011 ,"
//    "SHORT_BINARY_4    ", "d"  , 100 , 0x64 , 0b01100100 ,"
//    "SHORT_BINARY_5    ", "e"  , 101 , 0x65 , 0b01100101 ,"
//    "SHORT_BINARY_6    ", "f"  , 102 , 0x66 , 0b01100110 ,"
//    "SHORT_BINARY_7    ", "g"  , 103 , 0x67 , 0b01100111 ,"
//    "SHORT_BINARY_8    ", "h"  , 104 , 0x68 , 0b01101000 ,"
//    "SHORT_BINARY_9    ", "i"  , 105 , 0x69 , 0b01101001 ,"
//    "SHORT_BINARY_10   ", "j"  , 106 , 0x6a , 0b01101010 ,"
//    "SHORT_BINARY_11   ", "k"  , 107 , 0x6b , 0b01101011 ,"
//    "SHORT_BINARY_12   ", "l"  , 108 , 0x6c , 0b01101100 ,"
//    "SHORT_BINARY_13   ", "m"  , 109 , 0x6d , 0b01101101 ,"
//    "SHORT_BINARY_14   ", "n"  , 110 , 0x6e , 0b01101110 ,"
//    "SHORT_BINARY_15   ", "o"  , 111 , 0x6f , 0b01101111 ,"
//    "SHORT_BINARY_16   ", "p"  , 112 , 0x70 , 0b01110000 ,"
//    "SHORT_BINARY_17   ", "q"  , 113 , 0x71 , 0b01110001 ,"
//    "SHORT_BINARY_18   ", "r"  , 114 , 0x72 , 0b01110010 ,"
//    "SHORT_BINARY_19   ", "s"  , 115 , 0x73 , 0b01110011 ,"
//    "SHORT_BINARY_20   ", "t"  , 116 , 0x74 , 0b01110100 ,"
//    "SHORT_BINARY_21   ", "u"  , 117 , 0x75 , 0b01110101 ,"
//    "SHORT_BINARY_22   ", "v"  , 118 , 0x76 , 0b01110110 ,"
//    "SHORT_BINARY_23   ", "w"  , 119 , 0x77 , 0b01110111 ,"
//    "SHORT_BINARY_24   ", "x"  , 120 , 0x78 , 0b01111000 ,"
//    "SHORT_BINARY_25   ", "y"  , 121 , 0x79 , 0b01111001 ,"
//    "SHORT_BINARY_26   ", "z"  , 122 , 0x7a , 0b01111010 ,"
//    "SHORT_BINARY_27   ", "{"  , 123 , 0x7b , 0b01111011 ,"
//    "SHORT_BINARY_28   ", "|"  , 124 , 0x7c , 0b01111100 ,"
//    "SHORT_BINARY_29   ", "}"  , 125 , 0x7d , 0b01111101 ,"
//    "SHORT_BINARY_30   ", "~"  , 126 , 0x7e , 0b01111110 ,"
//    "SHORT_BINARY_31   ",      , 127 , 0x7f , 0b01111111 ," <BYTES> - binary/latin1 string, length encoded in low 5 bits of tag
//];
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
var Consts;
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
})(Consts || (Consts = {}));
var Tags;
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
})(Tags || (Tags = {}));
var DataReader = (function () {
    function DataReader(data) {
        this.view = this.toDataView(data);
        this.pos = 0;
        console.log("DataReader created, data=" + this.toHex());
    }
    DataReader.prototype.toDataView = function (data) {
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
    DataReader.prototype.toHex = function () {
        var list = [];
        for (var i = 0; i < this.view.byteLength; i++) {
            list.push(this.view.getUint8(i).toHex().padLeft(2, "0"));
        }
        return list.join(" ");
    };
    ////** returns a zero based index position where view.byteOffset normalizes to zero
    //getRelativePosition() {
    //    return this.pos - this.view.byteOffset;
    //}
    DataReader.prototype.toString = function () { return "DataReader pos=" + this.pos; };
    DataReader.prototype.getDouble = function () { throw new Error(); };
    DataReader.prototype.rewind = function () { this.pos = 0; };
    DataReader.prototype.hasRemaining = function () { return this.remaining() > 0; };
    DataReader.prototype.remaining = function () { return this.view.byteLength - this.pos; };
    DataReader.prototype.limit = function () { return this.view.byteLength - this.pos; };
    DataReader.prototype.order = function (type) { throw new Error(); };
    DataReader.prototype.getInt32 = function () { return this.getInt(); };
    //asInt8Array(): Int8Array { return new Int8Array(this.view.buffer, this.view.byteOffset, this.view.byteLength); }
    DataReader.prototype.asUint8Array = function () { return new Uint8Array(this.view.buffer, this.view.byteOffset, this.view.byteLength); };
    DataReader.prototype.getInt = function () {
        var value = this.view.getInt32(this.pos);
        this.pos += 4;
        return value;
    };
    DataReader.prototype.getBytesTo = function (buf) {
        var arr = new Uint8Array(buf);
        for (var i = 0; i < arr.length; i++)
            arr[i] = this.getByte();
        return buf;
    };
    DataReader.prototype.getByte = function () {
        var value = this.view.getUint8(this.pos);
        this.pos++;
        return value;
    };
    DataReader.prototype.getVarInt = function () {
        var out = { bytesRead: null };
        var value = varint.read(this.asUint8Array(), this.pos, out);
        if (out.bytesRead == null)
            throw new Error();
        this.pos += out.bytesRead;
        return value;
    };
    DataReader.prototype.getBytes = function (length) {
        if (length == null)
            length = this.view.byteLength - this.pos;
        var arr = new Uint8Array(this.view.buffer, this.view.byteOffset + this.pos, length);
        this.pos += length;
        return arr;
    };
    DataReader.prototype.getBytes2 = function (length) {
        var arr = this.getBytes(length);
        var reader = new DataReader(arr);
        return reader;
    };
    return DataReader;
})();
//class DataReader2 {
//    constructor(public _buffer: ArrayBuffer) {
//        this.view = new DataView(_buffer);
//        this.pos = this.view.byteOffset;
//    }
//    view: DataView;
//    pos: number;
//    toString():string { return "DataReader2 pos=" + this.pos; }
//    rewind(): void { this.pos = this.view.byteOffset; }
//    limit():number { return this.view.byteLength - this.pos; }
//    getInt32(): number { return this.getInt(); }
//    getInt8(): number { return this.getByte(); }
//    asInt8Array(): Int8Array { return new Int8Array(this._buffer); }
//    getInt() {
//        var value = this.view.getInt32(this.pos);
//        this.pos += 4;
//        return value;
//    }
//    getByte() {
//        var value = this.view.getInt8(this.pos);
//        this.pos++;
//        return value;
//    }
//    getVarInt() {
//        var out = { bytesRead: null };
//        var value = varint.read(this.asInt8Array(), this.pos, out);
//        this.pos += out.bytesRead || 1;
//        return value;
//    }
//    getBytes(length) {
//        if (length == null)
//            length = this._buffer.byteLength - this.pos;
//        var arr = new Int8Array(length);
//        for (var i = 0; i < length; i++)
//            arr[i] = this.getByte();
//        return arr.buffer;
//    }
//}
/** Decoder for Sereal */
var Decoder = (function () {
    function Decoder(options) {
        if (options === void 0) { options = null; }
        this.log = new Log();
        this.properties = {}; // where we save protocol, version and encoding
        this.tracked = {}; //new HashMap<String, Object>(); // where we track items for REFP purposes
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
        this.tagReaders[Tags.VARINT] = this.read_varint;
        this.tagReaders[Tags.ZIGZAG] = this.read_zigzag;
        this.tagReaders[Tags.DOUBLE] = this.read_double;
        this.tagReaders[Tags.DOUBLE] = this.read_double;
        this.tagReaders[Tags.TRUE] = function () { return true; };
        this.tagReaders[Tags.FALSE] = function () { return false; };
        this.tagReaders[Tags.UNDEF] = function () { return null; };
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
    Decoder.prototype.read_weaken = function () {
        this.log.fine("Weakening the next thing");
        // so the next thing HAS to be a ref (afaict) which means we can track it
        var placeHolder = new PerlReference(this.readSingleValue().getValue());
        var /*WeakReference<PerlReference>*/ wref = new WeakReference(placeHolder);
        return wref;
    };
    Decoder.prototype.read_alias = function () {
        this.log.fine("Reading an alias");
        var alias = new Alias(this.get_tracked_item());
        this.log.fine("Read alias: " + Utils.dump(alias));
        return alias;
    };
    Decoder.prototype.read_object_v = function () {
        this.log.fine("Reading an objectv");
        var className = this.get_tracked_item();
        this.log.fine("Read an objectv of class: " + className);
        var out = new PerlObject(className, this.readSingleValue());
        return out;
    };
    Decoder.prototype.read_refp = function () {
        this.log.fine("Reading REFP (ref to prev)");
        var offset_prev = this.read_varint();
        if (!this.tracked.hasOwnProperty("track_" + offset_prev)) {
            throw new SerealException("REFP to offset " + offset_prev + ", which is not tracked");
        }
        var prv_value = this.tracked.hasOwnProperty("track_" + offset_prev);
        var prev = this.perlRefs ? new PerlReference(prv_value) : prv_value;
        this.log.fine("Read prev: " + Utils.dump(prev));
        return prev;
    };
    Decoder.prototype.read_refn = function () {
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
    };
    Decoder.prototype.read_double = function () {
        // Java defaults to BE, maybe we can jsut do this generally, don't know yet (but think so)
        this.data.order(ByteOrder.LITTLE_ENDIAN);
        var d = this.data.getDouble();
        this.data.order(ByteOrder.BIG_ENDIAN);
        this.log.fine("Read double: " + d);
        return d;
    };
    /**
     * if tag == 0, next is varint for number of elements, otherwise lower 4 bits are length
     *
     * @param tag lower 4 bits is length or 0 for next varint is length
     * @param track we might need to track since array elements could refer to us
     */
    Decoder.prototype.read_array = function (tag, track) {
        var length = 0;
        if (tag == 0) {
            length = this.read_varint();
        }
        else {
            length = tag & 15;
        }
        this.log.fine("Array length: " + length);
        var out = new Array(length);
        if (track != 0)
            this.track_stuff(track, out);
        for (var i = 0; i < length; i++) {
            out[i] = this.readSingleValue();
            this.log.fine("Read array element " + i + ": " + Utils.dump(out[i]));
        }
        return out;
    };
    /** Reads a byte array, but was called read_binary in C, so for grepping purposes I kept the name. */
    Decoder.prototype.read_binary = function () {
        var /*int*/ length = this.read_varint();
        var /*byte[]*/ out = new Uint8Array(length); //byte[length];
        for (var i = 0; i < length; i++) {
            out[i] = this.data.getByte();
        }
        var buf = out.buffer;
        if (this.prefer_latin1) {
            var res2 = new Latin1String(buf).toString();
            return res2;
        }
        return buf;
    };
    Decoder.prototype.read_hash = function (tag, track) {
        var num_keys = 0;
        if (tag == 0) {
            num_keys = this.read_varint();
        }
        else {
            num_keys = tag & 15;
        }
        var hash = {}; //new HashMap<String, Object>( num_keys );
        if (track != 0) {
            this.track_stuff(track, hash);
        }
        this.log.fine("Reading " + num_keys + " hash elements");
        for (var i = 0; i < num_keys; i++) {
            var keyObject = this.readSingleValue();
            var key;
            if (typeof (keyObject) == "string") {
                key = keyObject;
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
    };
    /** top bit set (0x80) means next byte is 7 bits more more varint */
    Decoder.prototype.read_varint = function () {
        var uv = 0;
        var lshift = 0;
        var b = this.data.getByte();
        while (this.data.hasRemaining() && (b < 0)) {
            uv |= (b & 127) << lshift; // add 7 bits
            lshift += 7;
            b = this.data.getByte();
        }
        uv |= b << lshift; // add final (or first if there is only 1)
        return uv;
    };
    Decoder.prototype.readSingleValue = function () {
        this.checkNoEOD();
        var tag = this.data.getByte();
        var track = 0;
        if ((tag & Consts.TRACK_FLAG) != 0) {
            tag = tag & ~Consts.TRACK_FLAG;
            track = this.data.pos - 1;
            this.log.fine("Tracking stuff at position: " + track);
        }
        this.log.fine("Tag: " + (tag & 0xFF)); // + " = " + tag.toHex());
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
            var short_binary = this.read_short_binary(tag);
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
            var arr = this.read_array(tag, track);
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
        if (track != 0) {
            this.track_stuff(track, out);
        }
        this.log.fine("returning: " + out);
        return out;
    };
    /** Read a short binary ISO-8859-1 (latin1) string, the lower bits of the tag hold the length */
    Decoder.prototype.read_short_binary = function (tag) {
        var length = tag & Consts.MASK_SHORT_BINARY_LEN;
        this.log.fine("Short binary, length: " + length);
        var buf = new ArrayBuffer(length);
        this.data.getBytesTo(buf);
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
            regex = new Latin1String(str).toString();
        }
        else {
            throw new SerealException("Regex has to be built from a char or byte sequence");
        }
        this.log.fine("Read pattern: " + regex);
        // now read modifiers
        var /*byte*/ tag = this.data.getByte();
        if ((tag & Consts.SHORT_BINARY_LOW) == Consts.SHORT_BINARY_LOW) {
            var length = tag & Consts.MASK_SHORT_BINARY_LEN;
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
                        flags += "x"; //flags | Pattern.COMMENTS;
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
    };
    Decoder.prototype.read_object = function () {
        // first read the classname
        // Maybe we should have some kind of read_string() method?
        var position = this.data.pos;
        var tag = this.data.getByte();
        var className;
        if ((tag & Consts.SHORT_BINARY_LOW) == Consts.SHORT_BINARY_LOW) {
            var length = tag & Consts.MASK_SHORT_BINARY_LEN;
            var /*byte[]*/ buf = new ArrayBuffer(length);
            this.data.getBytesTo(buf);
            className = new Latin1String(/*new String(*/ buf /*)*/).toString();
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
            }
            catch (e) {
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
    };
    Decoder.prototype.get_tracked_item = function () {
        var offset = this.read_varint();
        this.log.fine("Creating ref to item previously read at offset: " + offset + " which is: " + this.tracked["track_" + offset]);
        this.log.fine("keys: " + Object.keys(this.tracked) + " vals: " + Object.values(this.tracked));
        return this.tracked["track_" + offset];
    };
    /** Set the data to deserealize(for calling decode multiple times when there are concatenated packets)(never tested)     */
    Decoder.prototype.setData = function (data) {
        this.data = new DataReader(data);
        this.data.rewind();
    };
    Decoder.prototype.track_stuff = function (pos, thing) {
        this.log.fine("Saving " + thing + " at offset " + pos);
        var ref = thing; // autoboxing ftw
        this.tracked["track_" + pos] = ref;
    };
    Decoder.prototype.reset = function () {
        this.data = null;
        this.realData = null;
        this.tracked = {}; //.clear();
    };
    Decoder.prototype.checkNoEOD = function () {
        if (this.data.remaining() > 0)
            return;
        throw new SerealException("Unexpected end of data at byte " + this.data.limit());
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
        var currentPosition = this.data.pos; // remember where we parked
        // note: you might think you'd like to use mark() and reset(), but setting position(..) discards the mark
        this.data.pos = originalPosition;
        var copy = this.readSingleValue();
        this.data.pos = currentPosition; // go back to where we were
        return copy;
    };
    Decoder.PERL_OBJECT = "PERL_OBJECT"; // Perl style object (name + hash)
    Decoder.POJO = "POJO"; // Dynamically compile a Plain Old Java Object
    return Decoder;
})();
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
Number.prototype.toHex = function () { return this.toString(16); };
//String.prototype.toHex = function () { return this.toString(16); }
//# sourceMappingURL=sereal.js.map