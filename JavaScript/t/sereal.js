var C = getConsts2();
var TAGS = [];
getConsts().forEach(t=>TAGS[parseInt(t.Dec)] = t);

var _dv;
var _reader;
var MAGIC = 1039364716;//(0x6c) + (0x72 << 8) + (0x73 << 16) + (0x3d << 24);
function main() {
    var binaryText = localStorage.getItem("sereal");

    var res = decodeSereal(binaryText);
    console.log(res);


    var dec = new Decoder();
    dec.setData(str2ab2(binaryText));
    var x = dec.decode();
    console.log(x);
    return;




}

function decodeSereal(binaryText) {
    var _buf = str2ab2(binaryText);
    _reader = new DataReader2(_buf);
    var pos = 0;
    if (_reader.getInt32() != MAGIC)
        throw new Error();
    var s = _reader.getInt8().toString(2);
    var version = parseInt(s.substr(0, 4), 2);
    var type = parseInt(s.substr(4, 4), 2);
    console.log({ version, type });

    var headerSuffixSize = _reader.getVarInt();
    var eightBitField = _reader.getInt8();
    var hasUserMetadata = toBitString(eightBitField).last() == "1";
    if (hasUserMetadata) {
        var md = decodeDocumentBody2();
    }
    if (type == 3) {
        //zlib.Deflate(new Uint8Array(_buf, ))
    }
    //console.log(x);

    function toBitString(byte) {
        return byte.toString(2).padLeft(8, "0");
    }
    function decodeDocumentBody2() {
        var dec = new Decoder({prefer_latin1:true});
        dec.setData(_reader.getBytes(headerSuffixSize));//str2ab2(binaryText));
        var x = dec.readSingleValue();
        console.log(x);
        return x;
    }

    function decodeDocumentBody() {
        var byte = _reader.getByte();
        var bits = toBitString(byte);
        console.log(bits);
        var trackFlag = bits[0] == "1";
        var tagDec = parseInt(bits.substr(1), 2);

        var tag = TAGS[tagDec];

        //throw new Error();

    }
}



function DataReader2(_buffer) {
    var _this = this;
    Function.addTo(_this, [rewind, limit, getInt, get, getInt32, pos, getVarInt, getInt8, getByte, toString, getBytes]);
    var _view = new DataView(_buffer);
    var _pos = _view.byteOffset;

    _this._view = _view;

    function toString() {
        return "DataReader2 pos=" + _pos;
    }

    function pos() {
        return _pos;
    }

    function rewind() {
        _pos = _view.byteOffset;
    }
    function limit() {
        return _view.byteLength - _pos;
    }
    function getInt32() {
        return getInt();
    }
    function getInt() {
        var value = _view.getInt32(_pos);
        _pos += 4;
        return value;
    }
    function getByte() {
        return get();
    }
    function getInt8() {
        return get();
    }
    function get() {
        var value = _view.getInt8(_pos);
        _pos++;
        return value;
    }
    function getVarInt() {
        var value = varint.read(asInt8Array(), _pos);
        _pos += varint.bytesRead || 1;
        return value;
    }
    function asInt8Array() {
        return new Int8Array(_buffer);
    }
    function getBytes(length) {
        var arr = new Int8Array(length);
        for(var i=0;i<length;i++)
            arr[i] = getByte();
        return arr.buffer;
    }
}










function test() {
    $('#graph').makeGraph({
        sources: [
            {
                source: "Proxy",
                options: {
                    url: "http://rk101riak-01.ams4.prod.booking.com:8098/api/v1/events?primary_urls=1&epoch=1447922840&dcs=1&types=WEB"
                }
            }
        ],
    });


    var oReq = new XMLHttpRequest();
    oReq.open("GET", "/html/out2.htm", true);
    oReq.responseType = "arraybuffer";

    oReq.onload = function (oEvent) {
        var arrayBuffer = oReq.response; // Note: not oReq.responseText
        if (arrayBuffer) {
            var byteArray = new Uint8Array(arrayBuffer);
            console.log(byteArray);
            for (var i = 0; i < byteArray.byteLength; i++) {
                // do something with each byte in the array
            }
        }
    };

    oReq.send(null);
}



function generate(text) {
    var lines = text.lines();
    var tokens = lines[0].split('|');
    var rows = lines.skip(2).select(line => {
        var i = 0;
        var values = tokens.select(token => {
            var s = line.substr(i, token.length);
            i += token.length + 1;
            return s.trim();
        });
        return values;
    });
    var props = tokens.select(t=>t.trim());
    var list = rows.select(row => {
        var obj = {};
        props.forEach((prop, i) => obj[prop] = row[i]);
        return obj;
    });
    $("textarea").val(list.select(t=>Q.stringifyFormatted(t)).join(",\n"));
    return list;
}

function generate2() {
    var obj = {};
    getConsts().forEach(t => obj[t.Tag] = t.Dec);
    return Object.keys(obj).select(key => key + ":" + obj[key]).join(",\n");
}

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
    var buf = new ArrayBuffer(str.length); // 2 bytes for each char
    var bufView = new Uint8Array(buf);
    for (var i = 0, strLen = str.length; i < strLen; i++) {
        bufView[i] = str.charCodeAt(i);
    }
    return buf;
}
function ab2str2(buf) {
    return String.fromCharCode.apply(null, new Uint8Array(buf));
}



function varint() {
    var MSB = 0x80, REST = 0x7F
    varint.read = read;
    function read(buf, offset) {
        var res = 0
          , offset = offset || 0
          , shift = 0
          , counter = offset
          , b
          , l = buf.length

        do {
            if (counter >= l) {
                read.bytesRead = 0
                return undefined
            }
            b = buf[counter++]
            res += shift < 28
              ? (b & REST) << shift
              : (b & REST) * Math.pow(2, shift)
            shift += 7
        } while (b >= MSB)

        read.bytes = counter - offset

        return res
    }

}
varint();

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
