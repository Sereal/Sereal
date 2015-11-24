/// <reference path="../src/External.ts"/>
/// <reference path="../src/Common.ts"/>
/// <reference path="../src/DataReader.ts"/>
/// <reference path="../src/DataReader2.ts"/>
/// <reference path="../src/Decoder.ts"/>
/// <reference path="../libs/jquery/jquery.d.ts"/>

var C = getConsts2();
var TAGS = [];
getConsts().forEach(t=> TAGS[parseInt(t.Dec)] = t);

var _dv;
var _reader;
var MAGIC = 1039364716;//(0x6c) + (0x72 << 8) + (0x73 << 16) + (0x3d << 24);
function main() {
    
    var binaryText = localStorage.getItem("sereal");

    var res = decodeSereal(binaryText);
    console.log(res);


    //var dec = new Decoder();
    //dec.setData(str2ab2(binaryText));
    //var x = dec.decode();
    //console.log(x);
    //return;
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
        var md = decodeDocumentBody2(_reader.getBytes(headerSuffixSize));
        console.log(md);
    }
    if (type == 3) {
        var arr = new Int8Array(_reader.getBytes());
        var zip = new Zlib.Deflate(arr);

        //console.log(arr2);
    }
    //console.log(x);

    function toBitString(byte) {
        return byte.toString(2).padLeft(8, "0");
    }
    function decodeDocumentBody2(buffer) {
        var dec = new Decoder({ prefer_latin1: true });
        dec.setData(buffer);
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
    var props = tokens.select(t=> t.trim());
    var list = rows.select(row => {
        var obj = {};
        props.forEach((prop, i) => obj[prop] = row[i]);
        return obj;
    });
    $("textarea").val(list.select(t=> Q.stringifyFormatted(t)).join(",\n"));
    return list;
}

function generate2() {
    var obj = {};
    getConsts().forEach(t => obj[t.Tag] = t.Dec);
    return Object.keys(obj).select(key => key + ":" + obj[key]).join(",\n");
}

