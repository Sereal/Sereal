/// <reference path="../src/External.ts"/>
/// <reference path="../src/Common.ts"/>
/// <reference path="../src/DataReader.ts"/>
/// <reference path="../src/Decoder.ts"/>
/// <reference path="../libs/jquery/jquery.d.ts"/>

class SerealDocument {
    magic: number;
    version: number;
    type: number;
    header_suffix_size: number;
    eight_bit_field: { value: number, has_user_metadata?: boolean };
    user_metadata: any;
    body: any;
    body_compressed_length: number;
    body_uncompressed_length: number;
}

class IndexPage {
    C = getConsts2();
    reader: DataReader;
    doc: SerealDocument;
    msgText: string;
    static main(): void {
        var page = new IndexPage();
        $(() => page.domReady());
    }
    domReady() {
        this.msgText = atob("PfNybDMLAUFobWV0YWRhdGEpjgB4AXNUU0snCgAAQH4P2A==");
        this.main();
        //$.get("m1.txt").done(t=> {
        //    console.log(t);
        //    console.log(atob(t));
        //    this.msgText = atob(t);
        //    this.main();
        //});
    }
    main() {
        var binaryText = this.msgText;//localStorage.getItem("sereal");
        var res = this.decodeSereal(binaryText);
        console.log(res);
        //var dec = new Decoder();
        //dec.setData(str2ab2(binaryText));
        //var x = dec.decode();
        //console.log(x);
        //return;
    }

    toBitString(byte) {
        return byte.toString(2).padLeft(8, "0");
    }

    decodeDocumentBody2(byteArray: Uint8Array): any {
        var dec = new Decoder({ prefer_latin1: true });
        dec.setData(byteArray);
        var x = dec.readSingleValue();
        console.log(x);
        return x;
    }


    decodeSereal(binaryText: string) {
        this.doc = new SerealDocument();

        var doc = this.doc;
        var _buf = str2ab2(binaryText);
        this.reader = new DataReader(new DataView(_buf));
        var pos = 0;
        console.log({ pos: this.reader.pos });
        doc.magic = this.reader.getInt32();
        if (doc.magic != Consts.MAGIC)
            throw new Error();

        console.log({ pos: this.reader.pos });
        var s = this.reader.getByte().toString(2);
        doc.version = parseInt(s.substr(0, 4), 2);
        doc.type = parseInt(s.substr(4, 4), 2);
        console.log(doc.version, doc.type);
        console.log("before header_suffix_size", { pos: this.reader.pos });
        doc.header_suffix_size = this.reader.getVarInt();
        console.log("after header_suffix_size", { pos: this.reader.pos });
        if (doc.header_suffix_size > 0) {
            doc.eight_bit_field = { value: this.reader.getByte(), };
            doc.eight_bit_field.has_user_metadata = this.toBitString(doc.eight_bit_field.value).last() == "1";
            console.log({ pos: this.reader.pos });
            if (doc.eight_bit_field.has_user_metadata) {
                console.log({ pos: this.reader.pos });
                var mdByteArray = this.reader.getBytes(doc.header_suffix_size - 1);
                doc.user_metadata = this.decodeDocumentBody2(mdByteArray);
                console.log({ pos: this.reader.pos });
                console.log("METADATA", doc.user_metadata);
            }
        }

        console.log({ pos: this.reader.pos });
        doc.body_uncompressed_length = this.reader.getVarInt();
        console.log({ pos: this.reader.pos });
        doc.body_compressed_length = this.reader.getVarInt();
        console.log({ uncomp: doc.body_uncompressed_length, comp: doc.body_compressed_length });
        console.log({ pos: this.reader.pos, remaining: this.reader.remaining() });
        console.log(doc);
        var deflated = this.deflate();
        doc.body = this.decodeDocumentBody2(deflated);
        console.log("DONE!!!!!!!!!", doc);


        //if (doc.type == 3) {
        //    while (this.reader.hasRemaining() && tries < 11) {
        //        if (this.tryDeflate()) {
        //            console.log("success!!!");
        //            break;
        //        }
        //        this.reader.getByte();
        //        tries++;
        //    }
        //    //var buf = this.reader.getBytes();
        //    //var arr = new Int8Array(buf);
        //    //var zip = new Zlib.InflateStream(arr);
        //    //var deflated: Int8Array = zip.decompress();
        //    //var deflatedBuf = deflated.buffer;
        //    //doc.body = this.decodeDocumentBody2(deflatedBuf);
        //    //console.log("FINISHED! DOC = ",doc);
        //}
    }

    deflate(): Uint8Array {
        var pos = this.reader.pos;
        var arr = this.reader.getBytes();
        var zip = new Zlib.Inflate(arr);
        var deflated = zip.decompress();
        return deflated;
    }

    decodeDocumentBody() {
        var byte = this.reader.getByte();
        var bits = this.toBitString(byte);
        console.log(bits);
        var trackFlag = bits[0] == "1";
        var tagDec = parseInt(bits.substr(1), 2);

        //var tag = this.TAGS[tagDec];

        //throw new Error();

    }










    test() {
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



    generate(text) {
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

    generate2() {
        var obj = {};
        getConsts().forEach(t => obj[t.Tag] = t.Dec);
        return Object.keys(obj).select(key => key + ":" + obj[key]).join(",\n");
    }

}