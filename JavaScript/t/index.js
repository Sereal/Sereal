/// <reference path="jquery.d.ts"/>
/// <reference path="../build/sereal.d.ts"/>
"use strict";
var IndexPage = (function () {
    function IndexPage() {
    }
    IndexPage.main = function () {
        var page = new IndexPage();
        $(function () { return page.domReady(); });
    };
    IndexPage.prototype.domReady = function () {
        var _this = this;
        var bytes = Sereal.Utils.stringToBytes(localStorage.getItem("test"));
        var dec = new Sereal.Decoder();
        var doc = dec.decodeDocument(bytes);
        console.log(doc);
        return;
        $.get("/sereal/riak_event_base64.txt").done(function (res) {
            _this.msgText = atob(res);
            var res2 = res.replaceAll("\r", "").replaceAll("\n", "");
            var msgText2 = atob(res2);
            if (msgText2 != _this.msgText)
                throw new Error();
            _this.main();
        });
        //var samples = [
        //    "PfNybDMLAUFobWV0YWRhdGEpjgB4AXNUU0snCgAAQH4P2A==",
        //    "PfNybDMLAUFobWV0YWRhdGFSzgB4AV3AOxWAIAAF0KNuzs6el8MKpgDxryj+JYWLCxkIQhsqMMMtJaEVq5u264dxmvmyim0/zut+XilIiFMPGEwEHeNL8CtbpMiQO048HnM=",
        //    "PfNybDMLAUFobWV0YWRhdGFq3QB4AWXAvRFAMBgG4ENHq3Zf6YxgmzfiJwkh/mUKjcYMBrGNFdTOEycBOHFUdJxPCkURJIXINPtAQZeLnG4HJe0emMGP8AELlvG8KCshVd3otjP9ME7zsm72BbmuJxI=",
        //    "PfNybDMLAUFobWV0YWRhdGFu2wB4AV3KOxJAMBRG4dHSqs0tjSXYzR8hL0K8ZR8aK7IbW1Ck4uvOzMmLBJpSVJZ9QNB5PSXgwSpeN0IqbdrO9oMbp3lZt/3wUDE4cciwGspQ0x2hCc0c/l4EFiqB",
        //    "PfNybDMLAUFobWV0YWRhdGFy4AB4AWXJORJAMABG4dHSqk1KNHqVq/yxZCPELvfQOILLuYIZKuNrXvHC2IMkPqjBjyIBYEHzoqwYF1LVjW470w/jNC/rZlGGp5MQUBRRhorsx5WCvREuck0/wJ91A0uaK2I=",
        //    "PfNybAMLAUFobWV0YWRhdGEoKgJheSVheCgqAWFhYWI=",
        //    "PfNybDMLAUFobWV0YWRhdGGlAccAeAG9ybkNgDAMAEAhdkGBrZzExn+BKAIjMSVMwbVXthnuRfa/wCjPdABArbW11ntHRCIa3zGziKiqmbl7RGQmXOv5AsYSQQc=",
        //];
        //var sample = samples[2];
        //this.msgText = atob(sample);
        //this.main();
        //this.msgText = localStorage.getItem("sereal");
        //$.get("m1.txt").done(t=> {
        //    console.log(t);
        //    console.log(atob(t));
        //    this.msgText = atob(t);
        //    this.main();
        //});
    };
    IndexPage.prototype.main = function () {
        var binaryText = this.msgText; //
        var dec = new Sereal.Decoder();
        var doc = dec.decodeDocument(binaryText);
        console.log(doc);
    };
    IndexPage.prototype.generate = function (text) {
        var lines = text.lines();
        var tokens = lines[0].split('|');
        var rows = lines.skip(2).select(function (line) {
            var i = 0;
            var values = tokens.select(function (token) {
                var s = line.substr(i, token.length);
                i += token.length + 1;
                return s.trim();
            });
            return values;
        });
        var props = tokens.select(function (t) { return t.trim(); });
        var list = rows.select(function (row) {
            var obj = {};
            props.forEach(function (prop, i) { return obj[prop] = row[i]; });
            return obj;
        });
        $("textarea").val(list.select(function (t) { return Q.stringifyFormatted(t); }).join(",\n"));
        return list;
    };
    return IndexPage;
})();
