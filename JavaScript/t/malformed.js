// regression tests for malformed input; run with: node JavaScript/t/malformed.js
"use strict";
var fs = require("fs"), vm = require("vm"), path = require("path");

var root = path.join(__dirname, "..");
var ctx = { console: console };
vm.createContext(ctx);
vm.runInContext(fs.readFileSync(path.join(root, "libs/zlib-0.2.0/zlib.min.js"), "utf8"), ctx);
vm.runInContext("var Zlib = this.Zlib;" + fs.readFileSync(path.join(root, "build/sereal.js"), "utf8") + "\nthis.Sereal = Sereal;", ctx);
var Sereal = ctx.Sereal;
var CtxUint8Array = vm.runInContext("Uint8Array", ctx);

// every packet starts with the v3 header: magic, version/type byte 0x03
// (protocol 3, raw body) and an empty header suffix (varint 0)
var HEADER = "3df3726c0300";

function decode(hexBody, header) {
    var bytes = new CtxUint8Array(Buffer.from((header == null ? HEADER : header) + hexBody, "hex"));
    return new Sereal.Decoder().decodeDocument(bytes).body;
}

var failed = 0, count = 0;
function ok(cond, name, extra) {
    count++;
    console.log((cond ? "ok " : "not ok ") + count + " - " + name + (cond || extra == null ? "" : " # " + extra));
    if (!cond) failed++;
}
function rejects(hexBody, re, name, header) {
    try {
        var res = decode(hexBody, header);
        ok(false, name, "decoded without error: " + String(JSON.stringify(res)).slice(0, 80));
    } catch (e) {
        var isOwn = e instanceof Sereal.SerealException;
        ok(isOwn && re.test(e.msg), name, (e && e.constructor && e.constructor.name) + ": " + (isOwn ? e.msg : e.message));
    }
}
function decodes(hexBody, expected, name, header) {
    try {
        var res = JSON.stringify(decode(hexBody, header));
        ok(res === JSON.stringify(expected), name, "got " + res);
    } catch (e) {
        ok(false, name, (e && e.constructor && e.constructor.name) + ": " + (e.msg || e.message));
    }
}
function repeat(s, n) { return new Array(n + 1).join(s); }

// ARRAYREF_2 [1, SHORT_BINARY_3 "abc"]
decodes("420163616263", [1, "abc"], "valid document");

// COPY to offset 1, which is the COPY tag itself: used to recurse until the stack overflowed
rejects("2f01", /COPY/, "copy referring to itself");
// COPY to offset 0, which is before the body
rejects("2f00", /COPY/, "copy offset zero");
// ARRAYREF_2 [COPY to offset 3, 1]: the target is after the COPY tag
rejects("422f0301", /COPY/, "copy referring forward");
// ARRAY of 19 arrays, each holding two COPYs of the previous one: doubles in size per level
rejects("2b1340422f032f03422f042f04422f092f09422f0e2f0e422f132f13422f182f18422f1d2f1d422f222f22422f272f27422f2c2f2c422f312f31422f362f36422f3b2f3b422f402f40422f452f45422f4a2f4a422f4f2f4f422f542f54",
    /COPY/, "copy of an item containing a copy");
// ARRAYREF_2 [SHORT_BINARY_1 "a", COPY to offset 2]
decodes("4261612f02", ["a", "a"], "valid copy");
// 100000 nested ARRAYREF_1 tags around the integer 1
rejects(repeat("41", 100000) + "01", /nesting/, "deep nesting");
decodes(repeat("41", 3) + "01", [[[1]]], "shallow nesting");

// HASHREF_1 {"__proto__": HASHREF_1 {"polluted": 1}}: used to replace the decoded object's prototype
(function () {
    var h = decode("51695f5f70726f746f5f5f5168706f6c6c7574656401");
    var CtxObject = vm.runInContext("Object", ctx);
    ok(Object.getPrototypeOf(h) === CtxObject.prototype, "__proto__ key leaves the prototype alone");
    ok(h.polluted === undefined, "__proto__ key does not add inherited properties");
    ok(Object.prototype.hasOwnProperty.call(h, "__proto__") && JSON.stringify(Object.keys(h)) === '["__proto__"]',
        "__proto__ key is an own enumerable property");
    ok(Object.getOwnPropertyDescriptor(h, "__proto__") && Object.getOwnPropertyDescriptor(h, "__proto__").value.polluted === 1,
        "__proto__ key keeps its value");
})();
// HASHREF_2 {"a": 1, "a": 2}: a repeated key keeps the last value
decodes("52616101616102", { a: 2 }, "repeated hash key");

// header suffix of 3 bytes with no user metadata (bit field 0x00) followed by
// two more suffix bytes and the body 1: the suffix bytes used to be read as the body
decodes("01", 1, "header suffix without metadata is skipped", "3df3726c0303000506");
// header suffix of 3 bytes with user metadata 5, body 1
(function () {
    var doc = new Sereal.Decoder().decodeDocument(new CtxUint8Array(Buffer.from("3df3726c0303010500" + "01", "hex")));
    ok(doc.body === 1 && doc.user_metadata === 5, "header suffix with metadata", JSON.stringify([doc.body, doc.user_metadata]));
})();
// header suffix of 5 bytes with only two bytes left in the document
rejects("01", /suffix/, "header suffix past end", "3df3726c030500");
// bad magic
rejects("01", /magic/, "bad magic", "3df3726d0300");
// ARRAY with a count of 2^32-1 and no elements
rejects("2bffffffff0f", /./, "array count past end");
// STR_UTF8 of length 1000 with two bytes left
rejects("27e8076162", /./, "string length past end");
// REGEXP with pattern "(" and no flags
rejects("31612860", /./, "invalid regexp");

if (failed) process.exitCode = 1;
console.log(failed ? "# " + failed + " of " + count + " failed" : "# all " + count + " passed");
