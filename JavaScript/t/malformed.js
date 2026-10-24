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

if (failed) process.exitCode = 1;
console.log(failed ? "# " + failed + " of " + count + " failed" : "# all " + count + " passed");
