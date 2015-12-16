"use strict";
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

interface Number {
    toHex(): string;
    to8BitString(): string;
}
Number.prototype.toHex = function () { return this.toString(16); }
Number.prototype.to8BitString = function () { return this.toString(2).padLeft(8, "0"); }


interface String {
    padLeft(totalLength: number, paddingChar: string): string;
    last(): string;
}

String.prototype.padLeft = function (totalWidth:number, paddingChar?:string):string {
    if (paddingChar == null || paddingChar == "")
        paddingChar = " ";
    var s = this;
    while (s.length < totalWidth)
        s = paddingChar + s;
    return s;
}
String.prototype.last = function (predicate?:any):string {
    if (this.length == 0)
        return null;
    if (predicate == null)
        return this[this.length - 1];
    for (var i = this.length; i >= 0; i--) {
        if (predicate(this[i]))
            return this[i];
    }
    return null;
}

