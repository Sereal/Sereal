declare module Zlib {
    class Deflate {
        constructor(arr: Int8Array);
        compress(): Int8Array;
    }
    class Inflate {
        constructor(arr: Int8Array);
        decompress(): Int8Array;
    }
    class RawInflate {
        constructor(arr: Int8Array);
        decompress(): Int8Array;
    }
    class InflateStream {
        constructor(arr: Int8Array);
        decompress(): Int8Array;
    }
}


declare class Q {
    static stringifyFormatted(value: any): string;

}


interface ObjectConstructor {
    values(obj: Object): string[];
}

interface Array<T> {
    //map<U>(callbackfn: (value: T, index: number, array: T[]) => U, thisArg?: any): U[];
    select<R>(selector: (item: T) => R): Array<R>;
    forEachAsyncProgressive(action: (item: T, callback: () => void) => void, finalCallback: () => void);
}
interface Number {
    toHex(): string;
}
Number.prototype.toHex = function () { return this.toString(16); }

interface JQuery {
    makeGraph(opts: any): JQuery;
}

interface ArrayConstructor {
    generateNumbers(from: number, until: number): number[];
}
interface JQueryXHR extends JQueryPromise {

}