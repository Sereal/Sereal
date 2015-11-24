declare module Zlib {
    class Deflate {
        constructor(arr: ArrayBuffer);
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
}
interface Number {
    toHex(): string;
}
Number.prototype.toHex = function () { return this.toString(16); }

interface JQuery {
    makeGraph(opts: any): JQuery;
}

