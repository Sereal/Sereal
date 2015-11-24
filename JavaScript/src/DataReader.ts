class DataReader {
    constructor(public _buffer: ArrayBuffer) {
        this._view = new DataView(_buffer);
    }
    _view: DataView;
    _pos: number;
    //var _this = this;
    //Function.addTo(_this, [rewind, limit, getInt, get, hasRemaining, position, remaining, toString]);
    //var _view = new DataView(_buffer);
    //var _pos;

    //_this._view = _view;

    toString(): string {
        return "DataReader pos=" + this._pos;
    }
    getDouble(): number {
        throw new Error();
    }

    position(value?: number): number {
        if (arguments.length == 0)
            return this._pos;
        this._pos = value;
    }
    rewind() {
        this._pos = this._view.byteOffset;
    }
    hasRemaining(): boolean {
        return this.remaining() > 0;
    }
    remaining(): number {
        return this._view.byteLength - this._pos;
    }
    limit(): number {
        return this._view.byteLength - this._pos;
    }
    getInt(): number {
        var value = this._view.getInt32(this._pos);
        this._pos += 4;
        return value;
    }
    getBytes(buf: ArrayBuffer): ArrayBuffer {
        var arr = new Uint8Array(buf);
        for (var i = 0; i < arr.length; i++)
            arr[i] = this.get();
        return buf;
    }
    get(): any {
        var value = this._view.getInt8(this._pos);
        this._pos++;
        return value;
    }
    order(type: ByteOrder) {
        throw new Error();
    }
}
