class DataReader2 {
    constructor(public _buffer: ArrayBuffer) {
        this._view = new DataView(_buffer);
        this._pos = this._view.byteOffset;
    }
    _pos: number;
    _view: DataView;
    //Function.addTo(_this, [rewind, limit, getInt, get, getInt32, pos, getVarInt, getInt8, getByte, toString, getBytes]);

    toString() {
        return "DataReader2 pos=" + this._pos;
    }

    pos(): number {
        return this._pos;
    }

    rewind() {
        this._pos = this._view.byteOffset;
    }
    limit() {
        return this._view.byteLength - this._pos;
    }
    getInt32() {
        return this.getInt();
    }
    getInt() {
        var value = this._view.getInt32(this._pos);
        this._pos += 4;
        return value;
    }
    getByte() {
        return this.get();
    }
    getInt8() {
        return this.get();
    }
    get() {
        var value = this._view.getInt8(this._pos);
        this._pos++;
        return value;
    }
    getVarInt() {
        var out = { bytesRead: null };
        var value = varint.read(this.asInt8Array(), this._pos, out);
        this._pos += out.bytesRead || 1;
        return value;
    }
    asInt8Array() {
        return new Int8Array(this._buffer);
    }
    getBytes(length) {
        if (length == null)
            length = this._buffer.byteLength - this._pos;
        var arr = new Int8Array(length);
        for (var i = 0; i < length; i++)
            arr[i] = this.getByte();
        return arr.buffer;
    }
}
