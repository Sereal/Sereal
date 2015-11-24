class DataReader {
    constructor(public _buffer: ArrayBuffer) {
        this.view = new DataView(_buffer);
        this.pos = this.view.byteOffset;
    }

    view: DataView;
    pos: number;

    toString(): string { return "DataReader pos=" + this.pos; }
    getDouble(): number { throw new Error(); }
    rewind() { this.pos = this.view.byteOffset; }
    hasRemaining(): boolean { return this.remaining() > 0; }
    remaining(): number { return this.view.byteLength - this.pos; }
    limit(): number { return this.view.byteLength - this.pos; }
    order(type: ByteOrder) { throw new Error(); }
    getInt32(): number { return this.getInt(); }
    getInt8(): number { return this.getByte(); }
    asInt8Array(): Int8Array { return new Int8Array(this._buffer); }
    getInt(): number {
        var value = this.view.getInt32(this.pos);
        this.pos += 4;
        return value;
    }
    getBytesTo(buf: ArrayBuffer): ArrayBuffer {
        var arr = new Uint8Array(buf);
        for (var i = 0; i < arr.length; i++)
            arr[i] = this.getByte();
        return buf;
    }
    getByte(): any {
        var value = this.view.getInt8(this.pos);
        this.pos++;
        return value;
    }
    getVarInt() {
        var out = { bytesRead: null };
        var value = varint.read(this.asInt8Array(), this.pos, out);
        this.pos += out.bytesRead || 1;
        return value;
    }
    getBytes(length) {
        if (length == null)
            length = this._buffer.byteLength - this.pos;
        var arr = new Int8Array(length);
        for (var i = 0; i < length; i++)
            arr[i] = this.getByte();
        return arr.buffer;
    }

}
