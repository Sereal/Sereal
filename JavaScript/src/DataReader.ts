class DataReader {
    constructor(data: any) {
        this.view = this.toDataView(data);
        this.pos = 0;
        console.log("DataReader created, data=" + this.toHex());
    }
    view: DataView;
    pos: number;

    toDataView(data: any): DataView {
        if (data instanceof DataView)
            return data;
        if (ArrayBuffer.isView(data)) {
            return new DataView(data.buffer, data.byteOffset, data.byteLength);
        }
        if (data instanceof ArrayBuffer) {
            var buf: ArrayBuffer = data;
            return new DataView(buf);//, 0, buf.byteLength
        }
        console.warn("unknown data", data);
        return null;
    }

    toHex(): string {
        var list = [];
        for (var i = 0; i < this.view.byteLength; i++) {
            list.push(this.view.getUint8(i).toHex().padLeft(2, "0"));
        }
        return list.join(" ");
    }
    toString(): string { return "DataReader pos=" + this.pos; }
    getDouble(): number { throw new Error(); }
    rewind() { this.pos = 0; }
    hasRemaining(): boolean { return this.remaining() > 0; }
    remaining(): number { return this.view.byteLength - this.pos; }
    limit(): number { return this.view.byteLength - this.pos; }
    order(type: ByteOrder) { throw new Error(); }
    getInt32(): number { return this.getInt(); }
    asUint8Array(): Int8Array { return new Uint8Array(this.view.buffer, this.view.byteOffset, this.view.byteLength); }
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
    getByte(): number {
        var value = this.view.getUint8(this.pos);
        this.pos++;
        return value;
    }
    getVarInt(): number {
        var out = { bytesRead: null };
        var value = varint.read(this.asUint8Array(), this.pos, out);
        if (out.bytesRead == null)
            throw new Error();
        this.pos += out.bytesRead;
        return value;
    }
    getBytes(length?: number): Uint8Array {
        if (length == null)
            length = this.view.byteLength - this.pos;
        var arr = new Uint8Array(this.view.buffer, this.view.byteOffset + this.pos, length);
        this.pos += length;
        return arr;
    }
    getBytes2(length?: number): DataReader {
        var arr = this.getBytes(length);
        var reader = new DataReader(arr);
        return reader;
    }

}
