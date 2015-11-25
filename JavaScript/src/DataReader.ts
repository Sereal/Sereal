"use strict";
module Sereal {

    export class DataReader {
        constructor(data: any) {
            this.view = DataReader.toDataView(data);
            this.pos = 0;
        }
        view: DataView;
        pos: number;

        readDouble(): number { throw new Error(); }
        readInt32(): number { return this.readInt(); }
        asUint8Array(): Uint8Array { return new Uint8Array(this.view.buffer, this.view.byteOffset, this.view.byteLength); }
        readInt(): number {
            var value = this.view.getInt32(this.pos);
            this.pos += 4;
            return value;
        }
        readBytesTo(buf: ArrayBuffer): ArrayBuffer {
            var arr = new Uint8Array(buf);
            for (var i = 0; i < arr.length; i++)
                arr[i] = this.readByte();
            return buf;
        }
        readByte(): number {
            var value = this.view.getUint8(this.pos);
            this.pos++;
            return value;
        }
        readVarInt(): number {
            var out = { bytesRead: null };
            var value = varint.read(this.asUint8Array(), this.pos, out);
            if (out.bytesRead == null)
                throw new Error();
            this.pos += out.bytesRead;
            return value;
        }
        readBytes(length?: number): Uint8Array {
            if (length == null)
                length = this.view.byteLength - this.pos;
            var arr = new Uint8Array(this.view.buffer, this.view.byteOffset + this.pos, length);
            this.pos += length;
            return arr;
        }


        get absPos(): number {
            return this.view.byteOffset + this.pos;
        }
        toHex(): string {
            var list = [];
            for (var i = 0; i < this.view.byteLength; i++) {
                list.push(this.view.getUint8(i).toHex().padLeft(2, "0"));
            }
            return list.join(" ");
        }
        hasRemaining(): boolean { return this.remaining() > 0; }
        remaining(): number { return this.view.byteLength - this.pos; }
        static toDataView(data: any): DataView {
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
    }

}