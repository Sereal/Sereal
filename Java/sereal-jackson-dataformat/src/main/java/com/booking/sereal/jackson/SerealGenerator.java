package com.booking.sereal.jackson;

import com.booking.sereal.ByteArray;
import com.booking.sereal.EncoderOptions;
import com.booking.sereal.SerealException;
import com.booking.sereal.TokenEncoder;
import com.fasterxml.jackson.core.Base64Variant;
import com.fasterxml.jackson.core.JsonGenerationException;
import com.fasterxml.jackson.core.ObjectCodec;
import com.fasterxml.jackson.core.Version;
import com.fasterxml.jackson.core.base.GeneratorBase;
import com.fasterxml.jackson.core.json.JsonWriteContext;
import java.io.IOException;
import java.io.OutputStream;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.regex.Pattern;

/**
 * A {@link com.fasterxml.jackson.core.JsonGenerator} implementation for Sereal.
 * <p>
 * It does not support writing raw values, except for {@link SerealGenerator#writeRawUTF8String(byte[], int, int)}.
 * <p>
 * {@link java.math.BigInteger} and {@link java.math.BigDecimal} are written as strings.
 * <p>
 * Binary values are written as binary/ISO-8859-1 strings.
 */
public class SerealGenerator extends GeneratorBase {
  private final TokenEncoder tokenEncoder;
  private final OutputStream outputStream;

  SerealGenerator(EncoderOptions encoderOptions, int features, ObjectCodec codec, OutputStream out) throws IOException {
    super(features, codec, JsonWriteContext.createRootContext(null));
    try {
      this.tokenEncoder = new TokenEncoder(encoderOptions);
      this.outputStream = out;
      this.tokenEncoder.startDocument();
    } catch (SerealException e) {
      throw new IOException(e);
    }
  }

  @Override
  public Version version() {
    return PackageVersion.VERSION;
  }

  @Override
  public void close() throws IOException {
    if (isClosed()) {
      return;
    }
    super.close();
    flush();
    outputStream.close();
  }

  @Override
  public boolean canWriteBinaryNatively() {
    return true;
  }

  @Override
  public void writeStartArray() throws IOException {
    try {
      tokenEncoder.startArray();
    } catch (SerealException e) {
      throw this._constructError("Sereal internal error", e);
    }
  }

  @Override
  public void writeEndArray() throws IOException {
    try {
      tokenEncoder.endArray();
      maybeFinishDocument();
    } catch (SerealException e) {
      throw this._constructError("Sereal internal error", e);
    }
  }

  @Override
  public void writeStartObject() throws IOException {
    try {
      tokenEncoder.startHash();
    } catch (SerealException e) {
      throw this._constructError("Sereal internal error", e);
    }
  }

  @Override
  public void writeEndObject() throws IOException {
    try {
      tokenEncoder.endHash();
      maybeFinishDocument();
    } catch (SerealException e) {
      throw this._constructError("Sereal internal error", e);
    }
  }

  @Override
  public void writeFieldName(String s) throws IOException {
    try {
      tokenEncoder.appendString(s);
    } catch (SerealException e) {
      throw this._constructError("Sereal internal error", e);
    }
  }

  @Override
  public void writeString(String s) throws IOException {
    try {
      tokenEncoder.appendString(s);
    } catch (SerealException e) {
      throw this._constructError("Sereal internal error", e);
    }
  }

  @Override
  public void writeString(char[] chars, int offset, int length) throws IOException {
    try {
      tokenEncoder.appendString(chars, offset, length);
    } catch (SerealException e) {
      throw this._constructError("Sereal internal error", e);
    }
  }

  @Override
  public void writeRawUTF8String(byte[] bytes, int offset, int length) throws IOException {
    try {
      tokenEncoder.appendUTF8(bytes, offset, length);
    } catch (SerealException e) {
      throw this._constructError("Sereal internal error", e);
    }
  }

  @Override
  public void writeUTF8String(byte[] bytes, int offset, int length) throws IOException {
    try {
      tokenEncoder.appendUTF8(bytes, offset, length);
    } catch (SerealException e) {
      throw this._constructError("Sereal internal error", e);
    }
  }

  @Override
  public void writeRaw(String s) {
    _reportUnsupportedOperation();
  }

  @Override
  public void writeRaw(String s, int i, int i1) {
    _reportUnsupportedOperation();
  }

  @Override
  public void writeRaw(char[] chars, int i, int i1) {
    _reportUnsupportedOperation();
  }

  @Override
  public void writeRaw(char c) {
    _reportUnsupportedOperation();
  }

  @Override
  public void writeBinary(Base64Variant base64Variant, byte[] bytes, int offset, int length)
      throws IOException {
    try {
      tokenEncoder.appendBinary(bytes, offset, length);
    } catch (SerealException e) {
      throw this._constructError("Sereal internal error", e);
    }
  }

  @Override
  public void writeNumber(int i) throws IOException {
    try {
      tokenEncoder.appendLong(i);
    } catch (SerealException e) {
      throw this._constructError("Sereal internal error", e);
    }
  }

  @Override
  public void writeNumber(long l) throws IOException {
    try {
      tokenEncoder.appendLong(l);
    } catch (SerealException e) {
      throw this._constructError("Sereal internal error", e);
    }
  }

  @Override
  public void writeNumber(BigInteger bigInteger) throws IOException {
    try {
      tokenEncoder.appendString(bigInteger.toString());
    } catch (SerealException e) {
      throw this._constructError("Sereal internal error", e);
    }
  }

  @Override
  public void writeNumber(double v) throws IOException {
    try {
      tokenEncoder.appendDouble(v);
    } catch (SerealException e) {
      throw this._constructError("Sereal internal error", e);
    }
  }

  @Override
  public void writeNumber(float v) throws IOException {
    try {
      tokenEncoder.appendFloat(v);
    } catch (SerealException e) {
      throw this._constructError("Sereal internal error", e);
    }
  }

  @Override
  public void writeNumber(BigDecimal bigDecimal) throws IOException {
    try {
      tokenEncoder.appendString(bigDecimal.toString());
    } catch (SerealException e) {
      throw this._constructError("Sereal internal error", e);
    }
  }

  @Override
  public void writeNumber(String s) {
    this._reportUnsupportedOperation();
  }

  @Override
  public void writeBoolean(boolean b) throws IOException {
    try {
      tokenEncoder.appendBoolean(b);
    } catch (SerealException e) {
      throw this._constructError("Sereal internal error", e);
    }
  }

  @Override
  public void writeNull() throws IOException {
    try {
      tokenEncoder.appendCanonicalUndef();
    } catch (SerealException e) {
      throw this._constructError("Sereal internal error", e);
    }
  }

  @Override
  public void flush() throws IOException {
    outputStream.flush();
  }

  @Override
  protected void _releaseBuffers() {
    // no intermediate buffers are used
  }

  @Override
  protected void _verifyValueWrite(String typeMsg) {
    // only needed for raw values in the base encoder, and it is not really
    // needed by Sereal
  }

  public void writePattern(Pattern p) throws IOException {
    try {
      tokenEncoder.appendRegexp(p);
    } catch (SerealException e) {
      throw this._constructError("Sereal internal error", e);
    }
  }

  private JsonGenerationException _constructError(String msg, Throwable t) {
    return new JsonGenerationException(msg, t, this);
  }

  private void maybeFinishDocument() throws IOException, SerealException {
    if (tokenEncoder.isComplete()) {
      tokenEncoder.endDocument();

      ByteArray data = tokenEncoder.getDataReference();
      outputStream.write(data.array, 0, data.length);
    }
  }
}
