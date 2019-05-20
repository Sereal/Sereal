package com.booking.sereal.jackson;

import com.booking.sereal.ByteArray;
import com.booking.sereal.DecoderOptions;
import com.booking.sereal.EncoderOptions;
import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.Version;
import com.fasterxml.jackson.core.io.IOContext;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Reader;
import java.io.Writer;
import java.util.Arrays;

/**
 * {@link com.fasterxml.jackson.core.JsonFactory} implementation that creates {@link SerealParser} and {@link SerealGenerator} instances.
 * <p>
 * It only supports byte-oriented I/O ({@code byte[]}, {@link java.io.InputStream} or {@link java.io.OutputStream}): any
 * attempt of using methods that take a {@code char[]}, {@link java.lang.String}, {@link java.io.Reader} or {@link java.io.Writer}
 * throws an exception.
 * <p>
 * Incremental parsing is not supported: using an {@link java.io.InputStream} reads the full contents in memory before
 * starting to parse.
 */
public class SerealFactory extends JsonFactory {
  private final EncoderOptions encoderOptions;
  private final DecoderOptions decoderOptions;

  /**
   * Create a new instance with default encoder/decoder options.
   */
  public SerealFactory() {
    this(null, null);
  }

  /**
   * Create a new instance with the specified encoder/decoder options.
   *
   * @param encoderOptions {@code null} to use default options.
   * @param decoderOptions {@code null} to use default options.
   */
  public SerealFactory(EncoderOptions encoderOptions, DecoderOptions decoderOptions) {
    this.encoderOptions = encoderOptions != null ? encoderOptions : new EncoderOptions();
    this.decoderOptions = decoderOptions != null ? decoderOptions : new DecoderOptions();
  }

  private SerealFactory(SerealFactory src) {
    super(src, null);
    this.encoderOptions = src.encoderOptions;
    this.decoderOptions = src.decoderOptions;
  }

  @Override
  public Version version() {
    return PackageVersion.VERSION;
  }

  @Override
  public SerealFactory copy() {
    return new SerealFactory(this);
  }

  @Override
  public boolean canHandleBinaryNatively() {
    return true;
  }

  @Override
  protected JsonParser _createParser(InputStream in, IOContext ctxt) throws IOException {
    ByteArray bytes = slurpStream(in, ctxt);

    return _createParser(bytes.array, 0, bytes.length, ctxt);
  }

  /** Fails with an exception. */
  @Override
  protected JsonParser _createParser(Reader r, IOContext ctxt) {
    return _nonByteSource();
  }

  /** Fails with an exception. */
  @Override
  protected JsonParser _createParser(char[] data, int offset, int len, IOContext ctxt,
      boolean recyclable) {
    return _nonByteSource();
  }

  @Override
  protected JsonParser _createParser(byte[] data, int offset, int len, IOContext ctxt)
      throws IOException {
    return new SerealParser(decoderOptions, data, offset, len, this._objectCodec);
  }

  /** Fails with an exception. */
  @Override
  protected JsonGenerator _createGenerator(Writer out, IOContext ctxt) {
    return _nonByteTarget();
  }

  @Override
  protected JsonGenerator _createUTF8Generator(OutputStream out, IOContext ctxt) throws IOException {
    return new SerealGenerator(encoderOptions, _generatorFeatures, _objectCodec, out);
  }

  private <T> T _nonByteSource() {
    throw new UnsupportedOperationException("Can not create parser for non-byte-based source");
  }

  private <T> T _nonByteTarget() {
    throw new UnsupportedOperationException("Can not create generator for non-byte-based target");
  }

  private ByteArray slurpStream(InputStream is, IOContext ctxt) throws IOException {
    byte[] buffer = null;
    try {
      buffer = ctxt.allocReadIOBuffer(1024);

      int read = is.read(buffer);
      if (read < buffer.length) {
        return new ByteArray(Arrays.copyOf(buffer, read));
      }
      ByteArray result = new ByteArray(Arrays.copyOf(buffer, Math.max(read * 3 / 2, 2048)), read);

      while (true) {
        read = is.read(buffer);
        result.ensure(result.length + read);
        System.arraycopy(buffer, 0, result.array, result.length, read);
        result.length += read;
        if (read < buffer.length) {
          return result;
        }
      }
    } finally {
      ctxt.releaseReadIOBuffer(buffer);
    }
  }
}
