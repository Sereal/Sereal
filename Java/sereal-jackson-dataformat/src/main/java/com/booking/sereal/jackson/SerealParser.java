package com.booking.sereal.jackson;

import com.booking.sereal.ByteArray;
import com.booking.sereal.DecoderOptions;
import com.booking.sereal.Float128;
import com.booking.sereal.SerealException;
import com.booking.sereal.SerealToken;
import com.booking.sereal.TokenDecoder;
import com.fasterxml.jackson.core.Base64Variant;
import com.fasterxml.jackson.core.JsonLocation;
import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.core.JsonStreamContext;
import com.fasterxml.jackson.core.JsonToken;
import com.fasterxml.jackson.core.ObjectCodec;
import com.fasterxml.jackson.core.Version;
import com.fasterxml.jackson.core.base.ParserMinimalBase;
import java.io.IOException;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.nio.charset.StandardCharsets;
import java.util.ArrayDeque;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

/**
 *  A {@link com.fasterxml.jackson.core.JsonParser} implementation for Sereal.
 *  <p>
 *  It does not perform any Perl-style coercions on the values.
 *  <p>
 *  When it encounters a {@link SerealToken#COPY}, {@link SerealToken#REFP} or {@link SerealToken#ALIAS},
 *  it (recursively) follows the reference.
 */
public class SerealParser extends ParserMinimalBase {
  private static final ByteArray EMPTY_ARRAY = new ByteArray(new byte[0]);

  private final TokenDecoder tokenDecoder;
  private ObjectCodec objectCodec;
  private SerealReaderContext parsingContext;
  private SerealToken serealToken = SerealToken.NONE;
  private final int maxRecursionDepth;
  // nesting of arrays, hashes and followed references
  private int depth;
  // offsets of the REFP/ALIAS/COPY targets being followed, innermost last
  private final ArrayDeque<Integer> followedStack = new ArrayDeque<>();
  private final Set<Integer> followed = new HashSet<>();

  SerealParser(DecoderOptions decoderOptions, byte[] data, int offset, int len, ObjectCodec objectCodec) throws IOException {
    this.objectCodec = objectCodec;
    this.maxRecursionDepth = decoderOptions.maxRecursionDepth();
    this.parsingContext = new SerealReaderContext();
    try {
      this.tokenDecoder = new TokenDecoder(decoderOptions);
      this.tokenDecoder.setData(new ByteArray(data, offset, len));
      this.tokenDecoder.prepareDecodeBody();
    } catch (SerealException e) {
      throw this._constructError("Sereal format error", e);
    }
  }

  @Override
  public JsonToken nextToken() throws IOException {
    try {
      // REFN, WEAKEN and OBJECT tags wrapping the next value
      int wrappers = 0;
      for (;;) {
      serealToken = tokenDecoder.nextToken();

      switch (serealToken) {
        case LONG:
        case UNSIGNED_LONG:
          return (_currToken = JsonToken.VALUE_NUMBER_INT);
        case BINARY:
          if (tokenDecoder.isHashKey()) {
            parsingContext.nextIndex();
            parsingContext.setCurrentName(tokenDecoder);
            return (_currToken = JsonToken.FIELD_NAME);
          } else {
            return (_currToken = JsonToken.VALUE_STRING);
          }
        case ARRAY_START:
          enterNesting();
          parsingContext = new SerealReaderContext(parsingContext, 1, -1);
          return (_currToken = JsonToken.START_ARRAY);
        case ARRAY_END:
          depth--;
          parsingContext = parsingContext.getParent();
          return (_currToken = JsonToken.END_ARRAY);
        case HASH_START:
          enterNesting();
          parsingContext = new SerealReaderContext(parsingContext, 2, -1);
          return (_currToken = JsonToken.START_OBJECT);
        case HASH_END:
          depth--;
          parsingContext = parsingContext.getParent();
          return (_currToken = JsonToken.END_OBJECT);
        case END:
          return (_currToken = null);
        case FLOAT:
          return (_currToken = JsonToken.VALUE_NUMBER_FLOAT);
        case DOUBLE:
          return (_currToken = JsonToken.VALUE_NUMBER_FLOAT);
        case TRUE:
          return (_currToken = JsonToken.VALUE_TRUE);
        case FALSE:
          return (_currToken = JsonToken.VALUE_FALSE);
        case UNDEF:
        case CANONICAL_UNDEF:
          return (_currToken = JsonToken.VALUE_NULL);
        case UTF8:
          if (tokenDecoder.isHashKey()) {
            parsingContext.nextIndex();
            parsingContext.setCurrentName(tokenDecoder);
            return (_currToken = JsonToken.FIELD_NAME);
          } else {
            return (_currToken = JsonToken.VALUE_STRING);
          }
        case WEAKEN:
        case REFN:
        case OBJECT_START:
          if (++wrappers > maxRecursionDepth) {
            throw recursionLimitError();
          }
          continue;
        case OBJECT_END:
          continue;
        case REFP:
        case ALIAS:
        case COPY:
          int target = tokenDecoder.backreferenceOffset();
          // following a reference to a value that encloses it would never end
          if (!followed.add(target)) {
            throw this._constructError(String.format(
              "Sereal format error: reference at offset %d to the enclosing value at offset %d",
              tokenDecoder.tokenOffset(), target));
          }
          followedStack.addLast(target);
          enterNesting();
          tokenDecoder.startSubDecode(target);
          continue;
        case SUBDECODE_END:
          followed.remove(followedStack.removeLast());
          depth--;
          continue;
        case REGEXP:
        case FLOAT128:
          return (_currToken = JsonToken.VALUE_EMBEDDED_OBJECT);
        case VSTRING:
          return (_currToken = JsonToken.VALUE_STRING);
        default:
          throw this._formatError("Unsupported Sereal token %s", serealToken);
      }
      }
    } catch (SerealException e) {
      throw this._constructError("Sereal format error", e);
    }
  }

  private void enterNesting() throws JsonParseException {
    if (++depth > maxRecursionDepth) {
      throw recursionLimitError();
    }
  }

  private JsonParseException recursionLimitError() {
    return this._constructError(
      "Sereal format error: reached recursion limit (" + maxRecursionDepth + ") during deserialization");
  }

  @Override
  protected void _handleEOF() throws JsonParseException {
    if (!parsingContext.inRoot()) {
      String marker = parsingContext.inArray() ? "Array" : "Object";
      _reportInvalidEOF(String.format(
        ": expected close marker for %s (start marker at %s)",
        marker, null),
        null);
    }
  }

  @Override
  public String getCurrentName() {
    SerealReaderContext ctxt = parsingContext;
    if (_currToken == JsonToken.START_OBJECT || _currToken == JsonToken.START_ARRAY) {
      ctxt = ctxt.getParent();
    }
    return ctxt.getCurrentName();
  }

  @Override
  public ObjectCodec getCodec() {
    return objectCodec;
  }

  @Override
  public void setCodec(ObjectCodec objectCodec) {
    this.objectCodec = objectCodec;
  }

  @Override
  public Version version() {
    return PackageVersion.VERSION;
  }

  @Override
  public void close() {
    tokenDecoder.setData(EMPTY_ARRAY);
    parsingContext = null;
  }

  @Override
  public boolean isClosed() {
    return parsingContext == null;
  }

  @Override
  public JsonStreamContext getParsingContext() {
    return parsingContext;
  }

  @Override
  public JsonLocation getTokenLocation() {
    int offset = tokenDecoder.tokenOffset();

    return new JsonLocation(null, offset, 0, 0, offset);
  }

  @Override
  public JsonLocation getCurrentLocation() {
    int offset = tokenDecoder.currentOffset();

    return new JsonLocation(null, offset, 0, 0, offset);
  }

  @Override
  public void overrideCurrentName(String name) {
    // Simple, but need to look for START_OBJECT/ARRAY's "off-by-one" thing:
    SerealReaderContext ctxt = parsingContext;
    if (_currToken == JsonToken.START_OBJECT || _currToken == JsonToken.START_ARRAY) {
      ctxt = ctxt.getParent();
    }
    ctxt.setCurrentName(name);
  }

  @Override
  public String getText() throws IOException {
    switch (serealToken) {
      case UTF8:
        return new String(
          tokenDecoder.decoderBuffer(),
          tokenDecoder.binarySliceStart(),
          tokenDecoder.binarySliceLength(),
          StandardCharsets.UTF_8
        );
      case BINARY:
      case VSTRING: // the v-string spelling, e.g. "v1.2.3"
        return new String(
          tokenDecoder.decoderBuffer(),
          tokenDecoder.binarySliceStart(),
          tokenDecoder.binarySliceLength(),
          StandardCharsets.ISO_8859_1
        );
      case FLOAT:
        return String.valueOf(tokenDecoder.floatValue());
      case DOUBLE:
        return String.valueOf(tokenDecoder.doubleValue());
      case LONG:
        return String.valueOf(tokenDecoder.longValue());
      case UNSIGNED_LONG:
        return tokenDecoder.bigintValue().toString();
      case REGEXP:
      case FLOAT128:
        return null; // so getEmbeddedObject is called
      case ARRAY_START:
        return "[";
      case ARRAY_END:
        return "]";
      case HASH_START:
        return "{";
      case HASH_END:
        return "}";
      case NONE:
        return null;
      default:
        throw this._formatError("Unable to coerce Sereal token %s to a string", serealToken);
    }
  }

  @Override
  public char[] getTextCharacters() throws IOException {
    throw this._constructError("getTextCharacters called even if hasTextCharacters returned false");
  }

  @Override
  public boolean hasTextCharacters() {
    return false;
  }

  @Override
  public Number getNumberValue() throws IOException {
    switch (serealToken) {
      case LONG:
        return tokenDecoder.longValue();
      case FLOAT:
        return tokenDecoder.floatValue();
      case DOUBLE:
        return tokenDecoder.doubleValue();
      case UNSIGNED_LONG:
        return tokenDecoder.bigintValue();
      default:
        throw this._formatError("Unable to coerce Sereal token %s to a number", serealToken);
    }
  }

  @Override
  public NumberType getNumberType() throws IOException {
    switch (serealToken) {
      case LONG:
        return NumberType.LONG;
      case FLOAT:
        return NumberType.FLOAT;
      case DOUBLE:
        return NumberType.DOUBLE;
      case UNSIGNED_LONG:
        return NumberType.BIG_INTEGER;
      default:
        throw this._formatError("Unable to coerce Sereal token %s to a number", serealToken);
    }
  }

  @Override
  public int getIntValue() throws IOException {
    return (int) getLongValue();
  }

  @Override
  public long getLongValue() throws IOException {
    switch (serealToken) {
      case LONG:
      case UNSIGNED_LONG:
        return tokenDecoder.longValue();
      case DOUBLE:
        return (long) tokenDecoder.doubleValue();
      case FLOAT:
        return (long) tokenDecoder.floatValue();
      default:
        throw this._formatError("Unable to coerce Sereal token %s to an integer", serealToken);
    }
  }

  @Override
  public BigInteger getBigIntegerValue() throws IOException {
    switch (serealToken) {
      case DOUBLE:
        return BigInteger.valueOf((long) tokenDecoder.doubleValue());
      case FLOAT:
        return BigInteger.valueOf((long) tokenDecoder.floatValue());
      case LONG:
        return BigInteger.valueOf(tokenDecoder.longValue());
      case UNSIGNED_LONG:
        return tokenDecoder.bigintValue();
      default:
        throw this._formatError("Unable to coerce Sereal token %s to a big integer", serealToken);
    }
  }

  @Override
  public float getFloatValue() throws IOException {
    switch (serealToken) {
      case FLOAT:
        return tokenDecoder.floatValue();
      case DOUBLE:
        return (float) tokenDecoder.doubleValue();
      case LONG:
        return tokenDecoder.longValue();
      case UNSIGNED_LONG:
        return tokenDecoder.bigintValue().floatValue();
      default:
        throw this._formatError("Unable to coerce Sereal token %s to a float", serealToken);
    }
  }

  @Override
  public double getDoubleValue() throws IOException {
    switch (serealToken) {
      case DOUBLE:
        return tokenDecoder.doubleValue();
      case FLOAT:
        return tokenDecoder.floatValue();
      case LONG:
        return tokenDecoder.longValue();
      case UNSIGNED_LONG:
        return tokenDecoder.bigintValue().doubleValue();
      default:
        throw this._formatError("Unable to coerce Sereal token %s to a double", serealToken);
    }
  }

  @Override
  public BigDecimal getDecimalValue() throws IOException {
    switch (serealToken) {
      case DOUBLE:
        return new BigDecimal(tokenDecoder.doubleValue());
      case FLOAT:
        return new BigDecimal(tokenDecoder.floatValue());
      case LONG:
        return new BigDecimal(tokenDecoder.longValue());
      case UNSIGNED_LONG:
        return new BigDecimal(tokenDecoder.bigintValue());
      default:
        throw this._formatError("Unable to coerce Sereal token %s to a decimal", serealToken);
    }
  }

  @Override
  public Object getEmbeddedObject() throws IOException {
    try {
      switch (serealToken) {
        case REGEXP:
          String pattern = new String(
            tokenDecoder.decoderBuffer(),
            tokenDecoder.binarySliceStart(),
            tokenDecoder.binarySliceLength(),
            tokenDecoder.binaryIsUtf8() ? StandardCharsets.UTF_8 : StandardCharsets.ISO_8859_1
          );
          int flags = TokenDecoder.decodeRegexpFlags(
            tokenDecoder.decoderBuffer(),
            tokenDecoder.regexpFlagsSliceStart(),
            tokenDecoder.regexpFlagsSliceEnd()
          );

          try {
            return Pattern.compile(pattern, flags);
          } catch (PatternSyntaxException e) {
            throw this._constructError("Sereal format error: invalid regexp", e);
          }
        case FLOAT128:
          return new Float128(Arrays.copyOfRange(
            tokenDecoder.decoderBuffer(),
            tokenDecoder.binarySliceStart(),
            tokenDecoder.binarySliceEnd()
          ));
        default:
          return null;
      }
    } catch (SerealException e) {
      throw this._constructError("Sereal format error", e);
    }
  }

  @Override
  public int getTextLength() throws IOException {
    throw this._constructError("getTextLength called even if hasTextCharacters returned false");
  }

  @Override
  public int getTextOffset() throws IOException {
    throw this._constructError("getTextOffset called even if hasTextCharacters returned false");
  }

  @Override
  public byte[] getBinaryValue(Base64Variant base64Variant) throws IOException {
    switch (serealToken) {
      case BINARY:
      case UTF8:
        return Arrays.copyOfRange(
          tokenDecoder.decoderBuffer(),
          tokenDecoder.binarySliceStart(),
          tokenDecoder.binarySliceEnd()
        );
      default:
        throw this._formatError("Unable to coerce Sereal token %s to a byte array", serealToken);
    }
  }

  private JsonParseException _formatError(String format, Object arg) {
    return this._constructError(String.format(format, arg));
  }
}
