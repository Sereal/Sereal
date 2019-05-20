package com.booking.sereal.jackson;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import java.io.IOException;

/**
 * Custom deserializer using Perl rules for binary value coercion.
 * <p>
 * Sereal string and binary/ISO-8859-1 values are returned as-is, numeric values are converted to their
 * default string representation, the canonical Sereal {@code true} is converted to {@code "1"} and {@code false}
 * to the empty array.
 * <p>
 * Any non-scalar value is an error.
 */
public class SerealBytesDeserializer extends JsonDeserializer<byte[]> {
  private static final byte[] FALSE = new byte[] {};

  @Override
  public byte[] deserialize(JsonParser jsonParser, DeserializationContext deserializationContext)
      throws IOException, JsonProcessingException {
    switch (jsonParser.currentToken()) {
      case VALUE_STRING:
        return jsonParser.getBinaryValue();
      case VALUE_NUMBER_INT:
        long longValue = jsonParser.getLongValue();

        return String.valueOf(longValue).getBytes();
      case VALUE_NUMBER_FLOAT:
        double doubleValue = jsonParser.getDoubleValue();

        return String.valueOf(doubleValue).getBytes();
      case VALUE_FALSE:
        return FALSE;
      case VALUE_TRUE:
        return new byte[] { '1' };
      default:
        throw new JsonParseException(jsonParser, "Unsupported token " + jsonParser.currentToken());
    }
  }
}
