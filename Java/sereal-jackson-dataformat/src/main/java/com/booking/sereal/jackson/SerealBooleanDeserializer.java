package com.booking.sereal.jackson;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import com.fasterxml.jackson.databind.JsonMappingException;
import java.io.IOException;

/**
 * Custom deserializer using Perl rules for {@code boolean} value coercion.
 * <p>
 * Integer and floating point {@code 0} values, and the strings {@code ""} and {@code "0"} are converted to
 * {@code false}, other numeric or string values are converted to {@code true}.
 * <p>
 * Any non-scalar value is an error.
 */
public class SerealBooleanDeserializer extends JsonDeserializer<Boolean> {
  @Override
  public Boolean deserialize(JsonParser jsonParser, DeserializationContext deserializationContext)
      throws IOException, JsonProcessingException {
    switch (jsonParser.currentToken()) {
      case VALUE_TRUE:
        return Boolean.TRUE;
      case VALUE_FALSE:
        return Boolean.FALSE;
      case VALUE_NUMBER_INT:
        int intValue = jsonParser.getIntValue();

        return Boolean.valueOf(intValue != 0);
      case VALUE_NUMBER_FLOAT:
        double doubleValue = jsonParser.getDoubleValue();

        return Boolean.valueOf(doubleValue != 0);
      case VALUE_STRING:
        String stringValue = jsonParser.getText();

        return Boolean.valueOf(!(stringValue.isEmpty() || stringValue.equals("0")));
      default:
        throw new JsonParseException(jsonParser, "Unsupported token " + jsonParser.currentToken());
    }
  }

  public Boolean getNullValue(DeserializationContext ctxt) throws JsonMappingException {
    return Boolean.FALSE;
  }
}
