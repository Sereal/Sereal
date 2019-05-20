package com.booking.sereal.jackson;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;
import java.io.IOException;
import java.util.regex.Pattern;

/**
 * Custom serializer for {@link java.util.regex.Pattern} objects.
 */
public class SerealPatternSerializer extends JsonSerializer<Pattern> {
  @Override
  public void serialize(Pattern pattern, JsonGenerator jsonGenerator,
      SerializerProvider serializerProvider) throws IOException {
    SerealGenerator serealGenerator = (SerealGenerator) jsonGenerator;

    serealGenerator.writePattern(pattern);
  }
}
