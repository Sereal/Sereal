package com.booking.sereal.jackson;

import com.booking.sereal.DecoderOptions;
import com.booking.sereal.EncoderOptions;
import com.fasterxml.jackson.core.Version;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.module.SimpleModule;
import java.util.regex.Pattern;

/**
 * Simple {@link com.fasterxml.jackson.databind.ObjectMapper} subclass using Perl coercion rules.
 * <p>
 * The standard coercions performed by Jackson treat some common Perl idioms as errors (e.g. passing a
 * numeric {@code 0} value in place of a {@code boolean}).
 * <p>
 * {@code SerealObjectMapper} instances use {@link SerealBooleanDeserializer}, {@link SerealBytesDeserializer} and
 * {@link SerealPatternSerializer} to provide a smoother integration.
 */
public class SerealObjectMapper extends ObjectMapper {
  /**
   * Create a new instance with default encoder/decoder options.
   */
  public SerealObjectMapper() {
    super(new SerealFactory());
    registerSerealModule();
  }

  /**
   * Create a new instance with the specified encoder/decoder options.
   *
   * @param encoderOptions {@code null} to use default options.
   * @param decoderOptions {@code null} to use default options.
   */
  public SerealObjectMapper(EncoderOptions encoderOptions, DecoderOptions decoderOptions) {
    super(new SerealFactory(encoderOptions,decoderOptions));
    registerSerealModule();
  }

  protected SerealObjectMapper(SerealObjectMapper src) {
    super(src);
  }

  @Override
  public Version version() {
    return PackageVersion.VERSION;
  }

  @Override
  public SerealObjectMapper copy() {
    _checkInvalidCopy(SerealObjectMapper.class);
    return new SerealObjectMapper(this);
  }

  private void registerSerealModule() {
    SimpleModule m = new SimpleModule("SerealPatternModule", PackageVersion.VERSION);
    m.addSerializer(Pattern.class, new SerealPatternSerializer());
    m.addDeserializer(Boolean.TYPE, new SerealBooleanDeserializer());
    m.addDeserializer(Boolean.class, new SerealBooleanDeserializer());
    m.addDeserializer(byte[].class, new SerealBytesDeserializer());
    registerModule(m);
  }
}
