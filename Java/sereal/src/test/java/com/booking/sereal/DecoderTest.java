package com.booking.sereal;

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.instanceOf;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThat;

import com.booking.sereal.EncoderOptions.CompressionType;
import java.lang.ref.WeakReference;
import java.math.BigInteger;
import java.util.Arrays;
import java.util.List;
import org.junit.Test;

public class DecoderTest {
  @Test
  public void weakObjectRefWithPerlRefs() throws SerealException {
    Decoder decoder = new Decoder(new DecoderOptions().perlReferences(true));

    decoder.setData(TestUtils.byteArray(0x3d, 0xf3, 0x72, 0x6c, 0x04, 0x00, 0x42, 0x2c, 0x63, 0x46, 0x6f, 0x6f, 0x28, 0xaa, 0x00, 0x30, 0x29, 0x08));
    Object decoded = decoder.decode();

    assertThat(decoded, instanceOf(PerlReference.class));
    Object refValue = ((PerlReference) decoded).getValue();
    assertThat(refValue, instanceOf(List.class));
    List<?> list = (List<?>) refValue;
    assertThat(list.size(), equalTo(2));
    Object first = list.get(0), second = list.get(1);

    assertThat(first, instanceOf(PerlObject.class));
    assertThat(second, instanceOf(WeakReference.class));
    assertSame(((PerlReference) ((PerlObject) first).getData()).getValue(), ((PerlReference) ((WeakReference) second).get()).getValue());
  }

  @Test
  public void weakObjectRefWithoutPerlRefs() throws SerealException {
    Decoder decoder = new Decoder(new DecoderOptions().perlReferences(false));

    decoder.setData(TestUtils.byteArray(0x3d, 0xf3, 0x72, 0x6c, 0x04, 0x00, 0x42, 0x2c, 0x63, 0x46, 0x6f, 0x6f, 0x28, 0xaa, 0x00, 0x30, 0x29, 0x08));
    Object decoded = decoder.decode();

    assertThat(decoded, instanceOf(List.class));
    List<?> list = (List<?>) decoded;
    assertThat(list.size(), equalTo(2));
    Object first = list.get(0), second = list.get(1);

    assertThat(first, instanceOf(PerlObject.class));
    assertThat(second, instanceOf(WeakReference.class));
    assertSame(((PerlObject) first).getData(), ((WeakReference) second).get());
  }

  @Test
  public void ignoresTrailingData() throws SerealException {
    testDecodeWithTrailing(new EncoderOptions());
    testDecodeWithTrailing(
      new EncoderOptions()
        .compressionThreshold(1)
        .compressionType(CompressionType.SNAPPY));
    testDecodeWithTrailing(
      new EncoderOptions()
        .compressionThreshold(1)
        .compressionType(CompressionType.ZLIB));
    testDecodeWithTrailing(
      new EncoderOptions()
        .compressionThreshold(1)
        .compressionType(CompressionType.ZSTD));
  }

  private void testDecodeWithTrailing(EncoderOptions options) throws SerealException {
    String originalString = "abcddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd";
    Encoder encoder = new Encoder(options);
    Decoder decoder = new Decoder();

    encoder.write(originalString);
    byte[] originalData = encoder.getData();
    byte[] trailData = Arrays.copyOf(originalData, originalData.length + 10);
    System.arraycopy("XXXXXXXXXXXXXXX".getBytes(), 0, trailData, originalData.length, 10);

    decoder.setData(originalData);
    assertThat(decoder.decode(), equalTo(originalString));

    decoder.setData(trailData);
    assertThat(decoder.decode(), equalTo(originalString));
  }

  @Test
  public void testDecodeWithLargeNumber() throws Exception  {
    Decoder decoder = new Decoder();
    // the long value 11781050652756758130 is larger thant Long.MAX_VALUE, can't be represented as a Long, so it's returned as a BigInteger
    decoder.setData(TestUtils.byteArray(0x3d, 0xf3, 0x72, 0x6c, 0x4, 0x0, 0x20, 0xf2, 0x9c, 0xcd, 0xb3, 0xe6, 0xe4, 0xac, 0xbf, 0xa3, 0x01));
    Object decoded = decoder.decode();

    assertThat(decoded, instanceOf(BigInteger.class));
    assertThat(decoded.toString(), equalTo("11781050652756758130"));
  }
}
