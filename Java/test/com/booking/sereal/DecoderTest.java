package com.booking.sereal;

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.instanceOf;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThat;

import java.lang.ref.WeakReference;
import java.util.List;
import org.junit.Test;

public class DecoderTest {
  @Test
  public void weakObjectRefWithPerlRefs() throws SerealException {
    Decoder decoder = new Decoder(new DecoderOptions().perlReferences(true));

    decoder.setData(new byte[] { 0x3d, (byte) 0xf3, 0x72, 0x6c, 0x04, 0x00, 0x42, 0x2c, 0x63, 0x46, 0x6f, 0x6f, 0x28, (byte) 0xaa, 0x00, 0x30, 0x29, 0x08 });
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

    decoder.setData(new byte[] { 0x3d, (byte) 0xf3, 0x72, 0x6c, 0x04, 0x00, 0x42, 0x2c, 0x63, 0x46, 0x6f, 0x6f, 0x28, (byte) 0xaa, 0x00, 0x30, 0x29, 0x08 });
    Object decoded = decoder.decode();

    assertThat(decoded, instanceOf(List.class));
    List<?> list = (List<?>) decoded;
    assertThat(list.size(), equalTo(2));
    Object first = list.get(0), second = list.get(1);

    assertThat(first, instanceOf(PerlObject.class));
    assertThat(second, instanceOf(WeakReference.class));
    assertSame(((PerlObject) first).getData(), ((WeakReference) second).get());
  }
}
