package com.booking.sereal.jackson;

import com.booking.sereal.TokenDecoder;
import com.fasterxml.jackson.core.JsonStreamContext;
import java.nio.charset.StandardCharsets;

class SerealReaderContext extends JsonStreamContext {
  private final SerealReaderContext parent;
  private Object currentValue;
  private String currentName;
  private byte[] buffer;
  private int nameStart, nameLength;
  private boolean nameIsUtf8;

  SerealReaderContext() {
    this.parent = null;
  }

  SerealReaderContext(SerealReaderContext parent, int type, int index) {
    super(type, index);
    this.parent = parent;
  }

  void nextIndex() {
    _index++;
  }

  void setCurrentName(TokenDecoder decoder) {
    currentName = null;
    buffer = decoder.decoderBuffer();
    nameStart = decoder.binarySliceStart();
    nameLength = decoder.binarySliceLength();
    nameIsUtf8 = decoder.binaryIsUtf8();
  }

  @Override
  public SerealReaderContext getParent() {
    return parent;
  }

  @Override
  public String getCurrentName() {
    if (currentName == null && buffer != null) {
      currentName = new String(buffer, nameStart, nameLength, nameIsUtf8 ? StandardCharsets.UTF_8 : StandardCharsets.ISO_8859_1);
    }
    return currentName;
  }

  void setCurrentName(String currentName) {
    this.currentName = currentName;
  }

  @Override
  public boolean hasCurrentName() {
    return buffer != null;
  }

  @Override
  public Object getCurrentValue() {
    return currentValue;
  }

  @Override
  public void setCurrentValue(Object value) {
    currentValue = value;
  }
}
