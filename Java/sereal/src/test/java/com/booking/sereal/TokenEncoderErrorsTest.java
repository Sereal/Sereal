package com.booking.sereal;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

public class TokenEncoderErrorsTest {
  TokenEncoder encoder = new TokenEncoder();

  @Rule
  public ExpectedException exceptionRule = ExpectedException.none();

  @Test
  public void missingBody() throws SerealException {
    exceptionRule.expect(IllegalStateException.class);
    exceptionRule.expectMessage("Missing startDocument/endDocument calls");

    encoder.startHeader();
    encoder.appendLong(7);
    encoder.endHeader();
    encoder.getData();
  }

  @Test
  public void missingStartDocument() throws SerealException {
    exceptionRule.expect(IllegalStateException.class);
    exceptionRule.expectMessage("Missing startDocument/endDocument calls");

    encoder.appendLong(7);
    encoder.getData();
  }

  @Test
  public void missingEndDocument() throws SerealException {
    exceptionRule.expect(IllegalStateException.class);
    exceptionRule.expectMessage("Missing endHeader/endDocument call");

    encoder.startDocument();
    encoder.appendLong(7);
    encoder.getData();
  }

  @Test
  public void missingEndStructure() throws SerealException {
    exceptionRule.expect(IllegalStateException.class);
    exceptionRule.expectMessage("Missing endHash/endArray/endObject call");

    encoder.startDocument();
    encoder.startArray(2);
    encoder.appendLong(7);
    encoder.getData();
  }

  @Test
  public void documentInHeader() throws SerealException {
    exceptionRule.expect(IllegalStateException.class);
    exceptionRule.expectMessage("startDocument called while already inside startHeader/startDocument");

    encoder.startHeader();
    encoder.startDocument();
  }

  @Test
  public void headerInDocument() throws SerealException {
    exceptionRule.expect(IllegalStateException.class);
    exceptionRule.expectMessage("startHeader called while already inside startHeader/startDocument");

    encoder.startDocument();
    encoder.startHeader();
  }

  @Test
  public void startDocumentTwice() throws SerealException {
    exceptionRule.expect(IllegalStateException.class);
    exceptionRule.expectMessage("startDocument called twice");

    encoder.startDocument();
    encoder.appendLong(7);
    encoder.endDocument();
    encoder.startDocument();
  }

  @Test
  public void startHeaderTwice() throws SerealException {
    exceptionRule.expect(IllegalStateException.class);
    exceptionRule.expectMessage("startHeader called twice");

    encoder.startHeader();
    encoder.appendLong(7);
    encoder.endHeader();
    encoder.startHeader();
  }

  @Test
  public void headerAfterBody() throws SerealException {
    exceptionRule.expect(IllegalStateException.class);
    exceptionRule.expectMessage("startHeader called after emitting document body");

    encoder.startDocument();
    encoder.appendLong(7);
    encoder.endDocument();
    encoder.startHeader();
  }

  @Test
  public void endHeaderTwice() throws SerealException {
    exceptionRule.expect(IllegalStateException.class);
    exceptionRule.expectMessage("Mismatched begin/end calls");

    encoder.startDocument();
    encoder.appendLong(7);
    encoder.endDocument();
    encoder.endDocument();
  }

  @Test
  public void endDocumentTwice() throws SerealException {
    exceptionRule.expect(IllegalStateException.class);
    exceptionRule.expectMessage("Mismatched begin/end calls");

    encoder.startHeader();
    encoder.appendLong(7);
    encoder.endHeader();
    encoder.endHeader();
  }
}
