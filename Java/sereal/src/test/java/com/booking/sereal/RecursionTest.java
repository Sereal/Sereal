package com.booking.sereal;

import org.junit.Test;

import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.fail;

public class RecursionTest {

    private Map<Integer, Object> createNestedMap(int levels) {
        Map<Integer, Object> prev = new HashMap<>();
        for (int i = levels; i > 1; i--) {
            Map<Integer, Object> current = new HashMap<>();
            current.put(i, prev);
            prev = current;
        }

        Map<Integer, Object> result = new HashMap<>();
        result.put(1, prev);

        return result;
    }

    private List<Object> createNestedArray(int levels) {
        int objLevels = 0;
        List<Object> prev = new ArrayList<>();
        for (int i = levels; i > 1; i--) {
            List<Object> current = new ArrayList<>();
            current.add(prev);
            prev = current;
            objLevels++;
        }

        List<Object> result = new ArrayList<>();
        result.add(prev);
        objLevels++;

        return result;
    }

    @Test
    public void nestedMapsEncoder() {
        int recursionLevel = 10;
        Encoder encoder = new Encoder(new EncoderOptions()
                .protocolVersion(4)
                .maxRecursionDepth(recursionLevel));
        Map<Integer, Object> nestedMap = createNestedMap(recursionLevel + 1);
        try {
            byte[] encoded = encoder.write(nestedMap).getData();
            fail("Expected recursion limit exception");
        } catch (SerealException ex) {
            assertEquals("Reached recursion limit (10) during serialization", ex.getMessage());
        }
    }

    @Test
    public void nestedMapsDecoder() throws SerealException {
        int recursionLevel = 10;
        Encoder encoder = new Encoder(new EncoderOptions()
                .protocolVersion(4));
        Map<Integer, Object> nestedMap = createNestedMap(recursionLevel + 1);
        byte[] encoded = encoder.write(nestedMap).getData();

        Decoder decoder = new Decoder(new DecoderOptions()
                .maxRecursionDepth(recursionLevel)
        );
        decoder.setData(encoded);

        try {
            Map<Integer, Object> decodedData = (Map<Integer, Object>) decoder.decode();
            fail("Expected recursion limit exception");
        } catch (SerealException ex) {
            assertEquals("Reached recursion limit (10) during deserialization", ex.getMessage());
        }
    }

    @Test
    public void flatMapEncoder() throws SerealException {
        Encoder encoder = new Encoder(new EncoderOptions()
                .protocolVersion(4)
                .maxRecursionDepth(0));
        Map<String, Object> flatMap = new HashMap<>();
        for (int i = 0; i < 10; i++) {
            flatMap.put(String.valueOf(i), "Some string with value " + i);
        }
        encoder.write(flatMap);
        ByteArray encoded = encoder.getDataReference();
        // Arriving here, means that no exception was thrown
        assertNotNull(encoded);
    }

    @Test
    public void flatMapDecoder() throws SerealException {
        Encoder encoder = new Encoder(new EncoderOptions()
                .protocolVersion(4));
        Map<String, Object> flatMap = new HashMap<>();
        for (int i = 0; i < 10; i++) {
            flatMap.put(String.valueOf(i), "Some string with value " + i);
        }
        flatMap.put("byte array", "A string as a byte array".getBytes());
        flatMap.put("List", Arrays.asList("a", "b", "c", "d", "e", "f", "g"));

        byte[] encoded = encoder.write(flatMap).getData();

        Decoder decoder = new Decoder(new DecoderOptions()
                // See DecoderOptions.maxRecursionDepth() documentation
                .maxRecursionDepth(3)
        );
        decoder.setData(encoded);
        Map<String, Object> decodedData = (Map<String, Object>) decoder.decode();
        // Arriving here, means that no exception was thrown
        assertNotNull(decodedData);
    }

    @Test
    public void nestedArraysEncoder() {
        int recursionLevel = 10;
        Encoder encoder = new Encoder(new EncoderOptions()
                .protocolVersion(4)
                .maxRecursionDepth(recursionLevel));
        List<Object> nestedArray = createNestedArray(recursionLevel + 1);
        try {
            byte[] encoded = encoder.write(nestedArray).getData();
            fail("Expected recursion limit exception");
        } catch (SerealException ex) {
            assertEquals("Reached recursion limit (10) during serialization", ex.getMessage());
        }
    }

    @Test
    public void nestedArraysDecoder() throws SerealException {
        int recursionLevel = 10;
        Encoder encoder = new Encoder(new EncoderOptions()
                .protocolVersion(4));
        List<Object> nestedArray = createNestedArray(recursionLevel + 1);
        byte[] encoded = encoder.write(nestedArray).getData();

        Decoder decoder = new Decoder(new DecoderOptions()
                .maxRecursionDepth(recursionLevel)
        );
        decoder.setData(encoded);

        try {
            List<Object> decodedData = (List<Object>) decoder.decode();
            fail("Expected recursion limit exception");
        } catch (SerealException ex) {
            assertEquals("Reached recursion limit (10) during deserialization", ex.getMessage());
        }
    }

    @Test
    public void stackOverflowEncoder() throws SerealException {
        Encoder encoder = new Encoder(new EncoderOptions()
                .protocolVersion(4));
        List<Object> nestedArray = createNestedArray(10_000);
        boolean throwException = false;
        try {
            byte[] encoded = encoder.write(nestedArray).getData();
        } catch (SerealException ex) {
            assertEquals("StackOverflowError: Reached recursion limit during serialization", ex.getMessage());
            throwException = true;
        }

        if (!throwException) {
            fail("Expected StackOverflowError");
        }
    }


    @Test
    public void stackOverflowDecoder() throws SerealException {
        Encoder encoder = new Encoder(new EncoderOptions()
                .protocolVersion(4)
                .compressionType(EncoderOptions.CompressionType.NONE)
        );

        /* The next code is kind of the same of doing:
            List<Object> nestedArray = createNestedArray(1000);
            byte[] encoded = encoder.write(nestedArray).getData();
           But the previous code would throw a StackOverflow
        */
        ByteBuffer buffer = ByteBuffer.allocate(40_000);
        buffer.put(new byte[] {0x3D, (byte)0xF3, 0x72, 0x6C, 0x04, 0x00});
        for (int i = 0; i < 10_000; i++) {
            buffer.put(new byte[] {0x28, 0x2B, 0x01});
        }
        byte[] encoded = buffer.array();
        Decoder decoder = new Decoder(new DecoderOptions());
        decoder.setData(encoded);

        boolean throwException = false;
        try {
            List<Object> decodedData = (List<Object>) decoder.decode();
            fail("Expected StackOverflowError");
        } catch (SerealException ex) {
            assertEquals("StackOverflowError: Reached recursion limit during deserialization", ex.getMessage());
            throwException = true;
        }

        if (!throwException) {
            fail("Expected StackOverflowError");
        }
    }
}
