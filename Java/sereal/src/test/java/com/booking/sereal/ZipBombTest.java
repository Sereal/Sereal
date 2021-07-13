package com.booking.sereal;

import org.junit.Test;

import java.text.DecimalFormat;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

public class ZipBombTest {

    @Test
    public void zlibTooLarge() throws SerealException {
        // This array should consume 100MB of memory
        byte[] data = getHugeBlob(1024 * 1024 * 100);

        //System.out.println("--- ZLIB Bomb Test ---");
        //System.out.println("Source data size is " + sizeFormat(data.length) + " bytes");

        Encoder encoder = new Encoder(new EncoderOptions()
                .protocolVersion(4)
                .compressionThreshold(0)
                .compressionType(EncoderOptions.CompressionType.ZLIB));
        byte[] encoded = encoder.write(data).getData();

        //System.out.println("Encoded data size is " + sizeFormat(encoded.length) + " bytes");
        //System.out.println("Compression ratio is " + sizeFormat(data.length / encoded.length));

        assertEquals(0x34, encoded[4]);

        Decoder decoder = new Decoder(new DecoderOptions()
                .maxBufferSize(1024 * 1024 * 10));
        decoder.setData(encoded);

        boolean thrownException = false;
        byte[] decodedData = null;
        try {
            decodedData = (byte[]) decoder.decode();
            decoder.close();
        } catch (SerealException ex) {
            thrownException = true;
        }
        assertTrue(thrownException);
        assertNull(decodedData);
    }

    @Test
    public void snappyTooLarge() throws SerealException {
        // This array should consume 100MB of memory
        byte[] data = getHugeBlob(1024 * 1024 * 100);

        //System.out.println("--- Snappy Bomb Test ---");
        //System.out.println("Source data size is " + sizeFormat(data.length) + " bytes");

        Encoder encoder = new Encoder(new EncoderOptions()
                .protocolVersion(4)
                .compressionThreshold(0)
                .compressionType(EncoderOptions.CompressionType.SNAPPY));
        byte[] encoded = encoder.write(data).getData();
        //System.out.println("Sereal Begin       : " + bytesToHex(encoded, 25));
        //System.out.println("Encoded data size is " + sizeFormat(encoded.length) + " bytes");
        //System.out.println("Compression ratio is " + sizeFormat(data.length / encoded.length));

        assertEquals(0x24, encoded[4]);

        Decoder decoder = new Decoder(new DecoderOptions()
                .maxBufferSize(1024 * 1024 * 10));
        decoder.setData(encoded);

        boolean thrownException = false;
        byte[] decodedData = null;
        try {
            decodedData = (byte[]) decoder.decode();
        } catch (SerealException ex) {
            thrownException = true;
        }
        assertTrue(thrownException);
        assertNull(decodedData);
    }

    @Test
    public void zstdTooLarge() throws SerealException {
        // This array should consume 100MB of memory
        byte[] data = getHugeBlob(1024 * 1024 * 100);

        //System.out.println("--- ZSTD Bomb Test ---");
        //System.out.println("Source data size is " + sizeFormat(data.length) + " bytes");

        Encoder encoder = new Encoder(new EncoderOptions()
                .protocolVersion(4)
                .compressionThreshold(0)
                .compressionType(EncoderOptions.CompressionType.ZSTD));
        byte[] encoded = encoder.write(data).getData();

        //System.out.println("Encoded data size is " + sizeFormat(encoded.length) + " bytes");
        //System.out.println("Compression ratio is " + sizeFormat(data.length / encoded.length));

        assertEquals(0x44, encoded[4]);

        Decoder decoder = new Decoder(new DecoderOptions()
                .maxBufferSize(1024 * 1024 * 10));
        decoder.setData(encoded);
        boolean thrownException = false;

        byte[] decodedData = null;
        try {
            decodedData = (byte[]) decoder.decode();
        } catch (SerealException ex) {
            thrownException = true;
        }
        assertTrue(thrownException);
        assertNull(decodedData);
    }

    @Test
    public void zlibForgedSize() throws SerealException {
        // This array should consume 100MB of memory
        byte[] data = getHugeBlob(1024 * 1024 * 100);

        //System.out.println("--- ZLIB Forged Test ---");
        //System.out.println("Source data size is " + sizeFormat(data.length) + " bytes");

        Encoder encoder = new Encoder(new EncoderOptions()
                .protocolVersion(4)
                .compressionThreshold(0)
                .compressionType(EncoderOptions.CompressionType.ZLIB));
        byte[] encoded = encoder.write(data).getData();

        //System.out.println("Encoded data size is " + sizeFormat(encoded.length) + " bytes");
        //System.out.println("Compression ratio is " + sizeFormat(data.length / encoded.length));

        //System.out.println("Sereal Begin       : " + bytesToHex(encoded, 25));

        assertEquals(0x34, encoded[4]);

        // Lets validate we have the expected uncompress size: 100MB
        assertEquals((byte) 0x85, encoded[6]);
        assertEquals((byte) 0x80, encoded[7]);

        byte[] forgedArray = new byte[encoded.length - 4];
        System.arraycopy(encoded, 0, forgedArray, 0, 10);
        System.arraycopy(encoded, 14, forgedArray, 10, encoded.length - 14);

        // Forge a new uncompressed size to avoid the internal controls
        forgedArray[6] = (byte) 0x83; // uncompress size
        forgedArray[7] = (byte) 0x08; // compress size
        forgedArray[8] = (byte) 0x80; //
        forgedArray[9] = (byte) 0x32; // ???

        //System.out.println("Forged Sereal Begin: " + bytesToHex(forgedArray, 25));

        Decoder decoder = new Decoder(new DecoderOptions()
                .maxBufferSize(1024 * 1024 * 10));
        decoder.setData(forgedArray);

        boolean thrownException = false;
        byte[] decodedData = null;
        try {
            decodedData = (byte[]) decoder.decode();
            decoder.close();
        } catch (SerealException ex) {
            thrownException = true;
            assertEquals("The uncompressed size is larger than the expected size", ex.getMessage());
        }
        assertTrue(thrownException);
        assertNull(decodedData);
    }

    @Test
    public void snappyForgedSize() throws SerealException {
        // This array should consume 100MB of memory
        byte[] data = getHugeBlob(1024 * 1024 * 100);

        //System.out.println("--- Snappy Forged Test ---");
        //System.out.println("Source data size is " + sizeFormat(data.length) + " bytes");

        Encoder encoder = new Encoder(new EncoderOptions()
                .protocolVersion(4)
                .compressionThreshold(0)
                .compressionType(EncoderOptions.CompressionType.SNAPPY));
        byte[] encoded = encoder.write(data).getData();

        //System.out.println("Encoded data size is " + sizeFormat(encoded.length) + " bytes");
        //System.out.println("Compression ratio is " + sizeFormat(data.length / encoded.length));
        //System.out.println("Sereal Begin       : " + bytesToHex(encoded, 25));

        assertEquals(0x24, encoded[4]);

        // Lets validate we have the expected uncompress size: 100MB
        assertEquals((byte) 0x8F, encoded[6]);
        assertEquals((byte) 0x99, encoded[7]);

        byte[] forgedArray = new byte[encoded.length - 4];
        System.arraycopy(encoded, 0, forgedArray, 0, 10);
        System.arraycopy(encoded, 14, forgedArray, 10, encoded.length - 14);

        // Forge a new uncompressed size to avoid the internal controls
        forgedArray[6] = (byte) 0xB7; // uncompress size
        forgedArray[7] = (byte) 0x00; // uncompress size

        //System.out.println("Forged Sereal Begin: " + bytesToHex(forgedArray, 25));

        Decoder decoder = new Decoder(new DecoderOptions()
                .maxBufferSize(1024 * 1024 * 10));
        decoder.setData(forgedArray);

        boolean thrownException = false;
        byte[] decodedData = null;
        try {
            decodedData = (byte[]) decoder.decode();
        } catch (SerealException ex) {
            thrownException = true;
            assertEquals("Invalid snappy data", ex.getMessage());
        }
        assertTrue(thrownException);
        assertNull(decodedData);
    }


    @Test
    public void zstdForgedSize() throws SerealException {
        // This array should consume 100MB of memory
        byte[] data = getHugeBlob(1024 * 1024 * 100);

        //System.out.println("--- ZSTD Forged Test ---");
        //System.out.println("Source data size is " + sizeFormat(data.length) + " bytes");

        Encoder encoder = new Encoder(new EncoderOptions()
                .protocolVersion(4)
                .compressionThreshold(0)
                .compressionType(EncoderOptions.CompressionType.ZSTD));
        byte[] encoded = encoder.write(data).getData();

        //System.out.println("Encoded data size is " + sizeFormat(encoded.length) + " bytes");
        //System.out.println("Compression ratio is " + sizeFormat(data.length / encoded.length));
        //System.out.println("Sereal Begin       : " + bytesToHex(encoded, 15));

        assertEquals(0x44, encoded[4]);

        // Lets validate we have the expected uncompress size: 100MB
        assertEquals((byte) 0x9F, encoded[6]);
        assertEquals((byte) 0x99, encoded[7]);

        byte[] forgedArray = new byte[encoded.length - 4];
        System.arraycopy(encoded, 0, forgedArray, 0, 10);
        System.arraycopy(encoded, 14, forgedArray, 10, encoded.length - 14);

        // Forge a new uncompressed size to avoid the internal controls
        forgedArray[6] = (byte) 0x95; // uncompress size
        forgedArray[7] = (byte) 0x00; // compress size
        forgedArray[8] = (byte) 0x93; // ???
        forgedArray[9] = (byte) 0x00; // ???

        //System.out.println("Forged Sereal Begin: " + bytesToHex(forgedArray, 20));

        Decoder decoder = new Decoder(new DecoderOptions()
                .maxBufferSize(1024 * 1024 * 10));
        decoder.setData(encoded);

        boolean thrownException = false;
        byte[] decodedData = null;
        try {
            decodedData = (byte[]) decoder.decode();
        } catch (SerealException ex) {
            thrownException = true;
            assertEquals("The expected uncompressed size is larger than the allowed maximum size", ex.getMessage());
        }
        assertTrue(thrownException);
        assertNull(decodedData);
    }

    private String sizeFormat(int size) {
        String pattern = "###,###.##";
        DecimalFormat decimalFormat = new DecimalFormat(pattern);

        return decimalFormat.format(size);
    }

    private byte[] getHugeBlob(int size) {
        return new byte[size];
    }

    final protected static char[] hexArray = "0123456789ABCDEF".toCharArray();

    public static String bytesToHex(byte[] bytes, int length) {
        char[] hexChars = new char[length * 2];

        for (int j = 0; j < length; j++) {
            int v = bytes[j] & 0xFF;
            hexChars[j * 2] = hexArray[v >>> 4];
            hexChars[j * 2 + 1] = hexArray[v & 0x0F];
        }
        return new String(hexChars);
    }
}
