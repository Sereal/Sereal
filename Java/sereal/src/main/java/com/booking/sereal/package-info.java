/**
 * Encoder/Decoder interface for <a href="https://github.com/Sereal/Sereal">Sereal</a>
 * <p>
 * See {@code com.booking.sereal.jackson.SerealObjectMapper} for a higher-level, easier to use interface.
 * <p>
 * The main entry points are {@link com.booking.sereal.Encoder} and {@link com.booking.sereal.Decoder},
 * which allow simple encoding and decoding of nested hash/array data structures, and mirror
 * the functionality available in the Perl implementation.
 * <p>
 * {@link com.booking.sereal.TokenEncoder} and {@link com.booking.sereal.TokenDecoder} offer a low-level
 * interface that can be used to build custom encoders/decoders, but is generally inconvenient and
 * error-prone when used directly.
 * <p>
 * Protocol versions 1 to 6 are supported; protocol version 6 documents can carry a keyed checksum,
 * configured through the {@code checksumV*Key} and {@code checksumPolicy} options and the
 * {@link com.booking.sereal.SerealChecksum} helpers.
 */
package com.booking.sereal;