#ifndef SRL_FLOAT_H_
#define SRL_FLOAT_H_

#include "srl_common.h"
#include "srl_protocol.h"
#include <float.h>
#include <math.h>
#include <stdio.h>

#ifndef NV_MIN_EXP
# ifdef USE_LONG_DOUBLE
#  define NV_MIN_EXP LDBL_MIN_EXP
# else
#  define NV_MIN_EXP DBL_MIN_EXP
# endif
#endif
#ifndef NV_MAX_EXP
# ifdef USE_LONG_DOUBLE
#  define NV_MAX_EXP LDBL_MAX_EXP
# else
#  define NV_MAX_EXP DBL_MAX_EXP
# endif
#endif

/* C long double is an ABI choice, so its name alone does not identify the
 * bytes that an encoder writes. These IDs describe the representations that
 * the native LONG_DOUBLE declaration can distinguish. Binary64 has 53 bits
 * of precision in 8 bytes; x87 has 64 bits of precision in 10 meaningful
 * bytes within a 12- or 16-byte object; binary128 has 113 bits in 16 bytes.
 * IBM double-double stores two binary64 components in 16 bytes.
 *
 * The high bit records byte order separately from representation. The
 * declaration also records storage size because, for example, two x87 ABIs
 * can agree on precision and byte order while disagreeing on padding size.
 * Portable FLOAT_128 does not use these IDs: its bytes always have the same
 * IEEE binary128 representation and little-endian order. */

/* Compiler size and precision identify candidate layouts, but they do not
 * establish byte or component order. The repeating binary fraction -0.1L
 * supplies nonzero bits in both double-double components, allowing the
 * byte checks to distinguish those layouts as well. Returning zero means
 * that no known format ID matches. Explicit native output can still use
 * opaque storage, whose reader must opt into trusting ABI compatibility. */
SRL_STATIC_INLINE U8
srl_native_long_double_format(void)
{
    const long double sample = -0.1L;
    const U8 *b = (const U8 *)&sample;
    U8 format = 0;
    U8 big = 0;
    if (sizeof(long double) == 8 && LDBL_MANT_DIG == 53) {
        if (b[0] == 0x9a && b[7] == 0xbf)
            format = SRL_LD_BINARY64;
        else if (b[0] == 0xbf && b[7] == 0x9a) {
            format = SRL_LD_BINARY64;
            big = 1;
        }
    } else if (sizeof(long double) == 16 && LDBL_MANT_DIG == 113) {
        if (b[0] == 0x9a && b[15] == 0xbf)
            format = SRL_LD_BINARY128;
        else if (b[0] == 0xbf && b[15] == 0x9a) {
            format = SRL_LD_BINARY128;
            big = 1;
        }
    } else if ((sizeof(long double) == 12 || sizeof(long double) == 16)
               && LDBL_MANT_DIG == 64) {
        if (b[0] == 0xcd && b[9] == 0xbf)
            format = SRL_LD_X87;
        else if (b[0] == 0xbf && b[9] == 0xcd) {
            format = SRL_LD_X87;
            big = 1;
        }
    } else if (sizeof(long double) == 16
               && (LDBL_MANT_DIG == 106 || LDBL_MANT_DIG == 107)) {
        /* The declaration has one endianness bit for the complete value.
         * These patterns therefore require component order to agree with
         * byte order; mixed native layouts cannot use this format ID. */
        if (b[0] == 0x9a && b[7] == 0x3c && b[15] == 0xbf)
            format = SRL_LD_DOUBLEDOUBLE;
        else if (b[0] == 0xbf && b[8] == 0x3c && b[15] == 0x9a) {
            format = SRL_LD_DOUBLEDOUBLE;
            big = 1;
        }
    }
    return format ? (U8)(format | (big ? SRL_LD_BIG_ENDIAN : 0)) : 0;
}

/* An opaque declaration records raw ABI storage without claiming a known
 * representation. Its byte-order bit records the producer's integer byte
 * order as diagnostic information; it cannot describe an unknown layout.
 * Readers must opt into trusting ABI compatibility before using those bytes. */
SRL_STATIC_INLINE U8
srl_native_long_double_declaration_format(void)
{
    U8 format = srl_native_long_double_format();
    if (format) return format;
#if BYTEORDER == 0x4321 || BYTEORDER == 0x87654321
    return SRL_LD_OPAQUE | SRL_LD_BIG_ENDIAN;
#else
    return SRL_LD_OPAQUE;
#endif
}

SRL_STATIC_INLINE int
srl_long_double_format_is_known(U8 format)
{
    switch (format & 127) {
        case SRL_LD_BINARY64:
        case SRL_LD_X87:
        case SRL_LD_BINARY128:
        case SRL_LD_DOUBLEDOUBLE:
            return 1;
        default:
            return 0;
    }
}

SRL_STATIC_INLINE void
srl_copy_float_little_endian(pTHX_ U8 *dst, const U8 *src)
{
    const float sample = 1.0f;
    const U8 *b = (const U8 *)&sample;
    U8 i;
    if (sizeof(float) != 4)
        croak("Unsupported native float width");
    if (b[3] == 0x3f && b[2] == 0x80)
        memcpy(dst, src, 4);
    else if (b[0] == 0x3f && b[1] == 0x80)
        for (i = 0; i < 4; i++) dst[i] = src[3 - i];
    else
        croak("Unsupported native float representation");
}

/* Some ABIs store the two words of a double in a different order from
 * their integer bytes. Inspecting the IEEE representation of 1.0 identifies
 * that arrangement directly. Each permutation is its own inverse, so the
 * same helper converts native bytes to little endian and back again. */
SRL_STATIC_INLINE void
srl_copy_double_little_endian(pTHX_ U8 *dst, const U8 *src)
{
    const double sample = 1.0;
    const U8 *b = (const U8 *)&sample;
    U8 i;
    if (sizeof(double) != 8)
        croak("Unsupported native double width");
    if (b[7] == 0x3f && b[6] == 0xf0)
        memcpy(dst, src, 8);
    else if (b[0] == 0x3f && b[1] == 0xf0)
        for (i = 0; i < 8; i++) dst[i] = src[7 - i];
    else if (b[3] == 0x3f && b[2] == 0xf0)
        for (i = 0; i < 8; i++) dst[i] = src[i ^ 4];
    else if (b[4] == 0x3f && b[5] == 0xf0)
        for (i = 0; i < 8; i++) dst[i] = src[i ^ 3];
    else
        croak("Unsupported native double representation");
}

SRL_STATIC_INLINE NV
srl_nv_scale(NV value, int exponent)
{
#ifdef Perl_ldexp
    return Perl_ldexp(value, exponent);
#elif defined(USE_LONG_DOUBLE)
    return ldexpl(value, exponent);
#else
    return ldexp(value, exponent);
#endif
}

SRL_STATIC_INLINE int
srl_binary128_bit(const U8 *bytes, int bit, int normal)
{
    if (bit == 112) return normal;
    return bit >= 0 && bit < 112 ? ((bytes[bit / 8] >> (bit % 8)) & 1) : 0;
}

/* A reader must be able to consume portable binary128 even when its Perl
 * uses double and its compiler provides no binary128 arithmetic. Decode the
 * significand as bits and round it to the NV precision before applying the
 * exponent. An intermediate conversion through double would discard bits
 * that an x87 or binary128 NV could otherwise retain.
 *
 * The number of discarded bits also accounts for the target's smallest
 * subnormal, so nearest/even rounding applies at the underflow boundary as
 * well as to normal values. Finite overflow becomes signed infinity;
 * underflow becomes a subnormal or signed zero. This conversion returns a
 * numeric NV rather than a retained wire bit pattern, so NaNs use NV_NAN
 * without mapping their payload bits between representations. */
SRL_STATIC_INLINE NV
srl_binary128_to_nv(const U8 bytes[16])
{
    const int negative = (bytes[15] & 0x80) != 0;
    const int exponent = ((bytes[15] & 0x7f) << 8) | bytes[14];
    const int scale = (exponent ? exponent - 16383 : -16382) - 112;
    int drop = 113 - NV_MANT_DIG;
    int i, sticky = 0;
    NV value = 0;
    if (exponent == 32767) {
        for (i = 0; i < 14; i++) sticky |= bytes[i];
        value = sticky ? (NV)NV_NAN : (NV)NV_INF;
        return negative ? -value : value;
    }
    if (drop < 0) drop = 0;
    if (NV_MIN_EXP - NV_MANT_DIG - scale > drop)
        drop = NV_MIN_EXP - NV_MANT_DIG - scale;
    if (drop > 113) return negative ? -(NV)0.0 : (NV)0.0;
    for (i = 112; i >= drop; i--)
        value = value * 2 + srl_binary128_bit(bytes, i, exponent != 0);
    if (drop) {
        for (i = 0; i < drop - 1; i++)
            sticky |= srl_binary128_bit(bytes, i, exponent != 0);
        if (srl_binary128_bit(bytes, drop - 1, exponent != 0)
            && (sticky || srl_binary128_bit(bytes, drop, exponent != 0)))
            value += 1;
    }
    if (value != 0) {
        int value_exponent;
        (void)Perl_frexp(value, &value_exponent);
        if (value_exponent + scale + drop > NV_MAX_EXP)
            value = (NV)NV_INF;
        else
            value = srl_nv_scale(value, scale + drop);
    }
    return negative ? -value : value;
}

SRL_STATIC_INLINE int
srl_nv_signbit(NV value)
{
#ifdef Perl_signbit
    return Perl_signbit(value) != 0;
#else
    /* Older Perls do not expose signbit on every C library. Comparisons
     * handle nonzero finite values and infinities, but cannot distinguish
     * the two zeros. Formatting preserves that sign and also handles NaN.
     * Only zero and NaN reach sprintf, so neither their magnitude nor the
     * current locale can produce an unbounded numeric expansion here. */
    char text[32];
    if (value < 0) return 1;
    if (value > 0) return 0;
    sprintf(text, "%g", (double)value);
    return text[0] == '-';
#endif
}

/* Encoding must produce the same binary128 layout for x87, binary128 and
 * double-double NVs. Arithmetic extraction avoids copying ABI-specific
 * storage or depending on a compiler binary128 type. The destination is
 * initialized before any bits are set, so native padding is never emitted.
 *
 * A double-double value can have two widely separated nonzero components.
 * Its usual precision is less than 113 bits, but the span between those
 * components can exceed a binary128 significand. Retaining a guard bit and
 * the remaining fraction allows those values to be rounded as well. */
SRL_STATIC_INLINE void
srl_nv_to_binary128(NV value, U8 bytes[16])
{
    int exponent, encoded_exp, bit, i;
    NV fraction;
    int negative = srl_nv_signbit(value);
    memset(bytes, 0, 16);
    if (Perl_isnan(value)) {
        bytes[13] = 0x80; bytes[14] = 0xff; bytes[15] = 0x7f;
    } else if (Perl_isinf(value)) {
        bytes[14] = 0xff; bytes[15] = 0x7f;
    } else if (value != 0) {
        if (negative) value = -value;
        fraction = Perl_frexp(value, &exponent);
        encoded_exp = exponent - 1 + 16383;
        if (encoded_exp >= 32767) {
            bytes[14] = 0xff; bytes[15] = 0x7f;
        } else {
            bit = 112;
            if (encoded_exp <= 0) {
                bit += encoded_exp - 1;
                encoded_exp = 0;
            }
            while (bit >= 0) {
                fraction *= 2;
                if (fraction >= 1) {
                    if (bit < 112) bytes[bit / 8] |= (U8)(1 << (bit % 8));
                    fraction -= 1;
                }
                bit--;
            }
            /* The next bit is the guard bit; the rest of the fraction
             * distinguishes an exact tie from a value above it. In a tie,
             * the retained low bit selects the even significand. */
            fraction *= 2;
            if (fraction > 1 || (fraction == 1 && (bytes[0] & 1))) {
                for (i = 0; i < 14; i++)
                    if (++bytes[i]) break;
                if (i == 14) encoded_exp++;
            }
            bytes[14] = (U8)encoded_exp;
            bytes[15] = (U8)(encoded_exp >> 8);
        }
    }
    if (negative) bytes[15] |= 0x80;
}
#endif
