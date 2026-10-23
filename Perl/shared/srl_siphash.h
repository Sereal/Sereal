/*
 * Small, portable SipHash implementation shared by the Encoder and Decoder.
 *
 * Version 0 is SipHash-2-4-64, version 1 is SipHash-2-4-128, and version 2
 * is SipHash-3-6-128.
 * The 128-bit output follows the SipHash-128 initialization/finalization
 * convention (v1 ^= 0xee, then v2 ^= 0xee and v1 ^= 0xdd).
 */
#ifndef SRL_SIPHASH_H
#define SRL_SIPHASH_H

#include <stddef.h>

/* Older Perl headers expose U64 only to the interpreter itself. */
typedef U64TYPE srl_u64;

typedef struct {
    srl_u64 v0, v1, v2, v3;
    srl_u64 total;
    U8 tail[8];
    U8 tail_len;
    U8 compression_rounds;
    U8 finalization_rounds;
    U8 is_128_bit;
} srl_siphash_ctx;

SRL_STATIC_INLINE srl_u64
srl_siphash_rotl64(srl_u64 x, unsigned int b)
{
    return (x << b) | (x >> (64 - b));
}

SRL_STATIC_INLINE srl_u64
srl_siphash_load_le64(const U8 *p)
{
    return ((srl_u64)p[0])       | ((srl_u64)p[1] << 8)  |
           ((srl_u64)p[2] << 16) | ((srl_u64)p[3] << 24) |
           ((srl_u64)p[4] << 32) | ((srl_u64)p[5] << 40) |
           ((srl_u64)p[6] << 48) | ((srl_u64)p[7] << 56);
}

#define SRL_SIPHASH_ROUND(ctx) do { \
    (ctx)->v0 += (ctx)->v1; (ctx)->v1 = srl_siphash_rotl64((ctx)->v1, 13); \
    (ctx)->v1 ^= (ctx)->v0; (ctx)->v0 = srl_siphash_rotl64((ctx)->v0, 32); \
    (ctx)->v2 += (ctx)->v3; (ctx)->v3 = srl_siphash_rotl64((ctx)->v3, 16); \
    (ctx)->v3 ^= (ctx)->v2; \
    (ctx)->v0 += (ctx)->v3; (ctx)->v3 = srl_siphash_rotl64((ctx)->v3, 21); \
    (ctx)->v3 ^= (ctx)->v0; \
    (ctx)->v2 += (ctx)->v1; (ctx)->v1 = srl_siphash_rotl64((ctx)->v1, 17); \
    (ctx)->v1 ^= (ctx)->v2; (ctx)->v2 = srl_siphash_rotl64((ctx)->v2, 32); \
} while (0)

SRL_STATIC_INLINE void
srl_siphash_compress(srl_siphash_ctx *ctx, srl_u64 m)
{
    unsigned int i;
    ctx->v3 ^= m;
    for (i = 0; i < ctx->compression_rounds; i++)
        SRL_SIPHASH_ROUND(ctx);
    ctx->v0 ^= m;
}

SRL_STATIC_INLINE void
srl_siphash_init_rounds(srl_siphash_ctx *ctx, const U8 key[16],
                        U8 compression_rounds, U8 finalization_rounds,
                        int is_128_bit)
{
    srl_u64 k0 = srl_siphash_load_le64(key);
    srl_u64 k1 = srl_siphash_load_le64(key + 8);
    ctx->v0 = ((srl_u64)0x736f6d6570736575ULL) ^ k0;
    ctx->v1 = ((srl_u64)0x646f72616e646f6dULL) ^ k1;
    ctx->v2 = ((srl_u64)0x6c7967656e657261ULL) ^ k0;
    ctx->v3 = ((srl_u64)0x7465646279746573ULL) ^ k1;
    if (is_128_bit)
        ctx->v1 ^= (srl_u64)0xee;
    ctx->total = 0;
    ctx->tail_len = 0;
    ctx->compression_rounds = compression_rounds;
    ctx->finalization_rounds = finalization_rounds;
    ctx->is_128_bit = (U8)(is_128_bit != 0);
}

SRL_STATIC_INLINE void
srl_siphash_init(srl_siphash_ctx *ctx, const U8 key[16], U8 version)
{
    if (version == SRL_CHECKSUM_VERSION_SIPHASH_3_6_128)
        srl_siphash_init_rounds(ctx, key, 3, 6, 1);
    else if (version == SRL_CHECKSUM_VERSION_SIPHASH_2_4_128)
        srl_siphash_init_rounds(ctx, key, 2, 4, 1);
    else
        srl_siphash_init_rounds(ctx, key, 2, 4, 0);
}

/* Zero means an unsupported version; callers must validate before indexing
 * key slots or using a tag length. */
SRL_STATIC_INLINE size_t
srl_checksum_tag_length(U8 version)
{
    if (version == SRL_CHECKSUM_VERSION_SIPHASH_2_4_64)
        return 8;
    if (version == SRL_CHECKSUM_VERSION_SIPHASH_2_4_128
            || version == SRL_CHECKSUM_VERSION_SIPHASH_3_6_128)
        return 16;
    return 0;
}

SRL_STATIC_INLINE void
srl_siphash_update(srl_siphash_ctx *ctx, const U8 *data, size_t len)
{
    size_t i = 0;
    ctx->total += (srl_u64)len;

    if (ctx->tail_len) {
        while (i < len && ctx->tail_len < 8)
            ctx->tail[ctx->tail_len++] = data[i++];
        if (ctx->tail_len == 8) {
            srl_siphash_compress(ctx, srl_siphash_load_le64(ctx->tail));
            ctx->tail_len = 0;
        }
    }

    while (len - i >= 8) {
        srl_siphash_compress(ctx, srl_siphash_load_le64(data + i));
        i += 8;
    }
    while (i < len)
        ctx->tail[ctx->tail_len++] = data[i++];
}

SRL_STATIC_INLINE srl_u64
srl_siphash_final64(srl_siphash_ctx *ctx)
{
    srl_u64 b = (ctx->total & (srl_u64)0xff) << 56;
    unsigned int i;
    for (i = 0; i < ctx->tail_len; i++)
        b |= (srl_u64)ctx->tail[i] << (8 * i);
    srl_siphash_compress(ctx, b);
    ctx->v2 ^= (srl_u64)0xff;
    for (i = 0; i < ctx->finalization_rounds; i++)
        SRL_SIPHASH_ROUND(ctx);
    return ctx->v0 ^ ctx->v1 ^ ctx->v2 ^ ctx->v3;
}

SRL_STATIC_INLINE void
srl_siphash_final128(srl_siphash_ctx *ctx, U8 out[16])
{
    srl_u64 b = (ctx->total & (srl_u64)0xff) << 56;
    srl_u64 first, second;
    unsigned int i;
    for (i = 0; i < ctx->tail_len; i++)
        b |= (srl_u64)ctx->tail[i] << (8 * i);
    srl_siphash_compress(ctx, b);
    ctx->v2 ^= (srl_u64)0xee;
    for (i = 0; i < ctx->finalization_rounds; i++)
        SRL_SIPHASH_ROUND(ctx);
    first = ctx->v0 ^ ctx->v1 ^ ctx->v2 ^ ctx->v3;
    ctx->v1 ^= (srl_u64)0xdd;
    for (i = 0; i < ctx->finalization_rounds; i++)
        SRL_SIPHASH_ROUND(ctx);
    second = ctx->v0 ^ ctx->v1 ^ ctx->v2 ^ ctx->v3;
    for (i = 0; i < 8; i++) {
        out[i] = (U8)(first >> (8 * i));
        out[i + 8] = (U8)(second >> (8 * i));
    }
}

SRL_STATIC_INLINE void
srl_siphash_prepare_key(const U8 *key, size_t key_len,
                        const U8 *opt_key, size_t opt_key_len, U8 out[16])
{
    static const U8 initial_key[] = "Call me Ishmael.";
    static const U8 first_prefix[] = "Out damned spot!";
    static const U8 second_prefix[] = "Luke, I am your father.";
    U8 intermediate[16];
    srl_siphash_ctx ctx;

    srl_siphash_init_rounds(&ctx, initial_key, 2, 4, 1);
    srl_siphash_update(&ctx, first_prefix, sizeof(first_prefix) - 1);
    srl_siphash_update(&ctx, opt_key, opt_key_len);
    srl_siphash_final128(&ctx, intermediate);

    srl_siphash_init_rounds(&ctx, intermediate, 2, 4, 1);
    srl_siphash_update(&ctx, second_prefix, sizeof(second_prefix) - 1);
    srl_siphash_update(&ctx, key, key_len);
    srl_siphash_final128(&ctx, out);
}

#undef SRL_SIPHASH_ROUND

#endif /* SRL_SIPHASH_H */
