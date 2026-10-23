/*
 * Shared SipHash implementation checks.
 *
 * The SipHash-2-4 values are the reference vectors from
 * https://github.com/veorq/SipHash. The 3-6-128 check compares one-shot input
 * with differently chunked updates; its sample outputs are printed so they
 * can be recorded as local regression vectors.
 */
#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

typedef uint8_t U8;
typedef uint64_t U64;
#define SRL_STATIC_INLINE static inline
#define SRL_CHECKSUM_VERSION_SIPHASH_3_6_128 1
#include "../srl_siphash.h"

static const U64 siphash24_vectors[64] = {
    0x726fdb47dd0e0e31ULL, 0x74f839c593dc67fdULL, 0x0d6c8009d9a94f5aULL, 0x85676696d7fb7e2dULL,
    0xcf2794e0277187b7ULL, 0x18765564cd99a68dULL, 0xcbc9466e58fee3ceULL, 0xab0200f58b01d137ULL,
    0x93f5f5799a932462ULL, 0x9e0082df0ba9e4b0ULL, 0x7a5dbbc594ddb9f3ULL, 0xf4b32f46226bada7ULL,
    0x751e8fbc860ee5fbULL, 0x14ea5627c0843d90ULL, 0xf723ca908e7af2eeULL, 0xa129ca6149be45e5ULL,
    0x3f2acc7f57c29bdbULL, 0x699ae9f52cbe4794ULL, 0x4bc1b3f0968dd39cULL, 0xbb6dc91da77961bdULL,
    0xbed65cf21aa2ee98ULL, 0xd0f2cbb02e3b67c7ULL, 0x93536795e3a33e88ULL, 0xa80c038ccd5ccec8ULL,
    0xb8ad50c6f649af94ULL, 0xbce192de8a85b8eaULL, 0x17d835b85bbb15f3ULL, 0x2f2e6163076bcfadULL,
    0xde4daaaca71dc9a5ULL, 0xa6a2506687956571ULL, 0xad87a3535c49ef28ULL, 0x32d892fad841c342ULL,
    0x7127512f72f27cceULL, 0xa7f32346f95978e3ULL, 0x12e0b01abb051238ULL, 0x15e034d40fa197aeULL,
    0x314dffbe0815a3b4ULL, 0x027990f029623981ULL, 0xcadcd4e59ef40c4dULL, 0x9abfd8766a33735cULL,
    0x0e3ea96b5304a7d0ULL, 0xad0c42d6fc585992ULL, 0x187306c89bc215a9ULL, 0xd4a60abcf3792b95ULL,
    0xf935451de4f21df2ULL, 0xa9538f0419755787ULL, 0xdb9acddff56ca510ULL, 0xd06c98cd5c0975ebULL,
    0xe612a3cb9ecba951ULL, 0xc766e62cfcadaf96ULL, 0xee64435a9752fe72ULL, 0xa192d576b245165aULL,
    0x0a8787bf8ecb74b2ULL, 0x81b3e73d20b49b6fULL, 0x7fa8220ba3b2eceaULL, 0x245731c13ca42499ULL,
    0xb78dbfaf3a8d83bdULL, 0xea1ad565322a1a0bULL, 0x60e61c23a3795013ULL, 0x6606d7e446282b93ULL,
    0x6ca4ecb15c5f91e1ULL, 0x9f626da15c9625f3ULL, 0xe51b38608ef25f57ULL, 0x958a324ceb064572ULL
};

static const size_t siphash36_128_lengths[] = { 0, 1, 15, 16, 63 };
static const U8 siphash36_128_vectors[][16] = {
    { 0x37,0x22,0x8a,0x2b,0x7a,0x1e,0x50,0x5b,0x60,0xae,0x19,0xc4,0x86,0x95,0xeb,0x27 },
    { 0x3a,0x4b,0xa0,0x8b,0x5e,0x26,0xad,0xc3,0x5d,0x3a,0x78,0x15,0xe8,0x06,0x42,0x65 },
    { 0x5d,0x0f,0x3f,0x22,0x69,0xd0,0xf3,0xa4,0x11,0x54,0x40,0xcd,0xc7,0x4c,0x25,0x0d },
    { 0x7a,0x16,0xfb,0x88,0x79,0x60,0xa8,0xa5,0x41,0xe6,0x7e,0x60,0xab,0x16,0xc7,0x27 },
    { 0xd2,0xe0,0x09,0x88,0x7d,0x83,0xd3,0x47,0xed,0x8c,0x02,0x4a,0x91,0x9a,0x35,0xad }
};

int
main(void)
{
    U8 key[16], input[64];
    size_t i, len;

    for (i = 0; i < sizeof(key); i++)
        key[i] = (U8)i;
    for (i = 0; i < sizeof(input); i++)
        input[i] = (U8)i;

    for (len = 0; len < 64; len++) {
        srl_siphash_ctx ctx;
        srl_siphash_init_rounds(&ctx, key, 2, 4, 0);
        srl_siphash_update(&ctx, input, len);
        if (srl_siphash_final64(&ctx) != siphash24_vectors[len]) {
            fprintf(stderr, "SipHash-2-4 vector %lu failed\n", (unsigned long)len);
            return 1;
        }
    }

    /* Verify incremental update handling for SipHash-3-6-128 across all tail
     * and block boundaries. Emit a handful of stable outputs for regression. */
    for (len = 0; len < 64; len++) {
        srl_siphash_ctx one_shot, chunked;
        U8 expected[16], actual[16];
        size_t first = len / 3;
        size_t second = (len - first) / 2;
        srl_siphash_init(&one_shot, key, 1);
        srl_siphash_update(&one_shot, input, len);
        srl_siphash_final128(&one_shot, expected);

        srl_siphash_init(&chunked, key, 1);
        srl_siphash_update(&chunked, input, first);
        srl_siphash_update(&chunked, input + first, second);
        srl_siphash_update(&chunked, input + first + second, len - first - second);
        srl_siphash_final128(&chunked, actual);
        if (memcmp(expected, actual, sizeof(expected)) != 0) {
            fprintf(stderr, "SipHash-3-6-128 chunked vector %lu failed\n", (unsigned long)len);
            return 1;
        }
        for (i = 0; i < sizeof(siphash36_128_lengths) / sizeof(siphash36_128_lengths[0]); i++) {
            if (len == siphash36_128_lengths[i] &&
                memcmp(expected, siphash36_128_vectors[i], sizeof(expected)) != 0) {
                fprintf(stderr, "SipHash-3-6-128 stored vector %lu failed\n", (unsigned long)len);
                return 1;
            }
        }
    }

    puts("SipHash-2-4 reference vectors and SipHash-3-6-128 vectors passed");
    return 0;
}
