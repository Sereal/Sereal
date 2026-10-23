#ifndef SRL_READER_CHECKSUM_H_
#define SRL_READER_CHECKSUM_H_

/* Reading and verifying the protocol v6 keyed checksum envelope:
 *
 *   <CHECKSUM-TYPE-U8> <CHECKSUM-LENGTH-VARINT> <CHECKSUM>
 *   <PAYLOAD-LENGTH-VARINT> <PAYLOAD>
 *
 * Shared by Sereal::Decoder and the tools that read Sereal documents
 * (Sereal::Merger, Sereal::Splitter, Sereal::Path::Iterator). */

#include "srl_inline.h"
#include "srl_common.h"
#include "srl_reader_types.h"
#include "srl_reader.h"
#include "srl_reader_error.h"
#include "srl_reader_varint.h"
#include "srl_protocol.h"
#include "srl_siphash.h"
#include "srl_checksum_policy.h"

typedef struct {
    U8 checksum_type;
    const U8 *tag;
    size_t tag_len;
    const U8 *payload_len_start;
    size_t payload_len_varint_size;
    const U8 *payload;
    size_t payload_len;
} srl_checksum_envelope_t;

SRL_STATIC_INLINE void
srl_write_u64_le(U8 *p, srl_u64 value)
{
    U8 i;
    for (i = 0; i < 8; i++)
        p[i] = (U8)(value >> (8 * i));
}

SRL_STATIC_INLINE void
srl_read_checksum_envelope(pTHX_ srl_reader_buffer_t *buf, srl_checksum_envelope_t *envelope)
{
    UV tag_len_uv, payload_len_uv;
    size_t available;
    int known_type;
    SRL_RDR_ASSERT_SPACE(buf, 1, " while reading the checksum type");
    envelope->checksum_type = *buf->pos++;
    known_type = srl_checksum_tag_length(envelope->checksum_type) != 0;
    tag_len_uv = srl_read_varint_uv(aTHX_ buf);
    envelope->tag_len = (size_t)tag_len_uv;
    if (expect_false((UV)envelope->tag_len != tag_len_uv))
        SRL_RDR_ERROR(buf, "Sereal checksum length is too large for this platform");
    available = (size_t)(buf->end - buf->pos);
    if (expect_false(envelope->tag_len > available))
        SRL_RDR_ERROR(buf, "Sereal checksum length exceeds the available input");

    if (known_type) {
        const size_t expected_len = srl_checksum_tag_length(envelope->checksum_type);
        if (expect_false(envelope->tag_len != expected_len))
            SRL_RDR_ERRORf2(buf, "Invalid checksum length %"UVuf" for checksum type %u",
                            tag_len_uv, envelope->checksum_type);
    }

    envelope->tag = buf->pos;
    envelope->payload_len_start = envelope->tag + envelope->tag_len;
    buf->pos = (U8 *)envelope->payload_len_start;
    payload_len_uv = srl_read_varint_uv(aTHX_ buf);
    envelope->payload_len_varint_size = (size_t)(buf->pos - envelope->payload_len_start);
    envelope->payload = buf->pos;
    envelope->payload_len = (size_t)payload_len_uv;
    if (expect_false((UV)envelope->payload_len != payload_len_uv))
        SRL_RDR_ERROR(buf, "Checksummed Sereal payload length is too large for this platform");
    if (expect_false(envelope->payload_len > (size_t)(buf->end - envelope->payload)))
        SRL_RDR_ERROR(buf, "Checksummed Sereal payload length exceeds the available input");

}

SRL_STATIC_INLINE int
srl_checksum_matches(pTHX_ srl_reader_buffer_t *buf, const srl_checksum_envelope_t *envelope, const U8 *key)
{
    U8 actual[SRL_CHECKSUM_TAG_LENGTH_128];
    U8 expected[SRL_CHECKSUM_TAG_LENGTH_128];
    U8 mismatch = 0;
    U8 i;
    srl_siphash_ctx sip;
    /* callers validate known types first; this keeps a future caller from overrunning the stack buffers */
    if (expect_false(envelope->tag_len > sizeof(expected)))
        SRL_RDR_ERROR(buf, "Sereal checksum tag is too long to verify");
    Copy(envelope->tag, expected, envelope->tag_len, U8);
    srl_siphash_init(&sip, key, envelope->checksum_type);
    srl_siphash_update(&sip, (const U8 *)buf->start, (size_t)(envelope->tag - buf->start));
    srl_siphash_update(&sip, envelope->payload_len_start, envelope->payload_len_varint_size);
    srl_siphash_update(&sip, envelope->payload, envelope->payload_len);
    if (envelope->tag_len == SRL_CHECKSUM_TAG_LENGTH_128) {
        srl_siphash_final128(&sip, actual);
    } else {
        srl_u64 digest = srl_siphash_final64(&sip);
        srl_write_u64_le(actual, digest);
    }
    for (i = 0; i < envelope->tag_len; i++)
        mismatch |= (U8)(actual[i] ^ expected[i]);
    return mismatch == 0;
}

/* Enforce a checksum policy on an envelope that srl_read_checksum_envelope()
 * has just read: reject unknown types and missing keys under the strict
 * policies, warn under the permissive ones, and verify the tag whenever a
 * matching key is available. Croaks on failure. */
SRL_STATIC_INLINE void
srl_checksum_apply_policy(pTHX_ srl_reader_buffer_t *buf, const srl_checksum_envelope_t *envelope,
                          const srl_checksum_config_t *cfg)
{
    const U8 policy = cfg->policy;
    int known_type, has_key, verify;
    const int strict = policy == SRL_CHECKSUM_POLICY_DIE
                    || policy == SRL_CHECKSUM_POLICY_REQUIRE;
    known_type = srl_checksum_tag_length(envelope->checksum_type) != 0;
    has_key = known_type && (cfg->key_mask & (U8)(1U << envelope->checksum_type));
    if (strict && !known_type)
        SRL_RDR_ERRORf1(buf, "Unsupported Sereal checksum type %u", envelope->checksum_type);
    if (strict && !has_key) {
        SRL_RDR_ERRORf1(buf, "Checksummed Sereal document requires a '%s'",
                       srl_checksum_key_option_name(envelope->checksum_type));
    }
    verify = has_key && (policy == SRL_CHECKSUM_POLICY_VERIFY_IF_AVAILABLE || strict);
    if (policy == SRL_CHECKSUM_POLICY_IGNORE_WARN)
        warn("Ignoring Sereal checksum authentication by request");
    else if (policy == SRL_CHECKSUM_POLICY_VERIFY_IF_AVAILABLE && !has_key)
        warn("Decoding checksummed Sereal document without authentication: no matching checksum key is configured");
    if (verify && !srl_checksum_matches(aTHX_ buf, envelope, cfg->keys[envelope->checksum_type]))
        SRL_RDR_ERROR(buf, "Sereal packet checksum verification failed");
}

/* Store one checksum key, padding or truncating it to 16 bytes with a
 * warning. The policy-specific length checks have already been done by
 * srl_validate_checksum_keys(). An undefined key leaves the slot unset. */
SRL_STATIC_INLINE void
srl_checksum_store_key(pTHX_ U8 key_out[SRL_CHECKSUM_KEY_LENGTH], U8 *key_mask, U8 version, SV *val)
{
    if (val && SvOK(val)) {
        STRLEN key_len;
        const char *option_name = srl_checksum_key_option_name(version);
        char *key = SvPVbyte(val, key_len);
        if (key_len != SRL_CHECKSUM_KEY_LENGTH) {
            if (key_len < SRL_CHECKSUM_KEY_LENGTH)
                warn("The '%s' key is shorter than 16 bytes; padding with nulls. Use Sereal::Decoder::prepare_checksum_key() to normalize it", option_name);
            else
                warn("The '%s' key is longer than 16 bytes; extra bytes are ignored and may contain unused entropy. Use Sereal::Decoder::prepare_checksum_key() to normalize it", option_name);
        }
        Zero(key_out, SRL_CHECKSUM_KEY_LENGTH, U8);
        Copy(key, key_out,
             key_len < SRL_CHECKSUM_KEY_LENGTH ? key_len : SRL_CHECKSUM_KEY_LENGTH, U8);
        *key_mask |= (U8)(1U << version);
    }
}

/* Load the decoder-compatible checksum options (checksum_policy,
 * checksum_v0_key, checksum_v1_key, checksum_v2_key) from an options hash.
 * Without any key option, $Sereal::DefaultKey supplies all three keys. */
SRL_STATIC_INLINE void
srl_checksum_config_from_hv(pTHX_ srl_checksum_config_t *cfg, HV *opt)
{
    static const char * const key_options[3] = {
        "checksum_v0_key", "checksum_v1_key", "checksum_v2_key"
    };
    SV *keys[3] = { NULL, NULL, NULL };
    SV *policy = NULL;
    int key_options_present = 0;
    U8 version;
    SV **svp;

    if (opt != NULL) {
        svp = hv_fetchs(opt, "checksum_policy", 0);
        if (svp)
            policy = *svp;
        for (version = 0; version <= 2; version++) {
            svp = hv_fetch(opt, key_options[version], (I32)strlen(key_options[version]), 0);
            if (svp) {
                keys[version] = *svp;
                key_options_present = 1;
            }
        }
    }
    if (!key_options_present)
        keys[0] = keys[1] = keys[2] = get_sv("Sereal::DefaultKey", 0);

    Zero(cfg, 1, srl_checksum_config_t);
    cfg->policy = srl_get_checksum_policy(aTHX_ policy);
    srl_validate_checksum_keys(aTHX_ keys, cfg->policy);
    for (version = 0; version <= 2; version++)
        srl_checksum_store_key(aTHX_ cfg->keys[version], &cfg->key_mask, version, keys[version]);
}

#endif
