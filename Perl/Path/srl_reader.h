#ifndef SRL_READER_H_
#define SRL_READER_H_

#include <assert.h>

#ifndef srl_reader_t
#error define srl_reader_t before including srl_reader.h
#endif

#include "srl_inline.h"
#include "srl_common.h"
#include "srl_reader_types.h"

/* reader buffer operations */
#define SRL_RB_SIZE(r)           ((r)->rb_end -  (r)->rb_start)
#define SRL_RB_SPACE_LEFT(r)     ((r)->rb_end -  (r)->rb_pos)
#define SRL_RB_DONE(r)           ((r)->rb_pos >= (r)->rb_end)
#define SRL_RB_NOT_DONE(r)       ((r)->rb_pos <  (r)->rb_end)
#define SRL_RB_POS_OFS(r)        ((r)->rb_pos -  (r)->rb_start)
#define SRL_RB_BODY_POS_OFS(r)   ((r)->rb_pos -  (r)->rb_body_pos)

#define SRL_RB_CLEAR(r) STMT_START {                                            \
    (r)->rb_start = NULL;                                                       \
    (r)->rb_end = NULL;                                                         \
    (r)->rb_pos = NULL;                                                         \
    (r)->rb_body_pos = NULL;                                                    \
    (r)->r_protocol_version = 0;                                                \
} STMT_END

/* Sereal v1 and newer version use start body offset from different positions */
#define SRL_RB_SET_BODY_POS(r, pos) ((r)->rb_body_pos = pos)
#define SRL_RB_UPDATE_BODY_POS(r) STMT_START {                                  \
    if (expect_false(((r)->r_protocol_version) == 1)) {                         \
        SRL_RB_SET_BODY_POS((r), (r)->rb_start);                                \
    } else {                                                                    \
        SRL_RB_SET_BODY_POS((r), (r)->rb_pos - 1);                              \
    }                                                                           \
} STMT_END

#define SRL_RB_ASSERT_SPACE(r, len, msg) STMT_START {                           \
    if (expect_false( (UV)SRL_RB_SPACE_LEFT((r)) < (UV)(len) )) {               \
        SRL_RDR_ERRORf3((r), "Unexpected termination of packet%s, "             \
                        "want %"UVuf" bytes, only have %"UVuf" available",      \
                        (msg), (UV)(len), (UV)SRL_RB_SPACE_LEFT((r)));          \
    }                                                                           \
} STMT_END

#ifndef NDEBUG
#   define DEBUG_ASSERT_RB_SANE(rdr) STMT_START {                               \
        if ((rdr)->rb_pos < (rdr)->rb_start || (rdr)->rb_pos > (rdr)->rb_end) { \
            warn("failed sanity assertion check - pos: %ld [%p %p %p] %ld",     \
                 (long)SRL_RB_POS_OFS(rdr), (rdr)->rb_start,                    \
                 (rdr)->rb_pos, (rdr)->rb_end, (long)SRL_RB_SPACE_LEFT(rdr));   \
        }                                                                       \
        assert((rdr)->rb_pos >= (rdr)->rb_start);                               \
        assert((rdr)->rb_pos <= (rdr)->rb_end);                                 \
    } STMT_END 
#else
#   define DEBUG_ASSERT_RB_SANE(rdr)
#endif

/* trace functions */
#ifdef TRACE_READER
#   define SRL_RB_TRACE(msg, args...) \
        fprintf(stderr, "%s:%d:%s(): "msg"\n", __FILE__, __LINE__, __func__, args)
#else
#   define SRL_RB_TRACE(msg, args...)
#endif

#define SRL_RDR_REPORT_TAG(rdr, tag) STMT_START {                                   \
    SRL_RB_TRACE(                                                                   \
        "Reader: tag SRL_HDR_%s (int: %d hex: 0x%x) at ofs %"UVuf" body_ofs %"UVuf, \
        SRL_TAG_NAME((tag)),                                                        \
        (tag), (tag),                                                               \
        (UV) SRL_RB_POS_OFS((rdr)),                                                 \
        (UV) SRL_RB_BODY_POS_OFS((rdr))                                             \
    );                                                                              \
} STMT_END

/* not sure that this's the best location for this function */
SRL_STATIC_INLINE IV
srl_validate_header_version_pv_len(pTHX_ srl_reader_char_ptr strdata, STRLEN len)
{
    if ( len >= SRL_MAGIC_STRLEN + 3 ) {
        /* + 3 above because:
         * at least one version/flag byte,
         * one byte for header len,
         * one type byte (smallest payload)
         */

        /* Do NOT do *((U32*)strdata at least for these reasons:
         * (1) Unaligned access can "Bus error" on you
         *     (char* can be much less aligned than U32).
         * (2) In ILP64 even if aligned the U32 would be 64 bits wide,
         *     and the deref would read 8 bytes, more than the smallest
         *     (valid) message.
         * (3) Endianness.
         */
        U8 version_encoding= strdata[SRL_MAGIC_STRLEN];
        U8 version= version_encoding & SRL_PROTOCOL_VERSION_MASK;

        if ( memEQ(SRL_MAGIC_STRING, strdata, SRL_MAGIC_STRLEN) ) {
            if ( 0 < version && version < 3 ) {
                return version_encoding;
            }
        }
        else
        if ( memEQ(SRL_MAGIC_STRING_HIGHBIT, strdata, SRL_MAGIC_STRLEN) ) {
            if ( 3 <= version ) {
                return version_encoding;
           }
        }
        else
        if ( memEQ(SRL_MAGIC_STRING_HIGHBIT_UTF8, strdata, SRL_MAGIC_STRLEN) ) {
            return 0;
        }
    }
    return -1;
}

#endif
