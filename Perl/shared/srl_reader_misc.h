#ifndef SRL_READER_MISC_H_
#define SRL_READER_MISC_H_

#include "srl_inline.h"
#include "srl_common.h"
#include "srl_reader.h"
#include "srl_protocol.h"

/* not sure that this's the best location for this function */
SRL_STATIC_INLINE IV
srl_validate_header_version(pTHX_ srl_reader_char_ptr strdata, STRLEN len)
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
