#ifndef SRL_READER_TYPES_H_
#define SRL_READER_TYPES_H_

typedef const unsigned char srl_reader_char_t;
typedef srl_reader_char_t * srl_reader_char_ptr;

#define SRL_READER_STRUCT                                                                      \
    srl_reader_char_ptr rb_start;    /* ptr to "physical" start of input buffer */             \
    srl_reader_char_ptr rb_end;      /* ptr to end of input buffer */                          \
    srl_reader_char_ptr rb_pos;      /* ptr to current possition */                            \
    srl_reader_char_ptr rb_body_pos; /* in Sereal V2, all offsets are relative to the body */  \
    U8 r_protocol_version;           /* Sereal protocol version */

#endif
