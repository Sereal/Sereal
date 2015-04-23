#ifndef SRL_READER_TYPES_H_
#define SRL_READER_TYPES_H_

typedef const unsigned char srl_reader_char_t;
typedef srl_reader_char_t * srl_reader_char_ptr;

struct srl_reader_buffer {
    srl_reader_char_ptr start;    /* ptr to "physical" start of input buffer */
    srl_reader_char_ptr end;      /* ptr to end of input buffer */
    srl_reader_char_ptr pos;      /* ptr to current possition */
    srl_reader_char_ptr body_pos; /* in Sereal V2, all offsets are relative to the body */
    U8 encoding_flags;
    U8 protocol_version;          /* Sereal protocol version */
};

typedef struct srl_reader_buffer srl_reader_buffer_t;
typedef srl_reader_buffer_t * srl_reader_buffer_ptr;

#endif
