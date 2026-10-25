#ifndef SRL_READER_TYPES_H_
#define SRL_READER_TYPES_H_

typedef const unsigned char srl_reader_char_t;
typedef srl_reader_char_t * srl_reader_char_ptr;

struct srl_reader_buffer {
    srl_reader_char_ptr start;    /* ptr to "physical" start of input buffer */
    srl_reader_char_ptr end;      /* ptr to end of input buffer */
    srl_reader_char_ptr pos;      /* ptr to current possition */
    srl_reader_char_ptr body_pos; /* in Sereal V2, all offsets are relative to the body */
    srl_reader_char_ptr body_start; /* first byte of the body; in V1 offsets can point before it */
};

typedef struct srl_reader_buffer srl_reader_buffer_t;
typedef srl_reader_buffer_t * srl_reader_buffer_ptr;

#define SRL_CHECKSUM_KEY_LENGTH 16

/* The document selects the checksum algorithm; policy sets any minimum. */
typedef struct {
    unsigned char policy;
    unsigned char has_key;
    unsigned char key[SRL_CHECKSUM_KEY_LENGTH];
} srl_checksum_config_t;

#endif
