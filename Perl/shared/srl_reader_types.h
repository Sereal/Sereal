#ifndef SRL_READER_TYPES_H_
#define SRL_READER_TYPES_H_

typedef const unsigned char srl_reader_char_t;
typedef srl_reader_char_t * srl_reader_char_ptr;

struct srl_reader_buffer {
    srl_reader_char_ptr start;    /* ptr to "physical" start of input buffer */
    srl_reader_char_ptr end;      /* ptr to end of input buffer */
    srl_reader_char_ptr pos;      /* ptr to current possition */
    srl_reader_char_ptr body_pos; /* in Sereal V2, all offsets are relative to the body */
};

typedef struct srl_reader_buffer srl_reader_buffer_t;
typedef srl_reader_buffer_t * srl_reader_buffer_ptr;

#define SRL_CHECKSUM_KEY_LENGTH 16

/* Checksum settings of a reader: the policy and the keys it was given,
 * indexed by checksum type. Bit N of key_mask is set when keys[N] is set. */
typedef struct {
    unsigned char policy;
    unsigned char key_mask;
    unsigned char keys[3][SRL_CHECKSUM_KEY_LENGTH];
} srl_checksum_config_t;

#endif
