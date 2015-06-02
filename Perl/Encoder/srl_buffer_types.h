#ifndef SRL_BUFFER_TYPES_H_
#define SRL_BUFFER_TYPES_H_

typedef unsigned char srl_buffer_char;
typedef struct {
    srl_buffer_char *start;    /* ptr to "physical" start of output buffer */
    srl_buffer_char *end;      /* ptr to end of output buffer */
    srl_buffer_char *pos;      /* ptr to current position within output buffer */
    srl_buffer_char *body_pos; /* ptr to start of body within output buffer for protocol V2 encoding */
} srl_buffer_t;

#endif
