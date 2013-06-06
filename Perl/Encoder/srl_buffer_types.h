#ifndef SRL_BUFFER_TYPES_H_
#define SRL_BUFFER_TYPES_H_

typedef struct {
    char *start;    /* ptr to "physical" start of output buffer */
    char *end;      /* ptr to end of output buffer */
    char *pos;      /* ptr to current position within output buffer */
    char *body_pos; /* ptr to start of body within output buffer for protocol V2 encoding */
} srl_buffer_t;

#endif
