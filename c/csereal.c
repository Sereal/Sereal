#include <csereal.h>
#include <srl_protocol.h>
#include <csrl_util.h>
#include <ptable.h>

typedef struct csrl_decoder {
  /* The input string: start, end, cur position */
  const U8 *buf_start;
  const U8 *buf_end;
  const U8 *pos;
  U8 f_is_dirty;
} csrl_decoder;


csrl_decoder *
csrl_new_decoder()
{
  csrl_decoder *dec = calloc(1, sizeof(csrl_decoder));
  /* dec->f_is_dirty = FALSE; */
  return dec;
}


void
csrl_free_decoder(csrl_decoder *dec)
{
  free(dec);
}


int
csrl_looks_like_sereal(csrl_decoder *dec, const U8 *input, size_t len)
{
  UNUSED_VAR(dec);
  return(
    /* at least one version/flag byte, one byte for header len, one type byte (smallest payload) */
    len >= SRL_MAGIC_STRLEN+3
    && memcmp(input, SRL_MAGIC_STRING, SRL_MAGIC_STRLEN) == 0
    /* FIXME this check could be much better using the proto versions and all */
    && input[SRL_MAGIC_STRLEN] != (const char)0
  );
}


static void
csrl_reset_decoder(csrl_decoder *dec)
{
  dec->buf_start = NULL;
  dec->buf_end = NULL;
  dec->pos = NULL;

  dec->f_is_dirty = FALSE;
}

static void
csrl_init_decoder(csrl_decoder *dec, const U8 *input, size_t len)
{
  dec->buf_start = input;
  dec->pos = input;
  dec->buf_end = dec->buf_start + len;
  dec->f_is_dirty = TRUE;
}


int
csrl_decode(csrl_decoder *dec, const U8 *input, size_t len, void **out)
{
  if (dec->f_is_dirty)
    return CSRL_STATUS_ERROR;

  csrl_init_decoder(dec, input, len);
  /* TODO implement */
  *out = NULL;

  csrl_reset_decoder(dec);
  return CSRL_STATUS_OK;
}

