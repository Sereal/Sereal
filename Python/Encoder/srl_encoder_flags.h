#ifndef _SRL_ENCODER_FLAGS_H_
#define _SRL_ENCODER_FLAGS_H_

/* Will default to "on". If set, hash keys will be shared using COPY.
 * Corresponds to the inverse of constructor option "no_shared_hashkeys" */
#define SRL_F_SHARED_HASHKEYS                1UL

/* If set, then we're using the OO interface and we shouldn't destroy the
 * encoder struct during SAVEDESTRUCTOR_X time 
#define SRL_F_REUSE_ENCODER                  2UL
*/

/* If set in flags, then we rather croak than serialize an object.
 * Corresponds to the 'croak_on_bless' option to the Perl constructor.
#define SRL_F_CROAK_ON_BLESS                 4UL
*/

/* If set in flags, then we will emit <undef> for all data types
 * that aren't supported.  Corresponds to the 'undef_unknown' option. */
#define SRL_F_UNDEF_UNKNOWN                  8UL
/* If set in flags, then we will stringify (SvPV) all data types
 * that aren't supported.  Corresponds to the 'stringify_unknown' option. */
#define SRL_F_STRINGIFY_UNKNOWN              16UL
/* If set in flags, then we warn() when trying to serialize an unsupported
 * data structure.  Applies only if stringify_unknown or undef_unknown are
 * set since we otherwise croak.  Corresponds to the 'warn_unknown' option. */
#define SRL_F_WARN_UNKNOWN                   32UL

/* WARNING: This is different from the protocol bit SRL_PROTOCOL_ENCODING_SNAPPY in that it's
 *          a flag on the encoder struct indicating that we want to use Snappy. */
#define SRL_F_COMPRESS_SNAPPY                64UL
#define SRL_F_COMPRESS_SNAPPY_INCREMENTAL   128UL


/* Only meaningful if SRL_F_WARN_UNKNOWN also set. If this one is set, then we don't warn
 * if the unsupported item has string overloading. 
#define SRL_F_NOWARN_UNKNOWN_OVERLOAD       256UL
*/

/* Only meaningful if SRL_F_WARN_UNKNOWN also set. If this one is set, then we don't warn
 * if the unsupported item has string overloading. */
#define SRL_F_SORT_KEYS                     512UL

/* Set while the encoder is in active use / dirty */
#define SRL_OF_ENCODER_DIRTY                 1UL

#endif
