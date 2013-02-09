#ifndef SRL_INLINE_H_
#define SRL_INLINE_H_

/* We do this because it seems that PERL_STATIC_INLINE isn't defined
 * or something like that. I haven't figured out why not.
 */

#ifdef NOINLINE
#   define SRL_STATIC_INLINE STATIC
#elif defined(_MSC_VER)
#   define SRL_STATIC_INLINE STATIC __inline
#else
#   define SRL_STATIC_INLINE STATIC inline
#endif


#endif
