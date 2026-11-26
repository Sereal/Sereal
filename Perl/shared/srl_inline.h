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

#ifndef SRL_NOINLINE
#   if defined(__clang__) || (defined(__GNUC__) && (__GNUC__ > 3 || (__GNUC__ == 3 && __GNUC_MINOR__ >= 1)))
#       define SRL_NOINLINE __attribute__((noinline))
#   elif defined(_MSC_VER) && _MSC_VER >= 1300
#       define SRL_NOINLINE __declspec(noinline)
#   else
#       define SRL_NOINLINE
#   endif
#endif

#ifndef SRL_UNUSED
#   if defined(__clang__) || defined(__GNUC__)
#       define SRL_UNUSED __attribute__((unused))
#   else
#       define SRL_UNUSED
#   endif
#endif

#endif
