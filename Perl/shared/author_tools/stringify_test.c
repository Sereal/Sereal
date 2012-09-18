#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <math.h>

#define DEBUGHACK 0

/* Call with a string buffer of at least ref_len+1 chars. */
static char *
my_itoa_check(register unsigned int i, char out_str[], char *ref_str, size_t ref_len)
{
    register char *out_ptr = out_str + ref_len;
    register char *ref_ptr = ref_str + ref_len-1;

    *out_ptr-- = 0;

    while(1) {
        *out_ptr = i % 10 + 0x30;
        if ( !(i /= 10) )
            break;
        if (DEBUGHACK) printf("!%c! !%c!\n", *out_ptr, *ref_ptr);
        if ( *ref_ptr-- != *out_ptr-- ) {
            if (DEBUGHACK) printf("NOT SAME: '%c' '%c'\n", out_ptr[1], ref_ptr[1]);
            return NULL;
        }
    }

    if (DEBUGHACK) printf("!!! ref_str='%p' ref_ptr='%p' out_str='%p' out_ptr='%p\n'", ref_str, ref_ptr, out_str, out_ptr);
    if ( (*ref_ptr != *out_ptr) || (ref_str != ref_ptr) )
    {
        if (DEBUGHACK) printf("NOT SAME: '%c' '%c'\n", out_ptr[1], ref_ptr[1]);
        return NULL;
    }

    return out_ptr;
}

static int
my_integer_string_equality_check(register unsigned int i, char *ref_str, size_t ref_len)
{
    register char *ref_ptr = ref_str + ref_len-1;
    char test_char;

    while(1) {
        test_char = i % 10 + 0x30;
        if ( !(i /= 10) )
            break;
        if (DEBUGHACK) printf("!%c! !%c!\n", test_char, *ref_ptr);
        if ( *ref_ptr-- != test_char ) {
            if (DEBUGHACK) printf("NOT SAME: '%c' '%c'\n", test_char, ref_ptr[1]);
            return 0;
        }
    }

    if (DEBUGHACK) printf("!!! ref_str='%p' ref_ptr='%p' test_char='%c' ref-char='%c'\n", ref_str, ref_ptr, test_char, *ref_ptr);
    if ( (*ref_ptr != test_char) || (ref_str != ref_ptr) )
    {
        if (DEBUGHACK) printf("NOT SAME: '%c' '%c'\n", test_char, ref_ptr[1]);
        return 0;
    }

    return 1;
}

int
main(int argc, char** argv) {
    unsigned int i = 1234;
    unsigned int in_len = (argc >= 2 ? atoi(argv[1]) : 3);
    char *o;
    char outstr[64];
    unsigned int ord;
    unsigned int j;
    unsigned int ref_num;
    char *str = malloc(in_len+1);
    ord = 1;
    for (j = 0; j < in_len; ++j) {
        str[j] = 0x30 + ((j+1) % 10);
        ord *= 10;
    }
    str[in_len] = '\0';
    ref_num = atoi(str);

    /*
     * o = my_itoa_check(i, outstr, str, in_len);
     * printf("SAME IF NOT NULL: '%s'\n", o);
     */

    for (j = 0; j < 100000000; ++j) {
        int k;
        i = (!(i % 2) ? ref_num : j % ord);
        if (DEBUGHACK) printf("%i %i %i %i '%s'\n", j, i, ord, in_len, str);
        k = my_integer_string_equality_check(i, str, in_len);
        if (k == 1) {
            if (DEBUGHACK) printf("!EUREKA!\n");
        }
    }

    /*
    if (my_integer_string_equality_check(i, str, 3)) {
        printf("YAY, SAME!\n");
    } else {
        printf("NOOOOO, NOT SAME!\n");
    }
    */

    return 1;
}
