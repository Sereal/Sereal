#ifndef UTILS_H
#define UTIILS_H

#define debug_print(fmt, ...)                                           \
    do { if (DEBUG) fprintf(stderr, "%s:%d:%s(): " fmt, __FILE__,       \
                                __LINE__, __func__, ##__VA_ARGS__); } while (0)

#endif // Included UTILS_H
