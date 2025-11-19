/*
 * Haskell FFI Wrapper for libregexp
 *
 * This wrapper provides a simplified interface for Haskell FFI bindings
 * and implements the required callbacks for libregexp.
 */

#include <stdlib.h>
#include <string.h>
#include "libregexp.h"

/* Callback implementations required by libregexp */

int lre_check_stack_overflow(void *opaque, size_t alloca_size) {
    /* For now, we don't check stack overflow.
     * Could be improved to check against a limit. */
    return 0;
}

int lre_check_timeout(void *opaque) {
    /* No timeout checking for now */
    return 0;
}

void *lre_realloc(void *opaque, void *ptr, size_t size) {
    if (size == 0) {
        free(ptr);
        return NULL;
    }
    return realloc(ptr, size);
}

/* Wrapper functions for easier FFI */

typedef struct {
    uint8_t *bytecode;
    int bytecode_len;
    int capture_count;
    int flags;
    char error_msg[256];
} ecma262_regex_t;

/* Compile a regular expression pattern */
ecma262_regex_t *ecma262_regex_compile(const char *pattern, size_t pattern_len, int flags) {
    ecma262_regex_t *regex = (ecma262_regex_t *)malloc(sizeof(ecma262_regex_t));
    if (!regex) return NULL;

    memset(regex, 0, sizeof(ecma262_regex_t));
    regex->flags = flags;

    regex->bytecode = lre_compile(&regex->bytecode_len, regex->error_msg,
                                   sizeof(regex->error_msg), pattern, pattern_len,
                                   flags, NULL);

    if (!regex->bytecode) {
        return regex; /* Return with error message */
    }

    regex->capture_count = lre_get_capture_count(regex->bytecode);
    return regex;
}

/* Free a compiled regex */
void ecma262_regex_free(ecma262_regex_t *regex) {
    if (regex) {
        if (regex->bytecode) {
            free(regex->bytecode);
        }
        free(regex);
    }
}

/* Get error message from compilation */
const char *ecma262_regex_get_error(ecma262_regex_t *regex) {
    return regex ? regex->error_msg : "NULL regex";
}

/* Get capture count */
int ecma262_regex_get_capture_count(ecma262_regex_t *regex) {
    return regex ? regex->capture_count : 0;
}

/* Get flags */
int ecma262_regex_get_flags(ecma262_regex_t *regex) {
    return regex ? regex->flags : 0;
}

/* Get group names (NULL-terminated string, or NULL if no named groups) */
const char *ecma262_regex_get_groupnames(ecma262_regex_t *regex) {
    if (!regex || !regex->bytecode) return NULL;
    return lre_get_groupnames(regex->bytecode);
}

/* Execute a match
 * Returns:
 *   1 if match found
 *   0 if no match
 *   negative value on error
 *
 * captures should be an array of (capture_count + 1) * 2 uint8_t pointers
 * (start and end for each capture group, plus the full match)
 */
int ecma262_regex_exec(ecma262_regex_t *regex, const uint8_t *subject,
                       int start_index, int subject_len, uint8_t **captures) {
    if (!regex || !regex->bytecode) return -1;

    /* cbuf_type: 0 = 8-bit characters, 1 = 16-bit characters */
    return lre_exec(captures, regex->bytecode, subject, start_index,
                   subject_len, 0, NULL);
}

/* Check if compilation was successful */
int ecma262_regex_is_valid(ecma262_regex_t *regex) {
    return regex && regex->bytecode != NULL;
}
