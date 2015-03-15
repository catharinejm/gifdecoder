#ifndef _BITCURSOR_H_
#define _BITCURSOR_H_

#include <stdint.h>

struct bitcursor {
    union {
        uint8_t *cur;
        uint16_t *cur16;
        uint32_t *cur32;
        uint64_t *cur64;
    };
    uint8_t *start;
    uint8_t *end;
    uint8_t bit;
};
void bitcursor_init(struct bitcursor *bc, void *buf, int len);
int bitcursor_upto8(struct bitcursor *bc, int cnt, uint8_t *dst);
int bitcursor_upto16(struct bitcursor *bc, int cnt, uint16_t *dst);

#endif
