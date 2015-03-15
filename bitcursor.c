#include "bitcursor.h"

void bitcursor_init(struct bitcursor *bc, void *buf, int len) {
    bc->start = (uint8_t*)buf;
    bc->end = ((uint8_t*)buf) + len;
    bc->cur = bc->start;
    bc->bit = 0;
}


/* Reads up to 8 bits from the bitcursor. Values above 8 are truncated.
 * Reading 0 bits has no effect and *dst is unmodified.
 * Returns the number of bits read. Will be less than bits if we reach the
 * end of the cursor's buffer. Unread bits in *dst will be 0.
 */
int bitcursor_upto8(struct bitcursor *bc, int cnt, uint8_t *dst) {
    if (cnt < 1) return 0;
    if (cnt > 8) cnt = 8;
    if (bc->cur >= bc->end) return 0;

    if (bc->bit + cnt >= 8) {
        int high = 8 - bc->bit;
        int low = cnt - high;
        bc->bit = low;
        *dst = ((*bc->cur++) & ((1 << high) - 1)) << low;
        if (bc->cur >= bc->end) return high;
        *dst |= (*bc->cur >> (8 - low));
    } else {
        int bits_left = 8 - bc->bit;
        *dst = (*bc->cur & ((1 << bits_left) - 1)) >> (bits_left - cnt);
        bc->bit += cnt;
    }
    return cnt;
}
