#include "bitcursor.h"

void bitcursor_init(struct bitcursor *bc, void *buf, int len) {
    bc->start = (uint8_t*)buf;
    bc->end = ((uint8_t*)buf) + len;
    bc->cur = bc->start;
    bc->bit = 0;
}


/* Reads up to 8 bits from the bitcursor. Values above 8 are truncated.
 * Reading fewer than 1 bit has no effect and the value of *dst is undefined.
 * Returns the number of bits read. Will be less than bits if we reach the
 * end of the cursor's buffer. *dst is set to the 8-bit equivalent value of
 * the bits read. E.g. if we read 3 bits: 010, *dst will be 0000 0010;
 */
int bitcursor_upto8(struct bitcursor *bc, int cnt, uint8_t *dst) {
    if (cnt < 1) return 0;
    if (cnt > 8) cnt = 8;
    if (bc->cur >= bc->end) return 0;

    if (bc->bit + cnt >= 8) {
        int high = 8 - bc->bit;
        int low = cnt - high;
        bc->bit = low;
        *dst = ((*bc->cur++) & ((1 << high) - 1));
        if (bc->cur >= bc->end) return high;
        *dst <<= low;
        *dst |= (*bc->cur >> (8 - low));
    } else {
        int bits_left = 8 - bc->bit;
        *dst = (*bc->cur & ((1 << bits_left) - 1)) >> (bits_left - cnt);
        bc->bit += cnt;
    }
    return cnt;
}

/* Reads up to 16 bits from the bitcursor. Values above 16 are truncated
 * Reading fewer than 1 bit has no efect and the value of *dst is undefined.
 * Returns the number of bits read. Will be fewer than requested if we reach
 * the end of the cursor's buffer. *dst is set to the 16-bit equavalent value of
 * the bits read. Note that bitstream is fully big-endian, but *dst is byte-wise
 * little-endian.
 * E.g. if we read 10 bits: 1100111100 *dst will be 00111100 00000011
 */
int bitcursor_upto16(struct bitcursor *bc, int cnt, uint16_t *dst) {
    *dst = 0;
    if (cnt < 1) return 0;
    if (cnt > 16) cnt = 16;
    if (bc->cur >= bc->end) return 0;
    if (cnt <= 8)
        return bitcursor_upto8(bc, cnt, (uint8_t*)dst);

    int rcnt;
    rcnt = bitcursor_upto8(bc, 8, (uint8_t*)dst);
    if (rcnt < 8)
        return rcnt;
    uint8_t low;
    rcnt = bitcursor_upto8(bc, cnt-8, &low);
    *dst <<= rcnt;
    *dst |= low;
    return rcnt+8;
}
