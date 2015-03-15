#include "test.h"
#include "../bitcursor.h"

#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

DEFTEST(test_bitcursor_upto8_truncate, "Truncates to 8 bits") {
    struct bitcursor bc;
    /* 1111 0000  0101 0101  0110 0011  0111 0111 */
    uint8_t bytes[4] = { 0xF0, 0x55, 0x63, 0x77 };
    bitcursor_init(&bc, bytes, 4);

    uint8_t res;
    int rcnt;
    rcnt = bitcursor_upto8(&bc, 9, &res);
    if (rcnt != 8) {
        SET_MESSAGE("Returned incorrect number of bytes read");
        return FAIL;
    }
    if (res != 0xF0) {
        SET_MESSAGE("Did not properly set output byte");
        return FAIL;
    }
    if (bc.cur != bytes+1) {
        SET_MESSAGE("Did not properly advance cursor");
        return FAIL;
    }
    if (bc.bit != 0) {
        SET_MESSAGE("Improperly set bit index");
        return FAIL;
    }
    return PASS;
}

int bitstring(uint8_t *out, int len, char *bits) {
    char *b = bits;
    int i = 0;
    while(i < len && *b) {
        if (*b == '0' || *b == '1') {
            out[i] = *b - '0';
            i++;
        }
        b++;
    }
    return i;
}

int bit2byte(uint8_t *bytes, int bylen, uint8_t *bits, int bitlen) {
    int i;
    for (i = 0; i < bitlen && i>>3 < bylen; i++) {
        if (! (i & 0x07)) bytes[i>>3] = 0;
        bytes[i>>3] |= (bits[i] ? 1 : 0) << (7 - (i & 0x07));
    }
    return i;
}

int bytestring(uint8_t *bytes, int bylen, char *bitstr) {
    uint8_t *bits = malloc(bylen*8);
    if (!bits)
        return -1;
    int bitlen = bitstring(bits, bylen*8, bitstr);
    int bcnt = bit2byte(bytes, bylen, bits, bitlen);
    
    free(bits);
    return bcnt;
}


DEFTEST(test_bitcursor_upto8_varying, "Consumes varying quantities of bits") {
    struct bitcursor bc;
    char *bitstr = "1111 0000  0101 0101  0110 0011  0111 0111";
    uint8_t bytes[4];
    uint8_t bits[32];
    if (bitstring(bits, 32, bitstr) != 32) {
        SET_MESSAGE("TEST ERROR: set improper number of bits!");
        return ERROR;
    }
    if (bit2byte(bytes, 4, bits, 32) != 32) {
        SET_MESSAGE("TEST ERROR: wrong number of bits written to bytes!");
        return ERROR;
    }
    bitcursor_init(&bc, bytes, 4);

    uint8_t expected[sizeof(bits)];
    bytestring(expected  , 1, "000 11110");
    bytestring(expected+1, 1, "000 00001");
    bytestring(expected+2, 1, "000 01010");
    bytestring(expected+3, 1, "000 10110");
    bytestring(expected+4, 1, "000 00110");
    bytestring(expected+5, 1, "000 11101");
    bytestring(expected+6, 1, "000 11000");
    uint8_t actual[sizeof(bits)];
    int bi, rcnt;
    for (bi = 0, rcnt = 0; bi < sizeof(bits); bi += 5, rcnt++) {
        bitcursor_upto8(&bc, 5, actual+rcnt);
    }
    if (memcmp(expected, actual, rcnt)) {
        SET_MESSAGE("Read bits do not match expected.");
        return FAIL;
    }
    return PASS;
}
