#include "test.h"
#include "../bitcursor.h"

#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

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

int bytestring(uint8_t *bytes, char *bitstr) {
    uint8_t bits[8];
    int bitlen = bitstring(bits, 8, bitstr);
    int bcnt = bit2byte(bytes, 1, bits, bitlen);
    
    return bcnt;
}

int shortstring(uint16_t *shrt, char *bitstr) {
    uint8_t bits[16];
    int bitlen = bitstring(bits, 16, bitstr);
    *shrt = 0;
    if (bitlen <= 8)
        return bit2byte((uint8_t*)shrt, 1, bits, bitlen);
    bit2byte(((uint8_t*)shrt)+1, 1, bits, 8);
    int bcnt = bit2byte((uint8_t*)shrt, 1, bits+8, bitlen-8);
    return 8+bcnt;
}

int byte2bit(uint8_t *bits, int bitlen, uint8_t *bytes, int bylen) {
    int i;
    for (i = 0; i < bitlen && i>>3 < bylen; i++) {
        int shift = 7 - (i & 0x07);
        bits[i] = (bytes[i>>3] & (1 << shift)) >> shift;
    }
    return i;
}


DEFTEST(test_bitcursor_upto8_take5, "Consumes 5 bits at a time") {
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
    bytestring(expected  , "000 11110");
    bytestring(expected+1, "000 00001");
    bytestring(expected+2, "000 01010");
    bytestring(expected+3, "000 10110");
    bytestring(expected+4, "000 00110");
    bytestring(expected+5, "000 11101");
    bytestring(expected+6, "000 00011"); // last 2 bits
    uint8_t actual[sizeof(bits)];
    int bi, rcnt;
    for (bi = 0, rcnt = 0; bi < sizeof(bits); bi += 5, rcnt++) {
        bitcursor_upto8(&bc, 5, actual+rcnt);
    }
    if (memcmp(expected, actual, rcnt)) {
        SET_MESSAGE("Read bits do not match expected.");
        return FAIL;
    }
    if (bc.cur != bc.end) {
        SET_MESSAGE("Bitcursor not advanced properly");
        return FAIL;
    }

    return PASS;
}

DEFTEST(test_byte_bit_sanity, "byte2bit and bit2byte work") {
    /* 1111 0000  0101 0101  0110 0011  0111 0111 */
    uint8_t expected_bytes[4] = { 0xF0, 0x55, 0x63, 0x77 };
    uint8_t expected_bits[32] = { 1,1,1,1, 0,0,0,0,
                                  0,1,0,1, 0,1,0,1,
                                  0,1,1,0, 0,0,1,1,
                                  0,1,1,1, 0,1,1,1 };
    uint8_t actual_bytes[4];
    uint8_t actual_bits[32];
    byte2bit(actual_bits, 32, expected_bytes, 4);
    if (memcmp(expected_bits, actual_bits, 32)) {
        SET_MESSAGE("it don't work");
        return FAIL;
    }
    bit2byte(actual_bytes, 4, actual_bits, 32);
    if (memcmp(actual_bytes, expected_bytes, 4)) {
        SET_MESSAGE("reverse don't work");
        return FAIL;
    }

    return PASS;
}

DEFTEST(test_byte_bit_unaligned, "unaligned bits are left shifted in last byte") {
    uint8_t expected_bytes[2] = { 0xFF, 0xC0 };
    uint8_t expected_bits[10] = { 1,1,1,1, 1,1,1,1, 1,1 };
    
    uint8_t actual_bytes[2];
    bit2byte(actual_bytes, 2, expected_bits, 10);
    if (memcmp(expected_bytes, actual_bytes, 2)) {
        SET_MESSAGE("bytes don't match");
        return FAIL;
    }
    uint8_t actual_bits[10];
    byte2bit(actual_bits, 10, expected_bytes, 2);
    if (memcmp(expected_bits, actual_bits, 10)) {
        SET_MESSAGE("bits don't match");
        return FAIL;
    }
    return PASS;
}

DEFTEST(test_shortstring, "it returns a little-endian short of the given big-endian bytestring") {
    uint16_t shrt;
    shortstring(&shrt, "0000 1111 1111 0000");
    if (shrt != 0x0FF0)
        return FAIL;
    return PASS;
}

DEFTEST(test_bitcursor_upto8_varying, "Consumes varying quantities of bits") {
    struct bitcursor bc;
    uint8_t bytes[4];
    uint8_t bits[32];

    for (int tries = 0; tries < 1000; tries++) {
        bitcursor_init(&bc, bytes, 4);
        arc4random_buf(bytes, 4);
        byte2bit(bits, 32, bytes, 4);
        for (int i = 0; i < 32;) {
            int take = arc4random_uniform(8) + 1;
            uint8_t expected, actual;
            int real_take = (take+i >= 32) ? 32 - i : take;
            bit2byte(&expected, 1, bits+i, real_take);
            expected >>= 8-real_take;
            bitcursor_upto8(&bc, take, &actual);
            if (actual != expected) {
                char *msg = malloc(256);
                snprintf(msg, 256, "Expected 0x%02hhX, got 0x%02hhX.\ti: %i, take: %i", expected, actual, i, take);
                SET_MESSAGE(msg);
                return FAIL;
            }
            i+=take;
        }
    }
    return PASS;
}

DEFTEST(test_bitcursor_upto16_fixed, "Consumes a fixed bit quantity into 16-bit blocks") {
    struct bitcursor bc;
    /* 1111 0000  0101 0110 */
    uint8_t bytes[2] = { 0xF0, 0x56 };
    uint16_t expecteds[16];
    shortstring(expecteds   , "0000 0000  0000 0001");
    shortstring(expecteds+ 1, "0000 0000  0000 0011");
    shortstring(expecteds+ 2, "0000 0000  0000 0111");
    shortstring(expecteds+ 3, "0000 0000  0000 1111");
    shortstring(expecteds+ 4, "0000 0000  0001 1110");
    shortstring(expecteds+ 5, "0000 0000  0011 1100");
    shortstring(expecteds+ 6, "0000 0000  0111 1000");
    shortstring(expecteds+ 7, "0000 0000  1111 0000");
    shortstring(expecteds+ 8, "0000 0001  1110 0000");
    shortstring(expecteds+ 9, "0000 0011  1100 0001");
    shortstring(expecteds+10, "0000 0111  1000 0010");
    shortstring(expecteds+11, "0000 1111  0000 0101");
    shortstring(expecteds+12, "0001 1110  0000 1010");
    shortstring(expecteds+13, "0011 1100  0001 0101");
    shortstring(expecteds+14, "0111 1000  0010 1011");
    shortstring(expecteds+15, "1111 0000  0101 0110");

    uint16_t actual;
    for (int i = 0; i < 16; i++) {
        bitcursor_init(&bc, bytes, 2);
        bitcursor_upto16(&bc, i+1, &actual);
        if (actual != expecteds[i]) {
            char *msg = malloc(256);
            snprintf(msg, 256, "expected 0x%04hX, got 0x%04hX\ti: %i", expecteds[i], actual, i);
            SET_MESSAGE(msg);
            return FAIL;
        }
    }
    return PASS;
}

DEFTEST(test_bitcursor_upto16_varying, "Consumes varying quantities of bits into 16-bit blocks") {
    struct bitcursor bc;
    uint8_t bytes[8];
    uint8_t bits[64];

    for (int tries = 0; tries < 1000; tries++) {
        bitcursor_init(&bc, bytes, 8);
        arc4random_buf(bytes, 8);
        byte2bit(bits, 64, bytes, 8);
        for (int i = 0; i < 64;) {
            int take = arc4random_uniform(16) + 1;
            uint16_t expected = 0, actual = 0;
            uint8_t high, low;
            int real_take = (take+i >= 64) ? 64 - i : take;
            if (real_take > 8) {
                bit2byte(&high, 1, bits+i, (real_take > 8 ? 8 : real_take));
                bit2byte(&low, 1, bits+i+8, real_take - 8);
                expected = (high << 8) | low;
                expected >>= 16-real_take;
            } else {
                bit2byte((uint8_t*)&expected, 1, bits+i, real_take);
                expected >>= 8-real_take;
            }
            bitcursor_upto16(&bc, take, &actual);
            if (actual != expected) {
                char *msg = malloc(256);
                snprintf(msg, 256, "Expected 0x%04hX, got 0x%04hX.\ti: %i, take: %i", expected, actual, i, take);
                SET_MESSAGE(msg);
                return FAIL;
            }
            i+=take;
        }
    }
    return PASS;
}
