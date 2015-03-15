#include "test.h"
#include "../bitcursor.h"

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
