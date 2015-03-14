#include <stdio.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <unistd.h>
#include <time.h>

#include "../pcg/pcg_basic.h"
#include "test.h"
#include "../decoder.h"

static void init_cursor(struct cursor *c, void *contents, int len) {
    c->start = contents;
    c->end = (uint8_t*)contents + len;
    c->cur = c->start;
}

DEFTEST(test_validate_header_valid) {
    struct cursor gif;
    
    init_cursor(&gif, "GIF89a", 6);
    validate_header(&gif);
    return PASS_UNLESS(test_did_exit);
}

DEFTEST(test_validate_header_eof) {
    struct cursor gif;
    
    init_cursor(&gif, "GIF", 3);
    validate_header(&gif);
    return PASS_IF(test_did_exit);
}

DEFTEST(test_validate_header_invalid) {
    struct cursor gif;
    
    init_cursor(&gif, "crap56", 6);
    validate_header(&gif);
    return PASS_IF(test_did_exit);
}

static void pack_sdesc(uint8_t *buf, struct screen_desc *sdesc) {
    struct cursor c;
    init_cursor(&c, buf, 7);
    (*c.cur16++) = sdesc->width;
    (*c.cur16++) = sdesc->height;
    uint8_t pack = 0;
    if (sdesc->has_gctbl)
        pack |= 0x80;
    pack |= (sdesc->color_res & 0x07) << 4;
    if (sdesc->sorted)
        pack |= 0x08;
    pack |= sdesc->gctbl_size & 0x07;
    (*c.cur++) = pack;
    (*c.cur++) = sdesc->bgcolor_idx;
    *c.cur = sdesc->px_aspect;
}

DEFTEST(test_parse_screen_desc) {
    pcg32_random_t rnd;
    pcg32_srandom_r(&rnd, time(NULL) ^ (intptr_t)&sprintf, (intptr_t)&memset);

    struct cursor gif;
    struct screen_desc expected, actual;
    uint8_t buf[7];
    int fails = 0;
    int exits = 0;
    for (int i = 0; i < 1000; i++) {
        memset(&expected, 0, sizeof(struct screen_desc));
        memset(&actual, 0, sizeof(struct screen_desc));
        
        expected.width = pcg32_random_r(&rnd);
        expected.height = pcg32_random_r(&rnd);
        expected.has_gctbl = pcg32_boundedrand_r(&rnd, 2);
        expected.color_res = pcg32_boundedrand_r(&rnd, 1<<3);
        expected.sorted = pcg32_boundedrand_r(&rnd, 2);
        expected.gctbl_size = pcg32_boundedrand_r(&rnd, 1<<3);
        expected.bgcolor_idx = pcg32_random_r(&rnd);
        expected.px_aspect = pcg32_random_r(&rnd);

        pack_sdesc(buf, &expected);
        init_cursor(&gif, buf, 7);
        parse_screen_desc(&actual, &gif);

        if (test_did_exit)
            exits++;
        else if (memcmp(&expected, &actual, sizeof(struct screen_desc)))
            fails++;
        reset_tests();
    }
    
    if (fails || exits) {
        char *err = malloc(256);
        snprintf(err, 256, "parse_screen_desc - exits: %i, fails: %i", exits, fails);
        SET_MESSAGE(err);
        return FAIL;
     }
    return PASS;
}
