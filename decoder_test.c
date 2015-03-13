#include <stdio.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <unistd.h>
#include <time.h>

#include "pcg/pcg_basic.h"

#define _TEST_

#include "decoder.c"

static int test_fd;
static FILE *test_file;

static int test_fail_count = 0;
#define MAX_FAILS 256
static const char *test_fail_names[MAX_FAILS];

static void print_results();
static void record_failure();

static void assert_no_exit(const char *tname) {
    if (test_did_exit)
        record_failure(tname);
    reset_tests();
}

static void assert_did_exit(const char *tname) {
    if (!test_did_exit)
        record_failure(tname);
    reset_tests();
}

static void record_failure(const char *tname) {
    if (test_fail_count >= MAX_FAILS){
        fprintf(stderr, "Max fail count reached!\n");
        print_results();
        exit(test_fail_count);
    }
    test_fail_names[test_fail_count++] = tname;
}

static void assert_equal(int expected, int actual, const char *msg) {
    if (expected != actual) {
        record_failure(msg);
    }
    reset_tests();
}

static void init_cursor(struct cursor *c, void *contents, int len) {
    c->start = contents;
    c->end = (uint8_t*)contents + len;
    c->cur = c->start;
}

static void test_validate_header() {
    struct cursor gif;
    
    init_cursor(&gif, "GIF89a", 6);
    validate_header(&gif);
    assert_no_exit("validate_header - expect valid");

    init_cursor(&gif, "GIF", 3);
    validate_header(&gif);
    assert_did_exit("validate_header - expect EOF");

    init_cursor(&gif, "crap56", 6);
    validate_header(&gif);
    assert_did_exit("validate_header - expect invalid");
}

void pack_sdesc(uint8_t *buf, struct screen_desc *sdesc) {
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

static void test_parse_screen_desc() {
    pcg32_random_t rnd;
    pcg32_srandom_r(&rnd, time(NULL) ^ (intptr_t)&sprintf, (intptr_t)&memset);

    struct cursor gif;
    struct screen_desc expected, actual;
    uint8_t buf[7];
    int fails = 0;
    int exits = 0;
    for (int i = 0; i < 100; i++) {
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
        record_failure(err);
    }
}

static void print_results() {
    printf("%i failures\n", test_fail_count);
    for (int i = 0; i < test_fail_count; i++)
        printf("%s - FAILED\n", test_fail_names[i]);
}

int main() {
    test_validate_header();
    test_parse_screen_desc();
    print_results();

    return test_fail_count;
}
