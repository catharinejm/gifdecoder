#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "test.h"

int test_did_exit;
int test_exit_code;

void test_harness(enum test_state(*testfn)(), const char *name, const char* desc) {
    static int test_index = 0;

    struct test_info *tinfo = &tests[test_index++];
    tinfo->name = name;
    tinfo->desc = NULL;
    if (desc && *desc)
        tinfo->desc = desc;
    tinfo->testfn = testfn;
    tinfo->result = testfn(&tinfo->msg);

    reset_tests();
}

void test_exit(int status) {
    test_did_exit = 1;
    test_exit_code = status;
}

void reset_tests() {
    test_did_exit = 0;
    test_exit_code = 0;
}

void print_result(struct test_info *tinfo) {
    printf("%s", tinfo->name);
    if (tinfo->desc)
        printf(" - %s", tinfo->desc);
    switch (tinfo->result) {
    case FAIL:
        printf(" - FAILED!\n");
        if (tinfo->msg)
            printf("\t%s\n", tinfo->msg);
        break;
    case PASS:
        printf(" - Passed!\n");
        break;
    default:
        printf(" - Skipped\n");
        break;
    }
}

void print_passes() {
    for (int i = 0; i < TEST_COUNT; i++) {
        if (tests[i].result == PASS)
            print_result(&tests[i]);
    }
}

void print_failures() {
    for (int i = 0; i < TEST_COUNT; i++) {
        if (tests[i].result == FAIL)
            print_result(&tests[i]);
    }
}

void print_not_runs() {
    for (int i = 0; i < TEST_COUNT; i++) {
        if (tests[i].result == NOT_RUN)
            print_result(&tests[i]);
    }
}

void print_totals() {
    int pass = 0, fail = 0, skip = 0;
    for (int i = 0; i < TEST_COUNT; i++) {
        if (PASS == tests[i].result)
            pass++;
        else if (FAIL == tests[i].result)
            fail++;
        else
            skip++;
    }
    
    printf("\n%i passed, %i failed, %i skipped\n\n", pass, fail, skip);
}

void print_results() {
    print_passes();
    print_failures();
    print_not_runs();
    print_totals();
}

/* Defined by runbuilder.js */
TEST_EXTERNS

int main() {
    test_did_exit = 0;
    test_exit_code = 0;
    memset(tests, 0, sizeof(tests));

    /* defined by runbuilder.js */
    RUN_TESTS();
    print_results();

    for (int i = 0; i < TEST_COUNT; i++)
        if (FAIL == tests[i].result)
            return 1;
    return 0;
}
