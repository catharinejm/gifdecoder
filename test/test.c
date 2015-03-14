#include <stdio.h>
#include <stdlib.h>
#include "test.h"

void test_exit(int status) {
    test_did_exit = 1;
    test_exit_code = status;
}

void reset_tests() {
    test_did_exit = 0;
    test_exit_code = 0;
}


void assert_no_exit(const char *tname) {
    if (test_did_exit)
        record_failure(tname);
    else
        test_pass_count++;
    reset_tests();
}

void assert_did_exit(const char *tname) {
    if (!test_did_exit)
        record_failure(tname);
    else
        test_pass_count++;
    reset_tests();
}

void record_failure(const char *tname) {
    if (test_fail_count >= MAX_FAILS){
        fprintf(stderr, "Max fail count reached!\n");
        print_results();
        print_failure(tname);
        exit(test_fail_count+1);
    }
    test_fail_names[test_fail_count++] = tname;
}

void assert_equal(int expected, int actual, const char *msg) {
    if (expected != actual) {
        record_failure(msg);
    } else
        test_pass_count++;
    reset_tests();
}

void print_failure(const char *s) {
    printf("%s - FAILED\n", s);
}

void print_results() {
    printf("%i passed, %i failed\n", test_pass_count, test_fail_count);
    for (int i = 0; i < test_fail_count; i++)
        print_failure(test_fail_names[i]);
}

/* Defined by runbuilder.js */
TEST_EXTERNS

int main() {
    /* defined by runbuilder.js */
    RUN_TESTS();
    print_results();
    return test_fail_count;
}
