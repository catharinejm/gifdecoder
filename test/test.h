#ifndef _TEST_TEST_H_
#define _TEST_TEST_H_

#define MAX_FAILS 256
static const char *test_fail_names[MAX_FAILS];
static int test_fail_count = 0;

static int test_did_exit = 0;
static int test_exit_code;

void print_failure(const char *s);
void print_results();
void record_failure();

void assert_no_exit(const char *tname);
void assert_did_exit(const char *tname);
void record_failure(const char *tname);
void assert_equal(int expected, int actual, const char *msg);

#define EXIT test_exit

void test_exit(int status);
void reset_tests();

#endif
