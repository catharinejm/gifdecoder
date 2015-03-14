#ifndef _TEST_TEST_H_
#define _TEST_TEST_H_

#include <stddef.h>

#ifndef TEST_COUNT
# error TEST_COUNT undefined!
#endif

enum test_state { NOT_RUN, PASS, FAIL };

struct test_info {
    const char *name;
    char *desc;
    char *msg;
    enum test_state result;
    enum test_state(*testfn)();
};

struct test_info tests[TEST_COUNT];

extern int test_did_exit;
extern int test_exit_code;

void test_harness(enum test_state(*testfn)(), const char *name, const char *desc);

#define DEFTEST(name, desc...)                                \
    enum test_state name##__IMPL();                           \
    void name() {                                             \
        test_harness(&name##__IMPL, #name, #desc);            \
    }                                                         \
    enum test_state name##__IMPL(const char **_test_message_)

#define SET_MESSAGE(msg)                        \
    do { *_test_message_ = (msg); } while (0)

#define PASS_IF(cond) ((cond) ? PASS : FAIL)
#define PASS_UNLESS(cond) PASS_IF(!(cond))
#define FAIL_IF PASS_UNLESS
#define FAIL_UNLESS PASS_IF

void print_result(struct test_info *tinfo);
void print_failures();
void print_passes();
void print_results();
void print_totals();

#define EXIT test_exit

void test_exit(int status);
void reset_tests();

#ifndef RUN_TESTS
# define RUN_TESTS()                                              \
    do {                                                          \
        printf("No tests to run! Did you run runbuilder.js?\n");  \
        exit(-1);                                                 \
    } while (0)
#endif
#ifndef TEST_EXTERNS
# define TEST_EXTERNS
#endif

#endif
