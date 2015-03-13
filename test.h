static int test_did_exit = 0;
static int test_exit_code;

#define EXIT test_exit

static void test_exit(int status) {
    test_did_exit = 1;
    test_exit_code = status;
}

static void reset_tests() {
    test_did_exit = 0;
    test_exit_code = 0;
}
