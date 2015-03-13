#include <stdio.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <unistd.h>

#define _TEST_

#include "decoder.c"

static int test_fd;
static FILE *test_file;

static int test_fail_count = 0;
static const char *test_fail_names[50];

static void print_results();

static void cleanup_die(const char *message, int code) {
    fprintf(stderr, "%s\n", message);
    shm_unlink("test_gif_file");
    print_results();
    exit(code);
}

static void assert_no_exit(const char *tname) {
    if (test_did_exit)
        test_fail_names[test_fail_count++] = tname;
    reset_tests();
}

static void assert_did_exit(const char *tname) {
    if (!test_did_exit)
        test_fail_names[test_fail_count++] = tname;
    reset_tests();
}

static void set_test_file(const char *contents, int len) {
    // errno = 0;
    // rewind(test_file);
    // if (errno) {
    //     cleanup_die("Error rewinding test file (1)", errno);
    // }
    int wcnt = fwrite(contents, 1, len, test_file);
    if (wcnt < len) {
        cleanup_die("Error writing to test file", -1);
    }
    errno = 0;
    rewind(test_file);
    if (errno) {
        cleanup_die("Error rewinding test file (2)", errno);
    }
}

static void test_validate_header() {
    set_test_file("GIF89a", 6);
    validate_header(test_file);
    assert_no_exit("validate_header - valid");

    set_test_file("GIF", 3);
    validate_header(test_file);
    assert_did_exit("validate_header - premature EOF");

    set_test_file("crap56", 6);
    validate_header(test_file);
    assert_did_exit("validate_header - invalid");
}


static void print_results() {
    for (int i = 0; i < test_fail_count; i++)
        printf("%s failed\n", test_fail_names[i]);
}

int main() {
    test_fd = shm_open("test_gif_file", O_RDWR | O_CREAT);
    if (-1 == test_fd) {
        fprintf(stderr, "Error creating shared memory file descriptor\n");
        return errno;
    }
        
    test_file = fdopen(test_fd, "r+");
    if (!test_file) {
        fprintf(stderr, "Error opening shared memory file\n");
        shm_unlink("test_gif_file");
        return errno;
    }
    
    // test_validate_header();
    // print_results();
    set_test_file("foobar", 6);
    char str[10];
    fread(str, 1, 6, test_file);
    str[6] = '0';
    printf("read: %s\n", str);

    shm_unlink("test_gif_file");
    return test_fail_count;
}
