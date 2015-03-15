#ifndef _DECODER_H_
#define _DECODER_H_

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "bitcursor.h"

#ifdef _TEST_
# include "test/test.h"
#endif

#ifndef EXIT
# define EXIT exit
#endif

#define GIF89a "GIF89a"
#define PXSIZE 3

#define DIE(msg)                                \
    do {                                        \
        if (errno) {                            \
            perror(msg);                        \
            EXIT(errno);                        \
        }                                       \
        fprintf(stderr, msg "\n");              \
        EXIT(-1);                               \
    } while(0)

#define DIEF(code, msg, ...)                      \
    do {                                          \
        fprintf(stderr, msg "\n", ##__VA_ARGS__); \
        EXIT(code);                               \
    } while(0)

#define CTBL_LEN(size) ((1 << size) * PXSIZE)

#define DIE_EOF(msg) DIE("Premature EOF: " msg)


static inline int cur_inrange(struct bitcursor *c, int n);
static inline int cur_move(struct bitcursor *c, int n);

struct screen_desc {
    uint16_t width, height;
    uint8_t has_gctbl, color_res, sorted, gctbl_size,
        bgcolor_idx, px_aspect;
};

struct img_desc {
    uint16_t left, top, width, height;
    uint8_t has_lctbl, interlaced, sorted, lctbl_size;
};

struct buffer {
    int len;
    uint8_t buf[0];
};
struct buffer *alloc_buffer(int len);

void validate_header(struct bitcursor *gif);
void read_ctable(struct buffer **ctbl, int ctbl_size, struct bitcursor *gif);
void copy_block(uint8_t *buf, int len, struct bitcursor *gif);
static inline uint8_t get_byte(struct bitcursor *c);
void parse_screen_desc(struct screen_desc *sdesc, struct bitcursor *gif);
void parse_image_desc(struct img_desc *idesc, struct bitcursor *gif);


struct ctable_ext {
    int cnt;
    int offsets[4096];
    struct buffer ctbl;
};
struct ctable_ext *alloc_ctable_ext(int buflen);
struct ctable_ext *expand_ctable_ext(struct ctable_ext *ctbl_ext);

#define CTE_GET(cte, idx)                                   \
    ((struct buffer*)(&cte->ctbl.buf + cte->offsets[idx]))

void decode_image(struct screen_desc *sdesc, struct img_desc *idesc,
                  struct buffer *ctbl, struct buffer* img, struct bitcursor *gif);

void load_file(const char *fname, struct bitcursor *c);
void parse_gif(struct bitcursor *gif);

#endif
