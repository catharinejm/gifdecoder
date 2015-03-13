#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>

#ifdef _TEST_
#include "test.h"
#endif

#ifndef EXIT
#define EXIT exit
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

struct cursor {
    union {
        uint8_t  *cur;
        uint16_t *cur16;
        uint32_t *cur32;
        uint64_t *cur64;
    };
    uint8_t *start;
    uint8_t *end;
};

/* n is a byte offset from c->cur
 * pass n<0 to check backwards
 */
static inline int cur_inrange(struct cursor *c, int n) {
    uint8_t *addr = c->cur + n;
    return addr < c->end && addr >= c->start;
}

/* pass n<0 to move backwards */
static inline int cur_move(struct cursor *c, int n) {
    if (cur_inrange(c, n)) {
        c->cur+=n;
        return n;
    }
    return 0;
}


void validate_header(struct cursor *gif) {
    if (!cur_inrange(gif, 6))
        DIE_EOF("Invalid header");
    if (memcmp(gif->cur, GIF89a, 6))
        DIE("Invalid header");
    gif->cur += 6;
}

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

struct buffer *alloc_buffer(int len) {
    struct buffer *buf = calloc(sizeof(struct buffer) + len, 1);
    if (!buf)
        DIE("Failed to allocate buffer");
    buf->len = len;
    return buf;
}

void read_ctable(struct buffer **ctbl, int ctbl_size, struct cursor *gif) {
    int ctbl_len = CTBL_LEN(ctbl_size);
    *ctbl = alloc_buffer(ctbl_len);

    if (cur_inrange(gif, ctbl_len)) {
        memcpy((*ctbl)->buf, gif->cur, ctbl_len);
        gif->cur += ctbl_len;
    } else
        DIE_EOF("Failed to read ctable");
}

void copy_block(uint8_t *buf, int len, struct cursor *gif) {
    if (gif->cur + len < gif->end)
        memcpy(buf, gif->cur, len);
    else
        DIE_EOF("Failed to copy block");
}

static inline uint8_t get_byte(struct cursor *c) {
    if (c->cur < c->end)
        return *c->cur++;
    DIE_EOF("Getting byte");
}

void parse_screen_desc(struct screen_desc *sdesc, struct cursor *gif) {
    if (!cur_inrange(gif, 7))
        DIE_EOF("Invalid screen description");

    sdesc->width = *gif->cur16++;
    sdesc->height = *gif->cur16++;

    uint8_t pack = *gif->cur++;
    sdesc->has_gctbl = pack >> 7;
    sdesc->color_res = (pack & 0x70) >> 4;
    sdesc->sorted = (pack & 0x08) >> 3;
    sdesc->gctbl_size = pack & 0x03;

    sdesc->bgcolor_idx = *gif->cur++;
    sdesc->px_aspect = *gif->cur++;
}

void parse_image_desc(struct img_desc *idesc, struct cursor *gif) {
    if (!cur_inrange(gif, 9))
        DIE_EOF("Invalid image description");

    idesc->left = *gif->cur16++;
    idesc->top = *gif->cur16++;
    idesc->width = *gif->cur16++;
    idesc->height = *gif->cur16++;

    uint8_t pack = *gif->cur++;
    idesc->has_lctbl = pack >> 7;
    idesc->interlaced = (pack & 0x40) >> 6;
    idesc->sorted = (pack & 0x20) >> 5;
    idesc->lctbl_size = pack & 0x3;
}

struct ctable_ext {
    int cnt;
    int offsets[4096];
    struct buffer ctbl;
};

struct ctable_ext *alloc_ctable_ext(int buflen) {
    struct ctable_ext *cte = calloc(sizeof(struct ctable_ext) + buflen, 1);
    if (!cte)
        DIE("Failed to allocate ctable extension");
    return cte;
}

struct ctable_ext *expand_ctable_ext(struct ctable_ext *ctbl_ext) {
    struct ctable_ext *cte = realloc(ctbl_ext, sizeof(struct ctable_ext) + ctbl_ext->ctbl.len * 2);
    if (!cte)
        DIE("Failed to expand ctable extension");
    return cte;
}

#define CTE_GET(cte, idx)                                   \
    ((struct buffer*)(&cte->ctbl.buf + cte->offsets[idx]))


void decode_image(struct screen_desc *sdesc, struct img_desc *idesc,
                  struct buffer *ctbl, struct buffer* img, struct cursor *gif)
{
    int code_size;
    int blk_len;
    uint8_t codes[256];
    struct ctable_ext *ctbl_ext = calloc(sizeof(struct ctable_ext) + 4096, 1);
    ctbl_ext->ctbl.len = 4096;
    int clear_code, eoi_code, inc_cs_flag;

    const int sc_row_len = sdesc->width * PXSIZE;
    const int img_row_len = idesc->width * PXSIZE;
    const int img_top = sc_row_len * idesc->top;
    const int img_left = idesc->left * PXSIZE;
    const int img_right = img_left + img_row_len;

    int img_idx = img_top + img_left;

    code_size = get_byte(gif) + 1;
    clear_code = 1 << (code_size - 1);
    eoi_code = clear_code + 1;
    inc_cs_flag = (1 << code_size) - 1;

    while ((blk_len = get_byte(gif))) {
        copy_block(codes, blk_len, gif);
        int byte_idx = 0;
        int bit_idx = 7;
        while (1) {
            int bits_read = 0;
            int mask, code;
            if (bit_idx < code_size) {
                code = codes[byte_idx++] << 8;
                bits_read += bit_idx + 8;
                bit_idx = 7;
                
                if (bits_read < code_size) {
                    if (byte_idx >= blk_len)
                        DIE("Premature end of sub-block");
                    code <<= 8;
                    code |= codes[byte_idx++];
                    bits_read += 8;
                }
                if (bits_read > code_size)
                    code >>= bits_read - code_size;

                /* TODO: All these image_right checks are wrong :-/
                 *       Need to adjust for the row too
                 */
                int coff = code * PXSIZE;
                if (coff < ctbl->len) {
                    memcpy(&img->buf+img_idx, &ctbl->buf+coff, PXSIZE);
                    img_idx += PXSIZE;
                    if (img_idx >= img_right)
                        img_idx += sc_row_len - img_row_len;
                    if (img_idx >= img->len)
                        DIE("Past end of image data");
                } else {
                    struct buffer *cte_entry = CTE_GET(ctbl_ext, coff);
                    if (img_idx + cte_entry->len > img_right) {
                        // copy portion that fits into img
                        // wrap img_idx
                        // copy remaining portion
                    } else {
                        // copy whole thing
                        // bump img_idx, wrap if necessary
                    }
                }
                //TODO: Extend ctbl_ext with new code
            }
        }
    }
}

void load_file(const char *fname, struct cursor *c) {
    int fd = open(fname, O_RDONLY);
    if (-1 == fd)
        DIE("Error opening file");
    struct stat st;
    if (fstat(fd, &st) == -1)
        DIE("Failed to stat file");
    /* TODO: Don't map huge files! */
    c->start = mmap(NULL, st.st_size, PROT_READ, MAP_PRIVATE, fd, 0);
    if (MAP_FAILED == c->start)
        DIE("Failed to mmap file");
    c->end = ((uint8_t*) c->start) + st.st_size;
    c->cur = (uint8_t*) c->start;
}

void parse_gif(struct cursor *gif) {
    struct buffer *img;
    
    validate_header(gif);

    struct screen_desc sdesc;
    parse_screen_desc(&sdesc, gif);

    img = alloc_buffer(sdesc.width * sdesc.height * PXSIZE);

    struct buffer *gctbl = NULL;
    if (sdesc.has_gctbl) {
        read_ctable(&gctbl, sdesc.gctbl_size, gif);
    }

    while (gif->cur < gif->end && *gif->cur != 0x3B) {
        switch(*gif->cur) {
        case 0x21:
            DIE("Graphics control extension parsing not implemented. :-/");
            break;
        case 0x2C:
            {
                struct buffer *ctbl = NULL;
                struct img_desc idesc;
                parse_image_desc(&idesc, gif);
                if (idesc.has_lctbl)
                    read_ctable(&ctbl, idesc.lctbl_size, gif);
                else
                    ctbl = gctbl;

                if (!ctbl)
                    DIE("No color table");

                decode_image(&sdesc, &idesc, ctbl, img, gif);

                if (ctbl != gctbl)
                    free(ctbl);
                break;
            }
        default:
            DIEF(-1, "Received 0x%2x - I don't know what to do", *gif->cur);
            break;
        }
    }

    if (gif->cur < gif->end)
        fprintf(stderr, "Warning: extra data after end of GIF contents.");
}

#ifndef _TEST_
int main() {
    struct cursor gif;
    load_file("./emulogic.gif", &gif);

    parse_gif(&gif);
    
    return 0;
}
#endif
