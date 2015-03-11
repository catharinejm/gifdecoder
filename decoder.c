#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>

#define GIF89a "GIF89a"
#define PXSIZE 3

#define DIE(msg, code)                          \
    do {                                        \
        fprintf(stderr, msg "\n");              \
        exit(code);                             \
    } while(0)

#define DIEF(code, msg, ...)                    \
    do {                                        \
        fprintf(stderr, msg, ##__VA_ARGS__);    \
        exit(code);                             \
    } while(0)

#define CTBL_LEN(size)                          \
    ((1 << size) * PXSIZE)

#define DIE_EOF(file)                           \
    do {                                        \
        int e;                                  \
        if (feof(file))                         \
            DIE("Premature EOF", -1);           \
        if ((e = ferror(file)))                 \
            DIE("Error reading file", e);       \
        DIE("An unknown error occurred", -2);   \
    } while(0)


void validate_header(FILE *gif) {
    char hdr[7];
    if (!fgets(hdr, 7, gif) || strcmp(hdr, GIF89a))
        DIE("Invalid header", -1);
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
        DIE("Failed to allocate buffer", errno);
    buf->len = len;
    return buf;
}

void read_tag(uint8_t *tagbuf, int len, FILE *gif) {
    int rcnt = fread(tagbuf, 1, len, gif);
    if (rcnt < len) DIE_EOF(gif);
}

void read_ctable(struct buffer *ctbl, int ctable_size, FILE *gif) {
    int ctbl_len = CTBL_LEN(ctbl_size);
    ctbl = alloc_buffer(ctbl_len);

    int rcnt = fread(ctbl->tbl, 1, ctbl_len, gif);
    if (rcnt < ctbl_len) DIE_EOF(gif);
}

void parse_screen_desc(struct screen_desc *sdesc, FILE *gif) {
    uint8_t bys[7];

    read_tag(bys, 7, gif);
    
    sdesc->width = *(uint16_t*)bys;
    sdesc->height = *(uint16_t*)bys+2;

    uint8_t pack = bys[4];
    sdesc->has_gctbl = pack >> 7;
    sdesc->color_res = (pack & 0x70) >> 4;
    sdesc->sorted = (pack & 0x08) >> 3;
    sdesc->gctbl_size = pack & 0x03;

    sdesc->bgcolor_idx = bys[5];
    sdesc->px_aspect = bys[6];
}

void parse_image_desc(struct img_desc *idesc, FILE *gif) {
    uint8_t bys[9];
    read_tag(bys, 9, gif);

    uint16_t *dims = (uint16_t*)bys;
    idesc->left = *dims++;
    idesc->top = *dims++;
    idesc->width = *dims++;
    idesc->height = *dims;

    uint8_t pack = bys[8];
    idesc->has_lctbl = pack >> 7;
    idesc->interlaced = (pack & 0x40) >> 6;
    idesc->sorted = (pack & 0x20) >> 5;
    idesc->lctbl_size = pack & 0x3;
}

int read_byte(FILE *gif) {
    int b = fgetc(gif);
    if (b == EOF) DIE_EOF(gif);
    return b;
}

void read_block(uint8_t *buf, int len, FILE *gif) {
    int rlen = fread(buf, 1, len, gif);
    if (rlen < len) DIE_EOF(gif);
}

struct ctable_ext {
    int cnt;
    int offsets[4096];
    struct buffer ctbl;
};

struct ctable_ext *alloc_ctable_ext(int buflen) {
    struct ctable_ext *cte = calloc(sizeof(struct ctable_ext) + buflen, 1);
    if (!cte)
        DIE("Failed to allocate ctable extension", errno);
    return cte;
}

void expand_ctable_ext(struct ctable_ext *ctbl_ext) {
    struct ctable_ext *cte = realloc(ctbl_ext, sizeof(struct ctable_ext) + ctbl_ext->ctbl.len * 2);
    if (!cte)
        DIE("Failed to expand ctable extension");
    return cte;
}

#define CTE_GET(cte, idx)                      \
    ((struct buffer*)(&cte->ctbl.buf + cte->offsets[idx]))


void decode_image(struct screen_desc *sdesc, struct img_desc *idesc,
                  struct buffer *ctbl, struct buffer* img, FILE *gif)
{
    int code_size;
    int blk_len;
    uint8_t codes[256];
    struct buffer *ctbl_ext = calloc(sizeof(struct ctable_ext) + 4096, 1);
    ctbl_ext->ctbl.len = 4096;
    int clear_code, eoi_code, inc_cs_flag;

    const int sc_row_len = sdesc->width * PXSIZE;
    const int img_row_len = idesc->width * PXSIZE;
    const int img_top = sc_row_len * idesc->top;
    const int img_left = idesc->left * PXSIZE;
    const int img_right = img_left + img_row_len;

    int img_idx = img_top + img_left;

    code_size = read_byte(gif) + 1;
    clear_code = 1 << (code_size - 1);
    eoi_code = clear_code + 1;
    ctbl_ext_idx = -1;
    inc_cs_flag = (1 << code_size) - 1;

    while ((blk_len = read_byte(gif))) {
        read_block(codes, blk_len, gif);
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
                        DIE("Premature end of sub-block", -1);
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
                        DIE("Past end of image data", -1);
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

int main() {
    FILE *gif = fopen("./emulogic.gif", "r");
    struct buffer *img;
    
    validate_header(gif);

    struct screen_desc sdesc;
    parse_screen_desc(&sdesc, gif);

    img = alloc_buffer(sdesc.width * sdesc.height * PXSIZE);

    struct buffer *gctbl = NULL;
    if (sdesc.has_gctbl) {
        read_ctable(gctbl, sdesc.gctbl_size, gif);
    }

    int cur;
    while ((cur = fgetc(gif)) != 0x3B && cur != EOF) {
        switch((uint8_t)cur) {
        case 0x21:
            DIE("Graphics control extension parsing not implemented. :-/", -1);
            break;
        case 0x2C:
            {
                struct buffer *ctbl = NULL;
                struct img_desc idesc;
                parse_image_desc(&idesc, gif);
                if (idesc.has_lctbl)
                    read_ctable(ctbl, idesc.lctbl_size, gif);
                else
                    ctbl = gctbl;

                if (!ctbl)
                    DIE("No color table", -1);

                decode_image(sdesc, idesc, ctbl, img, gif);

                if (ctbl != gctbl)
                    free(ctbl);
                break;
            }
        default:
            DIEF(-1, "Received 0x%2x - I don't know what to do", cur);
            break;
        }
    }

    int rd_err;
    if ((rd_err = ferror(gif)))
        DIE("Error reading file", rd_err);
    if (feof(gif))
        DIE("Premature EOF", -1);

    if (fgetc(gif) != EOF)
        fprintf(stderr, "Warning: extra data after end of GIF contents.");
    
    return 0;
}
