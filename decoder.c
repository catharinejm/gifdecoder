#include "decoder.h"

/* n is a byte offset from c->cur
 * pass n<0 to check backwards
 */
static inline int cur_inrange(struct bitcursor *c, int n) {
    uint8_t *addr = c->cur + n;
    return addr <= c->end && addr >= c->start;
}

/* pass n<0 to move backwards */
static inline int cur_move(struct bitcursor *c, int n) {
    if (cur_inrange(c, n)) {
        c->cur+=n;
        return n;
    }
    return 0;
}


void validate_header(struct bitcursor *gif) {
    if (!cur_inrange(gif, 6))
        DIE_EOF("Invalid header");
    if (memcmp(gif->cur, GIF89a, 6))
        DIE("Invalid header");
    gif->cur += 6;
}

struct buffer *alloc_buffer(int len) {
    struct buffer *buf = calloc(sizeof(struct buffer) + len, 1);
    if (!buf)
        DIE("Failed to allocate buffer");
    buf->len = len;
    return buf;
}

void read_ctable(struct buffer **ctbl, int ctbl_size, struct bitcursor *gif) {
    int ctbl_len = CTBL_LEN(ctbl_size);
    *ctbl = alloc_buffer(ctbl_len);

    if (cur_inrange(gif, ctbl_len)) {
        memcpy((*ctbl)->buf, gif->cur, ctbl_len);
        gif->cur += ctbl_len;
    } else
        DIE_EOF("Failed to read ctable");
}

void copy_block(uint8_t *buf, int len, struct bitcursor *gif) {
    if (gif->bit)
        DIE("Unaligned block copy");
    if (gif->cur + len < gif->end)
        memcpy(buf, gif->cur, len);
    else
        DIE_EOF("Failed to copy block");
}

static inline uint8_t get_byte(struct bitcursor *c) {
    if (c->bit)
        DIE("Unaligned byte read");
    if (c->cur < c->end)
        return *c->cur++;
    DIE_EOF("Getting byte");
    return -1;
}

void parse_screen_desc(struct screen_desc *sdesc, struct bitcursor *gif) {
    if (!cur_inrange(gif, 7))
        DIE_EOF("Invalid screen description");

    sdesc->width = *gif->cur16++;
    sdesc->height = *gif->cur16++;

    uint8_t pack = *gif->cur++;
    sdesc->has_gctbl = pack >> 7;
    sdesc->color_res = (pack & 0x70) >> 4;
    sdesc->sorted = (pack & 0x08) >> 3;
    sdesc->gctbl_size = pack & 0x07;

    sdesc->bgcolor_idx = *gif->cur++;
    sdesc->px_aspect = *gif->cur++;
}

void parse_image_desc(struct img_desc *idesc, struct bitcursor *gif) {
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


void decode_image(struct screen_desc *sdesc, struct img_desc *idesc,
                  struct buffer *ctbl, struct buffer* img, struct bitcursor *gif)
{
    int code_size;
    int blk_len;
    struct ctable_ext *ctbl_ext = calloc(sizeof(struct ctable_ext) + 4096, 1);
    ctbl_ext->ctbl.len = 4096;
    int clear_code, eoi_code, inc_cs_flag, prev_code = 0;

    const int sc_row_len = sdesc->width * PXSIZE;
    const int img_row_len = idesc->width * PXSIZE;
    const int img_top = sc_row_len * idesc->top;
    const int img_left = idesc->left * PXSIZE;
    const int wrap_offset = sc_row_len - img_row_len;

    int img_row_end = img_left + img_row_len;
    int img_idx = img_top + img_left;

    const int min_code_size = get_byte(gif) + 1;
    code_size = min_code_size;
    clear_code = 1 << (code_size - 1);
    eoi_code = clear_code + 1;
    inc_cs_flag = (1 << code_size) - 1;

    while ((blk_len = get_byte(gif))) {
        if (! cur_inrange(&gif, blk_len))
            DIE_EOF("Reading sub-block");

        struct bitcursor codes;
        bitcursor_init(&codes, gif->cur, blk_len);
        uint16_t code;
        if (bitcursor_upto16(&codes, code_size, &code) == code_size) {
            if (code == clear_code) {
                free(ctbl_ext);
                ctbl_ext = calloc(sizeof(struct ctable_ext) + 4096, 1);
                ctbl_ext->ctbl.len = 4096;
                code_size = min_code_size;
                continue;
            }
            int coff = code * PXSIZE;
            if (coff < ctbl->len) {
                memcpy(&img->buf+img_idx, &ctbl->buf+coff, PXSIZE);
                img_idx += PXSIZE;
                if (img_idx >= img_row_end) {
                    img_idx += wrap_offset;
                    img_row_end += sc_row_len;
                }
                if (img_idx >= img->len || img_row_end >= img->len)
                    DIE("Past end of image data");
                if (prev_code) { // will be 0 on init
                    int poff = prev_code * PXSIZE;
                    if (poff < ctbl->len) {
                        if (ctbl_ext->ctbl.len + sizeof(struct buffer)+6
                    }
                }
            } else {
                struct buffer *cte_entry = CTE_GET(ctbl_ext, coff-ctbl->len);
                int copied = 0;
                while (img_idx + cte_entry->len - copied > img_row_end) {
                    // copy portion that fits into img
                    int fits = img_row_end - img_idx;
                    memcpy(&img->buf+img_idx, cte_entry->buf+copied, fits);
                    copied += fits;
                    // wrap img_idx and img_row_end
                    img_idx += wrap_offset + fits;
                    img_row_end += sc_row_len;
                    if (img_idx >= img->len || img_row_end >= img->len)
                        DIE("Past end of image data");
                }
                if (copied < cte_entry->len) {
                    // copy remainder (or possibly first go)
                    memcpy(&img->buf+img_idx, cte_entry->buf+copied, cte_entry->len-copied);
                    // bump img_idx, wrap if necessary
                    img_idx += cte_entry->len-copied;
                    if (img_idx >= img_row_end) {
                        img_idx += wrap_offset;
                        img_row_end += sc_row_len;
                    }
                    if (img_idx >= img->len || img_row_end >= img->len)
                        DIE("Past end of image data");
                }
            }

            // increase codesize if necessary
            if (code == inc_cs_flag) {
                code_size++;
                clear_code = 1 << (code_size - 1);
                eoi_code = clear_code + 1;
                inc_cs_flag = (1 << code_size) - 1;
            }
            // remember previous code
            prev_code = code;
        } else {
            if (codes.cur+1 == codes.end)
                gif->cur = codes.end;
            else
                DIE_EOF("Premature end of sub-block");
        }
    }
}

void load_file(const char *fname, struct bitcursor *c) {
    int fd = open(fname, O_RDONLY);
    if (-1 == fd)
        DIE("Error opening file");
    struct stat st;
    if (fstat(fd, &st) == -1)
        DIE("Failed to stat file");
    /* TODO: Don't map huge files! */
    uint8_t *buf = mmap(NULL, st.st_size, PROT_READ, MAP_PRIVATE, fd, 0);
    if (MAP_FAILED == buf)
        DIE("Failed to mmap file");
    bitcursor_init(c, buf, st.st_size);
}

void parse_gif(struct bitcursor *gif) {
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
