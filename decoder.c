#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>

#define GIF89a "GIF89a"

#define DIE(msg, code)                               \
    do {                                             \
        fprintf(stderr, msg "\n");                   \
        exit(code);                                  \
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

void read_tag(uint8_t *tagbuf, int len, FILE *gif) {
    int rcnt = fread(tagbuf, 1, len, gif);
    if (rcnt < len) {
        int e;
        if (feof(gif))
            DIE("Premature EOF", -1);
        if (e = ferror(gif))
            DIE("Error reading file", e);
    }
}

void read_ctable(uint8_t *ctbl, int ctbl_size, FILE *gif) {
    int ctbl_len = (1 << ctbl_size) * 3;
    ctbl = calloc(ctbl_len, 1);
    if (!ctbl)
        DIE("Failed to allocate color table", errno);

    int rcnt = fread(ctbl, 1, ctbl_len, gif);
    if (rcnt < ctbl_len) {
        int rd_err;
        if (feof(gif))
            DIE("Premature EOF", -1);
        if (rd_err = ferror(gif))
            DIE("Error reading file", rd_err);
    }
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
    idesc->left = dims++;
    idesc->top = dims++;
    idesc->width = dims++;
    idesc->height = dims;

    uint8_t pack = bys[8];
    idesc->has_lctbl = pack >> 7;
    idesc->interlaced = (pack & 0x40) >> 6;
    idesc->sorted = (pack & 0x20) >> 5;
    idesc->lctbl_size = pack & 0x3;
}

int main() {
    FILE *gif = fopen("./emulogic.gif", "r");
    int rd_err;
    
    validate_header(gif);

    struct screen_desc sdesc;
    parse_screen_desc(&sdesc, gif);
    
    uint8_t *gctbl = NULL;
    if (sdesc->has_gctbl) {
        read_ctable(gctbl, sdesc->gctbl_size, gif);
    }

    int cur = fgetc(gif);
    while (cur != EOF) {
        switch((uint8_t)cur) {
        0x21:
            DIE("Graphics control extension parsing not implemented. :-/", -1);
            break;
        0x2C:
            {
                uint8_t *ctbl;
                struct img_desc idesc;
                parse_image_desc(&idesc, gif);
                if (idesc->has_lctbl)
                    read_ctable(ctbl, idesc->lctbl_size, gif);
                else
                    ctbl = gctbl;

                
                
                break;
            }
        default:
            DIE("I don't know what to do.", -1);
        }
    }

    if (rd_err = ferror(gif))
        DIE("Error reading file", rd_err)

    return 0;
}
