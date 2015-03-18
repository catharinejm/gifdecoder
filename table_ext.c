#include "table_ext.h"

struct table_ext *table_ext_alloc(int initial_cap) {
    struct table_ext *tbx = malloc(sizeof(struct table_ext) + initial_cap);
    if (!tbx)
        return NULL;
    int *offs = malloc(256);
    if (! offs) {
        free(tbx);
        return NULL;
    }
    *offs = 255;
    tbx->offsets = offs;
    tbx->cnt = 0;
    tbx->cap = initial_cap;
    tbx->fill = 0;
    return tbx;
}

void table_ext_free(struct table_ext *tbx) {
    free(tbx->offsets);
    free(tbx);
}

struct table_ext *table_ext_extend(struct table_ext *tbx) {
    struct table_ext *new_tbx = realloc(tbx, sizeof(struct table_ext) + 2*tbx->cap);
    if (!new_tbx)
        return NULL;
    new_tbx->cap *= 2;
    return new_tbx;
}

struct tbx_entry *table_ext_new_entry(struct table_ext **tbx_p, int blen) {
    int extra_size = sizeof(struct tbx_entry)+blen;
    if ((*tbx_p)->fill + extra_size >= (*tbx_p)->cap) {
        struct table_ext *new_tbx = table_ext_extend(*tbx_p);
        if (! new_tbx)
            return NULL;
        *tbx_p = new_tbx;
    }
    struct table_ext *tbx = *tbx_p;
    if (tbx->cnt+1 >= *tbx->offsets) {
        int new_len = (*tbx->offsets + 1)*2;
        int *new_offs = realloc(tbx->offsets, new_len);
        if (!new_offs)
            return NULL;
        *new_offs = new_len-1;
        tbx->offsets = new_offs;
    }
    int off = tbx->fill;
    tbx->offsets[tbx->cnt+1] = off;
    struct tbx_entry *entry = (struct tbx_entry*)(tbx->buf + off);
    entry->len = blen;
    tbx->fill += extra_size;
    tbx->cnt++;
    return entry;
}

struct tbx_entry *table_ext_get(struct table_ext* tbx, int index) {
    if (index >= tbx->cnt)
        return NULL;
    if (index < 0)
        index = tbx->cnt - index;
    if (index < 0)
        return NULL;
    return (struct tbx_entry*)(tbx->buf + tbx->offsets[index+1]);
}
