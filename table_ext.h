#ifndef _TABLE_EXT_H_
#define _TABLE_EXT_H_

#include <stdlib.h>

struct table_ext {
    int cnt; //number of entries
    int cap; // capacity in bytes
    int fill; // bytes in use
    int *offsets;
    uint8_t buf[0]; // entry pool
};

struct tbx_entry {
    uint16_t len; // length of the record in bytes
    uint8_t buf[0]; // entry data
};

struct table_ext *table_ext_alloc(int initial_cap);
void table_ext_free(struct table_ext *tbx);

struct table_ext *table_ext_extend(struct table_ext* tbx);

// Adds a new entry of size blen, returns a pointer to that entry for
// manipulation. Requires a pointer-to-pointer of table_ext in case a
// realloc is necessary. If reallocation happens, the value of *tbx
// may be altered. If realloc fails, table_ext_new_entry returns NULL
// and does not modify *tbx.
struct tbx_entry *table_ext_new_entry(struct table_ext** tbx, int blen);

struct tbx_entry *table_ext_get(struct table_ext* tbx, int index);

#endif
