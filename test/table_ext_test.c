#include "test.h"

#include "../table_ext.h"

DEFTEST(test_table_ext_alloc, "allocating a new table") {
    struct table_ext *tbx = table_ext_alloc(50);
    if (! tbx) {
        SET_MESSAGE("Failed to allocate table_ext");
        return ERROR;
    }
    if (tbx->cap != 50) {
        SET_MESSAGE("Improperly set initial capacity");
        return FAIL;
    }
    if (tbx->cnt != 0) {
        SET_MESSAGE("Initial count is not zero");
        return FAIL;
    }
    if (tbx->fill != 0) {
        SET_MESSAGE("Initial fill is not zero");
        return FAIL;
    }
    table_ext_free(tbx);
    return PASS;
}

DEFTEST(test_table_ext_new_entry_with_space, "adding an entry to a table when there is space") {
    struct table_ext *tbx = table_ext_alloc(10);
    if (! tbx) {
        SET_MESSAGE("Failed to allocate table_ext");
        return ERROR;
    }
    struct tbx_entry *entry = table_ext_new_entry(&tbx, 5);
    if (! entry) {
        SET_MESSAGE("Failed to allocate new entry");
        return ERROR;
    }

    if (tbx->cnt != 1) {
        SET_MESSAGE("Improperly incremented table count");
        return FAIL;
    }
    if (tbx->fill != sizeof(struct tbx_entry)+5) {
        SET_MESSAGE("Improperly increased fill");
        return FAIL;
    }
    if (tbx->cap != 10) {
        SET_MESSAGE("Capacity increased prematurely");
        return FAIL;
    }
    if (entry->len != 5) {
        SET_MESSAGE("Improperly set entry length");
        return FAIL;
    }
    if ((void*)entry != (void*)tbx->buf) {
        SET_MESSAGE("entry is at wrong address");
        return FAIL;
    }
    return PASS;
}
