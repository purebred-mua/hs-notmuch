#include <talloc.h>

void *wrap_talloc_steal(void *new_ctx, void *ptr) {
    return talloc_steal(new_ctx, ptr);
}
