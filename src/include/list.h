#ifndef LIST_H_
#define LIST_H_

enum list_array_sort_e {
    LIST_ARRAY_SORT_STR,
};

struct list_s {
    struct list_internal_s *head;
    struct list_internal_s *tail;
    size_t                  size;
};

struct module_list_s {
    int (*init)(struct list_s *l);
    int (*add)(struct list_s *l, void *userdata, int (*clean)(void *ptr));
    int (*del)(struct list_s *l, void *userdata);
    int (*map)(struct list_s *l, int (*cb)(struct list_s*, void*, void*),
               void *userdata);
    int (*size)(struct list_s *l, int *sz);
    int (*toarray)(struct list_s *l, void ***dst, int *ndst);
    int (*toarray_sort)(struct list_s *l, void ***dst, int *ndst,
                        enum list_array_sort_e las);
    int (*clean)(struct list_s *l);
};

extern const struct module_list_s list;

#endif
