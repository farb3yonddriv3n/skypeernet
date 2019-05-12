#ifndef LIST_H_
#define LIST_H_

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
    int (*clean)(struct list_s *l);
};

extern const struct module_list_s list;

#endif
