#ifndef LIST_H_
#define LIST_H_

#define LISTADD(m_list, m_column, m_key, m_nkey) \
    list.column.map(m_list, m_column, m_key, m_nkey, \
                    li, sizeof(li))

enum list_array_sort_e {
    LIST_ARRAY_SORT_STR,
};

struct list_param_s {
    const char *name;
};

struct list_column_s {
    char name[128];
    struct ht_s *ht;
};

struct list_s {
    struct list_internal_s *head;
    struct list_internal_s *tail;
    size_t                  size;
    struct {
        struct list_column_s *columns;
        int                  ncolumns;
    } filter;
};

struct module_list_s {
    int (*init)(struct list_s *l);
    int (*add)(struct list_s *l, void *userdata,
               int (*clean)(void *ptr));
    int (*add_head)(struct list_s *l, void *userdata, int (*clean)(void *ptr));
    int (*del)(struct list_s *l, void *userdata);
    int (*map)(struct list_s *l, int (*cb)(struct list_s*, void*, void*),
               void *userdata);
    int (*size)(struct list_s *l, int *sz);
    int (*toarray)(struct list_s *l, void ***dst, int *ndst);
    int (*toarray_sort)(struct list_s *l, void ***dst, int *ndst,
                        enum list_array_sort_e las);
    int (*reset)(struct list_s *l);
    int (*clean)(struct list_s *l);
    int (*queue_add)(struct list_s *l, void *userdata, int (*clean)(void *ptr));
    struct {
        int (*init)(struct list_s *l, struct list_param_s *columns,
                    int ncolumns);
        int (*add)(struct list_s *l, void *userdata,
                   int (*columns)(struct list_s *l, void *li, void *userdata),
                   int (*clean)(void *ptr));
        int (*map)(struct list_s *l, const char *column,
                   const void *key, const int nkey,
                   const void *value, const int nvalue);
        int (*find)(struct list_s *l, const char *column,
                    void *key, const int nkey,
                    int (*cb)(struct list_s *l, void *value,
                              const int nvalue, void **userdata),
                    void **userdata);
        int (*del)(struct list_s *l, const char *column,
                   void *key, int nkey);
    } column;
};

extern const struct module_list_s list;

#endif
