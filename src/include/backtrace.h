#ifndef BACKTRACE_H_
#define BACKTRACE_H_

#define BT_ADD backtrace.add(__FILE__, __LINE__);

struct module_backtrace_s {
    int (*init)();
    int (*clean)();
    void (*add)(const char *filename, int line);
    void (*show)();
};

extern const struct module_backtrace_s backtrace;

#endif
