#ifndef BACKTRACE_H_
#define BACKTRACE_H_

struct module_backtrace_s {
    int (*init)();
    int (*clean)();
    void (*add)(const char *filename, int line);
    void (*show)();
};

extern const struct module_backtrace_s backtrace;

#endif
