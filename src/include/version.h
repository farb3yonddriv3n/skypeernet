#ifndef VERISON_H_
#define VERSION_H_

struct module_version_s {
    int (*dump)(json_object **obj);
};

extern const struct module_version_s version;

#endif
