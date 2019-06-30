#ifndef CLI_H_
#define CLI_H_

struct module_cli_s {
    int (*init)(struct peer_s *p, char *line);
    struct {
        int  (*list)(struct peer_s *p, json_object **obj);
    } peers;
};


extern const struct module_cli_s cli;

#endif
