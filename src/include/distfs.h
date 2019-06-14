#ifndef DISTFS_H_
#define DISTFS_H_

struct distfs_s {
    struct peer_s *peer;
    struct {
        struct root_s  *local;
        struct group_s *remote;
        unsigned char   file[SHA256HEX];
    } blocks;
    struct list_s transactions;
    struct list_s jobs;
    struct {
        struct ev_timer jobs;
    } ev;
};

int dfs_transaction_add(struct distfs_s *dfs, char **argv, int argc);
int dfs_transaction_list(struct distfs_s *dfs, char **argv, int argc);
int dfs_transaction_share(struct distfs_s *dfs, char **argv, int argc);

#endif
